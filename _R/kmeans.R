library(tidyverse)

Posdef <- function (n, ev = runif(n, 0, 10)) {
  # https://stat.ethz.ch/pipermail/r-help/2008-February/153708
  Z <- matrix(ncol = n, rnorm(n ^ 2))
  decomp <- qr(Z)
  Q <- qr.Q(decomp)
  R <- qr.R(decomp)
  d <- diag(R)
  ph <- d / abs(d)
  O <- Q %*% diag(ph)
  Z <- t(O) %*% diag(ev) %*% O
  return(Z)
}

# data --------------------------------------------------------------------
data_gen <- function(n = 1000, groups = 3) {
  
  seq_len_groups <- seq_len(groups)
  
  mus <- purrr::map(seq_len_groups, ~ runif(2, min = -10, max = 10))
  
  sigmas <- purrr::map(seq_len_groups, function(k){
    
    Posdef(2)
    
  })
  
  props <- rbeta(groups, 5, 5)
  props <- props/sum(props)
  
  ns <- round(n * props)
  
  dpars <- tibble(
    n = ns,
    mu = mus,
    Sigma = sigmas
  )
  
  data <- pmap(dpars, MASS::mvrnorm) |> 
    map(as_tibble, .name_repair = "minimal") |> 
    map(set_names, c("x", "y")) |> 
    map2_df(seq_len_groups, ~ mutate(.x, group = .y, .before = 1))
  
  data
  
}


ggplot(data_gen(), aes(x, y)) +
  geom_point(aes(color = factor(group)))


set.seed(12345) # buena!! 

set.seed(12) # muy perfecto 

data <- data_gen()

ggplot(data, aes(x, y)) +
  geom_point(aes(color = factor(group)))


# adapted from
# https://theanlim.rbind.io/post/clustering-k-means-k-means-and-gganimate/#k-means-clustering-1
kmeans_iterations <- function (data, k = 3, tolerance = 10e-6, seed = 1001){
  
  set.seed(seed)
  
  daux <- data |> 
    mutate(id = row_number(), .before = 1) |> 
    mutate(cluster = 0)
  
  dcenters <- daux |> 
    sample_n(3) |> 
    select(cx = x, cy = y) |> 
    arrange(cx, cy) |> 
    mutate(cluster = row_number(), .before = 1)
  
  center_hist <- list(dcenters)
  data_hist   <- list(daux)
  
  while(TRUE){
    
    message("hey")
    
    daux <- crossing(
      daux |> select(id, group, x, y),  
      dcenters
      ) |> 
      mutate(dist = (x - cx)^2 + (y - cy)^2) |> 
      group_by(id) |> 
      # new cluster
      filter(dist == min(dist)) |> 
      ungroup()
    
    daux
    # daux |> count(group, cluster)
    
    dcenters <- daux |> 
      group_by(cluster) |> 
      # new centers
      summarise(cx = mean(x), cy = mean(y))
    
    dcenters
    
    daux <- daux |> 
      select(id, group, x, y, cluster)
    
    daux
    
    center_hist <- append(center_hist, list(dcenters))
    data_hist   <- append(data_hist, list(daux))
    
    
    c1 <- last(center_hist) |> select(cx, cy) |> as.matrix()
    c2 <- nth(center_hist, -2) |> select(cx, cy) |> as.matrix()
    
    if(mean((c1 - c2)^2) < tolerance) break
    
  }
  
  length(center_hist)
  length(data_hist)
  
  seq_along_i <- seq_len(length(center_hist))
  
  center_hist <- map2_df(center_hist, seq_along_i, ~ mutate(.x, iteration = .y, .before = 1))
  
  data_hist <- map2_df(data_hist, seq_along_i, ~ mutate(.x, iteration = .y, .before = 1))
  
  list(data_hist = data_hist, center_hist = center_hist)
 
}

res <- kmeans_iterations(data, k = 3)

data_hist   <- res$data_hist
center_hist <- res$center_hist 

library(gganimate) # install.packages("transformr")
library(ggforce) # install.packages("transformr")

ggplot(center_hist, aes(cx, cy)) +
  geom_point() +
  geom_path(aes(group = cluster))

ggplot() +
  geom_point(data = data_hist, aes(x, y, group = id, color = as.character(cluster)), size = 3, alpha = 0.1) +
  geom_point(data = center_hist, aes(cx, cy, group = cluster, color = as.character(cluster)), size = 5) +
  geom_voronoi_segment(data = center_hist, aes(cx, cy, color = as.character(cluster)), size = 5) +
  facet_wrap(vars(iteration))



data <- data.frame(
  x = runif(6),
  y = runif(6),
  state = rep(c('a', 'b'), 3)
)

ggplot(data, aes(x =x, y = y)) + 
  geom_voronoi_tile(fill = 'grey', colour = 'black', bound = c(0, 1, 0, 1)) + 
  geom_point() + 
  facet_wrap(~state)

ggplot(center_hist, aes(x =cx, y = cy)) + 
  geom_voronoi_tile(fill = 'grey', colour = 'black', bound = c(-10, 10, -10, 10)) +
  geom_point() + 
  facet_wrap(~iteration)

ggplot(center_hist, aes(x =cx, y = cy)) + 
  geom_voronoi_tile(fill = 'grey', colour = 'black', bound = c(-10, 10, -10, 10)) +
  geom_point() + 
  transition_states(iteration, transition_length = 2, state_length = 1)


data <- split(center_hist, center_hist$iteration)

data <- tweenr::tween_state(data[[1]], data[[2]], 'cubic-in-out', 40) %>% 
  tweenr::keep_state(3) %>% 
  tweenr::tween_state(data[[1]],'cubic-in-out', 40) %>% 
  tweenr::keep_state(3) |>
  as_tibble()

data
data$.frame <- round(scales::rescale(data$.frame , to = c(1, 100)))

ggplot(data, aes(x = cx, y = cy)) + 
  geom_voronoi_tile(fill = 'grey', colour = 'black', bound = c(-10, 10, -10, 10)) + 
  geom_point() + 
  transition_manual(.frame)











ggplot(data, aes(x = x, y = y)) + 
  geom_voronoi_tile(fill = 'grey', colour = 'black', bound = c(0, 1, 0, 1),
                    by.group = TRUE) + 
  geom_point() + 
  transition_states(state, transition_length = 3, state_length = 1) + 
  ease_aes('cubic-in-out')















ggplot() +
  
  transition_states(iteration, transition_length = 2, state_length = 1)


ggplot() +
  
  geom_point(data = data_hist, aes(x, y, group = id, color = factor(cluster)), size = 3, alpha = 0.2) +
  
  geom_point(data = center_hist, aes(cx, cy, group = cluster, color = factor(cluster)), size = 5) +
  
  transition_states(iteration, transition_length = 2, state_length = 1)


library(tidyverse)
library(gganimate) # install.packages("transformr")


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

data <- data_gen()

ggplot(data, aes(x, y)) +
  geom_point(aes(color = factor(group)))


set.seed(124) # muy perfecto 

data <- data_gen()

ggplot(data, aes(x, y)) +
  geom_point(aes(color = factor(group)))


# adapted from
# https://theanlim.rbind.io/post/clustering-k-means-k-means-and-gganimate/#k-means-clustering-1
kmeans_iterations <- function (data, k = 3, tolerance = 10e-6, seed = 123, iterations = 15){
  
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
  
  iteration <- 1
  
  while(TRUE){
    
    message(iteration)
    
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
    
    # if(mean((c1 - c2)^2) < tolerance) break
    iteration <- iteration + 1
    if(iteration == iterations) break
    
  }
  
  length(center_hist)
  length(data_hist)
  
  seq_along_i <- seq_len(length(center_hist))
  
  center_hist <- map2_df(center_hist, seq_along_i, ~ mutate(.x, iteration = .y, .before = 1))
  
  data_hist <- map2_df(data_hist, seq_along_i, ~ mutate(.x, iteration = .y, .before = 1))
  
  list(data_hist = data_hist, center_hist = center_hist)
 
}

kmi <- kmeans_iterations(data, k = 3)

animtate_kmeans <- function(kmi){
  
  data_hist <- kmi$data_hist |> 
    mutate(
      cluster = factor(cluster),
      cluster = fct_recode(cluster, "Start" = "0" ),
      group = factor(group)
      )
  
  data_hist |> count(cluster)
  
  center_hist <- kmi$center_hist |> 
    mutate(cluster = factor(cluster))
  
  k <- center_hist |> 
    count(cluster) |> 
    nrow()
  
  # ggplot(center_hist, aes(cx, cy)) +
  #   geom_point() +
  #   geom_path(aes(group = cluster))
  
  colors <- viridisLite::viridis(k, begin = 0.1, end = .9)
  
  colors <- set_names(colors, seq_len(k))
  
  colors <- c("Start" = "gray70", colors)
  
   # scales::show_col(colors)
  
  p <- ggplot() +
    geom_point(
      data = data_hist, 
      aes(x, y, group = id, color = cluster, shape = group),
      size = 3,
      alpha = 0.5
      ) +
    # kunstomverse::geom_point2
    geom_point(
      data = center_hist, 
      aes(cx, cy, group = cluster, fill = cluster),
      size = 6,
      alpha = 1,
      shape = 21,
      ) +
    labs(shape = "(Original) Group") +
    scale_color_manual(values = colors, name = "(Assigned) Cluster") +
    scale_fill_manual(values = colors, name = "(Assigned) Cluster") +
    facet_wrap(vars(iteration)) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  p
  
  pa <- p +
    # remove facet
    facet_null() +
    # then animate
    labs(
      title = "Iteration {closest_state}",
      caption = "Frame {frame} of {nframes}"
      ) +
    ease_aes("cubic-in-out") +
    # shadow_wake(wake_length = 0.2, alpha = 0.1) +
    transition_states(iteration, transition_length = 2, state_length = 1)
  
  
  # https://stackoverflow.com/questions/60254655/gganimate-package-argument-fps-must-be-a-factor-of-100
  # Finally I figure this out: I remove the package: magick that is installed before,
  # and reinstall gganimate, and also the package gifski
  animate(pa, fps = 30, duration = 10, width = 1000, height = 800, start_pause = 20, end_pause = 20)
  
}

an <- animtate_kmeans(kmi)
an

an2 <- animtate_kmeans(kmeans_iterations(data, k = 3, seed = 12))
an2


str(an)

library(magick)

a_mgif <- image_read(an)
b_mgif <- image_read(an2)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))

for(i in 2:length(a_mgif)){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}

new_gif



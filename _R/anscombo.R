# setup -------------------------------------------------------------------
library(tidyverse)
library(patchwork)

# set 1 -------------------------------------------------------------------
get_set1 <- function(n = 50, seed = 1234, sd = 0.1, b0 = 3, b1 = 0.5, meanx = 5,
                     x_dist = "norm"){

  set.seed(seed)

  x <- sort(rnorm(n, meanx))

  if(x_dist == "unif"){
    x <- sort(runif(n, min = min(x), max = max(x))  )
  }

  e <- rnorm(n, sd)
  y <- b0 + b1 * x + e

  df <- tibble(x, y)

  df

}

df <- get_set1(n = 100, sd = 1)
df <- get_set1(n = 100, sd = 0.1, b1 = 1, x_dist = "unif")

# plot helper -------------------------------------------------------------
plot_xy <- function(df){

  cor_xy <- cor(df[["x"]], df[["y"]])

  mod <- lm(y ~ x, data = df)

  b <- coefficients(mod)
  b0 <- round(b[1], 2)
  b1 <- round(b[2], 2)

  ggplot(df, aes(x, y)) +
    geom_point(shape = 21, color = "gray60", fill = "gray80") +
    geom_smooth(method = "lm", se = FALSE, color = "gray40") +
    xlim(c(0, NA)) +
    ylim(c(0, NA)) +
    labs(title = str_glue("Model: y = {b0} + {b1} x"))

}

plot_xy(df)

# set 2 -------------------------------------------------------------------



# set 3 -------------------------------------------------------------------
# outliers:
# x  will be same
# b1_3 will be 0.5 times b1
# function take tail 2*prop to random select prop elements
# and add some value to get same b1
# addtionaliy with factor_res will reduce de variance of residual to
# get a more visual efect of the outliers
get_set3 <- function(df, prop = .05, factor_res = 0.25){

  # pars
  modlm <- lm(y ~ x, data = df)
  b <- coefficients(modlm)
  e <- modlm$residuals
  n <- length(e)

  # creating y3 1st ver
  y3 <- b[1] + (b[2] * 0.5) * df[["x"]] + e * factor_res

  df[["y3"]] <- y3

  ggplot(data = df, aes(x)) +
    xlim(c(0, NA)) +
    ylim(c(0, NA)) +
    geom_point(aes(y = y)) +
    geom_smooth(aes(y = y), method = "lm", se = FALSE) +

    geom_point(aes(y = y3)) +
    geom_smooth(aes(y = y3), method = "lm", se = FALSE)

  ids <- sample(seq(round((1 - 2*prop) * n), n), round(n * prop))

  f_to_optim <- function(value = 0){

    y3_new <- y3
    y3_new[ids] <- y3_new[ids] + value
    y3_mod <- lm(y3_new ~ x, data = tibble(x = df[["x"]], y3_new))

    (b[2] - coefficients(y3_mod)[2])^2

  }

  value <- suppressWarnings(optim(0, f_to_optim)$par)

  # apply the noise
  df[["y3"]][ids] <- df[["y3"]][ids] + value

  ggplot(data = df, aes(x)) +
    xlim(c(0, NA)) +
    ylim(c(0, NA)) +
    geom_point(aes(y = y)) +
    geom_smooth(aes(y = y), method = "lm", se = FALSE) +
    geom_point(aes(y = y3), color = "darkred") +
    geom_smooth(aes(y = y3), method = "lm", se = FALSE)


  # fix mean adding differences between intercept
  # y3_new <- y3_new - (mean(y) - mean(y3_new)) + b[0]
  df[["y3"]] <- df[["y3"]] + (
    lm(y ~ x, df)$coefficients[1] -
      lm(y3 ~ x, df)$coefficients[1]
  )

  ggplot(data = df, aes(x)) +
    xlim(c(0, NA)) +
    ylim(c(0, NA)) +
    geom_point(aes(y = y)) +
    geom_smooth(aes(y = y), method = "lm", se = FALSE) +

    geom_point(aes(y = y3), color = "darkred") +
    geom_smooth(aes(y = y3), method = "lm", se = FALSE)

  df |>
    select(x, y = y3)

}

df3 <- get_set3(df)

plot_xy(df3)

plot_xy(get_set3(df, prop = 0.05))


# set 4 -------------------------------------------------------------------
# cluster
# x4 will be concentrate to some point, default to be between q45(x) and q50(x)
# and random order
#
# then pick some x4s and translate to left to have mean(x) = mean(x4)
#
get_set4 <- function(df, rescale_to = c(.10, .20), prop = 0.15){

  modlm <- lm(y ~ x, data = df)
  b <- coefficients(modlm)
  e <- modlm$residuals
  n <- length(e)

  qs <- quantile(df$x, rescale_to)

  df <- df |>
    mutate(
      x4 = sample(x),
      x4 = scales::rescale(x4, to = qs)
      )

  ggplot(data = df) +
    xlim(c(0, NA)) +
    ylim(c(0, NA)) +
    geom_point(aes(x, y)) +
    geom_smooth(aes(x, y), method = "lm", se = FALSE) +

    geom_point(aes(x4, y), color = "darkred") +
    geom_smooth(aes(x4, y), method = "lm", se = FALSE)

  ids <- sample(n, size = round(n * prop))

  f_to_optim <- function(value = 0){

    x4_new <- df$x4
    x4_new[ids] <- x4_new[ids] + value

    (mean(df$x) - mean(x4_new))^2

  }

  value <- suppressWarnings(optim(0, f_to_optim)$par)

  df <- df |>
    mutate(
      x4 = if_else(row_number() %in% ids, x4 + value, x4),
      # y4 = if_else(row_number() %in% ids, scales::rescale(y, quantile(y, c(.3, .7))), y)
    )

  ggplot(data = df) +
    xlim(c(0, NA)) +
    ylim(c(0, NA)) +
    geom_point(aes(x, y)) +
    geom_smooth(aes(x, y), method = "lm", se = FALSE) +

    geom_point(aes(x4, y), color = "darkred") +
    geom_smooth(aes(x4, y), method = "lm", se = FALSE)


  f_to_optim <- function(value = 10){

    y4_new <- df$y

    y4_new[ ids] <- y4_new[ ids] + value
    y4_new[-ids] <- y4_new[-ids] - value

    xy4_mod <- lm(y4 ~ x, data = tibble(x = df$x4, y4 = y4_new))

    # plot(df$x4, y4_new)

    (b[2] - coefficients(xy4_mod)[2])^2

  }

  value <- suppressWarnings(optim(0, f_to_optim)$par)

  df[["y4"]] <- df[["y"]]
  df[["y4"]][ ids] <- df[["y4"]][ ids] + value
  df[["y4"]][-ids] <- df[["y4"]][-ids] - value

  df[["y4"]] <- df[["y4"]] + (
    lm(y ~ x, df)$coefficients[1] -
      lm(y4 ~ x4, df)$coefficients[1]
  )

  ggplot(data = df) +
    xlim(c(0, NA)) +
    ylim(c(0, NA)) +
    geom_point(aes(x, y)) +
    geom_smooth(aes(x, y), method = "lm", se = FALSE) +

    geom_point(aes(x4, y4)) +
    geom_smooth(aes(x4, y4), method = "lm", se = FALSE)

 df |>
   select(x = x4, y = y4)

}

df4 <- get_set4(df)

plot_xy(df4)

plot_xy(get_set4(df, prop = .5))
plot_xy(get_set4(df, prop = .2))


# set 5 -------------------------------------------------------------------
# heteroscedasticity
get_set5 <- function(df, fun_var = identity){

  modlm <- lm(y ~ x, data = df)
  b <- coefficients(modlm)
  e <- modlm$residuals
  n <- length(e)

  e_new <- e * fun_var(1:length(e))

  e_new <- scales::rescale(
    e_new,
    to = c(min(abs(e)), max(abs(e)))
  )

  y5 <- b[1] + b[2] * df[["x"]] + e_new

  # take las value to get same b1
  f_to_optim <- function(value = 0){

    y5_new <- y5
    y5_new[length(y5)] <- y5_new[length(y5)] + value

    y5_mod <- lm(y5_new ~ x, data = tibble(x = df[["x"]], y5_new))

    (b[2] - coefficients(y5_mod)[2])^2

  }

  value <- suppressWarnings(optim(0, f_to_optim)$par)

  y5[length(y5)] <- y5[length(y5)] + value

  df[["y5"]] <- y5

  df[["y5"]] <- df[["y5"]] + (
    lm(y ~ x, df)$coefficients[1] -
      lm(y5 ~ x, df)$coefficients[1]
  )

  ggplot(data = df, aes(x)) +
    xlim(c(0, NA)) +
    ylim(c(0, NA)) +
    # geom_point(aes(y = y)) +
    # geom_smooth(aes(y = y), method = "lm", se = FALSE) +

    geom_point(aes(y = y5), color = "darkred") +
    geom_smooth(aes(y = y5))

  df |>
    select(x, y = y5)

}

df5 <- get_set5(df)

plot(get_set5(df))
plot(get_set5(df, fun_var = sin))


# check -------------------------------------------------------------------
sets <- list(df, df3, df4, df5)

df_all <- map2_df(
  sets,
  c(1, 3, 4, 5),
  ~ mutate(.x, set = .y)
)

df_all |>
  group_by(set) |>
  summarise(
    cor(x, y),
    mean(x),
    var(x),
    mean(y),
    var(y)
  )

df_mods <- dfall |>
  group_nest(set) |>
  mutate(
    model = map(data, lm, formula = y ~ x),
    value = map(model, coefficients),
    coef  = map(value, names)
  )

df_mods

df_mods |>
  select(-data, -model) |>
  unnest(cols = c(value, coef)) |>
  pivot_wider(names_from = coef, values_from = value)

ggplot(dfall, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(vars(set))

ggplot(dfall, aes(x, y, color = as.character(set))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d(option = "A", begin = 0.2, end = 0.8)

ggplot(dfall, aes(x, y, color = as.character(set))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d(option = "A", begin = 0.2, end = 0.8) +
  facet_wrap(vars(set))



# performance -------------------------------------------------------------
# install.packages("performance")
library(performance)


performance::check_model(lm(y ~ x, data = df5))

performance::check_model(lm(y ~ x, data = df))


df_mods |>
  pull(model) |>
  map(performance::check_model)

















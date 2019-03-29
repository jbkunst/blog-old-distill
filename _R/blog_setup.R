# general knitr option ----------------------------------------------------
knitr::opts_chunk$set(
  layout = "l-page",
  fig.path = "images/",
  cache.path = ".cache/",
  dpi = 72*2,
  fig.align = "center"
)

# ggplot2 -----------------------------------------------------------------
library(ggplot2)
library(extrafont)
# extrafont::font_import()
suppressMessages(suppressWarnings(loadfonts()))

theme_set(theme_minimal(base_family = "Segoe UI", base_size = 7))

# highcharter -------------------------------------------------------------
library(highcharter)

options(
  highcharter.theme = 
    hc_theme_smpl(
      chart = list(style = list(fontFamily = "Roboto")),
      title = list(style = list(fontFamily = "Roboto")),
      subtitle = list(style = list(fontFamily = "Roboto"))
      )
  )

hc_opts <- getOption("highcharter.chart")
hc_opts$credits <- list(enabled = TRUE, text = "Coded by Joshua Kunst", href = "http://jkunst.com")
options(highcharter.chart = hc_opts)




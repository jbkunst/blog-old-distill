# general knitr option ----------------------------------------------------
knitr::opts_chunk$set(
  fig.path = "images/",
  cache.path = ".cache/"
)

# ggplot2 -----------------------------------------------------------------
library(showtext) 
showtext_auto()

knitr::opts_chunk$set(fig.showtext = TRUE, dpi = 200)

font_add_google("Open Sans", "myfont")

ggplot2::theme_set(ggplot2::theme_minimal())



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




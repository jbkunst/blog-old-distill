# general knitr option ----------------------------------------------------
knitr::opts_chunk$set(
  layout = "l-page",
  fig.path = "images/",
  cache.path = "cache/",
  dpi = 72*2,
  fig.align = "center",
  echo = TRUE
)


# copy rss ----------------------------------------------------------------
# try(fs::file_copy("docs/index.xml", "docs/category/r/rss/index.xml", overwrite = TRUE))


# ggplot2 -----------------------------------------------------------------
library(ggplot2)
library(extrafont)
# extrafont::font_import()

# this makes fonts works
suppressMessages(suppressWarnings(loadfonts()))

theme_set(
  theme_minimal(base_family = "Segoe UI", base_size = 7) +
    theme(
      strip.background = element_rect(fill = "gray90", colour = NA),
      legend.position = "bottom"
    )
  )

# highcharter -------------------------------------------------------------
library(highcharter)

fntfmly <- '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"' 

options(
  highcharter.theme =
    hc_theme_smpl(
      chart = list(
        style = list(
          fontFamily = fntfmly
          )
        ),
      title = list(style = list(fontFamily = fntfmly)),
      subtitle = list(style = list(fontFamily = fntfmly)),
      credits = list(style = list(fontFamily = fntfmly))
      )
  )

hc_opts <- getOption("highcharter.chart")
hc_opts$chart <- list(style = list(fontFamily = fntfmly))
hc_opts$credits <- list(enabled = TRUE, text = "Coded by Joshua Kunst", href = "http://jkunst.com")
options(highcharter.chart = hc_opts)
rm(hc_opts, fntfmly)



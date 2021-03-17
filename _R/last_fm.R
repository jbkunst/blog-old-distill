# remotes::install_github("ppatrzyk/lastfmR")
# library(lastfmR)
# 
# scrobbles <- get_scrobbles(user = "jbkunst")
# 
# dplyr::glimpse(scrobbles)


library(tidyverse)
library(httr)

# https://public.tableau.com/es-es/s/blog/2019/07/visualize-your-listening-habits-lastfm-and-spotify-data

API_KEY	<- "a233e0cd55ac3ec2dc558221bde3794d"

audioscrobbler <- handle("http://ws.audioscrobbler.com/")

req <- GET(
  handle = audioscrobbler, 
  path = "2.0",
  query = list(
    api_key = API_KEY,
    method = "user.getrecenttracks",
    user = "jbkunst",
    format = "json",
    limit = 200
  )
)

info <- content(req, "parsed")

info <- info$recenttracks$`@attr`


data <- 1:info$totalPages %>% 
  map_df(function(page){
    
    message(page)
    
    req <- GET(
      handle = audioscrobbler, 
      path = "2.0",
      query = list(
        api_key = API_KEY,
        method = "user.getrecenttracks",
        user = "jbkunst",
        format = "json",
        limit = 200,
        page = page
      )
    )
    
    content <- content(req, "parsed") 
    
    content$recenttracks$track %>% 
      map_df(function(x){
        
        tibble(
          song_name = x$name,
          song_mbid = x$mbid,
          # date_uts = x$date$uts,
          date = if_else(is.null(x$date$`#text`), "", x$date$`#text`),
          artist_mbid = x$artist$mbid,
          artist_name = x$artist$`#text`,
          album_mbid = x$album$mbid,
          album_name = x$album$`#text`
        )
        
      })
    
    
  })


saveRDS(data, "~/data_last_fm.rds")

data <- readRDS("~/data_last_fm.rds")

data %>% count(artist_name, sort = TRUE)

data <- data %>% 
  mutate(date = lubridate::as_datetime(date))

data %>% 
  count(lubridate::year(date)) %>% 
  View()

lubridate::origin

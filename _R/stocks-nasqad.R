library(tidyverse)
library(quantmod)  # also loads xts and TTR


# Fetch all Symbols & store only the tickers to retrieve the data
symbols <- stockSymbols()

symbols <- as_tibble(symbols)

glimpse(symbols)

symbols %>% 
  count(Industry)


symbols <- symbols[,1]

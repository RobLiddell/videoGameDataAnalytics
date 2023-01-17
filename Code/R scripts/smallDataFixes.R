library(tidyverse)

setwd('C:/Users/user/Documents/Education/Brain Station/Data Analytics/Final Project')

tGameID <- read_csv("Data/tableau/tGameID.csv")
tGameGenre <- read_csv("Data/tableau/tGameGenre.csv")
tGameKeywords <- read_csv("Data/tableau/tGameKeywords.csv")
tGameTheme <- read_csv("Data/tableau/tGameTheme.csv")
tGameSales <- read_csv("Data/tableau/tGameSales.csv")
tGameTitleBreakdown <- read_csv("Data/tableau/tGameTitleBreakdown.csv")

tGameTitleBreakdown %>% 
  filter(is.na(Word))

if(FALSE){
tGameID %>% 
  mutate(Main_Genre=str_replace(Main_Genre,"misc","Misc")) %>% 
  write_csv("Data/tableau/tGameID.csv")
}

if(FALSE){
  tGameTitleBreakdown %>% 
    mutate(Word=str_remove_all(Word,"(?<![aeiou])s$")) %>% 
    write_csv("Data/tableau/tGameTitleBreakdown.csv")
}



tGameGenre <- read_csv("Data/tableau/tGameGenre.csv")



tGameGenre %>% 
  filter(genre=="Racing") %>% 
  pull(gameID) %>% 
  {filter(.data=tGameID, gameID%in%.)} %>% 
  filter(Main_Genre=='Action') %>% 
  print(n=60)

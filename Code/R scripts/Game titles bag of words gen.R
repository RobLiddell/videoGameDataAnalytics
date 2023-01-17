library(tidyverse)
library(stringr)

dat <- readxl::read_xlsx("C:/Users/user/Documents/Education/Brain Station/Data Analytics/Final Project/Data/vgSales-Cleaned.xlsx")

nameDat <- dat %>% 
  select(Name,Genre,Rank) %>% 
  rowwise() %>% 
  mutate(listName=str_split(Name," ")) 


wordsInGenre <- nameDat %>% 
  distinct(Name,.keep_all = TRUE) %>% 
  unnest(listName) %>%
  mutate(listName=str_remove_all(listName,"[:punct:]"),
         listName=str_remove_all(listName,"[:symbol:]")) %>% 
  filter(!str_detect(listName,"\\d+")) %>% 
  filter(!str_detect(listName,"[:punct:]+")) %>% 
  filter(str_length(listName)>3) %>% 
  group_by(Genre) %>% 
  count(listName) %>% 
  ungroup() %>% 
  rename(Word=listName, Count=n) %>% 
  mutate(Word=str_to_lower(Word))

wordsInGenre %>% 
  write_csv("C:/Users/user/Documents/Education/Brain Station/Data Analytics/Final Project/Data/titleWordsbyGenre.csv")

wordsInGenre %>% 
  filter(str_detect(Word,"^pok"))

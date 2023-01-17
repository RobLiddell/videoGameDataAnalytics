library(tidyverse)
library(readxl)


setwd("C:/Users/user/Documents/Education/Brain Station/Data Analytics/Final Project")
dataFiles <- paste0("Data/",c("",
                              "igdb Data/igdbGameData.csv",
                              "igdb Data/igdbGenreData.csv",
                              "igdb Data/igdbKeywordData.csv",
                              "cleaned/vgSales-cleaned.xlsx",
                              "tableau/titleWordsLongSales.csv"))


####Readin and format Data####

gameDat <- read_csv(dataFiles[2]) %>% 
  mutate(genreID = str_split(genreID,";"),
         keywordID=str_split(keywordID,";"))

genreDat <- read_csv(dataFiles[3])

keywordDat <- read_csv(dataFiles[4])

originalVgData <- read_xlsx(dataFiles[5])

titleWordsData <- read_csv(dataFiles[6])


####Splitting up gameDat and merging with genreDat and keywordDat####
gameGenreDat <- gameDat %>% 
  select(gameID,title,genreID) %>% 
  unnest_longer(genreID) %>% 
  drop_na() %>% 
  mutate(genreID=as.numeric(genreID)) %>% 
  left_join(genreDat)


gameKeywordDat <- gameDat %>% 
  select(gameID,title,keywordID) %>% 
  unnest_longer(keywordID) %>% 
  drop_na() %>% 
  mutate(keywordID=as.numeric(keywordID)) %>% 
  left_join(keywordDat)




top10KeywordGenre <- gameKeywordDat %>% 
  left_join(gameGenreDat) %>% 
  group_by(genre) %>% 
  count(keyword) %>% 
  arrange(desc(n)) %>%
  slice_head(n=10) %>% 
  ungroup()
  
  
# top10KeywordGenre %>% 
#   group_by(genre) %>% 
#   mutate(freqRank=row_number()) %>% 
#   pivot_wider(names_from = genre,
#                 names_glue = "{genre}_{.value}",
#                 values_from = c(keyword,n)) %>% 
#   write_csv(str_glue("{dataFiles[1]}igdb Data/top10KeywordsByGenre.csv"))
  
gameKeywordDat

gameDat %>% 
  select(gameID,hypes) %>% 
  drop_na() %>% 
  left_join(gameKeywordDat) %>% 
  drop_na() %>% 
  group_by(keyword) %>% 
  summarise(hypes=sum(hypes)) %>% 
  arrange(desc(hypes)) %>% 
  print(n=50)


####Cleaning Data For Tableau####

cleanedTitlesVGData <- originalVgData %>% 
  rename(Title=Name) %>% 
  mutate(kglID=row_number(),
         Title=str_remove_all(Title,"(?i)\\(.*sales.*\\)"),
         Title=str_remove_all(Title,"\\(PSP\\)"),
         Title=str_remove_all(Title,"\\([0-9]*\\)"),
         Title=str_remove_all(Title,"(?i)\\(.*version.*\\)"),
         Title=str_remove_all(Title,"(?i)\\(japan\\)"),
         Title=str_remove_all(Title,"\\(.*-.*\\)"),
         Title=str_remove_all(Title,"\\(3DS\\)"),
         Title=str_replace_all(Title,"\\(Red\\)","Red"),
         Title=str_remove_all(Title,"\\(.*\\)"))

tGames <- cleanedTitlesVGData %>% 
  select(Title,Year,Publisher) %>%
  distinct() %>% 
  mutate(kglID=row_number(),
         Title=str_remove_all(Title,"(?i)\\(.*sales.*\\)"),
         Title=str_remove_all(Title,"\\(PSP\\)"),
         Title=str_remove_all(Title,"\\([0-9]*\\)"),
         Title=str_remove_all(Title,"(?i)\\(.*version.*\\)"),
         Title=str_remove_all(Title,"(?i)\\(japan\\)"),
         Title=str_remove_all(Title,"\\(.*-.*\\)"),
         Title=str_remove_all(Title,"\\(3DS\\)"),
         Title=str_replace_all(Title,"\\(Red\\)","Red"),
         Title=str_remove_all(Title,"\\(.*\\)")) %>% 
  select(kglID,Title,Year,Publisher) %>% 
  write_csv(paste0(dataFiles[1],"/tableau/tGames.csv"))





originalVgData %>% 
  filter(Platform=="PSP")


                
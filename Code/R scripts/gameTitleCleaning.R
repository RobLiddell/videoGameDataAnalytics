library(tidyverse)
library(readxl)

setwd("C:/Users/user/Documents/Education/Brain Station/Data Analytics/Final Project")
dataFiles <- paste0("Data/",c("",
                              "cleaned/vgSales-cleaned.xlsx",
                              "tableau/titleWordsLongSales.csv"))


####Readin and format Data####

originalVgData <- read_xlsx(dataFiles[2])

titleWordsData <- read_csv(dataFiles[3])



cleanedTitlesSales <- originalVgData %>%
  rename(Title=Name) %>% 
  mutate(Year=as.numeric(Year),
         Year=ifelse(is.na(Year),str_extract(Title,"[0-9]{4}"),Year)) %>% 
  mutate(Title=str_remove_all(Title,"(?i)\\(.*sales.*\\)"),
         Title=str_remove_all(Title,"\\(PSP\\)"),
         Title=str_remove_all(Title,"\\([0-9]*\\)"),
         Title=str_remove_all(Title,"(?i)\\(.*version.*\\)"),
         Title=str_remove_all(Title,"(?i)\\(japan\\)"),
         Title=str_remove_all(Title,"\\(.*-.*\\)"),
         Title=str_remove_all(Title,"\\(3DS\\)"),
         Title=str_replace_all(Title,"\\(Red\\)","Red"),
         Title=str_remove_all(Title,"\\(.*\\)"))





cleanedTitles<- cleanedTitlesSales %>% 
  select(Title,Year,Publisher,Genre) %>%
  distinct() %>% 
  mutate(kglID=row_number()) %>% 
  select(kglID,Title,Year,Publisher,Genre) %>% 
  write_csv(paste0(dataFiles[1],"/igdb Data/cleanedGameTitles.csv"))

cleanedTitlesSales %>% 
  left_join(cleanedTitles) %>% 
  relocate(kglID,.before=Rank) %>% 
  write_csv("Data/igdb Data/kglDataCleanestSales.csv")

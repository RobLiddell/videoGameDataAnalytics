library(tidyverse)
library(stringr)
library(stopwords)

dat <- readxl::read_xlsx("C:/Users/user/Documents/Education/Brain Station/Data Analytics/Final Project/Data/vgSales-Cleaned.xlsx")

nameDat <- dat %>% 
  # select(-contains("sales")) %>% 
  rowwise() %>% 
  mutate(listName=str_replace_all(Name,"[:punct:]"," "),
         listName=str_replace_all(listName,"[:symbol:]"," "),
         listName=str_split(listName," ")) %>% 
  ungroup()

wordsInRank <- nameDat %>% 
  unnest(listName) %>%
  filter(str_detect(listName,"")) %>% 
  filter(!str_detect(listName,"\\d+")) %>% 
  filter(!str_detect(listName,"[:blank:]")) %>% 
  filter(!str_length(listName)<=2) %>% 
  mutate(Word=str_to_lower(listName),.keep="unused") %>% 
  filter(!Word%in%stopwords('en')) 

temp<-wordsInRank %>% 
  group_by(Rank) %>% 
  mutate(Count=n()) %>% 
  ungroup()

temp 


if(FALSE){
wordsInRank %>% 
  write_csv("C:/Users/user/Documents/Education/Brain Station/Data Analytics/Final Project/Data/titleWordsbyRank.csv")
}


wordsInRank %>% 
  group_by(Genre) %>% 
  count(Word) %>% 
  arrange(desc(n)) %>% 
  filter(row_number()<=10) %>% 
  mutate(wordRank=row_number()) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = Genre,names_from = wordRank,names_prefix = "Word_",values_from = Word)


wordsInRank %>% 
  group_by(Genre,Word) %>% 
  summarise(Sales=sum(Global_Sales)) %>% 
  arrange(desc(Sales)) %>% 
  filter(row_number()<=10) %>% 
  mutate(wordRank=row_number()) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = Genre,names_from = wordRank,names_prefix = "Word_",values_from = Word)


wordsInRank %>% 
  group_by(Rank) %>%
  mutate(titleLen=n()) %>% 
  ungroup() %>% 
  pivot_longer(cols = ends_with("Sales"),
               names_to = "Region",
               values_to = "Salesmill") %>% 
  mutate(salesPerWords=Salesmill/titleLen) %>% 
  pivot_wider(id_cols = )

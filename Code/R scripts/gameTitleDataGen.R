####ABOUT####
#This code generates a file which lists all the individual words used in the titles of the games. These words can be grouped by different 
#features to provide an indication of common themes within genre and by region
####Libraries####
library(tidyverse)
library(stringr)
library(stopwords)
library(qdapDictionaries)

####Read in Data####
dat <- readxl::read_xlsx("C:/Users/user/Documents/Education/Brain Station/Data Analytics/Final Project/Data/vgSales-Cleaned.xlsx")

####Data Cleaning####
#Here I want to remove numbers and punctuation and attempt to filter data to only accept full words for analysis
nameDat <- dat %>% 
  # select(-contains("sales")) %>% 
  rowwise() %>% 
  mutate(listName=str_replace_all(Name,"[:punct:]"," "),
         listName=str_replace_all(listName,"[:symbol:]"," "),
         listName=str_split(listName," ")) %>% 
  ungroup() %>%  
  unnest(listName) %>%
  filter(str_detect(listName,"")) %>% 
  filter(!str_detect(listName,"\\d+")) %>% 
  filter(!str_detect(listName,"[:blank:]")) %>% 
  filter(!str_length(listName)<=2) %>% 
  mutate(Word=str_to_lower(listName),.keep="unused") %>% 
  filter(!Word%in%stopwords('en')) %>% 
  group_by(Rank) %>% 
  mutate(TitleWordOrder=row_number(),
         titleLen=n()) %>% 
  ungroup()

####Data Reshaping and Categorizing####
#The goal here is to reshape the data into long format to aid with analysis in tableau.
#Futhermore I attempt to provide categorization to words
longSalesWordData <- nameDat %>% 
  pivot_longer(cols = ends_with("Sales"),
               names_to = "Region",
               values_to = "Salesmill") %>% 
  mutate(Region=str_remove(Region,"_Sales$"),
         SalesPerWord=Salesmill/titleLen,
         isPower=Word%in%power.words,
         isAction=Word%in%action.verbs,
         isPositive=Word%in%positive.words,
         isNegative=Word%in%negative.words,
         isAmplify=Word%in%amplification.words,
         isDeamplify=Word%in%deamplification.words,
         isFunction=Word%in%function.words,
         isStrong=Word%in%strong.words,
         isWeak=Word%in%weak.words,
         isSubmit=Word%in%submit.words,
         isWord=Word%in%DICTIONARY$word)
####Saving the data
if(FALSE){
  longSalesWordData %>% 
    write_csv("C:/Users/user/Documents/Education/Brain Station/Data Analytics/Final Project/Data/titleWordsLongSales.csv")
}

longSalesWordData %>% 
  filter(Word =='aquarium') %>%
  select(Name)

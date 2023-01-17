####Libraries####
library(httr2)
library(tidyverse)
####Setup Working Dir####
setwd('C:/Users/user/Documents/Education/Brain Station/Data Analytics/Final Project')

####Reading and Formatting Video Game Data####
gamesToSearch <- read_csv("Data/igdb Data/cleanedGameTitles.csv")
loginStuff <- read_csv("Code/Data Scrapping/Inputs/loginstuff.txt")

####Get app token from twitch####
twitchResp <- request('https://id.twitch.tv/oauth2/token') %>% 
  req_body_json(list(client_id=loginStuff %>% pull(client),
                     client_secret=loginStuff %>% pull(token),
                     grant_type='client_credentials')) %>% 
  req_perform()


accessToken <- twitchResp %>% 
  resp_body_json() %>% 
  .[['access_token']]

####Create Header information for IGDB####

igdbHeaderList=list(`Client-ID` = loginStuff %>% pull(client),Authorization = paste0('Bearer ',accessToken))

apiReqSetup <- function(req,igdbHeaderList){
  req %>%  req_headers(!!!igdbHeaderList) %>% 
    req_throttle(3.5) %>% 
    req_retry(4) %>% 
    return()
}

#Setup request url and headers for different api endpoints
igdbGamesBase <- request('https://api.igdb.com/v4/games') %>% 
  apiReqSetup(igdbHeaderList)


igdbKeywordsBase <- request('https://api.igdb.com/v4/keywords') %>% 
  apiReqSetup(igdbHeaderList)


igdbGenresBase <- request('https://api.igdb.com/v4/genres') %>% 
  apiReqSetup(igdbHeaderList)

igdbCompaniesRequestBase <- request('https://api.igdb.com/v4/companies') %>% 
  apiReqSetup(igdbHeaderList)

igdbInvolvedCompaniesRequestBase <- request('https://api.igdb.com/v4/involved_companies') %>% 
  apiReqSetup(igdbHeaderList)

igdbRequestBase_Theme <- request('https://api.igdb.com/v4/themes') %>% 
  apiReqSetup(igdbHeaderList)

igdbRequestBase_Search <- request('https://api.igdb.com/v4/search') %>% 
  apiReqSetup(igdbHeaderList)

igdbRequestBase_AgeRating <- request('https://api.igdb.com/v4/age_ratings') %>% 
  apiReqSetup(igdbHeaderList)

####Requesting all Game Information####

gamesRequestProcessing <- function(gameResponse){
  gameResponse %>% 
    enframe() %>% 
    hoist(value,id=list("id",1L),
          name=list("name",1L),
          theme=list("themes"),
          companies=list("involved_companies"),
          genre=list("genres"),
          keywords=list("keywords"),
          releasedData=list("first_release_date"),
          rating=list("rating"),
          hypes=list("hypes")) %>% 
    select(-value) %>% 
    return()
}

gamesHoister <- function(enframedResponse){
  enframedResponse %>% 
    hoist(value,gameID=list("id",1L),
          Title=list("name",1L),
          themeID=list("themes"),
          companyID=list("involved_companies"),
          genreID=list("genres"),
          keywordID=list("keywords"),
          releaseDate=list("first_release_date"),
          rating=list("rating"),
          ageRating=list("age_ratings"),
          hypes=list("hypes")) %>% 
    select(-value) %>% 
    return()
}

gamesQuery <- function(gameTitles){
  #Generate the list of game names to search
  queryGameNameString <- paste0('"',gameTitles,'"',collapse = ",")
  #Create the query call
  str_glue('fields *;limit {length(gameTitles)*2}; where name = ({queryGameNameString});') %>% 
  return()
}

igdbIDSearch <- function(searchIDs,n=length(searchIDs)-2,requestBase, hoister,queryGen){
  
  requestSplits <- c(seq(1,length(searchIDs),n),length(searchIDs)+1)
  
  nLoops=length(requestSplits)
  
  loop=0
  
  for(i in requestSplits){
    loop=loop+1
    if((loop %% max(1,floor(nLoops/10)))==0){
      print(str_glue("{floor(loop/nLoops*100)}%"))
    }
    #On first loop just initialize i
    if(i==1){
      curr=i-1
      next
    }
    #On all subsequent loops walk the curr values to prev, and set the new curr value to i. 
    prev=curr+1
    curr=i-1
    
    #Generate the list of ids to search on
    queryIDs <- searchIDs[prev:curr]
    #Create the query call
    query=queryGen(queryIDs)
    
    #if this is the second loop create the data frame structure
    if(prev==1){
      data <- requestBase %>% 
        req_body_raw(query) %>% 
        req_perform() %>% 
        resp_body_json() %>% 
        enframe() %>% 
        hoister() %>% 
        select(-name)
      next
    }
    #on all subsequent loops add searched data to the old data frame
    data <- requestBase %>% 
      req_body_raw(query) %>% 
      req_perform() %>% 
      resp_body_json() %>% 
      enframe() %>% 
      hoister() %>% 
      select(-name) %>%  
      bind_rows(data)
  }
  print("100%")
  return(data)
}

gameTitles <- gamesToSearch %>% 
  pull(Title)

igdbGameData <- igdbIDSearch(gameTitles,250,igdbGamesBase,gamesHoister, gamesQuery)


igdbData_GameYear <- igdbGameData %>% 
  mutate(releaseYear=lubridate::year(as.POSIXct(releaseDate,origin = "1970-01-01")),
         .keep="unused")




if(FALSE){
  igdbData_GameYear %>%
    rowwise() %>% 
    mutate(genreID=paste(genreID,collapse=";"),
           keywordID=paste(keywordID,collapse=";"),
           themeID=paste(themeID,collapse=";"),
           companyID=paste(themeID,collapse=";")) %>% 
    ungroup() %>% 
    write_csv("Data/igdb Data/igdbData_Games.csv")
}





####Genre Identification####

hoister_Genres<- function(enframedResponse){
  enframedResponse %>% 
    hoist(value,
          genreID=list("id"),
          genre=list("name")) %>% 
    return()
}

IDQuery <- function(IDs){
  #Generate the list of game names to search
  queryString <- paste0(IDs,collapse = ",")
  #Create the query call
  str_glue('fields *;limit {length(IDs)*2}; where id = ({queryString});') %>% 
    return()
}

genreIDs <- igdbData_GameYear %>% 
  select(genreID) %>% 
  unnest_longer(genreID) %>% 
  drop_na() %>% 
  distinct(genreID) %>% 
  arrange(genreID) %>% 
  pull()

igdbData_Genres <- igdbIDSearch(genreIDs,requestBase = igdbGenresBase,hoister = hoister_Genres, queryGen = IDQuery) %>% 
  select(-value) %>% 
  arrange(genreID)

igdbData_Genres


if(FALSE){
  igdbData_Genres %>%
    write_csv("Data/igdb Data/igdbData_Genres.csv")
}

####Keyword Identification####

hoister_Keywords <- function(enframedResponse){
  enframedResponse %>% 
    hoist(value,
          keywordID=list("id"),
          keyword=list("name")) %>% 
    return()
}

keywordIDs <- igdbData_GameYear %>% 
  select(keywordID) %>% 
  unnest_longer(keywordID) %>% 
  drop_na() %>% 
  distinct(keywordID) %>% 
  arrange(keywordID) %>% 
  pull()

igdbData_Keywords <- igdbIDSearch(keywordIDs,250,igdbKeywordsBase, hoister_Keywords, IDQuery) %>% 
  select(-value) %>% 
  arrange(keywordID)

igdbData_Keywords

if(FALSE){
  igdbData_Keywords %>%
    arrange(keywordID) %>% 
    write_csv("Data/igdb Data/igdbData_Keywords.csv")
}

####Theme Identification####

hoister_Themes <- function(enframedResponse){
  enframedResponse %>% 
    hoist(value,
          themeID=list("id"),
          theme=list("name")) %>% 
    return()
}

ThemeIDs <- igdbData_GameYear %>% 
  select(themeID) %>% 
  unnest_longer(themeID) %>% 
  drop_na() %>% 
  distinct(themeID) %>% 
  arrange(themeID) %>% 
  pull()

igdbData_Theme <- igdbIDSearch(ThemeIDs,10,igdbRequestBase_Theme, hoister_Themes, IDQuery) %>% 
  select(-value) %>% 
  arrange(themeID)

igdbData_Theme

if(FALSE){
  igdbData_Theme %>%
    write_csv("Data/igdb Data/igdbData_Themes.csv")
}



####Find Publisher####
hoister_Publisher <- function(enframedResponse){
  enframedResponse %>% 
    hoist(value,
          publisher=list("publisher"),
          companyID=list("company"),
          gameID=list("game")) %>% 
    return()
}

publisherQuery <- function(IDs){
  #Generate the list of game names to search
  queryString <- paste0(IDs,collapse = ",")
  #Create the query call
  str_glue('fields *;limit {length(IDs)*3}; where id = ({queryString}) & publisher=true;') %>% 
    return()
}

gameIDs <- igdbData_GameYear %>% 
  select(gameID) %>% 
  pull()

length(gameIDs)

igdbData_Publishers <- igdbIDSearch(gameIDs,100,igdbInvolvedCompaniesRequestBase,hoister_Publisher,publisherQuery)


igdbData_Publishers <- igdbData_Publishers %>% 
  select(companyID,gameID) %>% 
  distinct(gameID,.keep_all = TRUE)


if(FALSE){
  igdbData_Publishers %>%
    write_csv("Data/igdb Data/igdbData_Publishers.csv")
}

####Company Identification####

hoister_Company <- function(enframedResponse){
  enframedResponse %>% 
    hoist(value,
          companyID=list("id"),
          company=list("name")) %>% 
    return()
}


companyIDs <- igdbData_Publishers %>% 
  distinct(companyID) %>% 
  pull()

igdbData_Company <- igdbIDSearch(companyIDs,250,igdbCompaniesRequestBase,hoister_Company,IDQuery)


igdbData_Company <- igdbData_Company %>% 
  arrange(companyID) %>% 
  select(-value)
  
if(FALSE){
  igdbData_Company %>%
    write_csv("Data/igdb Data/igdbData_Company.csv")
}

####Data####
gamesToSearch

igdbData_Company
igdbData_Publishers
igdbData_GameYear

igdbData_Theme
igdbData_Genres
igdbData_Keywords

kglData <- read_csv("Data/igdb Data/kglDataCleanestSales.csv")


tGameID <- gamesToSearch %>%
  rename(Main_Genre=Genre) %>% 
  left_join(igdbData_GameYear) %>% 
  distinct(kglID,.keep_all = TRUE) %>% 
  select(kglID,gameID,Title,Year,Publisher,Main_Genre)

tGameTheme <- igdbData_GameYear %>% 
  select(gameID, themeID) %>%
  distinct() %>% 
  unnest_longer(themeID) %>% 
  left_join(igdbData_Theme) %>% 
  select(-themeID)

tGameGenre <- igdbData_GameYear %>% 
  select(gameID, genreID) %>%
  distinct() %>% 
  unnest_longer(genreID) %>% 
  left_join(igdbData_Genres) %>% 
  select(-genreID)

tGameKeywords <- igdbData_GameYear %>% 
  select(gameID, keywordID) %>%
  distinct() %>% 
  unnest_longer(keywordID) %>% 
  left_join(igdbData_Keywords) %>% 
  select(-keywordID)

tGameSales <- kglData %>% 
  select(-Publisher,-Genre,-Year) %>% 
  pivot_longer(cols=contains("Sales"),
               names_to = "Region",
               values_to = "Sales") %>% 
  mutate(Region=str_remove(Region,"_Sales$")) %>% 
  select(-Title)

library(stopwords)
library(qdapDictionaries)



tGameTitleBreakdown <- tGameID %>% 
  select(kglID,Title) %>% 
  mutate(Title=str_replace_all(Title,"[:punct:]"," "),
         Title=str_remove_all(Title,"[0-9]"),
         Title=str_remove(Title,"[:space:]*$")) %>% 
  mutate(splitTitle=str_split(Title," ")) %>% 
  unnest_longer(splitTitle) %>% 
  rename(Word=splitTitle) %>% 
  select(-Title) %>% 
  filter(!Word%in%stopwords('en')) %>% 
  mutate(isPower=Word%in%power.words,
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


tGameTitleBreakdown <- tGameTitleBreakdown %>% 
  mutate(Word=str_remove(Word,"(?<![aeiou])s$"))


####Saving Tables####

# tGameID
# tGameGenre
# tGameKeywords
# tGameTheme
# tGameSales
# tGameTitleBreakdown


if(FALSE){
  tGameID %>%
    write_csv("Data/tableau/tGameID.csv")
  tGameGenre %>%
    write_csv("Data/tableau/tGameGenre.csv")
  tGameKeywords %>%
    write_csv("Data/tableau/tGameKeywords.csv")
  tGameTheme %>%
    write_csv("Data/tableau/tGameTheme.csv")
  tGameSales %>%
    write_csv("Data/tableau/tGameSales.csv")
  tGameTitleBreakdown %>%
    write_csv("Data/tableau/tGameTitleBreakdown.csv")
}


####get Age Rating Data####

hoister_AgeRating <- function(enframedResponse){
  enframedResponse %>% 
    hoist(value,
          ageID=list("id"),
          ratingOrgID=list('category'),
          ratingID=list("rating")) %>% 
    return()
}

ageIDs <- igdbData_GameYear %>% 
  select(ageRating) %>% 
  unnest_longer(ageRating) %>% 
  drop_na() %>% 
  distinct(ageRating) %>% 
  arrange(ageRating) %>% 
  pull()

igdbData_AgeRating <- igdbIDSearch(ageIDs,250,igdbRequestBase_AgeRating, hoister_AgeRating, IDQuery) %>% 
  select(-value) %>% 
  arrange(ageID)

legend_Organization <- tibble(ratingOrgID=c(1:7),
       ratingOrg=c('ESRB','PEGI','CERO','USK','GRAC','CLASS_IND','ACB'))

legend_RatingID <- tibble(ratingID=c(1:38),
       rating=c('Three','Seven','Twelve','Sixteen','Eighteen','RP','EC','E',
                   'E10','T','M','AO','CERO_A','CERO_B','CERO_C','CERO_D','CERO_Z',
                   'USK_0','USK_6','USK_12','USK_18','GRAC_ALL','GRAC_Twelve','GRAC_Fifteen',
                   'GRAC_Eighteen','GRAC_TESTING','CLASS_IND_L','CLASS_IND_Ten','CLASS_IND_Twelve',
                   'CLASS_IND_Fourteen','CLASS_IND_Sixteen','CLASS_IND_Eighteen',
                   'ACB_G','ACB_PG','ACB_M','ACB_MA15','ACB_R18','ACB_RC'),
       ageAbove=c(3,7,12,16,18,NA,3,6,10,13,17,18,3,12,15,17,18,0,6,12,18,0,12,15,18,NA,6,10,12,14,16,18,3,6,12,15,18,18))



if(FALSE){
  igdbData_AgeRating %>% 
    left_join(legend_Organization) %>% 
    left_join(legend_RatingID) %>% 
    write_csv("Data/igdb Data/igdbData_AgeRating.csv")
}


tAgeRating <- igdbData_GameYear %>% 
  rename(ageID=ageRating) %>% 
  select(gameID, ageID) %>%
  distinct() %>% 
  unnest_longer(ageID) %>% 
  left_join(igdbData_AgeRating) %>% 
  left_join(legend_Organization) %>% 
  left_join(legend_RatingID) %>% 
  select(-ageID,-ratingOrgID,ratingID) %>% 
  arrange(gameID) %>% 
  drop_na() 

if(FALSE){
  tAgeRating %>% 
    write_csv("Data/tableau/tAgeRating.csv")
}


####Random Code####
gamesQuery <- function(gameTitles){
  #Generate the list of game names to search
  queryGameNameString <- paste0('"',gameTitles,'"',collapse = ",")
  #Create the query call
  str_glue('fields *;limit {length(gameTitles)*2}; where name = ({queryGameNameString});') %>% 
    return()
}


igdbRequestBase_Search %>% 
  req_body_raw('fields *;where game = 548;') %>% 
  req_perform() %>% 
  resp_body_json()



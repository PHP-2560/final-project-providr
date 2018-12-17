#' ProviderInStateByCounty
#' 
#' This fuction creates a data frame of the providers in a state divided by county
#' @param "state abbreviation", "provider taxonomy"
#' @return a data frame
#' @examples 
#' ProviderInStateByCounty("CA", "cardiologist")
#' PRoviderInStateByCounty("CT", "orthapedist")

ProviderInStateByCounty<-function(state,taxonomy){
  #install/load required packages
 if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }

  if(!require(rvest)){
    install.packages("rvest")
    library(rvest)
  }

  if(!require(XML)){
    install.packages("XML")
    library(XML)
  }

  if(!require(stringi)){
    install.packages("stringi")
    library(stringi)
  }

  if(!require(stringr)){
    install.packages("stringr")
    library(stringr)
  }

  #pulls data
  zips_used <- ZipsFromState(state)
  load("ProvidR.Final/Data/zcta_county_rel_10.Rda")
  load("ProvidR.Final/Data/co_est2017.Rda")
  zip_link = zcta_county_rel_10 #renaming the data sets
  census = co_est2017_alldata
   url1<- "https://npiregistry.cms.hhs.gov/registry/search-results-table?addressType=ANY&postal_code=" #setting the url to scrape from
  provider.data <- data.frame() #initializing an empty data frame
  skips <- seq(0,9999999,100) #create skips
  for (i in 1:length(zips_used)) { #iterating over all RI zip codes
    for (j in 1:length(skips)){ #also iterating over skils
      zip <- zips_used[i]
      skip <- skips[j]
      tax <- str_replace_all(taxonomy," ","+")
      url<-paste0(url1,zip,"&skip=",skip,"&taxonomy_description=",tax) #pasting the url, with the rhode island zip code and including the skips
      #text scrape to pull our places by zip code
      h <- read_html(url, timeout = 500)
      reps <- h %>% #setting up the repeating structure
        html_node("table") %>%
        html_table()
      if (any(is.na(reps[,1])) | all(is.na(reps[,1]))) { #if the page has no physicians, move to next zip code
        break}
      reps$zip <- zips_used[i]
      provider.data <- rbind(provider.data,reps) #binding together the provider data and reps data
    }
  }

  colnames(provider.data) <- c("NPI","Name","NPI_Type","Primary_Practice_Address","Phone","Primary_Taxonomy","zipcode")

  provider.data$state.name <- noquote(str_extract(provider.data$Primary_Practice_Address,pattern="(?<=, )[A-Z]+(?=\\s)")) #state postal code (2 characters)

  provider.data <- filter(provider.data,state.name==state)

  zip_link<- zip_link %>% #getting zip_link ready to be joined by selecting variables, renaming variables and making zipcode into a character variable
    select(ZCTA5, STATE, COUNTY, GEOID) %>%
    rename(zipcode = ZCTA5) %>%
    mutate(zipcode = as.character(zipcode))
  zip_link$zipcode = stri_pad_left(zip_link$zipcode, 5, "0") #adds 0's to the front of the zipcode to make it have a length of give

  NPI_join<-inner_join(provider.data, zip_link, by="zipcode") #joins the provider.data to zip_link

  NPI_join$STATE<-as.character(NPI_join$STATE) #makes the state variable a character variable

  NPI_to_census<-inner_join(NPI_join, census, by=c("STATE", "COUNTY")) #inner join the previously made data set to census data
  NPI_to_census$state.name.long <- as.character(NPI_to_census$STNAME) #makes the state name into a character variable

  load("ProvidR.Final/Data/state_abbrev.Rda") #loads the state abbreviation data set

  state_abbrev$state.name.long <- as.character(state_abbrev$State) #makes the state name into a character variable

  NPI_states <- left_join(NPI_to_census, state_abbrev, by=c("state.name.long")) #joins the NPI_to_census with the state_abbrev

  return(NPI_states) #returns the data frame

}




#' NumProvidersInZip
#' 
#' This function takes a zip code and a provider taxonomy and outputs the number of providers in a zip codes
#' @param "zipcode" and "taxonomy"
#' @return the count of providers in a zipcode
#' @examples 
#' NumProvidersInZip("90210", "addiction")
#' NumProvidersInZip("02906", "family practice")

NumProviderInZip<-function(zipcode,taxonomy){
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
  
  if(!require(stringr)){
    install.packages("stringr")
    library(stringr)
  }
  
  if(!require(stringi)){
    install.packages("stringri")
    library(stringi)
  }
  #load in census data
  load("ProvidR/Data/zcta_county_rel_10.Rda")
  load("ProvidR/Data/co_est2017.Rda")
  zip_link = zcta_county_rel_10
  census = co_est2017_alldata
  #prepare to pull data
  url1<- "https://npiregistry.cms.hhs.gov/registry/search-results-table?addressType=ANY&postal_code=" #setting the url to scrape from
  provider.data <- data.frame() #initializing an empty data frame
  skips <- seq(0,9999999,100) #create skips
  #pull data
  for (i in 1:length(zipcode)) { #iterating over all RI zip codes
    for (j in 1:length(skips)){ #also iterating over skils
      zip <- zipcode[i]
      skip <- skips[j]
      tax <- str_replace_all(taxonomy," ","+")
      url<-paste0(url1,zip,"&skip=",skip,"&taxonomy_description=",tax) #pasting the url, with the rhode island zip code and including the skips
      #text scrape to pull our places by zip code
      h <- ?read_html(url, timeout = 200)
      reps < h %>% #setting up the repeating structure
        html_node("table") %>%
        html_table()
      if (any(is.na(reps[,1])) | all(is.na(reps[,1]))) { #if the page has no physicians, move to next zip code
        break}
      reps$zip <- zipcode[i]
      provider.data <- rbind(provider.data,reps) #binding together the provider data and reps data
    }
  }
  colnames(provider.data) <- c("NPI","Name","NPI_Type","Primary_Practice_Address","Phone","Primary_Taxonomy","zipcode")

  provider.data$street <- noquote( str_extract(provider.data$Primary_Practice_Address,pattern="^[a-zA-Z0-9_. -]+?(?=\n)")) #street name and number
  provider.data$city <- noquote( str_extract(provider.data$Primary_Practice_Address,pattern="(?<=\t)[a-zA-Z_. -]+?(?=, )"))#city
  provider.data$statename<- noquote( str_extract(provider.data$Primary_Practice_Address,pattern="(?<=, )[A-Z]+(?=\\s)")) #state postal code (2 characters)
  provider.data<-mutate(provider.data, zipcode= as.character(zipcode))

  zip_link<- zip_link %>% #gets zip_link ready for a join by selecting variables, renaming and then changing zipcode from a numeric to character variable.
    select(ZCTA5, STATE, COUNTY, GEOID) %>%
    rename(zipcode = ZCTA5) %>%
    mutate(zipcode = as.character(zipcode))
  zip_link$zipcode = stri_pad_left(zip_link$zipcode, 5, "0") #adds 0's to the front of the zipcodes until they have the length of 5
  
  NPI_join<-inner_join(provider.data, zip_link, by="zipcode") #joins the provider.data to the zip_link.
  
  NPI_join$STATE<-as.character(NPI_join$STATE) #turns the state variable into a character variable.
  
  NPI_to_census<-inner_join(NPI_join, census, by=c("STATE", "COUNTY")) #inner join of the NPI_join to census data
  return(nrow(NPI_to_census)) #returns the number of providers in a zipcode by taxonomy because each provider is on its own row.
}


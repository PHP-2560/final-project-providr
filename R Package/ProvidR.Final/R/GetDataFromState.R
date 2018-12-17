#' GetDataFromState
#' 
#' This function, given a state abbreviation and a provider taxonomy outputs a data fram
#' @param "state abbreviation", "taxonomy"
#' @return a data frame
#' @examples 
#' GetDataFromState("RI", "primary care")
#' GetDataFromState("CT", "dermatologist")

GetDataFromState<-function(state, taxonomy) {
  #load neccesary packages
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
  zipcode_holder<-ZipsFromState(state) #using the helper function, zipfromstate, creates a data set of all of the zipcodes in a state
  data<-NPIcode_taxonomy(zipcode_holder, taxonomy) #feeds this through the base function that creates a data set of every provider in a state by a given taxonomy
}
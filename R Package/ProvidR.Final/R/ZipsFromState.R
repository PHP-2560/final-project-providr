#' ZipsFromState
#' Pulls out all the zipcodes from a state
#' This is a helper function that is used for other functions.
#' @param "State abbreviaton"
#' @return a list of zipcodes from the state
#' @examples 
#' ZipsFromState("CT")

ZipsFromState<-function(state_name){
  #install required packages
  if(!require(zipcode)){
    install.packages("zipcode")
    library(zipcode)
  }

  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  #process data
  data(zipcode) #creates data frame from zipcode package
  zip_holder<-zipcode%>%
    filter(state==state_name)%>% #remanes the state_name variable
    select(zip) #selects the zipcodes
  zip_state<-zip_holder[,1] #grabe the first row 
  return(zip_state)
}

#'CountByZip
#'
#'This is a function that given the data frame created by GetDataFromState returns a count of the number of providers in a zipcode.
#'This function is used in other functions and not used as a standalone function. 
#'@param the data frame created by GetDataFromState
#'@return the number of providers in a zipcode
#'@examples
#'DF1<-GetDataFromState("MA", "family medicine")
#'CountByZip(DF1)
CountByZip <- function(data){
  data %>% #creating a count of practices by zip code
    group_by(zipcode) %>% #groups by zipcode
    count() %>% #counts the number of providers in each zipcode
    arrange(n)%>% #arranges from least to greatest
    ungroup()
}
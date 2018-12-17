#'SummaryByCounty
#'This function gives you the number of providers in each county, the number of providers per 1,000 people, and the population of the county
#'@param a data from from provider in the state by county
#'@return a data frame
#' @examples 
#' SummaryByCounty("RI", "Cardiologist")

SummaryByCounty<-function(state, taxonomy){
  #install/load required packages
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  prov.dat <- ProviderInStateByCounty(state,taxonomy) #uses the ProviderInStateByCounty function to bring in a data set
  #process data
  rows <- prov.dat %>%
    filter(Abbreviation==state) %>% #renames the variable
    group_by(CTYNAME, STNAME, POPESTIMATE2010, state.name) %>% #grouping by the variables listed
    count() %>% #creates a count
    select(CTYNAME, POPESTIMATE2010, STNAME, state.name, n) %>% #selecting the columns to be displayed
    mutate(provider_density = n/POPESTIMATE2010*1000) %>% #creating a new column that has the number of providers per 1,000 people
    arrange(n) #arrange from least to greatest.
  return(rows) #return dataset
}

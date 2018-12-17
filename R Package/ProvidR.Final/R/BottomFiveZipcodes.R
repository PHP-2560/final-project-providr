#'BottomFiveZipcodes
#'
#'This function takes a data frame created by the GetDataFromState functiona and shows the zipcodes with the fewest providers
#'@param the data frame created by GetDataFromState
#'@return a bar graph of the zipcodes with the fewest providers
#'@examples 
#'Data<-GetDataFromState("MA", "Pediatrician")
#'BottomFiveZipcodes(Data)

BottomFiveZipcodes<-function(data){
  #install/load required packages
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
  }
  counts_holder<-data%>% #using dplyr functions to sort and count the data so that we can see the zipcodes with the fewest providers, uses the data frame as the main input.
    group_by(zipcode) %>% 
    count() %>%
    arrange(n) 
  counts<-head(counts_holder) #create data frame using the top five rows of the data set, aka zipcodes with lowest provider counts
  plot<-ggplot(counts, aes(x=zipcode, y=n))+ #set up plotting environment
    geom_bar(stat="identity", fill = "blue") + labs(x = "Zipcode", y = "Number of providers")+ #create bar graph
    theme_minimal()+
    coord_flip()
  print(plot + ggtitle("Bottom five zip codes by provider number")) #print plot with title
  return(plot)
}
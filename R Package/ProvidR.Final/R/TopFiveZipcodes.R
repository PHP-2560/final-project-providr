#'TopFiveZipcodes
#'
#'This function, given a data frame created with GetDataFromState, creates a vizualtation of the five zipcodes with the most providers
#'@param the data frame create by GetDataFromState
#'@return a graph of the five zipcodes with the most providers
#'@examples
#'Data<-GetDataFromState("FL", "addiction")
#'TopFiveZipcodes(Data)

TopFiveZipcodes<-function(data){
  #install required packages
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  
  if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
  }
  counts_holder<-data%>% 
    group_by(zipcode) %>% #groups data by zipcode 
    count() %>% #counts the number of providers in a zipcode
    arrange(desc(n)) #arranges from greatest to least
  counts<-head(counts_holder) #pulls the first five rows of the data set
  plot<-ggplot(counts, aes(x=zipcode, y=n))+ #sets up ggplot environment
    geom_bar(stat="identity", fill = "blue") + labs(x = "Zipcode", y = "Number of providers")+ #creates a bar graph and provides labels.
    theme_minimal()+ 
    coord_flip()
  print(plot + ggtitle("Top five zip codes by provider number")) #prints graph with a title
  return(plot)
}
#'BottomTaxonomiesZip
#'
#'This function, given a data frame, returns the least frequent provider taxonomies in a zipcode
#'@param the data frame created with GetDataFromStates
#'@return A visualization of the 5 least frequent provider taxonomies in a zipcode
#'@examples
#'Data<-GetDataFromState("MI", "Primary Care")
#'BottomTaxonomiesZip(Data)

BottomTaxonomiesZip<-function(data){
  #loading required packages
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
  }
  provider_grouping<-data %>% #creating a count of practices by providers using the data frame
    group_by(Primary_Taxonomy) %>%  #group by taxonomy
    count() %>% #count the number of those in each taxonomy
    arrange(n) #arrange in order
  bottom5providers<-head(provider_grouping) #creates a data set that is the five first rows of data set created above, aka the five least common taxonomies
  providers_bar<-ggplot(bottom5providers, aes(x=Primary_Taxonomy, y=n))+ #set up the ggplot environment
    geom_bar(stat="identity", fill = "blue")+ #create bar chart
    coord_flip()+
    theme_minimal()+
    labs(x = "Provider Type", y = "Number of Providers") #create lables
  print(providers_bar + ggtitle("Taxonomies with fewest number of providers")) #print plot with a title. 
}


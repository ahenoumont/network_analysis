#Cleaning Colaborator list
library(tidyverse)


##Defining Procedures
#Clean up names
clean_up <- function(name){
  name <- str_remove(name, "Sen\\.\\s") %>% 
  str_remove("\\s\\[.+$")
  return(name)
}

#Get Collaborators by iterating through each list to grab the  bill, name and role(sponsor/cosponsor)
#Returns data table
get_collaborators <- function(raw_data) {
  for(billnumber in 1:length(raw_data)){
    #populate sponsor into df
    bill <- raw_data[[billnumber]]$title %>% unlist() %>% str_extract("S.+(?=\\s\\-)")
    type <- 'Sponsor'
    name <- raw_data[[billnumber]]$sponsor %>% unlist() %>% clean_up()
    
    if(!exists('dfbills')){
      dfbills <- data.frame(bill, type, name, stringsAsFactors = FALSE)
    } else{
      dfbills <- rbind(dfbills, c(bill, type, name))
    }
    
    #Populate cosponsors
    type <- 'Cosponsor'
#    if (length(raw_data[[billnumber]]$cosponsor) > 1){
      for (i in 1:length(raw_data[[billnumber]]$cosponsor)) {
        name <- raw_data[[billnumber]]$cosponsor[[i]] %>% clean_up()
        
        #bind to df
        if(!exists('dfbills')){
          dfbills <- data.frame(bill, type, name, stringsAsFactors = FALSE)
        } else{
          dfbills <- rbind(dfbills, c(bill, type, name))
        }
        
      }
#    } else{
#      name <- raw_data[[billnumber]]$cosponsor %>% unlist()
#    }
  }
  
  return(dfbills)
}

#Make a table for bills with number, description, sponsor and stats
get_billstats <- function(raw_data) {
  for( billnumber in 1:length(raw_data)) {
    bill <- raw_data[[billnumber]]$title %>% unlist() %>% str_extract("S.+(?=\\s\\-)")
    desc <- raw_data[[billnumber]]$title %>% unlist() %>% str_extract("(?<=\\s\\-\\s).*")
    sponsor <- raw_data[[billnumber]]$stat %>% unlist() %>% str_extract("(?<=Sen\\.\\s).*(?=\\s\\[)")
    current_co <- raw_data[[billnumber]]$stat %>% unlist() %>% str_extract("[:digit:]+(?=\\scurrent)")
    original_co <- raw_data[[billnumber]]$stat %>% unlist() %>% str_extract("[:digit:]+(?=\\soriginal)")
    
    #bind to df
    if(!exists('billstat')){
      billstat <- data.frame(bill, desc, sponsor, current_co, original_co, stringsAsFactors = FALSE)
    } else{
      billstat <- rbind(billstat, c(bill, desc, sponsor, current_co, original_co))
    }
  }
  return(billstat)
}

#Execution
#Open data retrieved from scraer
raw_data <- readRDS('datasets/test_bills')

#Parse df from raw data
colab <- get_collaborators(raw_data)

stats <- get_billstats(raw_data)
#Test validity of data
#There should be 1 sponsor for each bill and 0 or more cosponsors
# Cosponsors may withdraw or be added to the bill, there for original and current numbers are available on the page
# We need to discover which is  represented on the list
stats %>% select(bill, current_co, original_co)

colab %>% 
  filter(name != 'No Cosponsors found') %>% 
  mutate(number = str_extract(bill, '[:digit:]+') %>% as.numeric()) %>% 
  group_by(number) %>% 
  summarise(cosponsors = sum(type == 'Cosponsor'))

#Names include current co sponsors

#Create matrix

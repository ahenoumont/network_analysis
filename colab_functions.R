#Colab Functions

library(RSelenium)
library(tidyverse)

#Scrapers########################333
##################################

#Get one bill
get_bill_from_page <- function(bill_number, sleep = 10, OpenDriver = TRUE, WebDriver=NA) {
  
  if (OpenDriver==TRUE){
    gc()
    rD <- rsDriver(browser = 'chrome')
    WebDriver <- rD[['client']]
  } else if (is.na(WebDriver)) {
    stop("WebDriver must be input if OpenDriver is FALSE.")
  }
  
  index <- bill_number
  number <- str_c('S.', bill_number)
  WebDriver$navigate(str_c('https://www.congress.gov/bill/115th-congress/senate-bill/', as.character(bill_number), '/cosponsors')) #Go to page
  
  webElement <- WebDriver$findElement(using = "css", ".legDetail") #Get title
  title <- webElement$getElementText() %>% unlist()
  
  webElement <- WebDriver$findElement(using = "css", "#display-message") #Get stats
  stat <- webElement$getElementText() %>% unlist()
  
  webElement <- WebDriver$findElement(using = "css", "#display-message a") #Get sponsor
  sponsor <- webElement$getElementText() %>% unlist()
  
  webElement <- try(WebDriver$findElements(using = "css", ".actions a")) #Get cosponsor element
  
  #iterate through object
  if(length(webElement) > 0){
    cosponsor <- list()
    for (i in 1:length(webElement)){ 
      cosponsor[i] <- webElement[[i]]$getElementText() %>% unlist()
    } 
  } else {
      cosponsor <- 'No Cosponsors found'
  }
  
  bill <- list(index = index, number = number, title = title, stat = stat, sponsor = sponsor, cosponsor = cosponsor)
  
  Sys.sleep(sleep)
  
  if (OpenDriver==TRUE){
    #Close Browser
    WebDriver$close()
    rD[['server']]$stop()
  }
  return(bill)
}

#Get multiple bills
get_more_bills <- function(range, sleep=10, output=NA){
  
  sleep=sleep
  #Open Driver
  gc()
  rD <- rsDriver(browser = 'chrome')
  WebDriver <- rD[['client']]
  
  bills <- map(range, get_bill_from_page, OpenDriver=F, sleep=sleep, WebDriver=WebDriver)
  
  #Close Driver
  WebDriver$close()
  rD[['server']]$stop()
  
  if(!is.na(output)){
    saveRDS(object = bills, file = output, compress = FALSE)
  }
  
  return(bills)
}

# Collect senators returns dataframe
get_senators <- function(url, filename=NA) {
  #This function opens browser collects names and details an return dataframe
  #Input: url
  #Output dataframe
  
  #Open browser
  rD <- rsDriver(browser = 'chrome')
  remDr <- rD[['client']]
  
  #Initialize variables
  name <- vector()
  details <- vector()
  state <- vector()
  party <- vector()
  senateyrs <- vector()
  houseyrs <- vector()
  
  
  #Go to page  
  remDr$navigate(str_c(url))
  
  #Extract Name into vector
  webElement <- remDr$findElements(using = 'css', value =".expanded .result-heading a")
  for (el in 1:length(webElement)) {
    name[el] <- webElement[[el]]$getElementText() %>% unlist() %>% str_remove('Senator ')
  }
  
  #Extract details into vector
  webElement <- remDr$findElements(using = 'css', value =".expanded .quick-search-member")
  for (el in 1:length(webElement)) {
    #details[el] <- webElement[[el]]$getElementText() %>% unlist()
    state[el] <- webElement[[el]]$getElementText() %>% unlist() %>% str_extract('(?<=State:\\s).+(?=\\\n)')
    party[el] <- webElement[[el]]$getElementText() %>% unlist() %>% str_extract('(?<=Party:\\s).+(?=\\\n)')
    senateyrs[el] <- webElement[[el]]$getElementText() %>% unlist() %>% str_extract('(?<=Senate:\\s).+')
    houseyrs[el] <- webElement[[el]]$getElementText() %>% unlist() %>% str_extract('(?<=House:\\s).+')
  }
  
  #Zip vectors into dataframe bind if multiple pages
  houseyrs[is.na(houseyrs)] <- 0
  senateyrs[is.na(senateyrs)] <- 0
  senators <- data.frame(name, state, party, senateyrs, houseyrs, stringsAsFactors = FALSE)
  
  remDr$close()
  rD[['server']]$stop()
  
  if (!is.na(filename)) {write.csv(senators, file = filename)}
  return(senators)
} #End Function


###Cleaners################################33
############################################

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
    index <- raw_data[[billnumber]]$index %>% unlist()
    bill <- raw_data[[billnumber]]$title %>% unlist() %>% str_extract("S.+(?=\\s\\-)")
    desc <- raw_data[[billnumber]]$title %>% unlist() %>% str_extract("(?<=\\s\\-\\s).*")
    sponsor <- raw_data[[billnumber]]$stat %>% unlist() %>% str_extract("(?<=Sen\\.\\s).*(?=\\s\\[)")
    current_co <- raw_data[[billnumber]]$stat %>% unlist() %>% str_extract("[:digit:]+(?=\\scurrent)")
    original_co <- raw_data[[billnumber]]$stat %>% unlist() %>% str_extract("[:digit:]+(?=\\soriginal)")
    
    #bind to df
    if(!exists('billstat')){
      billstat <- data.frame(index, bill, desc, sponsor, current_co, original_co, stringsAsFactors = FALSE)
    } else{
      billstat <- rbind(billstat, c(index, bill, desc, sponsor, current_co, original_co))
    }
  }
  return(billstat)
}



















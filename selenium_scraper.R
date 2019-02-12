#This scrip opens webdriver and opens all the pages of senate bills
# then captures the sponsor and cosponsor into list/df
# if a bill does not have a cosponsor, then the function must catch the error and return an na

library(RSelenium)
library(stringr)

# Scraper for bills returns list
fetch_collaborators <- function(start=1, stop=10, output=NA) {
  
  #Start Selenium
  rD <- rsDriver(browser = 'chrome')
  WebDriver <- rD[['client']]

  #Initialize vars
  bill <- list()
  for( i in start:stop){
    bill[[i]] <- list()
  }
  
  for (bill_number in start:stop) {
    #Get url
    #go to page
    WebDriver$navigate(str_c('https://www.congress.gov/bill/115th-congress/senate-bill/', bill_number, '/cosponsors'))
    
    #Bill title
    webElement <- WebDriver$findElement(using = "css", ".legDetail")
    bill[[bill_number]]$title <- webElement$getElementText()
    
    #Get stats
    webElement <- WebDriver$findElement(using = "css", "#display-message")
    bill[[bill_number]]$stat <- webElement$getElementText()
    
    #Get sponsor
    webElement <- WebDriver$findElement(using = "css", "#display-message a")
    bill[[bill_number]]$sponsor <- webElement$getElementText()
    
    #Get cosponsors unknown number of cosponsors
    webElement <- try(WebDriver$findElements(using = "css", ".actions a"))
    
    #iterate through object
    if(length(webElement) > 0){
      for (i in 1:length(webElement)){ 
        bill[[bill_number]]$cosponsor[i] <- webElement[[i]]$getElementText()
      } 
    } else {
      bill[[bill_number]]$cosponsor <- 'No Cosponsors found'
    }
    
    #Give Server a break
    Sys.sleep(1)
  } #End for loop
  
  #Close Browser
  WebDriver$close()
  rD[['server']]$stop()
  
  #Output
  if(is.na(output)) {
  return(bill)
  } else {
    saveRDS(bill, file = str_c('datasets/', output))
  }  
} #End function

# Collect senators returns dataframe
get_senators <- function(url) {
  #This function opens browser collects names and details an return dataframe
  #Input: url
  #Output dataframe
  
  #Open browser
    rD <- rsDriver(browser = 'chrome')
    remDr <- rD[['client']]
  
  #Initialize variables
    names <- vector()
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
      names[el] <- webElement[[el]]$getElementText() %>% unlist() %>% str_remove('Senator ')
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
    senators <- data.frame(names, state, party, senateyrs, houseyrs, stringsAsFactors = FALSE)

  remDr$close()
  rD[['server']]$stop()
  
  return(senators)
}

#Execution

fetch_collaborators(output = 'test_bills')
url <- 'https://www.congress.gov/search?q={%22source%22:%22members%22,%22congress%22:%22115%22,%22chamber%22:%22Senate%22}&pageSize=250'
senators <- get_senators(url)


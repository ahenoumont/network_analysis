#Workflow
source('colab_functions.R')
############################################
#Part 1: Scrape
########################################################################
#Endstate: Retrieve all Senate Bills from 115th Congress, with title, sponsor and cosponsor
#Retrieve senator information, including state, party

#Senator table
url <- 'https://www.congress.gov/search?q={%22source%22:%22members%22,%22congress%22:%22115%22,%22chamber%22:%22Senate%22}&pageSize=250'
file <- 'datasets/senators.csv'
#senators <- get_senators(url, file)

#Get collaborator files
#Theres a total of 3805 bills
path <- "data_pull/"
sleep = 2 # as per congres.gov/robots.txt

get_more_bills(1:500, sleep=sleep, output = str_c(path,'bills_1_500_', Sys.Date()))
get_more_bills(501:1000, sleep=sleep, output = str_c(path,'bills_501_1000_', Sys.Date()))
get_more_bills(1001:1500, sleep=sleep, output = str_c(path,'bills_1001_1500_', Sys.Date()))
get_more_bills(1501:2000, sleep=sleep, output = str_c(path,'bills_1501_2000_', Sys.Date()))
get_more_bills(2001:2500, sleep=sleep, output = str_c(path,'bills_2001_2500_', Sys.Date()))
get_more_bills(2501:3000, sleep=sleep, output = str_c(path,'bills_2501_3000_', Sys.Date()))
get_more_bills(3001:3500, sleep=sleep, output = str_c(path,'bills_3001_3500_', Sys.Date()))
get_more_bills(3501:3805, sleep=sleep, output = str_c(path,'bills_3501_3805_', Sys.Date()))

#Clear environment
rm(list = setdiff(ls(), lsf.str()))
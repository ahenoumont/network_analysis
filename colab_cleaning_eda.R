source('colab_functions.R')
######################################################################
#Part 2: Remove missing variables
#############################################
#Import data
#Import lists from data pull 
path <- "data_pull/"
files <- str_c(path,list.files(path = path, pattern = 'bills.*'))
raw_data <- map(files, readRDS) #Creates list of 5 lists

raw_data <- raw_data %>% unlist(recursive = F) #remove outer list


#Create df to look at results
stats <- get_billstats(raw_data)

map(stats, function(x) length(which(is.na(x)))) # There are 90 missing bills
missing_bills <- which(is.na(stats$bill))

#Scraper run if missing files
#get_more_bills(missing_bills, sleep = 2, output = str_c(path, "missing_bills", Sys.Date()))
#Endstate: dataframe with senator information, bill information, colaboration_matrix and edgelist

#Open file
files <- str_c(path,list.files(path = path, pattern = '^missing.*'))
bills_missing <- readRDS(files)

#get rid of na and bind missing bills arrange by na
for(i in 1:length(bills_missing)){
  raw_data[[bills_missing[[i]]$index]] <- bills_missing[[i]]
}

#Repeated from above to see if we got all the missing entries
stats <- get_billstats(raw_data)

map(stats, function(x) length(which(is.na(x)))) # There are 90 missing bills
missing_bills <- which(is.na(stats$bill))

#Missing data has been cleared
saveRDS(raw_data, file = 'datasets/complete_bill_list', compress = FALSE)
write_csv2(stats, path = 'datasets/bill_stats.csv')

#Clear environment
rm(list = setdiff(ls(), lsf.str()))
###################################
# Part 2.1 Manipulate data
##################################
senators <- read_csv('datasets/senators.csv')
bill_stats <- read_csv2('datasets/bill_stats.csv')
bill_data <- readRDS('datasets/complete_bill_list')

#Runs colaborator function in clab_function.R creates table of bill and senate participant
colab <- get_collaborators(bill_data)


#####Create collaboration matrix and edgelist

# Networks are composed of 2 elements, Nodes and Edges.
# Nodes: The senators (Collected in senators df)
# Edges: Sponsor--Cosponsor relationship (needs to be built)

#Initialize matrix (# of bills, # of senators)
collab_matrix <- matrix(nrow = nrow(bill_stats), ncol = nrow(senators), dimnames = list(bill_stats$bill, senators$name) )

#fill matrix
for (i in 1:nrow(colab)) {
  if (colab[i,]$name %in% colnames(collab_matrix)) {
    collab_matrix[colab[i,]$bill, colab[i,]$name] <- colab[i,]$type
  }
}


#create edgelist
edgelist_collab <- data.frame()

for (i in 1:nrow(collab_matrix)) {
  if (length(which(!is.na(collab_matrix[i,]))) > 1) {
    edgelist_collab <- rbind(
      edgelist_collab,
      colnames(collab_matrix)[which(!is.na (collab_matrix[i,]))] %>% combn(m = 2) %>% t()
    )
  }
}
names(edgelist_collab) <- c("Source", "Target")
####Feature engineering
#Kyl, Jon has non concurent years in the senate, index 57
head(senators)
senators_with_seniority <- senators[-57,] %>% 
  mutate(senate_yrs = str_replace(senateyrs, "Present", '2018') %>% str_split("-") %>% map(as.numeric) %>% map(diff) %>% unlist(),
                                                     house_yrs = ifelse(houseyrs != 0, str_split(houseyrs, "-") %>% map(as.numeric) %>% map(diff) %>% unlist(), 0),
                                                     congress_yrs = house_yrs + senate_yrs) %>% 
  select(-senateyrs, -houseyrs)

#Dealing with Kyl
senators_with_seniority[57,]
kyl = senators[57,] %>% mutate(senate_yrs = 19, #manual calculation "1995-2013, 2018-2019"
                               house_yrs = ifelse(houseyrs != 0, str_split(houseyrs, "-") %>% map(as.numeric) %>% map(diff) %>% unlist(), 0),
                               congress_yrs = house_yrs + senate_yrs) %>% 
  select(-senateyrs, -houseyrs)     

senators_with_seniority <- rbind(senators_with_seniority, kyl)                    

summary(senators_with_seniority)

#Adding participation to senator database
result <- matrix(NA, ncol= ncol(collab_matrix), nrow = 2, dimnames = list( c("Sponsor", "Cosponsor"), colnames(collab_matrix) ))

# Creates df of collaboration by senators
for (i in 1:ncol(collab_matrix)) {
  result[1,i] <- sum(collab_matrix[, i] == "Cosponsor", na.rm = TRUE)
  result[2,i] <- sum(collab_matrix[, i] == "Sponsor", na.rm = TRUE)
}

result = result %>% t() %>% as.data.frame()
result$name <- rownames(result)

senators_with_seniority_participation <- senators_with_seniority %>% 
  left_join(result, by = "name" ) %>% 
  mutate(participation = Sponsor + Cosponsor)

senators_with_seniority_participation %>% head()
clean_senators <- senators_with_seniority_participation %>% rename(Id = X1)


#Indexing Edgelist
edgelist_id <- edgelist_collab %>% as.data.frame() %>% left_join(clean_senators, by = c("Source" = "name")) %>% select(Id, Target) %>% rename(Source = Id)
edgelist_id <- edgelist_id %>% as.data.frame() %>% left_join(clean_senators, by = c("Target" = "name")) %>% select(Source, Id) %>% rename(Target = Id)
head(edgelist_id)
#save work
saveRDS(collab_matrix, 'datasets/collaboration_matrix.rds')
write_csv2(edgelist_collab, 'datasets/edgelist_collab.csv')
write_csv2(edgelist_id, 'datasets/edgelist_id.csv')
write_csv2(clean_senators, 'datasets/senators_clean.csv')

#Clear workspace?
rm(list = setdiff(ls(), lsf.str()))
#########
#######################################
#Part 3 EDA
########################################

##EDA and other descriptive Statistics
senators <- read_csv2('datasets/senators_clean.csv')

#Senators
# What is the average participitation in a 2 year period
median(senators$participation)

#Basic statistics by party
senators %>% 
  group_by(party) %>% 
  summarise(number = n(), 
            med_sponsor = median(Sponsor), 
            sd_sponsor = sd(Sponsor), 
            med_cosponsor = median(Cosponsor),
            med_partic = median(participation)) %>% 
  select(party, number, med_sponsor, med_cosponsor, med_partic)

# Who has sponsored the most bills?
senators %>% select(name, party, state, participation) %>%  arrange(desc(participation)) %>% head(10)

# Who has the most/least participation by party?
top_dem <- senators %>% select(name, party, state, Sponsor) %>% filter(party == "Democratic")%>% arrange(desc(Sponsor)) %>% head(5)
top_rep <- senators %>% select(name, party, state, Sponsor) %>% filter(party == "Republican")%>% arrange(desc(Sponsor)) %>% head(5)

#Chart top by Party
bind_rows(top_rep, top_dem) %>%
  ggplot(aes(x = reorder(name, -Sponsor), y = Sponsor, fill = party)) + geom_col()+coord_flip() 

#Density by party
ggplot(senators, aes(x=Sponsor, fill=party))+
  geom_density(alpha = 0.6)


senators %>% arrange(desc(congress_yrs)) %>% head(10)
#correlation with time in congress
library(GGally)
ggpairs(senators[5:ncol(senators)], )

#Participation by tenure, party
ggplot(senators, aes(x=Cosponsor, y=Sponsor, size = senate_yrs, color= factor(party), label = name)) + geom_text()
ggplot(senators, aes(x=Cosponsor, y=Sponsor, size = senate_yrs, color= factor(party), label = name)) + geom_point()
ggplot(senators, aes(x=congress_yrs, y=participation, size = congress_yrs, color= factor(party), label = name)) + geom_point()

##Bills 
bill_stats <- read_csv2('datasets/bill_stats.csv')
#Which bills have the most participitation/least?
bill_stats %>% arrange(desc(current_co)) %>% head(10)
bill_stats %>% arrange(desc(original_co)) %>% head(10)
#Biggest change on cosponsors
bill_stats %>% mutate(delta = current_co - original_co) %>% arrange(desc(delta)) %>% head(10)

##Gold Medal Acts
#There is high number of collaboration in Gold Medal Acts which may distort the network analysis
bill_stats %>% filter(grepl("Gold Medal", desc)) %>% arrange(desc(current_co))
#Notice some of the recipients

#Clear Workspace
rm(list = setdiff(ls(), lsf.str()))
###########################################

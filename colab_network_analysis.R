###########################################
#Network Analysis with igraph
###########################################
library(tidyverse)
library(igraph)
library(ggraph)
#import edge list
nodes <- read_csv2('datasets/senators_clean.csv')
ties <- read_csv2('datasets/edgelist_id.csv')

#Condense edgelist to merge weights
ties <- ties %>% 
  mutate(weight = 1) %>% 
  group_by(Source, Target) %>% 
  summarise(weight = sum(weight)) %>%
  ungroup() %>% 
  arrange(desc(weight))

head(ties)

nrow(ties) # There are 5357 distinct relationships
#Create igraph instance
g <- graph_from_data_frame(d = ties, directed = FALSE, vertices = nodes)

##Add Network stats to nodes and ties
#Add degree, strength, closeness, betweenness centrality  
nodes <- nodes %>% 
  mutate(degree = degree(g),
         strength = strength(g),
         closeness = closeness(g),
         betweenness = betweenness(g))

head(nodes)
# Betweenness of edges
dist_weight = 1 / ties$weight

#Add edge betweenness 
ties_with_betweenness <- ties %>% 
  mutate(betweenness = edge_betweenness(g, directed = F, weights = dist_weight)) 

#Join names to edge list
ties_joined <- ties_with_betweenness %>% 
  left_join(nodes, by = c("Source" = "Id")) %>% 
  left_join(nodes, by = c("Target" = "Id")) 

ties_selected <- ties_joined %>% select(Source, Target, name_source = name.x, name_target = name.y, weight, betweenness)

#recalculate gr
g <- graph_from_data_frame(d = ties_selected, directed = FALSE, vertices = nodes)

rm(list= c('ties_joined', 'ties_with_betweenness', 'ties', 'dist_weight'))
##############################
####Analysis
##################

##Degree/Connections of nodes
#Who has the least connections

#Who has the most connections
nodes %>% count(degree) # We see that all but 2 senators are connected to over 100 of the 105 senators, so this might not be a usefull measure
#Who are the 2 with the least connections
nodes %>% arrange(degree) %>% select(-senate_yrs, -house_yrs, -congress_yrs) %>% head(2)
# Kyl and Sessions

#Strength in connectons of nodes
nodes %>% count(strength) %>% nrow() 

strength_gt_sd <- mean(nodes$strength)
hist(nodes$strength)
abline(v = strength_gt_sd, col='red', lwd=2, lty=2)

nodes %>% filter(strength > strength_gt_sd)  %>% arrange(desc(strength)) %>% select(-senate_yrs, -house_yrs, -congress_yrs) %>% head(10)

nodes %>% arrange(desc(closeness)) %>% select(-senate_yrs, -house_yrs, -congress_yrs) %>% head(10)


nodes %>% filter(grepl('Rep',party)) %>% arrange(desc(degree), desc(strength)) %>% select(-senate_yrs, -house_yrs, -congress_yrs) %>% head(10)
nodes %>% filter(grepl('Rep',party)) %>% arrange(desc(strength)) %>% select(-senate_yrs, -house_yrs, -congress_yrs) %>% head(5)

nodes %>% filter(grepl('Dem',party)) %>% arrange(desc(degree)) %>% select(-senate_yrs, -house_yrs, -congress_yrs) %>% head(5)
nodes %>% filter(grepl('Dem',party)) %>% arrange(desc(strength)) %>% select(-senate_yrs, -house_yrs, -congress_yrs) %>% head(5)

nodes %>% filter(grepl('Ind',party)) %>% arrange(desc(degree)) %>% select(-senate_yrs, -house_yrs, -congress_yrs) %>% head(5)
nodes %>% filter(grepl('Ind',party)) %>% arrange(desc(strength)) %>% select(-senate_yrs, -house_yrs, -congress_yrs) %>% head(5)


ggraph(g, layout = 'with_kk')+
  geom_node_point(aes(size = strength, col = factor(party)))

mean(ties_selected$betweenness)
thin <- mean(E(g)$betweenness) + sd(E(g)$betweenness)
ggraph(g, layout = 'with_kk')+
  geom_edge_link(aes(alpha = betweenness, filter = betweenness > thin))+
  geom_node_point(aes(size = strength, color = factor(party))) +
  geom_node_point(aes(size = betweenness, color = factor(party))) +
  geom_node_text(aes(label = name, size = betweenness, filter = betweenness > 1000), repel = T, nudge_x = 1)
  
#John Kyl and Jeff Sessions 
  ties_selected %>% 
  filter(betweenness > thin) %>% 
  arrange(desc(betweenness))


#Get adjacency matrix
adj_collab <- get.adjacency(collab_network)

#Since there exists no directional relationship the matrix is symetric. Therefore we can remove the lower triangle before calculating the greatest binary relationships
adj_collab[lower.tri(adj_collab)] <- 0

#Greatest relationships
#Next we can extract the dyads with the strongest binary relationships
colab_10 <- min(sort(as.matrix(adj_collab), decreasing = TRUE)[1:10])
max_indices <- which(as.matrix(adj_collab) >= colab_10, arr.ind = TRUE)
#Theres a tie for number 10 so 11 will be displayed
export_names <- matrix(NA, ncol = 2, nrow = 11)
for (i in 1:nrow(max_indices)) {
  export_names[i, 1] <- rownames(adj_collab)[max_indices[i,1]]
  export_names[i, 2] <- rownames(adj_collab)[max_indices[i,2]]
}

export_names

#Next we want to symplify the network by transforming multiple edges to unique edges
#assigns weight of 1 for each connection
E(collab_network)$weight <- 1
collab_network_weighted <- simplify(collab_network)
collab_network_weighted
E(collab_network_weighted)$weight %>% length()

#After simplification the network still has 5357 unique edges. 
#In order to see key relationships in a visualization we can apply a thinning operation of
#the mean weight plus one standard deviation to isolate the more relevent relationships. 
E(collab_network_weighted)$weight %>% length()
#distribution of relationships
hist(E(collab_network_weighted)$weight,breaks = seq(0, 300, 5),
     main = "Distribution of collaborations per relationship from \n Senators in the 115th Congress (2017-2018)",
     sub = "\n Source: Congress.gov",
     xlab = "Collaborations")
abline(v=mean_weight +sd_weight, col = "red", lwd = 2, lty="dotted")
text(120, 400, expression(paste(mu, ' + ', sigma)), col="red")

mean_weight <- mean(E(collab_network_weighted)$weight)
sd_weight <- sd(E(collab_network_weighted)$weight)
plot_collab <- collab_network_weighted
plot_collab <- delete.edges(plot_collab, which(E(plot_collab)$weight < mean_weight + sd_weight))

E(plot_collab)$weight %>% length() #770 remaining edges

#Vizualizing
plot(plot_collab, 
     vertex.size=0, 
     vertex.color="white", 
     vertex.frame.color = NA, 
     vertex.label.color="black",
     edge.lty=1,
     edge.color="grey")


# The senators on the inside are the remaining senators after the thinning operation
#there is very llittle we can see therefore we will next process the data for Gephi, a more powerful network analysis tool.
rm(list = setdiff(ls(), lsf.str()))
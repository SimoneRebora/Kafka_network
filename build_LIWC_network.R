# Kafka Network Analysis
# built by SR 14/10/2020
# changed by JBH 15/10/2020

library(lsa)

# upload LIWC table
LIWC_analysis <- read.csv("LIWC_analyses/LIWC_analysis_140_experiential_epistemic.csv", row.names = 1, stringsAsFactors = F)

# exclude short texts
exclude <- which(LIWC_analysis$WC < 2000) # let's decide on threshold (5,000 or 2,000?)
rownames(LIWC_analysis)[exclude]
LIWC_analysis <- LIWC_analysis[-exclude,] 

# remove first two columns
LIWC_analysis <- LIWC_analysis[,-c(1,2)]

# reduce analysis to just a few selected columns
# my_selection <- 1:96
# LIWC_analysis <- LIWC_analysis[,my_selection]
# try also the other dimensions?

# calculate distance matrix using cosine
distance_matrix <- cosine(t(as.matrix(LIWC_analysis)))

# set number of nearest neighbours
nearest_neighbours <- 5

# loop on the distance matrix to create list of edges
edges_df <- data.frame()
for(i in 1:dim(distance_matrix)[1]){
  
  neighbours_list <- sort(distance_matrix[i,], decreasing = T)
  
  for(n in 1:nearest_neighbours){
    
    tmp_df <- data.frame(source = names(neighbours_list)[1], target = names(neighbours_list)[n+1], weight = nearest_neighbours-n+1, stringsAsFactors = F)
    edges_df <- rbind(edges_df, tmp_df)
    
  }
  
}

edges_df$type <- "undirected" # or "directed"

# write list of edges
write.csv(edges_df, file = paste("tables/edges_", nearest_neighbours, "neighbours.csv", sep = ""), row.names = F)

# prepare list of nodes
nodes_list <- unique(c(edges_df$source, edges_df$target))

# add author group
author <- strsplit(nodes_list, "_")
author <- unlist(lapply(author, function(x) x[1]))

# add another group (decades)
years <- strsplit(nodes_list, "_")
years <- unlist(lapply(years, function(x) x[3]))
years <- gsub(pattern = ".txt", replacement = "", years)

# function(s) for decade
floor_decade    = function(value){ return(value - value %% 10) }
ceiling_decade  = function(value){ return(floor_decade(value)+10) }
round_to_decade = function(value){ return(round(value / 10) * 10) }
decade <- floor_decade(as.numeric(years))

# add another group (nationality)
nationality <- strsplit(nodes_list, "_")
nationality <- unlist(lapply(nationality, function(x) x[4]))
nationality <- gsub(".txt", "", nationality)

nodes_df <- data.frame(ID = nodes_list, label = nodes_list, author, decade, nationality, stringsAsFactors = F)

# write list of nodes
write.csv(nodes_df, file = paste("tables/nodes_", nearest_neighbours, "neighbours.csv", sep = ""), row.names = F)

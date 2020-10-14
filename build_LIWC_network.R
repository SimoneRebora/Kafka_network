library(lsa)

# upload LIWC table
LIWC_analysis <- read.csv("LIWC_analysis.csv", row.names = 1, stringsAsFactors = F)

# remove first two columns
LIWC_analysis <- LIWC_analysis[,-c(1,2)]

# reduce analysis to just a few selected columns
my_selection <- 1:96
LIWC_analysis <- LIWC_analysis[,my_selection]

# calculate distance matrix using cosine
distance_matrix <- cosine(t(as.matrix(LIWC_analysis)))

# set number of nearest neighbours
nearest_neighbours <- 10

# loop on the distance matrix to create list of edges
edges_df <- data.frame()
for(i in 1:dim(distance_matrix)[1]){
  
  neighbours_list <- sort(distance_matrix[i,], decreasing = T)
  
  for(n in 1:nearest_neighbours){
    
    tmp_df <- data.frame(source = names(neighbours_list)[1], target = names(neighbours_list)[n+1], weight = nearest_neighbours-n+1, stringsAsFactors = F)
    edges_df <- rbind(edges_df, tmp_df)
    
  }
  
}

# write list of edges
write.csv(edges_df, file = paste("edges_", nearest_neighbours, "neighbours.csv", sep = ""), row.names = F)

# prepare list of nodes
nodes_list <- unique(c(edges_df$source, edges_df$target))
groups <- strsplit(nodes_list, "_")
groups <- unlist(lapply(groups, function(x) x[1]))

nodes_df <- data.frame(ID = nodes_list, label = nodes_list, groups, stringsAsFactors = F)

# write list of nodes
write.csv(nodes_df, file = paste("nodes_", nearest_neighbours, "neighbours.csv", sep = ""), row.names = F)
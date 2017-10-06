closeAllConnections()
graphics.off()
rm(list=ls()) #clear all variables

# set the working directory
#setwd("~/Dropbox/0Programs/R/Networks/Epidemics")

library(igraph) # Load the igraph package

################################################
# ###### READ FROM FILE ####
# Example of GML format
#Lesmis
#G = read_graph('lesmis.gml', format = c('gml'))
#G$layout <- layout.fruchterman.reingold
#plot(G, layout=layout.kamada.kawai, vertex.color="green")

#Networks in table format
#Hamster
str = 'hamster'
net <- read.table(paste(str,'.txt', sep = ''))

if(dim(net)[2] > 2){
  net[,3] = NULL
}
net = as.matrix(net)
if(min(net) == 0){
  net = net + 1
}
G <- graph.data.frame(net, directed=FALSE)
G = graph_from_edgelist(net, directed = FALSE)

######################################
######################################

#### extract the largest component ###
giant.component <- function(graph) {
  cl <- clusters(graph)
  induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
}
G <- giant.component(G)
clu <- components(G)


## transform to undirected networks
G = as.undirected(G)
G = simplify(G) #remove self loops and multiple edges
plot(G)

#A = as_adjacency_matrix(G)
#x = eigen(A) #eigenvectors and eigenvalues of A
N = vcount(G)
M = ecount(G) #number of edges

#### MEASURES ###
### degree
vk = degree(G)

### clustering
cc = transitivity(G, type="local")
cc2 = transitivity(G)
# betweenness centrality
bt = betweenness(G)
# closeness centrality
c = closeness(G)
### eigenvector centrality
eg = eigen_centrality(G)$vector
### PageRank
pr = page_rank(G, damping = 0.85)$vector



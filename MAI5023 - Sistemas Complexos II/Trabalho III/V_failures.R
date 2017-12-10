closeAllConnections()
graphics.off()
rm(list=ls())
#set.seed(101)

# Define the working directory
#setwd("~/Dropbox/0Programs/R/Networks/falhas-ataques")

library(igraph) # Load the igraph package

#### MODEL PARAMETERS ####
N = 500; # number of nodes
av.dg = 10; # average degree
m = av.dg/2; # number of new connections in BA model
q = 0.5 # probability of rewiring in WS model
p = av.dg/(N-1) # prob. of connections in ER model

#### extract the largest component ###
giant.component <- function(graph) {
  cl <- clusters(graph)
  induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
}

## MODELS ###
# BA network
G <- barabasi.game(N, m = av.dg/2) # generate a BA network

# WS network
#G <- sample_smallworld(dim=1,size=N, nei = av.dg/2, p = q)

#G <- giant.component(G)
#clu <- components(G)

#### FAILS ####
# BA network
#df = round(0.05*N) #number of removed nodes in each time step
df = 1 # remove only one node at a time
s = list() # list that stores the size of the largest component
t = list() # list that stores the fraction of removed nodes
net.size = vcount(G) # newtork size
step = 0
while(net.size > df){
  nodes = sample(seq(1,net.size),df) # select df*N nodes at random
  G = delete_vertices(G,nodes) # delete a hub
  G <- giant.component(G) # calculate the largest component
  net.size = vcount(G) # size of the largest component
  # Note that you can change the measure, as the average shortest path length.
  #measure = mean(shortest.paths(G))
  measure = vcount(G) # the analyzed measure is the size of the largest component.
  s = append(s,measure) # store the measure
  step = step + df # calculate the next fraction
  t = append(t,step)  # store the fraction of removed nodes
}
s = unlist(s) # unlist to plot
t = unlist(t)
plot(t,s/N,xlab = "Fraction of removed nodes", 
     ylab = "S/S0", type="l",lwd=2, col = 'blue', xlim = c(0,N), ylim = c(0,1))
par(new=TRUE) # used to put two curves in the same plot

# ER network
G2 <- erdos.renyi.game(N,p, type =c("gnp")) # create a ER network
y = list() # store the size of the largest component or other measure
x = list() # store the fraction of removed nodes
net.size = vcount(G2) # network size
step = 0 
while(net.size > df){# remove almost all nodes
  nodes = sample(seq(1,net.size),df) #select df*N nodes at random
  G2 = delete_vertices(G2,nodes) # delete this node
  G2 <- giant.component(G2) # calculate the largest component
  net.size = vcount(G2) # network size
  #measure = mean(shortest.paths(G2))
  measure = vcount(G2) # the analyzed measure is the size of the largest component.
  y = append(y,measure) # store the measure
  step = step + df # increase the fraction of removed nodes
  x = append(x,step)  # store the fraction of removed nodes
}
y = unlist(y) # unlist to plot
x = unlist(x)
plot(x,y/N, xlab = "Fraction of removed nodes", 
     main = "Failures - Most connected nodes removal",
     ylab = "S/S0", type="l",lwd=2, col = 'red', xlim = c(0,N), ylim = c(0,1))
legend('topright', c('BA','ER'), lty=1, col=c('red', 'blue'), bty='n', cex=.75)


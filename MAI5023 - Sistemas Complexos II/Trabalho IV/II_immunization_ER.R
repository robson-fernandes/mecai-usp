closeAllConnections()
#graphics.off()
rm(list=ls()) #clear all variables
#set.seed(101) #start the random seed

# set the working directory
#setwd("~/Dropbox/0Programs/R/Networks/Epidemics")

library(igraph) # Load the igraph package

#### MODEL PARAMETERS ####
N = 50; #number of nodes
av.dg = 8; #average degree
m = av.dg/2; # parameter of the BA model
q = 0.1 # rewiring probability in the WS model
p = av.dg/N # probability in the ER model

## MODELS ###
# BA network
#G <- barabasi.game(N, m = av.dg/2, directed = FALSE)
#str = "BA"
G_aleatoria <- erdos.renyi.game(N,p, type =c("gnp"))
# # ER network
G_hub <- sample_pa(N, power = 1, m = av.dg/2)
#str = "ER"
# # WS network
#G <- sample_smallworld(dim=1,size=N, nei = av.dg/2, p = q)
#str = "WS"
# ###### READ FROM FILE ####
#net <- read.table("test-star.txt")
#G <- graph.data.frame(net, directed=FALSE)
#plot(G, layout=layout.kamada.kawai, vertex.color="green")

#G <- make_graph("Zachary") # you can use the Zachary karate club network to test

#### SIR MODEL ####
# states: S:0 I:1 R:2
## Parameters of the SIR model
mu = 0.1 # probability of recovering
beta = 0.2 # probability of transmission
Tmax = 100

f_aleatoria <- function (G)
{
vertice = as.numeric(V(G))
grau = degree(G)
ver_grau = data.frame(vertice,grau)
ver_grau = ver_grau[order(ver_grau$grau,decreasing = T),]
grau = sort(degree(G), decreasing = T) #ordena os grau
targetnodes = seq(1,N)#elemento infectados
Ninf = matrix(0,nrow = N, ncol = Tmax) # matrix that stores the number of infected nodes at each time step
# Ninf[i,t] yields the number of infected nodes at time t when the infection starts on i

for(i in targetnodes){
  # is the seed node
  vstates = matrix(0, nrow = N, ncol = 1)
  print(paste('seed:', i))
  
  recuperado = ver_grau[1:10,1]
  vstates[recuperado] = 2
  
  vstates[i] = 1
  vinfected = which(vstates %in% 1)
  t = 1
  while(length(vinfected) > 0){ # while there are infected nodes
    vinfected = which(vstates %in% 1)
    
    # try to infect all the neighbors of infected nodes at step t
    for(j in vinfected){
      ng = neighbors(G, j)
      for(k in ng){
        if(runif(1,0,1) <= beta){
          if(vstates[k] == 0){# infect only susceptible nodes
            vstates[k] = 1
          } 
        }
      }
    }
    # try to recover all infected nodes at step (t+1)
    for(j in vinfected){
      if(runif(1,0,1) <= mu){
        vstates[j] = 2
      }
    }
    Ninf[i, t] = length(which(vstates %in% 1))/N # store the fraction of infected nodes at time t
    t = t + 1
  }
}
  rhoi = colMeans(Ninf) # average number if infected nodes from the result of each seed node
  return(rhoi)

}

rhoi_aleatory <- f_aleatoria(G_aleatoria)
t_aleatory = seq(1,length(rhoi_aleatory))

rhoi_hub <- f_aleatoria(G_hub)
t_hub = seq(1,length(rhoi_hub))


#----------------------------------------
#  Immunization ER
#----------------------------------------
plot(t_aleatory, rhoi_aleatory, xlab = "Time", ylab = "Fraction of infected nodes", 
     col = 12, lwd=5, xlim = c(0,Tmax), ylim = c(0,1), pch = 21, 
     type="p", main = "Immunization - Erdos Rényi")
par(new=T)
plot(t_hub, rhoi_hub, xlab = "Time", ylab = "Fraction of infected nodes", 
     col = 18, lwd=5, xlim = c(0,Tmax), ylim = c(0,1), pch = 21, 
     type="p", main = "Immunization - Erdos Rényi")
legend("topleft", legend = c("Aleatory","Hub"), col = c(12,18), lty = 1)
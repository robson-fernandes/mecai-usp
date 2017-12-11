closeAllConnections()
graphics.off()
rm(list=ls()) #clear all variables
#set.seed(101) #start the random seed

# set the working directory
#setwd("~/Dropbox/0Programs/R/Networks/Epidemics")

library(igraph) # Load the igraph package

#### MODEL PARAMETERS ####
N = 100; #number of nodes
av.dg = 8; #average degree
m = av.dg/2; # parameter of the BA model
q = 0.1 # rewiring probability in the WS model
p = av.dg/N # probability in the ER model

## MODELS ###
# BA network
G_ba <- barabasi.game(N, m = av.dg/2, directed = FALSE)
#str = "BA"
#G <- sample_pa(N, power = 1, m = av.dg/2)
# ER network
G_er <- erdos.renyi.game(N,p, type =c("gnp"))
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

grau_ba = as.vector(1)
bet_ba = as.vector(1)
page_ba = as.vector(1)
recuperacao_ba = as.vector(1)
coreness_ba = as.vector(1)
  
grau_er = as.vector(1)
bet_er = as.vector(1)
page_er = as.vector(1)
recuperacao_er = as.vector(1)
coreness_er = as.vector(1)

targetnodes = seq(1,N)#elemento infectados
Tmax = 100
Nrec = matrix(0,nrow = N, ncol = Tmax) # matrix that stores the number of infected nodes at each time step
# Ninf[i,t] yields the number of infected nodes at time t when the infection starts on i


f_networkRecuperado_BA <- function(G)
{
  for(i in 1:N){
    grau_ba[i] <<- degree(G,v = i)
    bet_ba[i] <<-  betweenness(G, v = i)
    page_ba[i] <<-  page.rank(G,vids = i)$vector
    coreness_ba[i] <<- coreness(G) 
    
    # is the seed node
    vstates = matrix(0, nrow = N, ncol = 1)
    print(paste('seed:', i))
    vstates[i] = 1
    vinfected = which(vstates %in% 1)
    t = 1
    controle_loop = 0
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
      Nrec[i, t] = length(which(vstates %in% 2))/N # store the fraction of infected nodes at time t
      t = t + 1
      controle_loop = controle_loop+1
    }
    recuperacao_ba[i] <<- mean(Nrec[i,1:controle_loop])
  }
  
  return(recuperacao_ba)
}


f_networkRecuperado_ER <- function(G)
{
  for(i in 1:N){
    grau_er[i] <<- degree(G,v = i)
    bet_er[i] <<-  betweenness(G, v = i)
    page_er[i] <<-  page.rank(G,vids = i)$vector
    coreness_er[i] <<- coreness(G) 
    
    # is the seed node
    vstates = matrix(0, nrow = N, ncol = 1)
    print(paste('seed:', i))
    vstates[i] = 1
    vinfected = which(vstates %in% 1)
    t = 1
    controle_loop = 0
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
      Nrec[i, t] = length(which(vstates %in% 2))/N # store the fraction of infected nodes at time t
      t = t + 1
      controle_loop = controle_loop+1
    }
    recuperacao_er[i] <<- mean(Nrec[i,1:controle_loop])
  }
  
  return(recuperacao_er)
}


recuperado_ba = f_networkRecuperado_BA(G_ba)
recuperado_er = f_networkRecuperado_ER(G_er)

#----------------------------------------
#  Recuperado vs Grau
#----------------------------------------
plot(grau_er, recuperado_er, xlab = "Degree", ylab = "Fraction of recovered nodes",
     col = 'black', lwd = 5, xlim = c(0,max(c(grau_er,grau_ba))), ylim = c(0,1), pch = 1, 
     type="p", main = "Influential Propagators - Degree")
par(new = T)
plot(grau_ba, recuperado_ba, xlab = "Degree", ylab = "Fraction of recovered nodes",
     col = 'blue', lwd = 5, xlim = c(0,max(c(grau_er,grau_ba))), ylim = c(0,1), pch = 1, 
     type="p",  main = "Influential Propagators - Degree")
legend("bottomright", legend = c("ER","BA"), col = c("black","blue"), lty = 1)

#----------------------------------------
#  Recuperado vs Betweenees
#----------------------------------------
plot(bet_er, recuperado_er, xlab = "Betweenness", ylab = "Fraction of recovered nodes", 
     col = 'black', lwd=5, xlim = c(0,max(c(bet_er,bet_ba))), ylim = c(0,1), pch = 21, 
     type="p", main = "Influential Propagators - Betweenness")
par(new = T)
plot(bet_ba, recuperado_ba, xlab = "Betweenness", ylab = "Fraction of recovered nodes", 
     col = 'blue', lwd=5, xlim = c(0,max(c(bet_er,bet_ba))), ylim = c(0,1), pch = 21, 
     type="p", main = "Influential Propagators - Betweenness")
legend("bottomright", legend = c("ER","BA"), col = c("black","blue"), lty = 1)

#----------------------------------------
#  Recuperado vs Page Rank
#----------------------------------------
plot(page_er, recuperado_er, xlab = "Page Rank", ylab = "Fraction of recovered nodes", 
     col = 'black', lwd=5, xlim = c(0,max(c(page_er,page_ba))), ylim = c(0,1), pch = 21, 
     type="p",  main = "Influential Propagators - Page Rank")
par(new=T)
plot(page_ba, recuperado_ba, xlab = "Page Rank", ylab = "Fraction of recovered nodes", 
     col = 'blue', lwd=5, xlim = c(0,max(c(page_er,page_ba))), ylim = c(0,1), pch = 21, 
     type="p", main = "Influential Propagators - Page Rank")
legend("bottomright", legend = c("ER","BA"), col = c("black","blue"), lty = 1)

#----------------------------------------
#  Recuperado vs K-Core
#----------------------------------------
plot(coreness_er, recuperado_er, xlab = "K-Core", ylab = "Fraction of recovered nodes", 
     col = 'black', lwd=5, xlim = c(0,max(c(page_er,page_ba))), ylim = c(0,1), pch = 21, 
     type="p",  main = "Influential Propagators - K-Core")
par(new=T)
plot(coreness_ba, recuperado_ba, xlab = "K-Core", ylab = "Fraction of recovered nodes", 
     col = 'blue', lwd=5, xlim = c(0,max(c(page_er,page_ba))), ylim = c(0,1), pch = 21, 
     type="p", main = "Influential Propagators - K-Core")
legend("bottomright", legend = c("ER","BA"), col = c("black","blue"), lty = 1)


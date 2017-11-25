require(igraph)

#
# 3 – Modelo	WS
#

#Watts-Strogatz
giant.component <- function(graph) {
  cl <- clusters(graph)
  induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
}

t<-rep(0,100)
g<-rep(0,100)
p<-rep(0,100)

for(i in 1:100)
{
  w <- watts.strogatz.game(dim=1,size=1000,nei=10,p=i/1000)
  w_1 <- giant.component(w)
  t[i]<-transitivity(w_1)
  g[i]<- mean(distances(w_1,v = V(w_1), mode = c("all"), weights = NULL))
  p[i]<-(i/100)
}

plot(log(p),t/max(t),type = 'l',   
     ylab = "C/Cmax or D/Dmax", 
     xlab = "Probability p",
     main = "Small World Model",
     col=2,
     ylim=c(0,1))
lines(log(p), g/max(g))
legend(x=-2, y = 0.2, 
       legend = c("clustering coeficient","mean vertex distance"), 
       col = c("red","black"),
       lty = c(1,1))
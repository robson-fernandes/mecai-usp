require(igraph)

#
# 5 – Falhas e	ataques:
#

#Remoção Aleatória
e<-erdos.renyi.game(1000, 0.002, type = "gnp")
nos<-rep(0,10)
size<-rep(0,10)

for(i in 1:100){
  e_1 <- components(e)
  #remove do mais conectado -> menos conectado
  size[i]<-max(e_1$csize)/sum(e_1$csize)
  nos[i]<-i
  e <- delete_edges(e,sample(E(e),1)) 
}

plot(nos,size,
     type = 'l',
     col=2,
     ylab = "Size", 
     xlab = "Edges",
     main = "Faults and attacks - Random Removal",
     ylim = c(0.6,1))

#Barabasi
b<- barabasi.game(1000,power = 1.2 ,m = NULL, out.dist = NULL, out.seq = NULL,
                  out.pref = FALSE, zero.appeal = 1, directed = FALSE,  algorithm ="psumtree", start.graph = NULL)

nos<-rep(0,10)
size<-rep(0,10)

for(i in 1:100){
  b_1 <- components(b)
  size[i]<-max(b_1$csize)/sum(b_1$csize)
  nos[i]<-i
  b <- delete_edges(b,sample(E(b),1)) 
  
}

lines(nos,size,col=3)
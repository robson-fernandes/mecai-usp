require(igraph)

#
# 2	– Modelo	ER
#
#Erdos-Renyi model
size<-rep(0,1000)
avg<-rep(0,1000)

for(i in 1:1000)
{
  g   <- erdos.renyi.game(1000, i/100000, type = "gnp")
  gc <- components(g)
  size[i]<-max(gc$csize)/sum(gc$csize)
  avg[i]<-1000*(i/100000)
}
plot(avg,size, 
     main = "ER Model - Size of Giant Comp. x Average Degree", 
     ylab='Size of Giant Comp. S',
     xlab = 'Average Degree c',
     col = 'blue')
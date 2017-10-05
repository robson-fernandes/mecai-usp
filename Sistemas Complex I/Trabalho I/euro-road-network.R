library(igraph)

#
# Limpa workspace e variaveis
#
ls()
rm(list=ls())
graphics.off()


#
# 1 - Leitura da Rede
#
str = 'euroroad'
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

plot(G)
#
# 2 - Extrair o maior componente da rede
#
giant.component <- function(graph) {
  cl <- clusters(graph)
  induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
}
G <- giant.component(G)
clu <- components(G)

dt <- as.data.frame(net)

cor.test(dt$V1,dt$V2)

library(Hmisc)
rcorr(as.matrix(dt), type="pearson")
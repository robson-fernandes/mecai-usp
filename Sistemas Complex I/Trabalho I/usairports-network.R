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
str = 'USairports'
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

plot(G, vertex.size=4, vertex.label = NA)

#
# 2 - Extrair o maior componente da rede
#
giant.component <- function(graph) {
  cl <- clusters(graph)
  induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
}
G <- giant.component(G)
clu <- components(G)

G = as.undirected(G)
G = simplify(G)
# Plotagem da Rede com maior Componente
plot(G, vertex.size=4, vertex.label = NA)

kMaximo <- max(degree(G))
kMaximo

#Distribuição do Grau - P(k) Cumulativo
deg.distc <- degree_distribution(G,mode="all",
                                 cumulative=TRUE)

plot(x=0:kMaximo,y=deg.distc, pch=19, cex=1.2,
     xlab="Grau",
     ylab="Frequencia",
     type="p",
     col="blue",
     main = "Rede US Airports - Distribuição do Grau")

N = vcount(G) #number of vertices
M = ecount(G) #number of edges

#grau
grau <- degree(G, mode = "all")
grau

#grau medio
kMedio <- mean(degree(G))
kMedio

#Segundo Momento do Grau
grau <- grau^2
sum.grau.square = sum(grau)
total.grau <- length(grau)

kQuadrado <- sum.grau.square / total.grau
kQuadrado

### Media do Coeficiente de Aglomeracao Local
cc = transitivity(G, type="local")
cc = cc[!is.na(cc)];
mean(cc)
### Media do Coeficiente de Aglomeracao - Transitividade
cc2 = transitivity(G)
cc2

### Média dos Menores Caminhos
menorCaminho = shortest.paths(G, v=V(G), to=V(G))
media.menorCaminho= mean(menorCaminho)
media.menorCaminho
###Diametro
d = diameter(G)
d
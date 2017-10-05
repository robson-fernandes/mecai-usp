library(igraph)

#
# Limpa workspace e variaveis
#
ls()
rm(list=ls())
graphics.off()

network <- graph.empty (4941, directed = FALSE)
ecount(network)

#
#Ler a tabela com os Nos e os Vertices
myFile <- file.choose()
collection.edges <- scan(myFile, what = integer(0), sep = ",")
collection.edges

network <- add.edges(network, collection.edges)
network 


#Total de vertices
n <- vcount(network)

#Total de arestas
m <- gsize(network)

#matriz de adjacencias
matrixA <- get.adjacency(network)
matrixA
sum(matrixA)

#<k> 
#grau medio
kMedio <- mean(degree(network))
kMedio

kMaximo <- max(degree(network))
kMaximo
#grau
grau <- degree(network, mode = "all")
grau

plot(network, vertex.size=grau)

#histograma
h <- hist(grau, breaks = 1:kMaximo, main = "Histograma dos Graus")


#densidade
graph.density(network)
edge_density(network, loops = FALSE)

#Distribuição do Grau - P(k) não Cumulativo
deg.dist <- degree_distribution(network,mode="all",
                                cumulative=FALSE)

plot(x=0:kMaximo,y=deg.dist, pch=19, cex=1.2,
     xlab="Grau",
     ylab="Frequencia",
     type="p",
     col="blue",
     main = "Distribuição do Grau")

#Distribuição do Grau - P(k) Cumulativo
deg.distc <- degree_distribution(network,mode="all",
                                 cumulative=TRUE)

plot(x=0:kMaximo,y=deg.distc, pch=19, cex=1.2,
     xlab="Grau",
     ylab="Frequencia",
     type="p",
     col="blue",
     main = "Distribuição do Grau")



#Segundo Momento do Grau
grau <- grau^2
sum.grau.square = sum(grau)
total.grau <- length(grau)

kQuadrado <- sum.grau.square / total.grau
kQuadrado

#Calcula da Variancia
variancia <- kQuadrado - kMedio^2
variancia
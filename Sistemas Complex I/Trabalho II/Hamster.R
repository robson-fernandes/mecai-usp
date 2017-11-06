library(igraph)
library(ggplot2)
library(scales)

#
# Limpa workspace e variaveis
#
ls()
rm(list=ls())
graphics.off()

#setwd("C:/Users/guima/OneDrive/Mestrado/Sistemas Complexos/Projeto2")

#
# 1 - Leitura da Rede
#
str = 'bases/hamster'
hamnet <- read.table(paste(str,'.txt', sep = ''))

#
# 2 - Caso tenha mais de 2 colunas a terceira ser? removida
#
if(dim(hamnet)[2] > 2){
  hamnet[,3] = NULL
}

hamnet = as.matrix(hamnet)
if(min(hamnet) == 0){
  hamnet = hamnet + 1
}

#
# 3 - Gera??o do Gr?fico
#
hamgraf <- graph.data.frame(hamnet, directed=FALSE)
hamgraf = graph_from_edgelist(hamnet, directed = FALSE)

#
# 4 - Plotagem da Rede
#
plot(hamgraf, vertex.size=4, vertex.label = NA)


#
# 5 - Extra??o do maior componente da rede
#
giant.component <- function(graph) {
  cl <- clusters(graph)
  induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
}
hamgraf <- giant.component(hamgraf)
clu <- components(hamgraf)


hamgraf = as.undirected(hamgraf)
hamgraf = simplify(hamgraf)


#
# 6 - Plotagem da Rede com maior Componente
#
plot(hamgraf, vertex.size=6, vertex.label = NA)

#
# 7 - Calcula Assortatividade
#
assortativity(hamgraf, types1 = V(hamgraf), directed = FALSE)

#
# 8 - Calcula Assortatividade (Grau M?dio)
#
assortativity_degree(hamgraf, directed = FALSE)

#
# 9 - Calcula o knn do grafo
#
hamknn <- knn(hamgraf, vids = V(hamgraf), weights=NULL)

#
# 10 - Calcula a distribui??o do Grau
#
hamdegdist <- degree_distribution(hamgraf,mode="all",
                                   cumulative=FALSE)

#
# 11 - Calcula o Grau
#
hamgrau <- degree(hamgraf, mode = "all")

#
# 12 - Criando uma tabela para plotar no ggplot
#
hamtabela = cbind.data.frame(hamgrau, hamknn$knn)

#
# 13 - Plotando com ggplot
#
hamplot <- ggplot(hamtabela, aes(x = hamgrau, y = hamknn$knn)) + 
  geom_point(colour="blue") +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw()+
  labs(x="Grau") + 
  labs(y="Média dos graus do vértice K") +
  ggtitle("Hamsterster - Grau(k) x Knn(k)") + 
  theme(plot.title = element_text(hjust = 0.5))

hamplot

#
# 14 - Calculando comunidades 
#Walktrap
#
hamwalktrap <- cluster_walktrap(hamgraf, 
                                 weights = E(hamgraf)$weight, 
                                 steps = 4,
                                 merges = TRUE, 
                                 modularity = TRUE, 
                                 membership = TRUE)

hamwalktrap <- cluster_walktrap(hamgraf)

#
#Fast Greedy
#

hamfastgree <- cluster_fast_greedy(hamgraf, 
                                    merges = TRUE, 
                                    modularity = TRUE,
                                    membership = TRUE, 
                                    weights = E(hamgraf)$weight)

hamfastgree <- cluster_fast_greedy(hamgraf)

#
#Betweeness Centrality
#
hamedgebetw <- cluster_edge_betweenness(hamgraf, 
                                         weights = E(hamgraf)$weight, 
                                         directed = TRUE,
                                         edge.betweenness = TRUE, 
                                         merges = TRUE, 
                                         bridges = TRUE,
                                         modularity = TRUE, 
                                         membership = TRUE)

hamedgebetw <- cluster_edge_betweenness(hamgraf)

#
#EigenVectors of Matrices
#
hameigenvec <- cluster_leading_eigen(hamgraf, steps = -1, 
                                      weights = NULL, 
                                      start = NULL,
                                      options = arpack_defaults, 
                                      callback = NULL, 
                                      extra = NULL,
                                      env = parent.frame())

hameigenvec <- cluster_leading_eigen(hamgraf)

#
# 15 - Calculando Modularidade 
#Walktrap
#
hamwalmod <- modularity(hamwalktrap)

#Fast Greedy
#
hamfasmod <- modularity(hamfastgree)

#Betweeness Centrality
#
hambetmod <- modularity(hamedgebetw)

#EigenVector
#
hameigmod <- modularity(hameigenvec)

plot(hamknn$knnk,
     pch=19, 
     cex=1.2,
     log = "xy", 
     xlab = "Grau da Rede",
     ylab = "M?dia do Grau - Knnk", 
     col = "blue", 
     type = 'p', 
     main = "M?dia do Grau x Grau da Rede")

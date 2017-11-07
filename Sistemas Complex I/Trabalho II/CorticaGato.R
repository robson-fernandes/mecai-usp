library(igraph)
library(ggplot2)
library(scales)

#
# Limpa workspace e variaveis
#
ls()
rm(list=ls())
graphics.off()

#setwd("~/Documents/projetos/mecai-usp/Sistemas Complex I/Trabalho II")

#
# 1 - Leitura da Rede
#
str = 'bases/cortical-gato'
crtgnet <- read.table(paste(str,'.txt', sep = ''))

#
# 2 - Caso tenha mais de 2 colunas a terceira ser? removida
#
if(dim(crtgnet)[2] > 2){
  crtgnet[,3] = NULL
}

crtgnet = as.matrix(crtgnet)
if(min(crtgnet) == 0){
  crtgnet = crtgnet + 1
}

#
# 3 - Gera??o do Gr?fico
#
crtggraf <- graph.data.frame(crtgnet, directed=FALSE)
crtggraf = graph_from_edgelist(crtgnet, directed = FALSE)

#
# 4 - Plotagem da Rede
#
plot(crtggraf, vertex.size=4, vertex.label = NA)


#
# 5 - Extra??o do maior componente da rede
#
giant.component <- function(graph) {
  cl <- clusters(graph)
  induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
}
crtggraf <- giant.component(crtggraf)
clu <- components(crtggraf)


crtggraf = as.undirected(crtggraf)
crtggraf = simplify(crtggraf)


#
# 6 - Plotagem da Rede com maior Componente
#
plot(crtggraf, vertex.size=6, vertex.label = NA)

#
# 7 - Calcula Assortatividade
#
assortativity(crtggraf, types1 = V(crtggraf), directed = FALSE)

#
# 8 - Calcula Assortatividade (Grau M?dio)
#
assortativity_degree(crtggraf, directed = FALSE)

#
# 9 - Calcula o knn do grafo
#
crtgknn <- knn(crtggraf, vids = V(crtggraf), weights=NULL)

#
# 10 - Calcula a distribui??o do Grau
#
crtgdegdist <- degree_distribution(crtggraf,mode="all",
                                   cumulative=FALSE)

#
# 11 - Calcula o Grau
#
crtggrau <- degree(crtggraf, mode = "all")


#
# 12 - Criando uma tabela para plotar no ggplot
#
crtgtabela = cbind.data.frame(crtggrau, knn=crtgknn$knn)
crtgtabela2 = cbind.data.frame(grau=1:length(crtgknn$knnk), knnk=crtgknn$knnk)
subset(crtgtabela2$knnk, !is.nan(crtgtabela2$knnk))

#
# 13 - Plotando com ggplot
#

crtgplot <- ggplot(crtgtabela, aes(x = crtggrau)) + 
  geom_point(aes(y = knn, colour="K(vértice) vs Knn(vértice)")) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw()+
  labs(x="Grau") + 
  labs(y="Média dos graus do vértice K") +
  ggtitle("Cortical Gato - Grau(k) x Knn(k)") + 
  theme(plot.title = element_text(hjust = 0.5), legend.justification = "top")

crtgplot + geom_line(data=crtgtabela2, 
                     aes(x=grau, 
                         y=knnk, colour="Knn(k)"))+
  scale_colour_manual("", 
                      breaks = c("Knn(k)", "K(vértice) vs Knn(vértice)"), 
                      values = c("blue", "red"))

#COEFICIENTE DE CORRELAÇÃO
cor(crtggrau, crtgknn$knn)

#
# 14 - Calculando comunidades 
#Walktrap
#
crtgwalktrap <- cluster_walktrap(crtggraf, 
                                 weights = E(crtggraf)$weight, 
                                 steps = 4,
                                 merges = TRUE, 
                                 modularity = TRUE, 
                                 membership = TRUE)

crtgwalktrap <- cluster_walktrap(crtggraf)

#
#Fast Greedy
#

crtgfastgree <- cluster_fast_greedy(crtggraf, 
                                    merges = TRUE, 
                                    modularity = TRUE,
                                    membership = TRUE, 
                                    weights = E(crtggraf)$weight)

crtgfastgree <- cluster_fast_greedy(crtggraf)

#
#Betweeness Centrality
#
crtgedgebetw <- cluster_edge_betweenness(crtggraf, 
                                         weights = E(crtggraf)$weight, 
                                         directed = TRUE,
                                         edge.betweenness = TRUE, 
                                         merges = TRUE, 
                                         bridges = TRUE,
                                         modularity = TRUE, 
                                         membership = TRUE)

crtgedgebetw <- cluster_edge_betweenness(crtggraf)

#
#EigenVectors of Matrices
#
crtgeigenvec <- cluster_leading_eigen(crtggraf, steps = -1, 
                                      weights = NULL, 
                                      start = NULL,
                                      options = arpack_defaults, 
                                      callback = NULL, 
                                      extra = NULL,
                                      env = parent.frame())

crtgeigenvec <- cluster_leading_eigen(crtggraf)

#
# 15 - Calculando Modularidade 
#Walktrap
#
crtgwalmod <- modularity(crtgwalktrap)

#Fast Greedy
#
crtgfasmod <- modularity(crtgfastgree)

#Betweeness Centrality
#
crtgbetmod <- modularity(crtgedgebetw)

#EigenVector
#
crtgeigmod <- modularity(crtgeigenvec)

plot(crtgknn$knnk,
     pch=19, 
     cex=1.2,
     log = "xy", 
     xlab = "Grau da Rede",
     ylab = "M?dia do Grau - Knnk", 
     col = "blue", 
     type = 'p', 
     main = "M?dia do Grau x Grau da Rede")

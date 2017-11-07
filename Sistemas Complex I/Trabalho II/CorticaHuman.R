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
str = 'bases/cortica-human'
crthnet <- read.table(paste(str,'.txt', sep = ''))

#
# 2 - Caso tenha mais de 2 colunas a terceira ser? removida
#
if(dim(crthnet)[2] > 2){
  crthnet[,3] = NULL
}

crthnet = as.matrix(crthnet)
if(min(crthnet) == 0){
  crthnet = crthnet + 1
}

#
# 3 - Gera??o do Gr?fico
#
crthgraf <- graph.data.frame(crthnet, directed=FALSE)
crthgraf = graph_from_edgelist(crthnet, directed = FALSE)

#
# 4 - Plotagem da Rede
#
plot(crthgraf, vertex.size=4, vertex.label = NA)


#
# 5 - Extra??o do maior componente da rede
#
giant.component <- function(graph) {
  cl <- clusters(graph)
  induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
}
crthgraf <- giant.component(crthgraf)
clu <- components(crthgraf)


crthgraf = as.undirected(crthgraf)
crthgraf = simplify(crthgraf)


#
# 6 - Plotagem da Rede com maior Componente
#
plot(crthgraf, vertex.size=6, vertex.label = NA)

#
# 7 - Calcula Assortatividade
#
assortativity(crthgraf, types1 = V(crthgraf), directed = FALSE)

#
# 8 - Calcula Assortatividade (Grau M?dio)
#
assortativity_degree(crthgraf, directed = FALSE)

#
# 9 - Calcula o knn do grafo
#
crthknn <- knn(crthgraf, vids = V(crthgraf), weights=NULL)

#
# 10 - Calcula a distribui??o do Grau
#
crthdegdist <- degree_distribution(crthgraf,mode="all",
                                   cumulative=FALSE)

#
# 11 - Calcula o Grau
#
crthgrau <- degree(crthgraf, mode = "all")


#
# 12 - Criando uma tabela para plotar no ggplot
#
crthtabela = cbind.data.frame(crthgrau, knn=crthknn$knn)
crthtabela2 = cbind.data.frame(grau=1:length(crthknn$knnk), knnk=crthknn$knnk)
subset(crthtabela2$knnk, !is.nan(crthtabela2$knnk))

#
# 13 - Plotando com ggplot
#

crthplot <- ggplot(crthtabela, aes(x = crthgrau)) + 
  geom_point(aes(y = knn, colour="K(vértice) vs Knn(vértice)")) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw()+
  labs(x="Grau") + 
  labs(y="Média dos graus do vértice K") +
  ggtitle("Cortical Humano - Grau(k) x Knn(k)") + 
  theme(plot.title = element_text(hjust = 0.5), legend.justification = "top")

crthplot + geom_line(data=crthtabela2, 
                     aes(x=grau, 
                         y=knnk, colour="Knn(k)"))+
  scale_colour_manual("", 
                      breaks = c("Knn(k)", "K(vértice) vs Knn(vértice)"), 
                      values = c("blue", "red"))


#COEFICIENTE DE CORRELAÇÃO
cor(crthgrau, crthknn$knn)

#
# 14 - Calculando comunidades 
#Walktrap
#
crthwalktrap <- cluster_walktrap(crthgraf, 
                                 weights = E(crthgraf)$weight, 
                                 steps = 4,
                                 merges = TRUE, 
                                 modularity = TRUE, 
                                 membership = TRUE)

crthwalktrap <- cluster_walktrap(crthgraf)

#
#Fast Greedy
#

crthfastgree <- cluster_fast_greedy(crthgraf, 
                                    merges = TRUE, 
                                    modularity = TRUE,
                                    membership = TRUE, 
                                    weights = E(crthgraf)$weight)

crthfastgree <- cluster_fast_greedy(crthgraf)

#
#Betweeness Centrality
#

crthedgebetw <- cluster_edge_betweenness(crthgraf, 
                                         weights = E(crthgraf)$weight, 
                                         directed = TRUE,
                                         edge.betweenness = TRUE, 
                                         merges = TRUE, 
                                         bridges = TRUE,
                                         modularity = TRUE, 
                                         membership = TRUE)

crthedgebetw <- cluster_edge_betweenness(crthgraf)

#
#EigenVectors of Matrices
#
crtheigenvec <- cluster_leading_eigen(crthgraf, steps = -1, 
                                      weights = NULL, 
                                      start = NULL,
                                      options = arpack_defaults, 
                                      callback = NULL, 
                                      extra = NULL,
                                      env = parent.frame())

crtheigenvec <- cluster_leading_eigen(crthgraf)

#
# 15 - Calculando Modularidade 
#Walktrap
#
crthwalmod <- modularity(crthwalktrap)

#Fast Greedy
#
crthfasmod <- modularity(crthfastgree)

#Betweeness Centrality
#
crthbetmod <- modularity(crthedgebetw)

#EigenVector
#
crtheigmod <- modularity(crtheigenvec)

plot(crthknn$knnk,
     pch=19, 
     cex=1.2,
     log = "xy", 
     xlab = "Grau da Rede",
     ylab = "M?dia do Grau - Knnk", 
     col = "blue", 
     type = 'p', 
     main = "M?dia do Grau x Grau da Rede")

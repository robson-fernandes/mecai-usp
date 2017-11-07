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
str = 'bases/euroroad'
euronet <- read.table(paste(str,'.txt', sep = ''))

#
# 2 - Caso tenha mais de 2 colunas a terceira ser? removida
#
if(dim(euronet)[2] > 2){
  euronet[,3] = NULL
}

euronet = as.matrix(euronet)
if(min(euronet) == 0){
  euronet = euronet + 1
}

#
# 3 - Gera??o do Gr?fico
#
eurograf <- graph.data.frame(euronet, directed=FALSE)
eurograf = graph_from_edgelist(euronet, directed = FALSE)

#
# 4 - Plotagem da Rede
#
plot(eurograf, vertex.size=4, vertex.label = NA)


#
# 5 - Extra??o do maior componente da rede
#
giant.component <- function(graph) {
  cl <- clusters(graph)
  induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
}
eurograf <- giant.component(eurograf)
clu <- components(eurograf)


eurograf = as.undirected(eurograf)
eurograf = simplify(eurograf)


#
# 6 - Plotagem da Rede com maior Componente
#
plot(eurograf, vertex.size=6, vertex.label = NA)

#
# 7 - Calcula Assortatividade
#
assortativity(eurograf, types1 = V(eurograf), directed = FALSE)

#
# 8 - Calcula Assortatividade (Grau M?dio)
#
assortativity_degree(eurograf, directed = FALSE)

#
# 9 - Calcula o knn do grafo
#
euroknn <- knn(eurograf, vids = V(eurograf), weights=NULL)

#
# 10 - Calcula a distribui??o do Grau
#
eurodegdist <- degree_distribution(eurograf,mode="all",
                                 cumulative=FALSE)

#
# 11 - Calcula o Grau
#
eurograu <- degree(eurograf, mode = "all")


#
# 12 - Criando uma tabela para plotar no ggplot
#
eurotabela = cbind.data.frame(eurograu, knn=euroknn$knn)
eurotabela2 = cbind.data.frame(grau=1:length(euroknn$knnk), knnk=euroknn$knnk)
subset(eurotabela2$knnk, !is.nan(eurotabela2$knnk))


#
# 13 - Plotando com ggplot
#

europlot <- ggplot(eurotabela, aes(x = eurograu)) + 
  geom_point(aes(y = knn, colour="K(vértice) vs Knn(vértice)")) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw()+
  labs(x="Grau") + 
  labs(y="Média dos graus do vértice K") +
  ggtitle("Euro Road - Grau(k) x Knn(k)") + 
  theme(plot.title = element_text(hjust = 0.5), legend.justification = "top")

europlot + geom_line(data=eurotabela2, 
                     aes(x=grau, 
                         y=knnk, colour="Knn(k)"))+
  scale_colour_manual("", 
                      breaks = c("Knn(k)", "K(vértice) vs Knn(vértice)"), 
                      values = c("blue", "red"))

#COEFICIENTE DE CORRELAÇÃO
cor(eurograu, euroknn$knn)


#
# 14 - Calculando comunidades 
#Walktrap
#
eurowalktrap <- cluster_walktrap(eurograf, 
                                 weights = E(eurograf)$weight, 
                                 steps = 4,
                                 merges = TRUE, 
                                 modularity = TRUE, 
                                 membership = TRUE)

eurowalktrap <- cluster_walktrap(eurograf)

#
#Fast Greedy
#

eurofastgree <- cluster_fast_greedy(eurograf, 
                                      merges = TRUE, 
                                      modularity = TRUE,
                                      membership = TRUE, 
                                      weights = E(eurograf)$weight)

eurofastgree <- cluster_fast_greedy(eurograf)

#
#Betweeness Centrality
#
euroedgebetw <- cluster_edge_betweenness(eurograf, 
                                         weights = E(eurograf)$weight, 
                                         directed = TRUE,
                                         edge.betweenness = TRUE, 
                                         merges = TRUE, 
                                         bridges = TRUE,
                                         modularity = TRUE, 
                                         membership = TRUE)

euroedgebetw <- cluster_edge_betweenness(eurograf)

#
#EigenVectors of Matrices
#
euroeigenvec <- cluster_leading_eigen(eurograf, steps = -1, 
                                      weights = NULL, 
                                      start = NULL,
                                      options = arpack_defaults, 
                                      callback = NULL, 
                                      extra = NULL,
                                      env = parent.frame())

euroeigenvec <- cluster_leading_eigen(eurograf)

#
# 15 - Calculando Modularidade 
#Walktrap
#
eurowalmod <- modularity(eurowalktrap)

#Fast Greedy
#
eurofasmod <- modularity(eurofastgree)

#Betweeness Centrality
#
eurobetmod <- modularity(euroedgebetw)

#EigenVector
#
euroeigmod <- modularity(euroeigenvec)

plot(euroknn$knnk,
     pch=19, 
     cex=1.2,
     log = "xy", 
     xlab = "Grau da Rede",
     ylab = "M?dia do Grau - Knnk", 
     col = "blue", 
     type = 'p', 
     main = "M?dia do Grau x Grau da Rede")

#================================================
# Evolução da Modularidade
#================================================

eurofastgreenoe <- cluster_fast_greedy(eurograf, 
                                      merges = TRUE, 
                                      modularity = TRUE,
                                      membership = TRUE, 
                                      weights = E(eurograf)$weight)
plot(eurofastgreenoe$modularity, 
     xlab = "Qtde de Vértices", 
     main = "Modularidade Euro Road",
     ylab="Modularidade (Fast Greedy)",
     pch=19, 
     cex=1.2,
     col = "blue", 
     type = 'p')

#================================================
# Comunidade Fast Greedy
#================================================
coords = layout_with_fr(eurograf)
plot(eurofastgree, eurograf, layout=coords, vertex.label=NA, vertex.size = 5)

#Tamanho das Comunidades
sizes(eurofastgree)

#Quantidade de Comunidades
length(eurofastgree)

#Plot do Dendrograma
plot_dendrogram(eurofastgree, mode="hclust", rect = 0, colbar = palette(),
                hang = 0.01, ann = FALSE, main = "", sub = "", xlab = "",
                ylab = "")

plot(sizes(eurofastgree))

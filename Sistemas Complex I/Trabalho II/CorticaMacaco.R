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
str = 'bases/cortical-macaco'
crtmnet <- read.table(paste(str,'.txt', sep = ''))

#
# 2 - Caso tenha mais de 2 colunas a terceira ser? removida
#
if(dim(crtmnet)[2] > 2){
  crtmnet[,3] = NULL
}

crtmnet = as.matrix(crtmnet)
if(min(crtmnet) == 0){
  crtmnet = crtmnet + 1
}

#
# 3 - Gera??o do Gr?fico
#
crtmgraf <- graph.data.frame(crtmnet, directed=FALSE)
crtmgraf = graph_from_edgelist(crtmnet, directed = FALSE)

#
# 4 - Plotagem da Rede
#
plot(crtmgraf, vertex.size=4, vertex.label = NA)


#
# 5 - Extra??o do maior componente da rede
#
giant.component <- function(graph) {
  cl <- clusters(graph)
  induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
}
crtmgraf <- giant.component(crtmgraf)
clu <- components(crtmgraf)


crtmgraf = as.undirected(crtmgraf)
crtmgraf = simplify(crtmgraf)


#
# 6 - Plotagem da Rede com maior Componente
#
plot(crtmgraf, vertex.size=6, vertex.label = NA)

#
# 7 - Calcula Assortatividade
#
assortativity(crtmgraf, types1 = V(crtmgraf), directed = FALSE)

#
# 8 - Calcula Assortatividade (Grau Médio)
#
assortativity_degree(crtmgraf, directed = FALSE)

#
# 9 - Calcula o knn do grafo
#
crtmknn <- knn(crtmgraf, vids = V(crtmgraf), weights=NULL)

#
# 10 - Calcula a distribuição do Grau
#
crtmdegdist <- degree_distribution(crtmgraf,mode="all",
                                   cumulative=FALSE)

#
# 11 - Calcula o Grau
#
crtmgrau <- degree(crtmgraf, mode = "all")

#
# 12 - Criando uma tabela para plotar no ggplot
#
crtmtabela = cbind.data.frame(crtmgrau, knn=crtmknn$knn)
crtmtabela2 = cbind.data.frame(grau=1:length(crtmknn$knnk), knnk=crtmknn$knnk)
subset(crtmtabela2$knnk, !is.nan(crtmtabela2$knnk))

#
# 13 - Plotando com ggplot
#

crtmplot <- ggplot(crtmtabela, aes(x = crtmgrau)) + 
  geom_point(aes(y = knn, colour="K(vértice) vs Knn(vértice)")) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw()+
  labs(x="Grau") + 
  labs(y="Média dos graus do vértice K") +
  ggtitle("Cortical Macaco - Grau(k) x Knn(k)") + 
  theme(plot.title = element_text(hjust = 0.5), legend.justification = "top")

crtmplot + geom_line(data=crtmtabela2, 
                     aes(x=grau, 
                         y=knnk, colour="Knn(k)"))+
  scale_colour_manual("", 
                      breaks = c("Knn(k)", "K(vértice) vs Knn(vértice)"), 
                      values = c("blue", "red"))



#COEFICIENTE DE CORRELAÇÃO
cor(crtmgrau, crtmknn$knn)

#
# 14 - Calculando comunidades 
#Walktrap
#
crtmwalktrap <- cluster_walktrap(crtmgraf, 
                                 weights = E(crtmgraf)$weight, 
                                 steps = 4,
                                 merges = TRUE, 
                                 modularity = TRUE, 
                                 membership = TRUE)

crtmwalktrap <- cluster_walktrap(crtmgraf)

#
#Fast Greedy
#

crtmfastgree <- cluster_fast_greedy(crtmgraf, 
                                    merges = TRUE, 
                                    modularity = TRUE,
                                    membership = TRUE, 
                                    weights = E(crtmgraf)$weight)

crtmfastgree <- cluster_fast_greedy(crtmgraf)

#
#Betweeness Centrality
#
crtmedgebetw <- cluster_edge_betweenness(crtmgraf, 
                                         weights = E(crtmgraf)$weight, 
                                         directed = TRUE,
                                         edge.betweenness = TRUE, 
                                         merges = TRUE, 
                                         bridges = TRUE,
                                         modularity = TRUE, 
                                         membership = TRUE)

crtmedgebetw <- cluster_edge_betweenness(crtmgraf)

#
#EigenVectors of Matrices
#
crtmeigenvec <- cluster_leading_eigen(crtmgraf, steps = -1, 
                                      weights = NULL, 
                                      start = NULL,
                                      options = arpack_defaults, 
                                      callback = NULL, 
                                      extra = NULL,
                                      env = parent.frame())

crtmeigenvec <- cluster_leading_eigen(crtmgraf)

#
# 15 - Calculando Modularidade 
#Walktrap
#
crtmwalmod <- modularity(crtmwalktrap)

#Fast Greedy
#
crtmfasmod <- modularity(crtmfastgree)

#Betweeness Centrality
#
crtmbetmod <- modularity(crtmedgebetw)

#EigenVector
#
crtmeigmod <- modularity(crtmeigenvec)

plot(crtmknn$knnk,
     pch=19, 
     cex=1.2,
     log = "xy", 
     xlab = "Grau da Rede",
     ylab = "Média do Grau - Knnk", 
     col = "blue", 
     type = 'p', 
     main = "Média do Grau x Grau da Rede")

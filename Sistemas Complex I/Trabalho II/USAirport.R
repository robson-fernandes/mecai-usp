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
str = 'bases/USairports'
usanet <- read.table(paste(str,'.txt', sep = ''))

#
# 2 - Caso tenha mais de 2 colunas a terceira ser? removida
#
if(dim(usanet)[2] > 2){
  usanet[,3] = NULL
}

usanet = as.matrix(usanet)
if(min(usanet) == 0){
  usanet = usanet + 1
}

#
# 3 - Gera??o do Gr?fico
#
usagraf <- graph.data.frame(usanet, directed=FALSE)
usagraf = graph_from_edgelist(usanet, directed = FALSE)

#
# 4 - Plotagem da Rede
#
plot(usagraf, vertex.size=4, vertex.label = NA)


#
# 5 - Extra??o do maior componente da rede
#
giant.component <- function(graph) {
  cl <- clusters(graph)
  induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
}
usagraf <- giant.component(usagraf)
clu <- components(usagraf)


usagraf = as.undirected(usagraf)
usagraf = simplify(usagraf)


#
# 6 - Plotagem da Rede com maior Componente
#
plot(usagraf, vertex.size=6, vertex.label = NA)

#
# 7 - Calcula Assortatividade
#
assortativity(usagraf, types1 = V(usagraf), directed = FALSE)

#
# 8 - Calcula Assortatividade (Grau M?dio)
#
assortativity_degree(usagraf, directed = FALSE)

#
# 9 - Calcula o knn do grafo
#
usaknn <- knn(usagraf, vids = V(usagraf), weights=NULL)

#
# 10 - Calcula a distribui??o do Grau
#
usadegdist <- degree_distribution(usagraf,mode="all",
                                   cumulative=FALSE)

#
# 11 - Calcula o Grau
#
usagrau <- degree(usagraf, mode = "all")


#
# 12 - Criando uma tabela para plotar no ggplot
#
ustabela = cbind.data.frame(usagrau, usaknn$knn)

#
# 13 - Plotando com ggplot
#
usaplot <- ggplot(ustabela, aes(x = usagrau, y = usaknn$knn)) + 
  geom_point(colour="blue") +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw()+
  labs(x="Grau") + 
  labs(y="Média dos graus do vértice K") +
  ggtitle("US Airport - Grau(k) x Knn(k)") + 
  theme(plot.title = element_text(hjust = 0.5))

usaplot

#
# 14 - Calculando comunidades 
#Walktrap
#
usawalktrap <- cluster_walktrap(usagraf, 
                                 weights = E(usagraf)$weight, 
                                 steps = 4,
                                 merges = TRUE, 
                                 modularity = TRUE, 
                                 membership = TRUE)

usawalktrap <- cluster_walktrap(usagraf)

#
#Fast Greedy
#
usafastgree <- cluster_fast_greedy(usagraf, 
                                    merges = TRUE, 
                                    modularity = TRUE,
                                    membership = TRUE, 
                                    weights = E(usagraf)$weight)

usafastgree <- cluster_fast_greedy(usagraf)

#
#Betweeness Centrality
#
usaedgebetw <- cluster_edge_betweenness(usagraf, 
                                         weights = E(usagraf)$weight, 
                                         directed = FALSE,
                                         edge.betweenness = TRUE, 
                                         merges = TRUE, 
                                         bridges = TRUE,
                                         modularity = TRUE, 
                                         membership = TRUE)

Sys.time()
usaedgebetw <- cluster_edge_betweenness(usagraf)
Sys.time()
#
#EigenVectors of Matrices
#
usaeigenvec <- cluster_leading_eigen(usagraf, steps = -1, 
                                      weights = NULL, 
                                      start = NULL,
                                      options = arpack_defaults, 
                                      callback = NULL, 
                                      extra = NULL,
                                      env = parent.frame())

usaeigenvec <- cluster_leading_eigen(usagraf)

#
# 15 - Calculando Modularidade 
#Walktrap
#
usawalmod <- modularity(usawalktrap)

#Fast Greedy
#
usafasmod <- modularity(usafastgree)

#Betweeness Centrality
#
usabetmod <- modularity(usaedgebetw)

#EigenVector
#
usaeigmod <- modularity(usaeigenvec)

plot(usaknn$knnk,
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

usafastgreenoe <- cluster_fast_greedy(usagraf, 
                                      merges = TRUE, 
                                      modularity = TRUE,
                                      membership = TRUE, 
                                      weights = E(usagraf)$weight)
plot(usafastgreenoe$modularity, 
     xlab = "Qtde de Vértices", 
     main = "Modularidade US Airport",
     ylab="Modularidade (Fast Greedy)",
     pch=19, 
     cex=1.2,
     col = "blue", 
     type = 'p')



#================================================
# Comunidade Fast Greedy
#================================================
coords = layout_with_fr(usagraf)
plot(usafastgree, usagraf, layout=coords, vertex.label=NA, vertex.size = 5)

#Tamanho das Comunidades
sizes(usafastgree)

#Quantidade de Comunidades
length(usafastgree)

#Plot do Dendrograma
plot_dendrogram(usafastgree, mode="hclust", rect = 0, colbar = palette(),
                hang = 0.00, ann = FALSE, main = "", sub = "", xlab = "",
                ylab = "")

plot(sizes(usafastgree))



###Localizando NaN
table(is.na(usaknn$knnk))
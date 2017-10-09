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

# função para plotar o grau de distribuição
plot_degree_distribution = function(graph,cumulative, title) {
  
  dd = degree.distribution(graph, mode = "all", cumulative = cumulative)
  
  # calcula o grau
  d = degree(graph, mode = "all")
  degree = 1:max(d)
  probability = dd[-1]
  # apaga os valores brancos
  nonzero.position = which(probability != 0)
  probability = probability[nonzero.position]
  degree = degree[nonzero.position]
  # plot
  plot(probability ~ degree, 
       pch=19, 
       cex=1.2,
       log = "xy", 
       xlab = "Grau (log)",
       ylab = "Probabilidade (log)", 
       col = "blue", 
       type = 'p', 
       main = title)
}

plot_degree_distribution(G, FALSE,"Rede US Airports - Distribuição do Grau") 
plot_degree_distribution(G, TRUE, "Rede US Airports - Distribuição Acumulada do Grau") #Acumulada


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
### Coeficiente de Aglomeracao - Transitividade
cc2 = transitivity(G)
cc2

### Média dos Menores Caminhos
menorCaminho = shortest.paths(G, v=V(G), to=V(G))
media.menorCaminho= mean(menorCaminho)
media.menorCaminho
###Diametro
d = diameter(G)
d

### 6 - Histograma Cumulativo de Coef. de Aglomeração local.
cc = transitivity(G, type="local")
cc = cc[!is.na(cc)]
hist(cc,plot=FALSE) -> h
# replace the cell freq.s by cumulative freq.s
h$counts <- cumsum(h$counts)/sum(h$counts) 
plot(h, 
     col="royalblue1",
     border="white",
     ylab = "Frequencia",
     xlab = "Coeficiente de Aglomeração local",
     main = "Rede - US Airports - Histograma - Distribuição Acumulada") 
mtext("Coeficiente de Aglomeração Local")

menorCaminho = shortest.paths(G, v=V(G), to=V(G))
hist(menorCaminho, 
     col="royalblue1",
     border="white",
     ylab = "Frequencia",
     xlab = "Menores Caminhos",
     main = "Rede - US Airports - Histograma")
mtext("Menores Caminhos")

#7 - ENTROPIA DE SHANNON
E(G)$weight <- runif(ecount(G))
usairports.network <- graph.diversity(G)
usairports.network = usairports.network[!is.na(usairports.network)];
hist(usairports.network)

# betweenness centrality
bt = betweenness(G)
hist(bt,
     col="royalblue1",
     border="white",
     freq = F,
     ylab = "Frequencia",
     xlab = "Betweenness Centrality",
     main = "Rede - US Airports - Distribuição de Probabilidade")
mtext("Betweenness Centrality")
lines(density(bt, na.rm = T,
              from = min(bt, na.rm = T), 
              to = max(bt, na.rm = T)),
      col="red")

# closeness centrality
c = closeness(G)
hist(c,
     col="royalblue1",
     border="white",
     freq = F,
     ylab = "Frequencia",
     xlab = "Closeness Centrality",
     main = "Rede - US Airports - Distribuição de Probabilidade")
mtext("Closeness Centrality")
lines(density(c, na.rm = T, from = min(c, na.rm = T), to = max(c, na.rm = T)),
      col="red")

### eigenvector centrality
eg = eigen_centrality(G)$vector
hist(eg,
     col="royalblue1",
     border="white",
     freq = F,
     ylab = "Frequencia",
     xlab = "Eignevector Centrality",
     main = "Rede - US Airports - Distribuição de Probabilidade")
mtext("Eignevector Centrality")
lines(density(eg, na.rm = T, from =0, to = max(eg, na.rm = T)),
      col="red")

### PageRank
pr = page_rank(G, damping = 0.85)$vector
hist(pr,
     col="royalblue1",
     border="white",
     freq = F,
     ylab = "Frequencia",
     xlab = "Page Rank",
     main = "Rede - US Airports - Distribuição de Probabilidade")
mtext("Page Rank")
lines(density(pr, na.rm = T, from =0, to = max(pr, na.rm = T)),
      col="red")

#8 - ANÁLISE DE CORRELAÇÃO 

#Correlação de Person
medidas <- data.frame("Betweenness" = bt, 
           "Closeness" = c, 
           "Eignevector" = eg,
           "PageRank" = pr
           )
# Teste de Correlação - Betweenness
cor(medidas[,'Betweenness'], medidas[,'Betweenness'])
cor(medidas[,'Betweenness'], medidas[,'Closeness'])
cor(medidas[,'Betweenness'], medidas[,'Eignevector'])
cor(medidas[,'Betweenness'], medidas[,'PageRank'])

# Teste de Correlação - Closeness
cor(medidas[,'Closeness'], medidas[,'Closeness'])
cor(medidas[,'Closeness'], medidas[,'Betweenness'])
cor(medidas[,'Closeness'], medidas[,'Eignevector'])
cor(medidas[,'Closeness'], medidas[,'PageRank'])

# Teste de Correlação - Eignevector
cor(medidas[,'Eignevector'], medidas[,'Eignevector'])
cor(medidas[,'Eignevector'], medidas[,'Betweenness'])
cor(medidas[,'Eignevector'], medidas[,'Closeness'])
cor(medidas[,'Eignevector'], medidas[,'PageRank'])

# Teste de Correlação - PageRank
cor(medidas[,'PageRank'], medidas[,'PageRank'])
cor(medidas[,'PageRank'], medidas[,'Betweenness'])
cor(medidas[,'PageRank'], medidas[,'Closeness'])
cor(medidas[,'PageRank'], medidas[,'Eignevector'])

#------------------------------------------------------------
# BETWEENNESS
#------------------------------------------------------------
#------------------------------------------------------------
# Correlação - Betweenness Centrality x Closeness Centrality
plot(bt, c, 
     col="royalblue1",
     main="Rede - US Airports - Análise de Correlação", 
     xlab="Betweenness Centrality",
     ylab="Closeness Centrality",
     pch=19)
mtext("Betweenness Centrality x Closeness Centrality")
abline(lm(c~bt), col="red")
lines(lowess(bt,c), col="green", type="b") 
legend("bottomright", legend=c("Regression Line", "Lowess Line"),
       col=c("red", "green"), lty=1:2, cex=0.8)

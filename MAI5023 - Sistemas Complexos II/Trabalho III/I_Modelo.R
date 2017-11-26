library(igraph)

closeAllConnections()
graphics.off()
rm(list=ls())

##Executar o código dos arquivos Exercicio1-ER, Exercicio1-WS e Exercicio1-BA

##erdosrenyi(G)
##watssstrogatz()
##barabasialbert()

#Exercício 1
#Gerar o gráfico 
#Erdos Renyi
er500 <- vector(mode="list", length=10)
for (i in 1:length(er500)){
  er500[[i]] <- erdos.renyi.game(500, 1/500)
  #plot(er500[[i]], vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Rede Erdos Renyi")
}

er1000 <- vector(mode="list", length =10)
for (i in 1:length(er1000)){
  er1000[[i]] <- erdos.renyi.game(1000, 1/1000)
  #plot(er1000[[i]], vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Rede Erdos Renyi")
}

er2000 <- vector(mode="list", length =10)
for (i in 1:length(er2000)){
  er2000[[i]] <- erdos.renyi.game(2000, 1/2000)
  #plot(er2000[[i]], vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Rede Erdos Renyi")
}


# Watts-Strogatz
ws500 <- vector(mode="list", length = 5)
for (i in 1:length(ws500)){
  ws500[[i]] <- watts.strogatz.game(1, 500, 1, 0.1, loops = FALSE, multiple = FALSE)
  #plot(ws500[[i]], vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Rede Watts-Strogatz")
}

ws1000 <- vector(mode="list", length = 5)
for (i in 1:length(ws1000)){
  ws1000[[i]] <- watts.strogatz.game(1, 1000, 1, 0.1, loops = FALSE, multiple = FALSE)
  #plot(ws1000[[i]], vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Rede Watts-Strogatz")
}

ws2000 <- vector(mode="list", length = 5)
for (i in 1:length(ws2000)){
  ws2000[[i]] <- watts.strogatz.game(1, 2000, 1, 0.1, loops = FALSE, multiple = FALSE)
  #plot(ws1000[[i]], vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Rede Watts-Strogatz")
}


ws5001 <- vector(mode="list", length = 5)
for (i in 1:length(ws5001)){
  ws5001[[i]] <- watts.strogatz.game(1, 500, 1, 0.001, loops = FALSE, multiple = FALSE)
  #plot(ws500[[i]], vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Rede Watts-Strogatz")
}

ws10001 <- vector(mode="list", length = 5)
for (i in 1:length(ws10001)){
  ws10001[[i]] <- watts.strogatz.game(1, 1000, 1, 0.001, loops = FALSE, multiple = FALSE)
  #plot(ws1000[[i]], vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Rede Watts-Strogatz")
}

ws20001 <- vector(mode="list", length = 5)
for (i in 1:length(ws20001)){
  ws20001[[i]] <- watts.strogatz.game(1, 2000, 1, 0.001, loops = FALSE, multiple = FALSE)
  #plot(ws1000[[i]], vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Rede Watts-Strogatz")
}

#Barabási-Albert
ba500 <- vector(mode="list", length=10)
for (i in 1:length(ba500)){
  ba500[[i]] <- barabasi.game(500, power = 1.2, m = NULL, out.dist = NULL, out.seq = NULL,
                              out.pref = FALSE, zero.appeal = 1, directed = FALSE,
                              algorithm ="psumtree", start.graph = NULL)
  #plot(ba500[[i]], vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Rede Barabási-Albert")
}

ba1000 <- vector(mode="list", length=10)
for (i in 1:length(ba1000)){
  ba1000[[i]] <- barabasi.game(1000, power = 1.2, m = NULL, out.dist = NULL, out.seq = NULL,
                               out.pref = FALSE, zero.appeal = 1, directed = FALSE,
                               algorithm ="psumtree", start.graph = NULL)
  #plot(ba1000[[i]], vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Barabási-Albert")
}

ba2000 <- vector(mode="list", length=10)
for (i in 1:length(ba2000)){
  ba2000[[i]] <- barabasi.game(2000, power = 1.2, m = NULL, out.dist = NULL, out.seq = NULL,
                               out.pref = FALSE, zero.appeal = 1, directed = FALSE,
                               algorithm ="psumtree", start.graph = NULL)
  #plot(ba2000[[i]], vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Barabási-Albert")
}

#
#Criar tabela para armazenar resultados
#
vetordistgrau <- matrix(data = vector(mode = 'list', length=1), nrow=12, ncol=1, byrow = TRUE)
colnames(vetordistgrau) <- c("Dist. Grau")
rownames(vetordistgrau) <- c("ER500",
                             "ER1000",
                             "ER2000", 
                             "WS50001", 
                             "WS100001",
                             "WS200001",
                             "WS50000001", 
                             "WS100000001",
                             "WS200000001",
                             "BA500",
                             "BA1000",
                             "BA2000")


#
#Obtendo a distribuição do Grau
#Erdos Renyi

er <- vector(mode="list", length=3)
er[[1]] <- degree_distribution(er500[[1]] ,mode="all",
                               cumulative=FALSE)
er[[2]] <- degree_distribution(er1000[[1]] ,mode="all",
                               cumulative=FALSE)
er[[3]] <- degree_distribution(er2000[[1]] ,mode="all",
                               cumulative=FALSE)
vetordistgrau$ER500  <- er[[1]]
vetordistgrau$ER1000 <- er[[2]]
vetordistgrau$ER2000 <- er[[3]]

# Watts-Strogatz
ws <- vector(mode="list", length=6)
ws[[1]] <- degree_distribution(ws500[[1]] ,mode="all",
                               cumulative=FALSE)
ws[[2]] <- degree_distribution(ws1000[[1]] ,mode="all",
                               cumulative=FALSE)
ws[[3]] <- degree_distribution(ws2000[[1]] ,mode="all",
                               cumulative=FALSE)
ws[[4]] <- degree_distribution(ws5001[[1]] ,mode="all",
                               cumulative=FALSE)
ws[[5]] <- degree_distribution(ws10001[[1]] ,mode="all",
                               cumulative=FALSE)
ws[[6]] <- degree_distribution(ws20001[[1]] ,mode="all",
                               cumulative=FALSE)
vetordistgrau$WS50001  <- ws[[1]]
vetordistgrau$WS100001 <- ws[[2]]
vetordistgrau$WS200001 <- ws[[3]]
vetordistgrau$WS50000001 <- ws[[4]]
vetordistgrau$WS100000001 <- ws[[5]]
vetordistgrau$WS200000001 <- ws[[6]]

#Barabási-Albert
ba <- vector(mode="list", length=3)
ba[[1]] <- degree_distribution(ba500[[1]] ,mode="all",
                               cumulative=FALSE)
ba[[2]] <- degree_distribution(ba1000[[1]] ,mode="all",
                               cumulative=FALSE)
ba[[3]] <- degree_distribution(ba2000[[1]] ,mode="all",
                               cumulative=FALSE)

vetordistgrau$BA500 <- ba[[1]]
vetordistgrau$BA1000 <- ba[[2]]
vetordistgrau$BA2000 <- ba[[3]]

f_entropia <- function(G)
{
  E(G)$weight <- runif(ecount(G))
  e <- graph.diversity(G)
  return (e)
}

f_degree <- function(G){
  
  grau <- degree(G, mode = "all")
  return (mean(grau))
  
}

f_second_moment <- function(G){
  
  grau  = f_degree(G)
  #Segundo Momento do Grau
  grau <- grau^2
  sum.grau.square = sum(grau)
  total.grau <- length(grau)
  
  kQuadrado <- sum.grau.square / total.grau
  kQuadrado
  
  return (kQuadrado)
}

#
#Cálculo da Média e Desvio Padrão das medidas abaixo:
# 
tabelamediaer <- matrix(data = NA, nrow= 30,ncol = 7, byrow = TRUE)
colnames(tabelamediaer) <- c("Grau",
                             "NVertices",
                             "AvgTransitivity", 
                             "Assortativity", 
                             "AvgSPLength",
                             "EntropiaShannon",
                             "SegMomentoGrau")

tabelasder <- matrix(data = NA, nrow= 30,ncol = 7, byrow = TRUE)
colnames(tabelasder) <- c("Grau",
                          "NVertices",
                          "AvgTransitivity", 
                          "Assortativity", 
                          "AvgSPLength",
                          "EntropiaShannon",
                          "SegMomentoGrau")


for (i in 1:10){
  tabelamediaer[i, 1] = mean(degree(er500[[i]]))
  tabelamediaer[i, 2] = 500 
  tabelamediaer[i, 3] = mean(transitivity(er500[[i]]))
  tabelamediaer[i, 4] = mean(assortativity.degree(er500[[i]], directed = FALSE))
  tabelamediaer[i, 5] = mean_distance(er500[[i]])
  tabelamediaer[i, 6] = mean(na.omit(f_entropia(er500[[i]])))
  tabelamediaer[i, 7] = mean(f_second_moment(er500[[i]]))
}

for (i in 1:10){
  tabelamediaer[i+10, 1] = mean(degree(er1000[[i]]))
  tabelamediaer[i+10, 2] = 1000 
  tabelamediaer[i+10, 3] = mean(transitivity(er1000[[i]]))
  tabelamediaer[i+10, 4] = mean(assortativity.degree(er1000[[i]], directed = FALSE))
  tabelamediaer[i+10, 5] = mean(mean_distance(er1000[[i]]))
  tabelamediaer[i+10, 6] = mean(na.omit(f_entropia(er1000[[i]])))
  tabelamediaer[i+10, 7] = mean(f_second_moment(er1000[[i]]))
}

for (i in 1:10){
  tabelamediaer[i+20, 1] = mean(degree(er2000[[i]]))
  tabelamediaer[i+20, 2] = 2000 
  tabelamediaer[i+20, 3] = mean(transitivity(er2000[[i]]))
  tabelamediaer[i+20, 4] = mean(assortativity.degree(er2000[[i]], directed = FALSE))
  tabelamediaer[i+20, 5] = mean(mean_distance(er2000[[i]]))
  tabelamediaer[i+20, 6] = mean(na.omit(f_entropia(er2000[[i]])))
  tabelamediaer[i+20, 7] = mean(f_second_moment(er2000[[i]]))
}

#Cálculo do Desvio Padrão

for (i in 1:10){
  tabelasder[i, 1] = sd(degree(er500[[i]]))
  tabelasder[i, 2] = 500 
  tabelasder[i, 3] = sd(transitivity(er500[[i]]))
  tabelasder[i, 4] = sd(assortativity.degree(er500[[i]], directed = FALSE))
  tabelasder[i, 5] = sd(mean_distance(er500[[i]]))
  tabelasder[i, 6] = sd(f_entropia(er500[[i]]))
  tabelasder[i, 7] = sd(f_second_moment(er500[[i]]))
}

for (i in 1:10){
  tabelasder[i+10, 1] = sd(degree(er1000[[i]]))
  tabelasder[i+10, 2] = 1000 
  tabelasder[i+10, 3] = sd(transitivity(er1000[[i]]))
  tabelasder[i+10, 4] = sd(assortativity.degree(er1000[[i]], directed = FALSE))
  tabelasder[i+10, 5] = sd(mean_distance(er1000[[i]]))
  tabelasder[i+10, 6] = sd(f_entropia(er1000[[i]]))
  tabelasder[i+10, 7] = sd(f_second_moment(er1000[[i]]))
}

for (i in 1:10){
  tabelasder[i+20, 1] = sd(degree(er2000[[i]]))
  tabelasder[i+20, 2] = 2000 
  tabelasder[i+20, 3] = sd(transitivity(er2000[[i]]))
  tabelasder[i+20, 4] = sd(assortativity.degree(er2000[[i]], directed = FALSE))
  tabelasder[i+20, 5] = sd(mean_distance(er2000[[i]]))
  tabelasder[i+20, 6] = sd(f_entropia(er2000[[i]]))
  tabelasder[i+20, 7] = sd(f_second_moment(er2000[[i]]))
}

tabelamediaws <- matrix(data = NA, nrow= 30,ncol = 7, byrow = TRUE)
colnames(tabelamediaws) <- c("Grau",
                             "NVertices",
                             "AvgTransitivity", 
                             "Assortativity", 
                             "AvgSPLength",
                             "EntropiaShannon",
                             "SegMomentoGrau")

tabelasdws <- matrix(data = NA, nrow= 30,ncol = 7, byrow = TRUE)
colnames(tabelasdws) <- c("Grau",
                          "NVertices",
                          "AvgTransitivity", 
                          "Assortativity", 
                          "AvgSPLength",
                          "EntropiaShannon",
                          "SegMomentoGrau")

for (i in 1:5){
  tabelamediaws[i, 1] = mean(degree(ws500[[i]]))
  tabelamediaws[i, 2] = 500 
  tabelamediaws[i, 3] = mean(transitivity(ws500[[i]]))
  tabelamediaws[i, 4] = mean(assortativity.degree(ws500[[i]], directed = FALSE))
  tabelamediaws[i, 5] = mean_distance(ws500[[i]])
  tabelamediaws[i, 6] = mean(na.omit(f_entropia(ws500[[i]])))
  tabelamediaws[i, 7] = mean(f_second_moment(ws500[[i]]))
}

for (i in 1:5){
  tabelamediaws[i+5, 1] = mean(degree(ws5001[[i]]))
  tabelamediaws[i+5, 2] = 500 
  tabelamediaws[i+5, 3] = mean(transitivity(ws5001[[i]]))
  tabelamediaws[i+5, 4] = mean(assortativity.degree(ws5001[[i]], directed = FALSE))
  tabelamediaws[i+5, 5] = mean_distance(ws5001[[i]])
  tabelamediaws[i+5, 6] = mean(na.omit(f_entropia(ws5001[[i]])))
  tabelamediaws[i+5, 7] = mean(f_second_moment(ws5001[[i]]))
}

for (i in 1:5){
  tabelamediaws[i+10, 1] = mean(degree(ws1000[[i]]))
  tabelamediaws[i+10, 2] = 1000 
  tabelamediaws[i+10, 3] = mean(transitivity(ws1000[[i]]))
  tabelamediaws[i+10, 4] = mean(assortativity.degree(ws1000[[i]], directed = FALSE))
  tabelamediaws[i+10, 5] = mean_distance(ws1000[[i]])
  tabelamediaws[i+10, 6] = mean(na.omit(f_entropia(ws1000[[i]])))
  tabelamediaws[i+10, 7] = mean(f_second_moment(ws1000[[i]]))
}

for (i in 1:5){
  tabelamediaws[i+15, 1] = mean(degree(ws10001[[i]]))
  tabelamediaws[i+15, 2] = 1000 
  tabelamediaws[i+15, 3] = mean(transitivity(ws10001[[i]]))
  tabelamediaws[i+15, 4] = mean(assortativity.degree(ws10001[[i]], directed = FALSE))
  tabelamediaws[i+15, 5] = mean_distance(ws10001[[i]])
  tabelamediaws[i+15, 6] = mean(na.omit(f_entropia(ws10001[[i]])))
  tabelamediaws[i+15, 7] = mean(f_second_moment(ws10001[[i]]))
}

for (i in 1:5){
  tabelamediaws[i+20, 1] = mean(degree(ws2000[[i]]))
  tabelamediaws[i+20, 2] = 2000 
  tabelamediaws[i+20, 3] = mean(transitivity(ws2000[[i]]))
  tabelamediaws[i+20, 4] = mean(assortativity.degree(ws2000[[i]], directed = FALSE))
  tabelamediaws[i+20, 5] = mean_distance(ws2000[[i]])
  tabelamediaws[i+20, 6] = mean(na.omit(f_entropia(ws2000[[i]])))
  tabelamediaws[i+20, 7] = mean(f_second_moment(ws2000[[i]]))
}

for (i in 1:5){
  tabelamediaws[i+25, 1] = mean(degree(ws20001[[i]]))
  tabelamediaws[i+25, 2] = 2000 
  tabelamediaws[i+25, 3] = mean(transitivity(ws20001[[i]]))
  tabelamediaws[i+25, 4] = mean(assortativity.degree(ws20001[[i]], directed = FALSE))
  tabelamediaws[i+25, 5] = mean_distance(ws20001[[i]])
  tabelamediaws[i+25, 6] = mean(na.omit(f_entropia(ws20001[[i]])))
  tabelamediaws[i+25, 7] = mean(f_second_moment(ws20001[[i]]))
}


#Cálculo do Desvio Padrão

for (i in 1:5){
  tabelasdws[i, 1] = sd(degree(ws500[[i]]))
  tabelasdws[i, 2] = 500 
  tabelasdws[i, 3] = sd(transitivity(ws500[[i]]))
  tabelasdws[i, 4] = sd(assortativity.degree(ws500[[i]], directed = FALSE))
  tabelasdws[i, 5] = sd(mean_distance(ws500[[i]]))
  tabelasdws[i, 6] = sd(na.omit(f_entropia(ws500[[i]])))
  tabelasdws[i, 7] = sd(f_second_moment(ws500[[i]]))
}

for (i in 1:5){
  tabelasdws[i+5, 1] = sd(degree(ws5001[[i]]))
  tabelasdws[i+5, 2] = 500 
  tabelasdws[i+5, 3] = sd(transitivity(ws5001[[i]]))
  tabelasdws[i+5, 4] = sd(assortativity.degree(ws5001[[i]], directed = FALSE))
  tabelasdws[i+5, 5] = sd(mean_distance(ws5001[[i]]))
  tabelasdws[i+5, 6] = sd(na.omit(f_entropia(ws5001[[i]])))
  tabelasdws[i+5, 7] = sd(f_second_moment(ws5001[[i]]))
}

for (i in 1:5){
  tabelasdws[i+10, 1] = sd(degree(ws1000[[i]]))
  tabelasdws[i+10, 2] = 1000 
  tabelasdws[i+10, 3] = sd(transitivity(ws1000[[i]]))
  tabelasdws[i+10, 4] = sd(assortativity.degree(ws1000[[i]], directed = FALSE))
  tabelasdws[i+10, 5] = sd(mean_distance(ws1000[[i]]))
  tabelasdws[i+10, 6] = sd(na.omit(f_entropia(ws1000[[i]])))
  tabelasdws[i+10, 7] = sd(f_second_moment(ws1000[[i]]))
}

for (i in 1:5){
  tabelasdws[i+15, 1] = sd(degree(ws10001[[i]]))
  tabelasdws[i+15, 2] = 1000 
  tabelasdws[i+15, 3] = sd(transitivity(ws10001[[i]]))
  tabelasdws[i+15, 4] = sd(assortativity.degree(ws10001[[i]], directed = FALSE))
  tabelasdws[i+15, 5] = sd(mean_distance(ws10001[[i]]))
  tabelasdws[i+15, 6] = sd(na.omit(f_entropia(ws10001[[i]])))
  tabelasdws[i+15, 7] = sd(f_second_moment(ws10001[[i]]))
}

for (i in 1:5){
  tabelasdws[i+20, 1] = sd(degree(ws2000[[i]]))
  tabelasdws[i+20, 2] = 2000 
  tabelasdws[i+20, 3] = sd(transitivity(ws2000[[i]]))
  tabelasdws[i+20, 4] = sd(assortativity.degree(ws2000[[i]], directed = FALSE))
  tabelasdws[i+20, 5] = sd(mean_distance(ws2000[[i]]))
  tabelasdws[i+20, 6] = sd(na.omit(f_entropia(ws2000[[i]])))
  tabelasdws[i+20, 7] = sd(f_second_moment(ws2000[[i]]))
}

for (i in 1:5){
  tabelasdws[i+25, 1] = sd(degree(ws20001[[i]]))
  tabelasdws[i+25, 2] = 2000 
  tabelasdws[i+25, 3] = sd(transitivity(ws20001[[i]]))
  tabelasdws[i+25, 4] = sd(assortativity.degree(ws20001[[i]], directed = FALSE))
  tabelasdws[i+25, 5] = sd(mean_distance(ws20001[[i]]))
  tabelasdws[i+25, 6] = sd(na.omit(f_entropia(ws20001[[i]])))
  tabelasdws[i+25, 7] = sd(f_second_moment(ws20001[[i]]))
}


tabelamediaba <- matrix(data = NA, nrow= 30,ncol = 7, byrow = TRUE)
colnames(tabelamediaba) <- c("Grau",
                             "NVertices",
                             "AvgTransitivity", 
                             "Assortativity", 
                             "AvgSPLength",
                             "EntropiaShannon",
                             "SegMomentoGrau")
tabelasdba <- matrix(data = NA, nrow= 30,ncol = 7, byrow = TRUE)
colnames(tabelasdba) <- c("Grau",
                          "NVertices",
                          "AvgTransitivity", 
                          "Assortativity", 
                          "AvgSPLength",
                          "EntropiaShannon",
                          "SegMomentoGrau")


for (i in 1:10){
  tabelamediaba[i, 1] = mean(degree(ba500[[i]]))
  tabelamediaba[i, 2] = 500 
  tabelamediaba[i, 3] = mean(transitivity(ba500[[i]]))
  tabelamediaba[i, 4] = mean(assortativity.degree(ba500[[i]], directed = FALSE))
  tabelamediaba[i, 5] = mean_distance(ba500[[i]])
  tabelamediaba[i, 6] = mean(na.omit(f_entropia(ba500[[i]])))
  tabelamediaba[i, 7] = mean(f_second_moment(ba500[[i]]))
}

for (i in 1:10){
  tabelamediaba[i+10, 1] = mean(degree(ba1000[[i]]))
  tabelamediaba[i+10, 2] = 1000 
  tabelamediaba[i+10, 3] = mean(transitivity(ba1000[[i]]))
  tabelamediaba[i+10, 4] = mean(assortativity.degree(ba1000[[i]], directed = FALSE))
  tabelamediaba[i+10, 5] = mean_distance(ba1000[[i]])
  tabelamediaba[i+10, 6] = mean(na.omit(f_entropia(ba1000[[i]])))
  tabelamediaba[i+10, 7] = mean(f_second_moment(ba1000[[i]]))
}

for (i in 1:10){
  tabelamediaba[i+20, 1] = mean(degree(ba2000[[i]]))
  tabelamediaba[i+20, 2] = 2000 
  tabelamediaba[i+20, 3] = mean(transitivity(ba2000[[i]]))
  tabelamediaba[i+20, 4] = mean(assortativity.degree(ba2000[[i]], directed = FALSE))
  tabelamediaba[i+20, 5] = mean_distance(ba2000[[i]])
  tabelamediaba[i+20, 6] = mean(na.omit(f_entropia(ba2000[[i]])))
  tabelamediaba[i+20, 7] = mean(f_second_moment(ba2000[[i]]))
}


#Desvio Padrão
for (i in 1:10){
  tabelasdba[i, 1] = sd(degree(ba500[[i]]))
  tabelasdba[i, 2] = 500 
  tabelasdba[i, 3] = sd(transitivity(ba500[[i]]))
  tabelasdba[i, 4] = sd(assortativity.degree(ba500[[i]], directed = FALSE))
  tabelasdba[i, 5] = sd(mean_distance(ba500[[i]]))
  tabelasdba[i, 6] = sd(na.omit(f_entropia(ba500[[i]])))
  tabelasdba[i, 7] = sd(f_second_moment(ba500[[i]]))
}

for (i in 1:10){
  tabelasdba[i+10, 1] = sd(degree(ba1000[[i]]))
  tabelasdba[i+10, 2] = 1000 
  tabelasdba[i+10, 3] = sd(transitivity(ba1000[[i]]))
  tabelasdba[i+10, 4] = sd(assortativity.degree(ba1000[[i]], directed = FALSE))
  tabelasdba[i+10, 5] = sd(mean_distance(ba1000[[i]]))
  tabelasdba[i+10, 6] = sd(na.omit(f_entropia(ba1000[[i]])))
  tabelasdba[i+10, 7] = sd(f_second_moment(ba1000[[i]]))
}

for (i in 1:10){
  tabelasdba[i+20, 1] = sd(degree(ba2000[[i]]))
  tabelasdba[i+20, 2] = 2000 
  tabelasdba[i+20, 3] = sd(transitivity(ba2000[[i]]))
  tabelasdba[i+20, 4] = sd(assortativity.degree(ba2000[[i]], directed = FALSE))
  tabelasdba[i+20, 5] = sd(mean_distance(ba2000[[i]]))
  tabelasdba[i+20, 6] = sd(na.omit(f_entropia(ba2000[[i]])))
  tabelasdba[i+20, 7] = sd(f_second_moment(ba2000[[i]]))
}

summary(tabelamediaer)
summary(tabelamediaba)
summary(tabelamediaws)
summary(tabelasdba)
summary(tabelasder)
summary(tabelasdws)
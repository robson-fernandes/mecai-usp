library(igraph)
library(dplyr)

closeAllConnections()
graphics.off()
rm(list=ls())


#Gera rede BA
cal_alpaha_ba <- function(n, alphaa){
  ba500 <- vector(mode="list", length=30)
  for (i in 1:length(ba500)){
    ba500[[i]] <- sample_pa(n, power = alphaa, m = 5, out.dist = NULL, out.seq = NULL,
                            out.pref = FALSE, zero.appeal = 1, directed = FALSE,
                            algorithm ="psumtree", start.graph = NULL)
    #plot(ba500[[i]], vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Rede BarabÃ¡si-Albert")
  }
  return(ba500)
}  


#
#Criar tabela para armazenar resultados
#
cal_distgrau <- function(ba500){
  
  vetordistgrau <- matrix(data = vector(mode = 'list', length=1), nrow=1, ncol=1, byrow = TRUE)
  colnames(vetordistgrau) <- c("Dist. Grau")
  rownames(vetordistgrau) <- "BA500"
  
  ba <- vector(mode="list", length=3)
  ba[[1]] <- degree_distribution(ba500[[1]] ,mode="all",
                                 cumulative=FALSE)
  
  return(ba[[1]])
}

calcula_medidas <- function(n, ba500){
  f_entropia <- function(G)
  {
    E(G)$weight <- runif(ecount(G))
    e <- graph.diversity(G)
    #e[is.nan(e)] <- 0
    e[!is.finite(e)] <- 0
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
  
  for (i in 1:30){
    tabelamediaba[i, 1] = mean(degree(ba500[[i]]))
    tabelamediaba[i, 2] = n 
    tabelamediaba[i, 3] = transitivity(ba500[[i]])
    tabelamediaba[i, 4] = assortativity.degree(ba500[[i]], directed = FALSE)
    tabelamediaba[i, 5] = mean_distance(ba500[[i]])
    tabelamediaba[i, 6] = mean((f_entropia(ba500[[i]])))
    tabelamediaba[i, 7] = f_second_moment(ba500[[i]])
  }
  
  #Desvio PadrÃ£o
  for (i in 1:30){
    tabelasdba[i, 1] = sd(degree(ba500[[i]]))
    tabelasdba[i, 2] = n 
    tabelasdba[i, 3] = transitivity(ba500[[i]])
    tabelasdba[i, 4] = assortativity.degree(ba500[[i]], directed = FALSE)
    tabelasdba[i, 5] = mean_distance(ba500[[i]])
    tabelasdba[i, 6] = sd((f_entropia(ba500[[i]])))
    tabelasdba[i, 7] = f_second_moment(ba500[[i]])
  }
  return(tabelasdba)
}


retorna_media_sd <- function(n, alphaa){
  
  ba500 = cal_alpaha_ba(n, alphaa)
  show(cal_distgrau(ba500))
  
  assign('distgrau', cal_distgrau(ba500), envir=globalenv())
  tabelasdba = calcula_medidas(n, ba500)
  #Resumo Media BA
  
  return (rbind(as.data.frame(tabelasdba) %>%
                  group_by(NVertices) %>%
                  summarize(Grau = mean(Grau), 
                            AvgTransitivity = mean(AvgTransitivity), 
                            Assortativity = mean((Assortativity)), 
                            AvgSPLength = mean(AvgSPLength), 
                            EntropiaShannon = mean(EntropiaShannon), 
                            SegMomentoGrau = mean(SegMomentoGrau)),  
                as.data.frame(tabelasdba) %>%
                  group_by(NVertices) %>%
                  summarize(Grau = sd(Grau), 
                            AvgTransitivity = sd(AvgTransitivity), 
                            Assortativity = sd((Assortativity)), 
                            AvgSPLength = sd(AvgSPLength), 
                            EntropiaShannon = sd(EntropiaShannon), 
                            SegMomentoGrau = sd(SegMomentoGrau))
  )
  )
}
ba_05 = retorna_media_sd(500,.5)
ba_05_dist = get('distgrau')

# Distribuição Grau - BA com mu = 0.5
plot(ba_05_dist,
     pch=19, 
     cex=1.2,
     log = "xy", 
     xlab = "",
     ylab = "Degree Distribution", 
     col = "blue", 
     type = 'p', 
     main = "Degree Distribution - BA - 500 e mu = 0.5")

ba_1 = retorna_media_sd(500,1)
ba_1_dist = get('distgrau')

# Distribuição Grau - BA com mu = 1
plot(ba_1_dist,
     pch=19, 
     cex=1.2,
     log = "xy", 
     xlab = "",
     ylab = "Degree Distribution", 
     col = "blue", 
     type = 'p', 
     main = "Degree Distribution - BA - 500 e mu = 1")

ba_15 = retorna_media_sd(500,1.5)
ba_15_dist = get('distgrau')

# Distribuição Grau - BA com mu = 1.5
plot(ba_15_dist,
     pch=19, 
     cex=1.2,
     log = "xy", 
     xlab = "",
     ylab = "Degree Distribution", 
     col = "blue", 
     type = 'p', 
     main = "Degree Distribution - BA - 500 e mu = 1.5")

ba_20 = retorna_media_sd(500,2)
ba_20_dist = get('distgrau')

# Distribuição Grau - BA com mu = 2
plot(ba_20_dist,
     pch=19, 
     cex=1.2,
     log = "xy", 
     xlab = "",
     ylab = "Degree Distribution", 
     col = "blue", 
     type = 'p', 
     main = "Degree Distribution - BA - 500 e mu = 2")
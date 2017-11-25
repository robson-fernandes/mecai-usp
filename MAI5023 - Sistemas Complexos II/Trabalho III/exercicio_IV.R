require(igraph)

#
# 4	– Barabási-Albert	não-linear:
#

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


f_entropia <- function(G)
{
  E(G)$weight <- runif(ecount(G))
  e <- graph.diversity(G)
  return (mean(e))
}


cal_alpaha_ba <- function(n, alphaa){
  medidas = matrix(data = NA, nrow = 30,ncol = 7)
  for(i in 1:30){
    network = sample_pa(n, alphaa)
    medidas[i,1] = 1#f_degree(network)
    medidas[i,2] = vcount(network)
    medidas[i,3] = f_second_moment(network)
    medidas[i,4] = transitivity(network)
    medidas[i,5] = assortativity_degree(network)
    medidas[i,6] = f_entropia(network)
    medidas[i,7] = mean_distance(network)
  }
  med_me_de = matrix(data = NA, nrow = 2, ncol = 7)
  for(i in 1:7){
    med_me_de[1,i] = mean(medidas[,i])
    med_me_de[2,i] = sd(medidas[,i])
  }
  return(med_me_de)
}


ba_05 = cal_alpaha_ba(500,.5)
ba_1 = cal_alpaha_ba(500,1)
ba_15 = cal_alpaha_ba(500,1.5)
ba_20 = cal_alpaha_ba(500,2)
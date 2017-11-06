library(igraph)

###funções que realizam as comparações entre as bases
compare_vector <- function(network, comunidade){
  ajuste = cluster_leading_eigen(network, steps = -1, weights = NULL, start = NULL,
                                 options = arpack_defaults, callback = NULL,
                                 extra = NULL)
  membro_estimado = membership(ajuste)
  membro_real = comunidade[,2]
  return(compare(membro_estimado, membro_real, method = "nmi"))
}

compare_waltrap <- function(network, comunidade){
  ajuste = cluster_walktrap(network, weights = NULL, steps = 4, merges = TRUE, 
                            modularity = TRUE, membership = TRUE)
  membro_estimado = membership(ajuste)
  membro_real = comunidade[,2]
  return(compare(membro_estimado, membro_real, method = "nmi"))
}

compare_fast <- function(network, comunidade){
  network = simplify(network)
  ajuste = cluster_fast_greedy(network, merges = TRUE, modularity = TRUE,
                               membership = TRUE, weights = E(network)$weight)
  
  membro_estimado = membership(ajuste)
  membro_real = comunidade[,2]
  return(compare(membro_estimado, membro_real, method = "nmi"))
}

compare_bet <- function(network, comunidade){
  ajuste = cluster_edge_betweenness(network, weights = NULL, directed = FALSE, 
                                    edge.betweenness = TRUE, merges = TRUE,
                                    bridges = TRUE, modularity = TRUE, 
                                    membership = TRUE)
  membro_estimado = membership(ajuste)
  membro_real = comunidade[,2]
  return(compare(membro_estimado, membro_real, method = "nmi"))
}





c1 <- read.delim("estrutura-comunidades/c1.dat", 
                 header=FALSE)
c2 <- read.delim("estrutura-comunidades/c2.dat",
                 header=FALSE)
c3 <- read.delim("estrutura-comunidades/c3.dat",
                 header=FALSE)
c4 <- read.delim("estrutura-comunidades/c4.dat",
                 header=FALSE)
c5 <- read.delim("estrutura-comunidades/c5.dat",
                 header=FALSE)
c6 <- read.delim("estrutura-comunidades/c6.dat",
                 header=FALSE)
c7 <- read.delim("estrutura-comunidades/c7.dat",
                 header=FALSE)
c8 <- read.delim("estrutura-comunidades/c8.dat",
                 header=FALSE)
c9 <- read.delim("estrutura-comunidades/c9.dat",
                 header=FALSE)
c10 <- read.delim("estrutura-comunidades/c10.dat",
                  header=FALSE)


n1 <- read.delim("estrutura-comunidades/n1.dat", 
                 header=FALSE)
n1 <- graph.data.frame(n1, directed=FALSE)
n2 <- read.delim("estrutura-comunidades/n2.dat", 
                 header=FALSE)
n2 <- graph.data.frame(n2, directed=FALSE)
n3 <- read.delim("estrutura-comunidades/n3.dat", 
                 header=FALSE)
n3 <- graph.data.frame(n3, directed=FALSE)
n4 <- read.delim("estrutura-comunidades/n4.dat", 
                 header=FALSE)
n4 <- graph.data.frame(n4, directed=FALSE)
n5 <- read.delim("estrutura-comunidades/n5.dat", 
                 header=FALSE)
n5 <- graph.data.frame(n5, directed=FALSE)
n6 <- read.delim("estrutura-comunidades/n6.dat", 
                 header=FALSE)
n6 <- graph.data.frame(n6, directed=FALSE)
n7 <- read.delim("estrutura-comunidades/n7.dat", 
                 header=FALSE)
n7 <- graph.data.frame(n7, directed=FALSE)
n8 <- read.delim("estrutura-comunidades/n8.dat", 
                 header=FALSE)
n8 <- graph.data.frame(n8, directed=FALSE)
n9 <- read.delim("estrutura-comunidades/n9.dat", 
                 header=FALSE)
n9 <- graph.data.frame(n9, directed=FALSE)
n10 <- read.delim("estrutura-comunidades/n10.dat", 
                  header=FALSE)
n10 <- graph.data.frame(n10, directed=FALSE)

c_b = c(1:10)
c_b[1] = compare_bet(n1,c1)
c_b[2] = compare_bet(n2,c2)
c_b[3] = compare_bet(n3,c3)
c_b[4] = compare_bet(n4,c4)
c_b[5] = compare_bet(n5,c5)
c_b[6] = compare_bet(n6,c6)
c_b[7] = compare_bet(n7,c7)
c_b[8] = compare_bet(n8,c8)
c_b[9] = compare_bet(n9,c9)
c_b[10] = compare_bet(n10,c10)

c_f = c(1:10)
c_f[1] = compare_fast(n1,c1)
c_f[2] = compare_fast(n2,c2)
c_f[3] = compare_fast(n3,c3)
c_f[4] = compare_fast(n4,c4)
c_f[5] = compare_fast(n5,c5)
c_f[6] = compare_fast(n6,c6)
c_f[7] = compare_fast(n7,c7)
c_f[8] = compare_fast(n8,c8)
c_f[9] = compare_fast(n9,c9)
c_f[10] = compare_fast(n10,c10)

c_w = c(1:10)
c_w[1] = compare_waltrap(n1,c1)
c_w[2] = compare_waltrap(n2,c2)
c_w[3] = compare_waltrap(n3,c3)
c_w[4] = compare_waltrap(n4,c4)
c_w[5] = compare_waltrap(n5,c5)
c_w[6] = compare_waltrap(n6,c6)
c_w[7] = compare_waltrap(n7,c7)
c_w[8] = compare_waltrap(n8,c8)
c_w[9] = compare_waltrap(n9,c9)
c_w[10] = compare_waltrap(n10,c10)

l_v = c(1:10)
l_v[1] = compare_vector(n1,c1)
l_v[2] = compare_vector(n2,c2)
l_v[3] = compare_vector(n3,c3)
l_v[4] = compare_vector(n4,c4)
l_v[5] = compare_vector(n5,c5)
l_v[6] = compare_vector(n6,c6)
l_v[7] = compare_vector(n7,c7)
l_v[8] = compare_vector(n8,c8)
l_v[9] = compare_vector(n9,c9)
l_v[10] = compare_vector(n10,c10)

mu = seq(.1,1, by = 0.1)

plot(mu, l_v, type = "l", col = "red", ylab = "NMI", xlab = "Mu", 
     main = "Avaliação dos métodos de detectação de comunidade")
lines(mu, c_f, type = "l", col = "blue")
lines(mu, c_w, type = "l", col = "orange")
lines(mu, c_b, type = "l", col = "yellow")
legend("topright", legend = c("Eigenvectors", "Fast-greedy", "Walktrap", "Betweenness"),
       col = c("red","blue","orange","yellow"), lty = 1:4, cex = 0.8)
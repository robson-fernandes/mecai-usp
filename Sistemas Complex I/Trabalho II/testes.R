g <- erdos.renyi.game(100, 1/100)
graphs <- decompose.graph(g)

V(g)$label <- seq(vcount(g))
graphs <- decompose.graph(g)

largest <- which.max(sapply(graphs, vcount))
plot(graphs[[largest]], layout=layout.fruchterman.reingold)

plot(g, layout=layout.fruchterman.reingold)


library(igraph)
g <- make_empty_graph(n = 5) %>%
  add_edges(c(1,2, 2,3, 3,4, 4,5)) %>%
  set_edge_attr("color", value = "red") %>%
  add_edges(c(5,1), color = "green")
E(g)[[]]
plot(g)
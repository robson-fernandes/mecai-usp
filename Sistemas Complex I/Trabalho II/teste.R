library(igraph)
size = 6

# tree graph with two children for each node
g = graph.tree(size, children = 2, mode = "undirected")
plot(g, layout=layout_with_fr, vertex.color ="Green")
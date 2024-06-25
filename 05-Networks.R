library(igraph)
library(ggraph)

g <- make_ring(10)
ggraph(g) + geom_edge_link() + geom_node_point()

ego_size(g, order = 0, 1:3)
ego_size(g, order = 1, 1:3)
ego_size(g, order = 2, 1:3)


ego(g, order = 1, 1:3)
ego(g, order = 2, 1:3)

# attributes are preserved
V(g)$name <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
make_ego_graph(g, order = 2, 1:3)

# connecting to the neighborhood
g <- make_ring(10)
g <- connect(g, 2) 

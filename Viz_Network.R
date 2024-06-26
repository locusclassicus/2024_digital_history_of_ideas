library(dplyr)
library(ggrepel)
library(igraph)
library(networkD3)
library(tidyr)
library(threejs)
library(visNetwork)

dg <- log(degree(merged, V(merged)))
dg <- (dg/max(dg))+1
V(merged)$size <- dg * 10

# find community
gg.com <- cluster_fast_greedy(merged)

# gg.com <- leading.eigenvector.community(merged)
# gg.com <- walktrap.community(merged)

V(merged)$color <- gg.com$membership + 1

# prune nodes
merged <- delete.vertices(merged, 
                              V(merged)[ degree(merged) < 26] )
# 
# # plot
# ggraph(merged, layout = "stress") + 
#   geom_edge_link(alpha = 0.5) +
#   geom_node_point(aes(fill = as.factor(vertex_attr(merged)$color)), 
#                   size = 4,
#                   shape = 21,
#                   show.legend = FALSE) + 
#   geom_label_repel(aes(label = name, x = x, y = y,
#                        color = as.factor(vertex_attr(merged)$color)),
#                    show.legend = FALSE,
#                    nudge_y = 0.05, 
#                    label.size = 1#,
#                    #fontface = "bold"
#   ) +
#   theme_void()


td <- toVisNetworkData(merged)

ws <- select(td$edges, starts_with("weight"))
#ws[is.na(ws)] <- 0
td$edges$value <- rowSums(ws)
td$nodes$font.size = 10
conc_output <- "none"

visNetwork(nodes=td$nodes,
           edges=td$edges,
           height='100vh',
           width='100%') |>
  visIgraphLayout(type="full")  |>
  visOptions(highlightNearest = list(enabled = T, hover = T),
             nodesIdSelection = T)  |>
  visExport()

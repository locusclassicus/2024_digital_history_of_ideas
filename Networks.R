library(igraph)
library(ggraph)

rels_graph_data <- rels |> 
  select(focal, bound, dpf) |> 
  filter(dpf > 1) |> 
  rename(weight = dpf)

rels_graph_data <- rels_graph_data |> 
  group_by(focal) |> 
  arrange(-weight) |> 
  slice_head(n = 20) |> 
  ungroup()

g <- graph_from_data_frame(rels_graph_data, 
                           directed = FALSE) 
is_weighted(g)

g<-igraph::simplify(g, remove.loops = TRUE)
is_weighted(g)

ego_terms <- "слеза"

if(nchar(ego_terms) > 1){
  tmp <- ego_terms %>% strsplit("[, ]") %>% unlist
  ego_node <- tmp[tmp!=''] 
  sublist <- make_ego_graph(g, 1, ego_node, mode = "all")
  merged <- do.call(igraph::union, sublist)
}else{
  merged <- g
}

is_weighted(merged)

bc <- betweenness(merged, cutoff = 4)
bcdf <- data.frame(bc, names(bc)) |> 
  arrange(desc(bc))

ec <- eigen_centrality(merged)
ecdf <- data.frame(ec, names(bc)) |> 
  arrange(desc(bc))

pr <- page_rank(merged)
prdf <-data.frame(pr$vector)

merged


rels_graph_data |> 
  filter(focal == "уведомлять") |> 
  select(focal, bound, weight) |> 
  arrange(-weight)

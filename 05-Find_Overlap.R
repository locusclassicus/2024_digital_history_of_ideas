get_top_words <- function(item, n, df) {
  df |> 
    filter(item1 == item) |> 
    arrange(-dpf) |> 
    mutate(rank = row_number()) |> 
    filter(rank < n+1) 
}

unique_items <- unique(df$item1)
result <- map_df(unique_items, get_top_words, df, n = 15)


vec1 <- result |> 
  filter(item1 == "лиза") |> 
  pull(item2)

vec2 <- result |> 
  filter(item1 == "эраст") |> 
  pull(item2)

vec1[which(vec1 %in% vec2)]
vec2[which(vec2 %in% vec1)]

save(result, file = "./data/result.Rdata")

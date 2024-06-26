library(tidyr)
library(widyr)
library(uwot)
source("./helper_functions/nearest_neighbors.R")

# svd 
tidy_vectors <- sel  |> 
  widely_svd(focal, bound, score, nv = 50)

nn <- nearest_neighbors(tidy_vectors, "слеза") |> 
  slice_head(n = 50) |> 
  pull(item1)

# prepare matrix for umap
tidy_vectors_w <- tidy_vectors |> 
  pivot_wider(names_from = dimension, 
              values_from = value) |> 
  tibble::column_to_rownames("focal")

viz <- umap(tidy_vectors_w, 
            n_neighbors = 15, 
            n_threads = 2,
            metric = "cosine",
            n_epochs = 5,
            init = "pca"
            )
# plot
plot_data <- tibble(word = rownames(viz), 
       V1 = viz[, 1], 
       V2 = viz[, 2]) |> 
  filter(word %in% nn | word == "слеза") |> 
  mutate(color = case_when(word == "слеза" ~ "tomato",
                   .default = "royalblue"))


plot_data |> 
  ggplot(aes(x = V1, y = V2, label = word)) + 
  ggrepel::geom_text_repel(size = 2, alpha = 0.7, 
            color = plot_data$color) +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL)


library(tidyverse)
library(widyr)
library(broom)
library(tidytext)
source("./helper_functions/nearest_neighbors.R")
load("./data/rels.Rdata")


# svd 
tidy_vectors <- rels |> 
  widely_svd(focal, bound, dpf, nv = 50)

nn <- nearest_neighbors(tidy_vectors, "слеза") |> 
  slice_head(n = 50) |> 
  pull(item1)

# plot tidy vectors 

tidy_vectors |> 
  filter(dimension %in% c(1, 2, 3)) |> 
  group_by(dimension) |> 
  top_n(12, abs(value)) |> 
  ungroup() |> 
  mutate(focal = reorder_within(focal, value, dimension)) |> 
  ggplot(aes(focal, value, fill = dimension)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~dimension, scales = "free_y", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  labs(NULL)

# prepare matrix for pca
tidy_vectors_w <- tidy_vectors |> 
  pivot_wider(names_from = dimension, 
              values_from = value) |> 
  tibble::column_to_rownames("focal")

pca_fit <- prcomp(tidy_vectors_w)

pca_fit |> 
  augment(tidy_vectors_w) |> 
  filter(.rownames %in% nn) |> 
  ggplot(aes(.fittedPC1, .fittedPC2)) + 
  geom_text(aes(label = .rownames), size = 2, alpha = 0.7)
  
  
 
 
 


plot_data |> 
  ggplot(aes(x = V1, y = V2, label = word)) + 
  ggrepel::geom_text_repel(size = 2, alpha = 0.7, 
            color = plot_data$color) +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL)


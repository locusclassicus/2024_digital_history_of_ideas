library(tidyverse)
library(ggpubr)
load("./data/rels.Rdata")

liza_sel <- rels |> 
  filter(focal == "слеза", bound_e < 400) |> 
  select(bound, focal_e, bound_e, dpf, pmi)

# plot 1
liza_sel |> 
  ggplot(aes(bound_e, pmi, color = pmi)) +
  geom_jitter(show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "tomato") +
  theme_bw() +
  ggrepel::geom_label_repel(aes(label = bound)) +
  theme(legend.position = "none") + 
  stat_cor(aes(label = ..r.label..),
           method = "pearson",
           label.x = 220,
           #label.y = 9,
           color = "tomato",
           geom = "label"
           ) +
  xlab(NULL)

liza_sel |> 
  ggplot(aes(bound_e, dpf, color = dpf)) +
  geom_jitter(show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "tomato") +
  theme_bw() +
  ggrepel::geom_label_repel(aes(label = bound)) +
  theme(legend.position = "none") + 
  stat_cor(aes(label = ..r.label..),
           method = "pearson",
           label.x = 220,
           #label.y = 9,
           color = "tomato",
           geom = "label"
  ) +
  xlab(NULL)


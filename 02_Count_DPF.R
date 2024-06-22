library(tidyr)
source("./helper_functions/dpf_function.R")
load("./data/liza_wind.Rdata")
load("./data/liza_freq.Rdata")

liza_count <- liza_wind |> 
  pairwise_count(lemma, window_id)

liza_count_w <- liza_count |> 
  pivot_wider(names_from = item2, values_from = n, values_fill = 0) |> 
  tibble::column_to_rownames("item1") |> 
  as.matrix()

#save(liza_count_w, file = "./data/liza_count_w.Rdata")

#load("./data/liza_count_w.Rdata")
liza_dpf <- dpf(liza_count_w, alpha = 0.78) |> 
  as.data.frame()

liza_dfp_l <- liza_dpf |> 
  tibble::rownames_to_column("item1") |> 
  pivot_longer(-item1, names_to = "item2",
               values_to = "dpf") 

liza_join <- liza_dfp_l |> 
  left_join(liza_freq)

save(liza_join, file = "./data/liza_join.Rdata")

data_sel <- liza_join |>
  filter(item1 == "слеза") |>
  filter(tf_perc < 2) |> 
  arrange(-dpf) |>
  mutate(rank = row_number())

data_sel

save(data_sel, file = "./data/data_sel.Rdata")

# plot cor between tf and dpf
data_sel |> 
  ggplot(aes(tf_perc, dpf, color = dpf)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "tomato") +
  theme_bw() +
  ggrepel::geom_label_repel(aes(label = item2)) +
  theme(legend.position = "none")

# cor coef
cor(data_sel$tf_perc, data_sel$dpf)
cor.test(data_sel$tf_perc, data_sel$dpf)


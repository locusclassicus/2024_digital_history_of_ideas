# based on code by agricolaMZ
# based on S. Evert's lecture: https://stephanie-evert.de/SIGIL/sigil_R/materials/04_collocation_analysis.pdf

library(tidyverse)
library(tidytext)
library(stopwords)
library(widyr)

# bigram count -----------------------------------------------------
load("./data/liza_wind.Rdata")

O11 <- liza_wind |> 
  pairwise_count(lemma, window_id) |> 
  rename(o11 = n)

# unigram count  ---------------------------------------------------

R1 <- O11 |> 
  group_by(item1) |> 
  summarise(r1 = sum(o11)) 

C1 <- O11 |> 
  group_by(item2) |> 
  summarise(c1 = sum(o11)) 

count_all <- O11 |> 
  left_join(R1) |> 
  left_join(C1) |> 
  mutate(o12 = r1 - o11,
         o21 = c1 - o11,
         N = sum(o11),
         o22 = N - o11 - o12 - o21,
         r2 = o21 + o22,
         c2 = o12 + o22) |> 
  relocate(N, .after = c2) |> 
  relocate(r1, .after = o22) |> 
  relocate(c1, .after = r2)

# merge them all and calculate measures -----------------------------------

measures_all <- count_all |>
  mutate(e11 = r1 * c1 / N, 
         e12 = r1 * c2 / N,
         e21 = r2 * c1 / N, 
         e22 = r2 * c2 / N,
         MI = log2(o11 / e11),
         t.score = (o11 - e11) / sqrt(o11),
         X2 = (o11-e11)^2/e11 + (o12-e12)^2/e12 + (o21-e21)^2/e21 + (o22-e22)^2/e22,
         DP = o11 / r1 - o21 / r2, 
         dpf = (o11 / N) / ( (r1/N) * (c1/N) )^0.7
         ) |> 
  arrange(desc(t.score)) |> 
  select(item1, item2, r1, c1, o11, o21, o12, N, MI, t.score, X2, DP, dpf) |> 
  rename(total = N,
         co_occurrence_frequency = o11,
         w1_frequency = r1,
         w2_frequency = c1)

# частотность и pmi
sel_mi <- measures_all |> 
  filter(item1 == "слеза") |> 
  select(item1, item2, MI, w2_frequency, total) |>
  arrange(-MI) |> 
  mutate(tf = (w2_frequency / total)*100) |> 
  select(-w2_frequency, -total)

cor.test(sel_mi$tf, sel_mi$MI)

sel_mi |>
  filter(item2 != "лиза") |> 
  ggplot(aes(tf, MI, colour = MI)) +
  geom_point(show.legend = FALSE) +
  ggrepel::geom_label_repel(aes(label = item2), 
                            show.legend = FALSE) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              color = "tomato") +
  theme_bw()

sel_mi |> 
  select(-tf)

cor.test(sel_mi$tf, sel_mi$MI)


# частотность и dpf
sel_dpf <- measures_all |> 
  filter(item1 == "слеза") |> 
  select(item1, item2, dpf, w2_frequency, total) |>
  arrange(-dpf) |> 
  mutate(tf = (w2_frequency / total)*100) |> 
  select(-w2_frequency, -total)

cor.test(sel_dpf$tf, sel_dpf$dpf)

sel_dpf |> 
  select(-tf)

sel_dpf |>
  filter(item2 != "лиза") |> 
  ggplot(aes(tf, dpf, colour = dpf)) +
  geom_point(show.legend = FALSE) +
  ggrepel::geom_label_repel(aes(label = item2), 
                            show.legend = FALSE) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              color = "tomato") +
  theme_bw()

save(measures_all, file = "./data/measures_all.Rdata")
save(sel_dpf, file = "./data/sel_dpf.Rdata")

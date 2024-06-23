library(dplyr)
library(purrr)
library(ggplot2)
source("./helper_functions/slide_windows.R")
library(widyr)
library(stopwords)

liza_df <- tibble(lemma = readLines(con = "./texts/liza_l.txt"))
          
# delete stopwords
stopwords_ru <- c(
  stopwords("ru", source = "snowball"),
  stopwords("ru", source = "marimo"),
  stopwords("ru", source = "nltk"))

# уберем повторы и упорядочим по алфавиту
stopwords_ru <- sort(unique(stopwords_ru))
stopwords_ru <- c(stopwords_ru, "это", "весь", "сей", "твой")

liza_df <- liza_df |> 
  anti_join(tibble(lemma = stopwords_ru))

# sliding window
liza_wind <- liza_df |> 
  slide_windows(10L)

save(liza_wind, file = "./data/liza_wind.Rdata")

liza_pmi <- liza_wind |> 
  pairwise_pmi(lemma, window_id)

liza_freq <- liza_df |> 
  mutate(total = length(lemma)) |> 
  group_by(lemma, total) |> 
  summarise(freq = n()) |> 
  mutate(tf_perc = (freq / total) * 100) |> 
  select(-freq, -total) |> 
  rename(item2 = lemma)

save(liza_freq, file = "./data/liza_freq.Rdata")

liza_join <- liza_pmi |> 
  left_join(liza_freq)

liza_sel <- liza_join |> 
  filter(item1 == "слеза", tf_perc < 2) |>
  arrange(-pmi) 

liza_sel

liza_sel |> 
  ggplot(aes(tf_perc, pmi, color = pmi)) +
  geom_jitter(show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "tomato") +
  theme_bw() +
  ggrepel::geom_label_repel(aes(label = item2)) +
  theme(legend.position = "none")

cor(liza_sel$tf_perc, liza_sel$pmi)
cor.test(liza_sel$tf_perc, liza_sel$pmi)

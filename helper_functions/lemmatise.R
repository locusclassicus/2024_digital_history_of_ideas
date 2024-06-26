library(udpipe)
library(dplyr)

liza <- readLines(con = "./texts/karamzin_liza.txt")
russian_syntagrus <- udpipe_load_model(file = "russian-syntagrus-ud-2.5-191206.udpipe")
liza_ann <- udpipe_annotate(russian_syntagrus, liza)

liza_vec <- as_tibble(liza_ann) |> 
  filter(!upos %in% c("PUNCT")) |>
  select(lemma) |> 
  mutate(lemma = tolower(lemma)) |> 
  pull(lemma)

writeLines(liza_vec, con = "./texts/liza_l.txt")

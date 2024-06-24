# based on code by agricolaMZ
# based on S. Evert's lecture: https://stephanie-evert.de/SIGIL/sigil_R/materials/04_collocation_analysis.pdf
# based on P. Nulty's code: https://github.com/pnulty/continental-drift/blob/main/evert_stats.R

library(tidyverse)
library(tidytext)
library(widyr)


# this is my sliding windows data
load("./data/liza_wind.Rdata")
window_size = 10

# calculate cooc 
rels <- liza_wind |> 
  pairwise_count(lemma, window_id) |> 
  rename(count = n, focal = item1, bound = item2) 
  
focal_fs <-  rels |> 
  group_by(focal) |> 
  summarise(count = sum(count)) |> 
  ungroup()

bound_fs <- rels |> 
  group_by(bound) |> 
  summarise(count = sum(count)) |> 
  ungroup()

rels <- left_join(rels, focal_fs, by = c('focal'))
rels <- left_join(rels, bound_fs, by = c('bound'))
rels <- rename(rels, bound_e=count, focal_e = count.y)
rels <- rename(rels, count=count.x)

# this adjusts counts for the window size
rels <- mutate(rels, focal_e = ((focal_e%/%window_size)+1))
rels <- mutate(rels, bound_e = ((bound_e%/%window_size)+1))

# total counts
thisN <- sum(rels$focal_e)

rels$o11 <- rels$count
rels$o12 <- rels$focal_e - rels$count
rels$o21 <- rels$bound_e - rels$count
rels$o22 <- thisN - (rels$focal_e + rels$bound_e)
rels$e11 <- (rels$focal_e * rels$bound_e)/thisN
rels$e12 <- ((rels$o11 +rels$o22) * (rels$o12 + rels$o22))/thisN

save(rels, file = "./data/rels.Rdata")


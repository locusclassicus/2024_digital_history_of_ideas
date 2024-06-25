library(tidyverse)

load("./data/rels.Rdata")
#source("scorers.R")

# total counts
thisN <- sum(rels$focal_e)

# e11 
rels <- mutate(rels, 
               # cl = map(count, ~poisson.test(.x)$conf.int[[1]]),
               # cu = map(count, ~poisson.test(.)$conf.int[[2]]),
               obs = (count/thisN)) #|> 
  #filter(obs > e11)

# If 𝑋~Pois(lambda) then E(X) = lambda and SD = sqrt(lambda)
rels <- mutate(rels, fel = focal_e - (1.96*sqrt(focal_e)), feu = focal_e +(1.96*sqrt(focal_e)))
rels <- mutate(rels, bel = bound_e - (1.96*sqrt(bound_e)), beu = bound_e + (1.96*sqrt(bound_e)))
rels <- mutate(rels, cl = count - (1.96*sqrt(count)), cu = count + (1.96*sqrt(count)))

thisNl <- sum(rels$fel)
thisNu <- sum(rels$feu)

rels <- mutate(rels, e11l = (fel/thisNl)*(bel/thisNl))
rels <- mutate(rels, obsl = (cl/thisNl))

rels <- mutate(rels, e11u = (feu/thisNu)*(beu/thisNu))
rels <- mutate(rels, obsu = (cu/thisNu))

rels$dpf <- rels$obs / rels$e11^0.7
rels$pmi <- log(rels$obs / rels$e11)
rels$scoreu <- rels$obsu / rels$e11u^0.7
rels$scorel <- rels$obsl / rels$e11l^0.7

save(rels, file = "./data/rels.Rdata")

rels |> 
  filter(focal == "слеза") |> 
  arrange(-dpf) |> 
  slice_head(n = 10) |> 
  ggplot(aes(reorder(bound, dpf), dpf, color = bound)) +
  geom_point(show.legend = FALSE) + 
  coord_flip() +
  theme_bw() +
  geom_errorbar(aes(ymax = scorel, ymin = scoreu), 
                show.legend = FALSE) +
  xlab(NULL)

rels |> 
  filter(focal == "пруд") |> 
  arrange(-dpf) |> 
  slice_head(n = 10) |> 
  ggplot(aes(reorder(bound, dpf), dpf, color = bound)) +
  geom_point(show.legend = FALSE) + 
  coord_flip() +
  theme_bw() +
  geom_errorbar(aes(ymax = scorel, ymin = scoreu), 
                show.legend = FALSE) +
  xlab(NULL)  



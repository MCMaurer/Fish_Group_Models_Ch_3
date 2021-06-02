# get session info
sess <- sessionInfo()
on_farm <- stringr::str_detect(sess$running, "Ubuntu")

# if running on Ubuntu (on the FARM), then change lib paths and load brms from a specific location, otherwise just load brms
if (on_farm) {
  .libPaths(.libPaths()[2:3])
  .libPaths()
  library(brms, lib.loc = "/home/mjculsha/RPackages/R3.5.1")
} else {library(brms)}

# read data
if(on_farm){
  d <- readRDS("data/cleaned/latency_typical_food.rds") 
} else {d <- readRDS("data/cleaned_data/ch_3_data_cleaned.rds")}

d

library(tidyverse)

dpo <- d %>% 
  filter(!is.na(pellet_order) & pellet_order > 0) %>% 
  mutate(pellet_order = as.integer(pellet_order))

model3.2 <- brm(data = dpo, family = cumulative(),
                bf(pellet_order ~ group_size + pellet_bites + rankformula + (1|group) + (1|group:fish_id), 
                   autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
                prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                          set_prior("normal(0, 1)", class = "b")),
                iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
                control = list(adapt_delta = 0.98, max_treedepth = 15))

model3.2 %>% write_rds("fitted_models/pel_ord_allgs_cumu_best.rds")


dpogg <- dpo %>% 
  mutate(group_size = factor(group_size, ordered = T))


model3.3 <- brm(data = dpo, family = cumulative(),
                bf(pellet_order ~ group_size + pellet_bites + rankformula + 
                     (1 + rankformula | group_size) + (1|group) + (1|group:fish_id), 
                   autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
                prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                          set_prior("normal(0, 1)", class = "b")),
                iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
                control = list(adapt_delta = 0.98, max_treedepth = 15))

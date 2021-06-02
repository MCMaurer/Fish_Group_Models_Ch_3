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

dpof <- d %>% 
  filter(!is.na(pellet_order) & pellet_order > 0) %>% 
  filter(!is.na(novel_order) & novel_order > 0) %>% 
  mutate(first = novel_order == 1,
         pellet_order = as.integer(pellet_order))

dpof

nov_first <- brm(data = dpof, family = bernoulli,
                   bf(first ~ weight + rankformula + mo(pellet_order) + (1 | group_size/group/fish_id)),
                   prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                             set_prior("normal(0, 1)", class = "b")),
                   iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
                   control = list(adapt_delta = 0.999, max_treedepth = 15))

nov_first %>% write_rds("fitted_models/nov_first.rds")

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

dpo2 <- dpo %>% 
  filter(group_size == 2) %>% 
  mutate(first = pellet_order == 1)

dpo2

model_g2 <- brm(data = dpo2, family = bernoulli,
                bf(first ~ weight + rankformula + (1|group) + (1|fish_id)),
                prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                          set_prior("normal(0, 1)", class = "b")),
                iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
                control = list(adapt_delta = 0.98, max_treedepth = 15))

saveRDS(model_g2, "fitted_models/pel_ord_gs2_bernoulli.rds")

conditional_effects(model_g2)
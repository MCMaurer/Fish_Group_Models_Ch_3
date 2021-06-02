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

d %>% 
  ggplot(aes(x = trial, y = pellet_order)) +
  geom_line() +
  facet_wrap(vars(fish_id))



d %>% 
  ggplot(aes(x = novel_bites)) +
  geom_histogram() +
  facet_wrap(vars(group_size))

colnames(d)

d %>% 
  filter(!is.na(novel_bites)) %>% 
  select(trial)

d %>% 
  ggplot(aes(x = trial, y = novel_bites)) +
  geom_line() +
  facet_wrap(vars(fish_id))

dpo <- d %>% 
  filter(!is.na(pellet_order) & pellet_order > 0) %>% 
  mutate(pellet_order = as.integer(pellet_order))

class(dpo$pellet_order)

as.factor(dpo$pellet_order)

dpo %>% 
  filter(trial == 1) %>% 
  ggplot(aes(x = pellet_order, y = group, group = interaction(group, trial))) +
  geom_point(aes(color = rankformula)) +
  facet_grid(rows = vars(group_size))

dpo %>% 
  ggplot(aes(x = pellet_order, fill = rankformula)) +
  geom_bar() +
  facet_grid(rows = vars(group_size))

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
model_g2

model_g3 <- brm(data = dpo %>% filter(group_size == 3), family = sratio(link = "cloglog"),
                bf(pellet_order ~ weight + rankformula + (1|group) + (1|fish_id)),
                prior = c(set_prior("normal(1, 1)", class = "Intercept"),
                          set_prior("normal(0, 1)", class = "b")),
                iter = 100000, warmup = 10000, chains = 4, cores = future::availableCores(),
                control = list(adapt_delta = 0.98, max_treedepth = 15))

saveRDS(model_g3, "fitted_models/pel_ord_gs3_sratio.rds")

model_g3c <- brm(data = dpo %>% filter(group_size == 3), family = cumulative(),
                bf(pellet_order ~ weight + rankformula + (1|group) + (1|fish_id)),
                prior = c(set_prior("normal(1, 1)", class = "Intercept"),
                          set_prior("normal(0, 1)", class = "b")),
                iter = 20000, warmup = 2000, chains = 4, cores = future::availableCores(),
                control = list(adapt_delta = 0.98, max_treedepth = 15))

saveRDS(model_g3c, "fitted_models/pel_ord_gs3_cumu.rds")

conditional_effects(model_g3c, categorical = T)

model_g4 <- brm(data = dpo %>% filter(group_size == 4), family = sratio(link = "cloglog"),
                bf(pellet_order ~ weight + rankformula + (1|group) + (1|fish_id)),
                prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                          set_prior("normal(0, 1)", class = "b")),
                iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
                control = list(adapt_delta = 0.98, max_treedepth = 15))

model_g4c <- brm(data = dpo %>% filter(group_size == 4), family = cumulative(),
                bf(pellet_order ~ weight + rankformula + (1|group) + (1|fish_id)),
                prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                          set_prior("normal(0, 1)", class = "b")),
                iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
                control = list(adapt_delta = 0.98, max_treedepth = 15))

saveRDS(model_g4c, "fitted_models/pel_ord_gs4_cumu.rds")

conditional_effects(model_g4c, categorical = T)

dpog <- dpo %>% 
  mutate(group_size = as.factor(group_size))

model_group_nested <- brm(data = dpog, family = cumulative(),
                 bf(pellet_order ~ weight + rankformula + (1 | group_size/group/fish_id)),
                 prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                           set_prior("normal(0, 1)", class = "b")),
                 iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
                 control = list(adapt_delta = 0.98, max_treedepth = 15))

saveRDS(model_group_nested, "fitted_models/pel_ord_gs_nested_cumu.rds")

conditional_effects(model_group_nested, categorical = T)

model_group_nested

model_group_nested_offset <- brm(data = dpo, family = sratio(link = "cloglog"),
                          bf(pellet_order ~ offset(group_size) + weight + rankformula + (1 | group/fish_id)),
                          prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                                    set_prior("normal(0, 1)", class = "b")),
                          iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
                          control = list(adapt_delta = 0.98, max_treedepth = 15))

conditional_effects(model_group_nested_offset, categorical = T)

library(tidybayes)

get_variables(model_group_nested)

model_group_nested %>% 
  spread_draws(r_group_size[group_size,Intercept]) %>% 
  ggplot(aes(x = group_size, y = r_group_size)) +
  geom_jitter(alpha = 0.2)


conditional_effects(model_group_nested, categorical = T)

model2 <- brm(data = dpo, family = sratio(link = "cloglog"),
              bf(pellet_order ~ offset(group_size) + pellet_bites + rankformula + (1|group) + (1|group:fish_id), 
                 autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                        set_prior("normal(0, 1)", class = "b")),
              iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
              control = list(adapt_delta = 0.98, max_treedepth = 15))

# TODO: could you nest fish_id within group within group_size?
# TODO: might just need separate models for each group size

conditional_effects(model2, categorical = T)

model2

# would this work? (1 | group_size / group / fish_id)



loo(model, model2)

model %>% 
  write_rds("fitted_models/pel_ord_sratio.rds")

model3 <- brm(data = dpo, family = cumulative(),
              bf(pellet_order ~ offset(group_size) + pellet_bites + rankformula + (1|group) + (1|group:fish_id), 
                 autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                        set_prior("normal(0, 1)", class = "b")),
              iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
              control = list(adapt_delta = 0.98, max_treedepth = 15))

model3

model3 %>% 
  write_rds("fitted_models/pel_ord_cumu_gsoffset_ar.rds")

model3 <- read_rds("fitted_models/pel_ord_cumu_gsoffset_ar.rds")

model3.1 <- brm(data = dpo, family = cumulative(),
              bf(pellet_order ~ offset(log(group_size)) + pellet_bites + rankformula + (1|group) + (1|group:fish_id), 
                 autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                        set_prior("normal(0, 1)", class = "b")),
              iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
              control = list(adapt_delta = 0.98, max_treedepth = 15))

model3.2 <- brm(data = dpo, family = cumulative(),
                bf(pellet_order ~ group_size + pellet_bites + rankformula + (1|group) + (1|group:fish_id), 
                   autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
                prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                          set_prior("normal(0, 1)", class = "b")),
                iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
                control = list(adapt_delta = 0.98, max_treedepth = 15))

model3.3 <- brm(data = dpo, family = cumulative(),
                bf(pellet_order ~ exp(group_size) + pellet_bites + rankformula + (1|group) + (1|group:fish_id), 
                   autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
                prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                          set_prior("normal(0, 1)", class = "b")),
                iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
                control = list(adapt_delta = 0.98, max_treedepth = 15))


model3 <- add_criterion(model3, criterion = "waic")
model3.1 <- add_criterion(model3.1, criterion = "waic")
model3.2 <- add_criterion(model3.2, criterion = "waic")
model3.3 <- add_criterion(model3.3, criterion = "waic")

loo_compare(model3, model3.1, model3.2, model3.3, criterion = "waic")
model_weights(model3, model3.1, model3.2, model3.3, weights = "waic") %>% round(3)


rstan:::is.stanfit(model3$fit)

waic(pel_ord_gs_nested_cumu, pel_ord_sratio)

pp_check(pel_ord_sratio)
pp_check(pel_ord_gs_nested_cumu)
pp_check(model3)

model4 <- brm(data = dpo, family = cumulative(),
              bf(pellet_order ~ offset(group_size) + pellet_bites + rankformula + (1|group) + (1|group:fish_id)),
              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                        set_prior("normal(0, 1)", class = "b")),
              iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
              control = list(adapt_delta = 0.98, max_treedepth = 15))

model5 <- brm(data = dpo, family = sratio(link = "cloglog"),
              bf(pellet_order ~ offset(group_size) + pellet_bites + rankformula + (1|group) + (1|group:fish_id)),
              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                        set_prior("normal(0, 1)", class = "b")),
              iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
              control = list(adapt_delta = 0.98, max_treedepth = 15))

model6 <- brm(data = dpo, family = cumulative(),
              bf(pellet_order ~ offset(group_size) + pellet_bites + rankformula + (1|group_size/group/fish_id)),
              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                        set_prior("normal(0, 1)", class = "b")),
              iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
              control = list(adapt_delta = 0.98, max_treedepth = 15))

model7 <- brm(data = dpo, family = cumulative(),
              bf(pellet_order ~  pellet_bites + rankformula + (1|group_size/group/fish_id)),
              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                        set_prior("normal(0, 1)", class = "b")),
              iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
              control = list(adapt_delta = 0.98, max_treedepth = 15))

waic(model3, model4, model5, model6, model7)

model8 <- brm(data = dpo, family = cumulative(),
              bf(pellet_order ~ group_size + pellet_bites + rankformula + (1|group/fish_id)),
              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                        set_prior("normal(0, 1)", class = "b")),
              iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
              control = list(adapt_delta = 0.98, max_treedepth = 15))

waic(model3, model4, model8)

# choices: cumulative or sratio, offset(group_size) or not, group_size as nesting effect or not, autocorrelation or not

# seems like it should be cumulative, use offset(group_size), and autocorrelation



model9 <- brm(data = dpo, family = cumulative(),
                          bf(pellet_order ~ pellet_bites + rankformula + 
                               (1 + rankformula + pellet_bites | group_size) + (1|group) + 1(group:fish_id), 
                             autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
                          prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                                    set_prior("normal(0, 1)", class = "b")),
                          iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
                          control = list(adapt_delta = 0.98, max_treedepth = 15))
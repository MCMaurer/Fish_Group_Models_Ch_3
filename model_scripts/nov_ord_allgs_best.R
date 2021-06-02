library(brms)
library(tidyverse)

d <- readRDS("data/cleaned_data/ch_3_data_cleaned.rds")

dpo <- d %>% 
  filter(!is.na(novel_order) & novel_order > 0) %>% 
  mutate(novel_order = as.integer(novel_order)) %>% 
  filter(!is.na(pellet_order) & pellet_order > 0) %>% 
  mutate(pellet_order = as.integer(pellet_order))

dpof <- d %>% 
  filter(!is.na(novel_order) & novel_order > 0) %>% 
  mutate(novel_order_f = factor(novel_order, levels = c("1", "2", "3", "4"), ordered = T),
         novel_order = as.integer(novel_order)) %>% 
  filter(!is.na(pellet_order) & pellet_order > 0) %>% 
  mutate(pellet_order_f = factor(pellet_order, levels = c("1", "2", "3", "4"), ordered = T),
         pellet_order = as.integer(pellet_order))

dpof %>% 
  select(novel_order, novel_order_f)

dpof %>% 
  ggplot(aes(x = novel_order)) +
  geom_bar()

levels(dpof$novel_order_f)
levels(dpof$pellet_order_f)

?cumulative()

dpof$novel_order_f
as.numeric(dpof$novel_order_f)


model <- brm(data = dpo, family = cumulative(),
                bf(novel_order ~ group_size + pellet_bites + pellet_order + rankformula + (1|group) + (1|group:fish_id), 
                   autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
                prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                          set_prior("normal(0, 1)", class = "b")),
                iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
                control = list(adapt_delta = 0.98, max_treedepth = 15))

pp_check(model)
conditional_effects(model, categorical = T)

model2 <- brm(data = dpof, family = cumulative(),
             bf(novel_order_f ~ group_size + pellet_bites + pellet_order_f + rankformula + (1|group) + (1|group:fish_id), 
                autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
             prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                       set_prior("normal(0, 1)", class = "b")),
             iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
             control = list(adapt_delta = 0.98, max_treedepth = 15))

pp_check(model2)
conditional_effects(model2, categorical = T)

waic(model, model2)

pp_check(model, nsamples = 100)
pp_check(model2, nsamples = 100)

loo(model, model2)

model3 <- brm(data = dpof, family = cumulative(),
              bf(novel_order ~ group_size + pellet_bites + pellet_order_f + rankformula + (1|group) + (1|group:fish_id), 
                 autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                        set_prior("normal(0, 1)", class = "b")),
              iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
              control = list(adapt_delta = 0.98, max_treedepth = 15))

model4 <- brm(data = dpof, family = cumulative(),
             bf(novel_order ~ group_size + pellet_bites + pellet_order + rankformula + (1|group) + (1|group:fish_id), 
                autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
             prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                       set_prior("normal(0, 1)", class = "b")),
             iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
             control = list(adapt_delta = 0.98, max_treedepth = 15))

pp_check(model3, nsamples = 100)
pp_check(model4, nsamples = 100)

model_weights(model3, model4, weights = "waic") %>% round(3)

waic(model3, model4)

conditional_effects(model4, categorical = T)
conditional_effects(model3, categorical = T)

model3 %>% write_rds("fitted_models/nov_ord_allgs_best.rds")

model5 <- brm(data = dpof, family = cumulative(),
              bf(novel_order ~ group_size + pellet_bites + mo(pellet_order) + rankformula + (1|group) + (1|group:fish_id), 
                 autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                        set_prior("normal(0, 1)", class = "b")),
              iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
              control = list(adapt_delta = 0.9999, max_treedepth = 15))

model5 %>% write_rds("fitted_models/nov_ord_allgs_best.rds")

waic(model3, model4, model5)
model_weights(model3, model4, model5, weights = "waic") %>% round(3)

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

model <- brm(data = d, family = poisson,
             bf(novel_bites ~ rankformula + group_size + weight + (1|group) + (1|fish_id), 
                autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
             prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                       set_prior("normal(0, 1)", class = "b"),
                       set_prior("cauchy(0, 2)", class = "sd")),
             iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
             control = list(adapt_delta = 0.98, max_treedepth = 15))

model

conditional_effects(model)

launch_shinystan(model)

model %>% 
  write_rds("fitted_models/nov_bite_pois_ar.rds")


model2 <- brm(data = d, family = poisson,
             bf(novel_bites ~ rankformula + trial + weight + (1|group) + (1|fish_id), 
                autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
             prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                       set_prior("normal(0, 1)", class = "b"),
                       set_prior("cauchy(0, 2)", class = "sd")),
             iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
             control = list(adapt_delta = 0.98, max_treedepth = 15))

model2 %>% 
  write_rds("fitted_models/nov_bite_pois_trial_ar.rds")

conditional_effects(model2)

waic(nov_bite_pois_ar, model2)

model3 <- brm(data = d, family = poisson,
              bf(novel_bites ~ rankformula + trial + group_size + weight + (1|group) + (1|fish_id), 
                 autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                        set_prior("normal(0, 1)", class = "b"),
                        set_prior("cauchy(0, 2)", class = "sd")),
              iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
              control = list(adapt_delta = 0.98, max_treedepth = 15))

model3 %>% 
  write_rds("fitted_models/nov_bite_pois_trial_groupsize_ar.rds")

model3 <- read_rds("fitted_models/nov_bite_pois_trial_groupsize_ar.rds")


model4 <- brm(data = d, family = poisson,
              bf(novel_bites ~ offset(group_size) + pellet_bites + rankformula + trial + weight + (1|group) + (1|fish_id), 
                 autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                        set_prior("normal(0, 1)", class = "b"),
                        set_prior("cauchy(0, 2)", class = "sd")),
              iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
              control = list(adapt_delta = 0.98, max_treedepth = 15))

model5 <- brm(data = d, family = poisson,
              bf(novel_bites ~ offset(log(group_size)) + pellet_bites + 
                   rankformula + trial + weight + (1|group) + (1|fish_id), 
                 autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                        set_prior("normal(0, 1)", class = "b"),
                        set_prior("cauchy(0, 2)", class = "sd")),
              iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
              control = list(adapt_delta = 0.98, max_treedepth = 15))

model6 <- brm(data = d, family = negbinomial,
              bf(novel_bites ~ offset(log(group_size)) + pellet_bites + 
                   rankformula + trial + weight + (1|group) + (1|fish_id), 
                 autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                        set_prior("normal(0, 1)", class = "b"),
                        set_prior("cauchy(0, 2)", class = "sd")),
              iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
              control = list(adapt_delta = 0.999, max_treedepth = 15))

# model5 %>% 
#   write_rds("fitted_models/nov_bite_pois_best_ar.rds")


model3 <- add_criterion(model3, c("waic", "loo"))
model4 <- add_criterion(model4, c("waic", "loo"))
model5 <- add_criterion(model5, c("waic", "loo"))
model6 <- add_criterion(model6, c("waic", "loo"))


loo_compare(model3, model4, model5, model6, criterion = "waic")

model_weights(model3, model4, model5, model6, weights = "waic") %>% 
  round(digits = 3)

loo(model4, model5)





conditional_effects(model3)
conditional_effects(model3, spaghetti = T)

conditional_effects(model4)
conditional_effects(model4, spaghetti = T)

model4

waic(nov_bite_pois_ar, model3)

loo(nov_bite_pois_ar, model3)

df <- d %>% 
  mutate(trial_f = factor(trial))

model5.1 <- brm(data = df, family = poisson,
              bf(novel_bites ~ offset(log(group_size)) + pellet_bites + 
                   rankformula + trial_f + weight + (1|group) + (1|fish_id), 
                 autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                        set_prior("normal(0, 1)", class = "b"),
                        set_prior("cauchy(0, 2)", class = "sd")),
              iter = 5000, warmup = 1000, chains = 4, cores = future::availableCores(),
              control = list(adapt_delta = 0.999, max_treedepth = 15))

model5.2 <- brm(data = d, family = zero_inflated_poisson,
                bf(novel_bites ~ offset(log(group_size)) + pellet_bites + 
                     rankformula + factor(trial) + weight + (1|group) + (1|fish_id), 
                   autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
                prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                          set_prior("normal(0, 1)", class = "b"),
                          set_prior("cauchy(0, 2)", class = "sd")),
                iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
                control = list(adapt_delta = 0.98, max_treedepth = 15))

waic(nov_bites, model5.1, model5.2)
model_weights(nov_bites, model5.1, model5.2, weights = "loo") %>% round(3)

# model5.1 %>% 
#   write_rds("fitted_models/nov_bite_pois_best_ar.rds")

model5.1.1 <- brm(data = df, family = poisson,
                bf(novel_bites ~ group_size + pellet_bites + 
                     rankformula + trial_f + weight + (1|group) + (1|fish_id), 
                   autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
                prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                          set_prior("normal(0, 1)", class = "b"),
                          set_prior("cauchy(0, 2)", class = "sd")),
                iter = 5000, warmup = 1000, chains = 4, cores = future::availableCores(),
                control = list(adapt_delta = 0.999, max_treedepth = 15))

model_weights(model5.1, model5.1.1, weights = "loo") %>% round(3)

model5.1.2 <- brm(data = df, family = poisson,
                  bf(novel_bites ~ group_size + mo(novel_order) + pellet_bites + 
                       rankformula + trial_f + weight + (1|group) + (1|fish_id), 
                     autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
                  prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                            set_prior("normal(0, 1)", class = "b"),
                            set_prior("cauchy(0, 2)", class = "sd")),
                  iter = 5000, warmup = 1000, chains = 4, cores = future::availableCores(),
                  control = list(adapt_delta = 0.999, max_treedepth = 15))

model5.1.2 %>% write_rds("fitted_models/nov_bite_pois_best_ar.rds")

model5.1.2.1 <- brm(data = df, family = poisson,
                  bf(novel_bites ~ group_size + mo(novel_order) + pellet_bites + 
                       rankformula + trial_f + weight + (1|group) + (1|fish_id) + (1|tank), 
                     autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
                  prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                            set_prior("normal(0, 1)", class = "b"),
                            set_prior("cauchy(0, 2)", class = "sd")),
                  iter = 5000, warmup = 1000, chains = 4, cores = future::availableCores(),
                  control = list(adapt_delta = 0.999, max_treedepth = 15))


model5.1.3 <- brm(data = df, family = poisson,
                  bf(novel_bites ~ offset(log(group_size)) + group_size + mo(novel_order) + pellet_bites + 
                       rankformula + trial_f + weight + (1|group) + (1|fish_id), 
                     autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
                  prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                            set_prior("normal(0, 1)", class = "b"),
                            set_prior("cauchy(0, 2)", class = "sd")),
                  iter = 5000, warmup = 1000, chains = 4, cores = future::availableCores(),
                  control = list(adapt_delta = 0.999, max_treedepth = 15))

model_weights(model5.1, model5.1.1, model5.1.2, model5.1.2.1, weights = "waic") %>% round(3)

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
  ggplot(aes(x = pellet_bites)) +
  geom_histogram() +
  facet_wrap(vars(group_size))

colnames(d)

du <- d %>% 
  rownames_to_column() %>% 
  select(rowname, fish_id, trial) %>% 
  unique()

d %>% 
  group_by(fish_id, trial) %>% 
  tally() %>% 
  arrange(desc(n))

model <- brm(data = d, family = zero_inflated_negbinomial,
             bf(pellet_bites ~ rankformula + group_size + weight + (1|group) + (1|fish_id), 
                autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
             prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                       set_prior("normal(0, 1)", class = "b"),
                       set_prior("cauchy(0, 2)", class = "sd")),
             iter = 5000, warmup = 1000, chains = 3, cores = future::availableCores(),
             control = list(adapt_delta = 0.98, max_treedepth = 15))
model %>% 
  write_rds("fitted_models/pel_bite_zinb_ar.rds")

model2 <- brm(data = d, family = zero_inflated_negbinomial,
             bf(pellet_bites ~ rankformula + offset(log(group_size)) + weight + (1|group) + (1|fish_id), 
                autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
             prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                       set_prior("normal(0, 1)", class = "b"),
                       set_prior("cauchy(0, 2)", class = "sd")),
             iter = 5000, warmup = 1000, chains = 3, cores = future::availableCores(),
             control = list(adapt_delta = 0.98, max_treedepth = 15))

pel_bite_zinb_ar <- read_rds("fitted_models/pel_bite_zinb_ar.rds")

pel_bite_zinb_ar <- add_criterion(pel_bite_zinb_ar, criterion = "waic")
model2 <- add_criterion(model2, criterion = "waic")

model_weights(pel_bite_zinb_ar, model2, weights = "waic") %>% round(3)

model2 %>% write_rds("fitted_models/pel_bite_zinb_gsoffset_ar.rds")

model3 <- brm(data = df, family = poisson,
                  bf(pellet_bites ~ group_size + mo(pellet_order) + 
                       rankformula + weight + (1|group) + (1|fish_id), 
                     autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
                  prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                            set_prior("normal(0, 1)", class = "b"),
                            set_prior("cauchy(0, 2)", class = "sd")),
                  iter = 5000, warmup = 1000, chains = 3, cores = future::availableCores(),
                  control = list(adapt_delta = 0.999, max_treedepth = 15))



model3.1 <- brm(data = df, family = zero_inflated_negbinomial,
              bf(pellet_bites ~ group_size + mo(pellet_order) + 
                   rankformula + weight + (1|group) + (1|fish_id), 
                 autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                        set_prior("normal(0, 1)", class = "b"),
                        set_prior("cauchy(0, 2)", class = "sd")),
              iter = 5000, warmup = 1000, chains = 3, cores = future::availableCores(),
              control = list(adapt_delta = 0.999, max_treedepth = 15))

df

model3.2 <- brm(data = df %>% filter(pellet_order > 0), family = zero_inflated_poisson,
                bf(pellet_bites ~ group_size + mo(pellet_order) + 
                     rankformula + weight + (1|group) + (1|fish_id), 
                   autocor = ~ ar(time = trial, gr = fish_id, p = 1, cov = T)),
                prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                          set_prior("normal(0, 1)", class = "b"),
                          set_prior("cauchy(0, 2)", class = "sd")),
                iter = 5000, warmup = 1000, chains = 3, cores = future::availableCores(),
                control = list(adapt_delta = 0.999, max_treedepth = 15))

model3.2 %>% write_rds("fitted_models/pel_bite_pois_best_ar.rds")

model_weights(pel_bites, model3, model3.1, model3.2, weights = "loo") %>% round(3)

library(emmeans)
library(tidybayes)

pel_bite_zinb_ar %>% 
  emmeans(~ rankformula) %>% 
  gather_emmeans_draws() %>% 
  ggplot(aes(x = rankformula, y = .value)) +
  stat_lineribbon()

pel_bite_zinb_ar %>% 
  emmeans(~ rankformula) %>% 
  gather_emmeans_draws(draws = 200) %>% 
  ggplot(aes(x = rankformula, y = .value, group = .draw)) +
  geom_line(alpha = 0.01)

# BEST ONE
pel_bite_zinb_ar %>% 
  emmeans(~ rankformula) %>% 
  contrast(method = "pairwise") %>% 
  gather_emmeans_draws() %>% 
  filter(!str_detect(contrast, ".*Unk.*")) %>% 
  ggplot(aes(y = contrast, x = .value)) +
  geom_halfeyeh(.width = c(0.025, 0.975)) +
  theme_minimal() +
  xlab("Mean Difference in Pellet Bites") +
  ylab("Contrasts Between Dominance Ranks")

conditional_effects(pel_bite_zinb_ar)

pel_bite_zinb_ar %>% 
  emmeans(~ rankformula) %>% 
  contrast(method = "pairwise") %>% 
  gather_emmeans_draws() %>% 
  median_qi(.width = c(0.025, 0.975)) %>% 
  ggplot(aes(y = contrast, x = .value, xmin = .lower, xmax = .upper)) +
  geom_pointintervalh() +
  theme_minimal()

pel_bite_zinb_ar %>% 
  emmeans(~ rankformula) %>% 
  contrast(method = "pairwise") %>% 
  gather_emmeans_draws() %>% 
  ggplot(aes(x = .value)) +
  geom_histogram() +
  facet_wrap(vars(contrast)) +
  theme_minimal()

  gather_emmeans_draws(draws = 200) %>% 
  ggplot(aes(x = rankformula, y = .value, group = .draw)) +
  geom_line(alpha = 0.01)

pel_bite_zinb_ar %>%
  get_variables()
  spread_draws(`b_rankformula.*`, pellet_bites, regex = TRUE) %>%
  compare_levels(pellet_bites, by = rankformula) %>%
  ggplot(aes(y = rankformula, x = pellet_bites)) +
  stat_halfeye()

?compare_levels

brms:::conditional_effects.brmsfit

hypoth
  
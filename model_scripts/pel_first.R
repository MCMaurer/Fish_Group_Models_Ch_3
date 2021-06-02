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
  mutate(first = pellet_order == 1)

model_first <- brm(data = dpof, family = bernoulli,
                bf(first ~ weight + rankformula + (1 | group_size/group/fish_id)),
                prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                          set_prior("normal(0, 1)", class = "b")),
                iter = 10000, warmup = 3000, chains = 4, cores = future::availableCores(),
                control = list(adapt_delta = 0.999, max_treedepth = 15))

model_first %>% write_rds("fitted_models/pel_first.rds")

conditional_effects(model_first)

# ok so the interpretation of these odds ratios
model_first %>% 
  emmeans(~ rankformula, type = "response") %>% 
  contrast(method = "pairwise") %>% 
  gather_emmeans_draws() %>% 
  mutate(.value = exp(.value)) %>% # this puts them back onto the odds ratio instead of log.odds ratio scale
  filter(!str_detect(contrast, ".*Unk.*")) %>% 
  mutate(contrast = recode(contrast, "Dom - Sub" = "Dom / Sub", "Mid - Sub" = "Mid / Sub", "Dom - Mid" = "Dom / Mid")) %>% 
  mutate(contrast = factor(contrast, levels = c("Dom / Sub", "Mid / Sub", "Dom / Mid"))) %>% 
  ggplot(aes(y = contrast, x = .value)) +
  geom_halfeyeh(.width = 0.95) +
  geom_vline(xintercept = 1, lty = 2, alpha = 0.3) +
  theme_minimal() +
  xlab("Odds ratio of eating first") +
  ylab("Contrasts between dominance ranks")

ggsave("figures/pel_first_contrasts.jpg")

model_first %>% 
  emmeans(~ rankformula) %>% 
  contrast(method = "revpairwise") %>% 
  gather_emmeans_draws() %>% 
  mutate(.value = exp(.value)) %>% # this puts them back onto the odds ratio instead of log.odds ratio scale
  filter(!str_detect(contrast, ".*Unk.*")) %>% 
  mutate(contrast = factor(contrast, levels = c("Mid - Dom", "Sub - Dom", "Sub - Mid"))) %>% 
  ggplot(aes(y = contrast, x = .value)) +
  geom_halfeyeh(.width = c(0.025, 0.975)) +
  geom_vline(xintercept = 0, lty = 2, alpha = 0.3) +
  theme_minimal()

model_first %>% 
  emmeans(~ rankformula) %>% 
  gather_emmeans_draws() %>% 
  median_qi()

model_first %>% 
  emmeans(~ rankformula) %>% 
  contrast(method = "pairwise") %>% 
  gather_emmeans_draws() %>% 
  mutate(.value = inv_logit_scaled(.value)) %>% 
  filter(!str_detect(contrast, ".*Unk.*")) %>% 
  mutate(contrast = factor(contrast, levels = c("Dom - Sub", "Mid - Sub", "Dom - Mid"))) %>% 
  ggplot(aes(y = contrast, x = .value)) +
  geom_halfeyeh(.width = c(0.025, 0.975)) +
  geom_vline(xintercept = 0, lty = 2, alpha = 0.3) +
  theme_minimal()


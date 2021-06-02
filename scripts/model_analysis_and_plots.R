library(brms)
library(tidyverse)
library(emmeans)
library(tidybayes)
source("plot_and_icc_functions.R")
theme_set(MCMsBasics::minimal_ggplot_theme())

# SUPER IMPORTANT
# TODO: the bite models should NOT use group_size as an offset! bites are measured at the individual level, it does not make sense to model them as per-capita!!







# model import ------------------------------------------------------------

# TODO: refit pel_bites and nov_bites models using pel_ord and nov_ord as predictors, and remove bites from ord models. 
# TODO: eventually should also refit bites models using group_size as a predictor in addition to using it as an offset. so that would mean we care about the effect of group size on per-capita bite rate
pel_bites <- read_rds("fitted_models/pel_bite_pois_best_ar.rds")
nov_bites <- read_rds("fitted_models/nov_bite_pois_best_ar.rds")
# pel_ord <- read_rds("fitted_models/pel_ord_allgs_cumu_best.rds")
# nov_ord <- read_rds("fitted_models/nov_ord_allgs_best.rds")
pel_first <- read_rds("fitted_models/pel_first.rds")
nov_first <- read_rds("fitted_models/nov_first.rds")

# posterior predictive checks ---------------------------------------------
pp_check(pel_bites)
pp_check(nov_bites)
pp_check(pel_first)
pp_check(nov_first)

# conditional effects plots -----------------------------------------------
pbce <- conditional_effects(pel_bites)
pbce
pbces <- conditional_effects(pel_bites, spaghetti = T, nsamples = 400)
pbces

pbce$pellet_order %>% 
  ggplot(aes(x = pellet_order, y = estimate__)) +
  geom_line() +
  geom_ribbon(aes(ymax = upper__, ymin = lower__), alpha = 0.2) +
  xlab("Feeding order") +
  ylab("Bites taken")


nbce <- conditional_effects(nov_bites)
nbce
nbces <- conditional_effects(nov_bites, spaghetti = T, nsamples = 400)
nbces

nbce$novel_order %>% 
  ggplot(aes(x = novel_order, y = estimate__)) +
  geom_line() +
  geom_ribbon(aes(ymax = upper__, ymin = lower__), alpha = 0.2) +
  xlab("Feeding order") +
  ylab("Bites taken")

pfce <- conditional_effects(pel_first)
pfce
pfces <- conditional_effects(pel_first, spaghetti = T, nsamples = 400)
pfces

nfce <- conditional_effects(nov_first)
nfce
nfces <- conditional_effects(nov_first, spaghetti = T, nsamples = 400)
nfces

# contrast plots ----------------------------------------------------------

pel_em <- pel_bites %>% 
  emmeans(~ rankformula) %>% 
  contrast(method = "pairwise") %>% 
  gather_emmeans_draws() %>% 
  mutate(.value = exp(.value)) %>% # this puts them back onto the odds ratio instead of log.odds ratio scale
  filter(!str_detect(contrast, ".*Unk.*"))

nov_em <- nov_bites %>% 
  emmeans(~ rankformula) %>% 
  contrast(method = "pairwise") %>% 
  gather_emmeans_draws() %>% 
  mutate(.value = exp(.value)) %>% # this puts them back onto the odds ratio instead of log.odds ratio scale
  filter(!str_detect(contrast, ".*Unk.*"))

pelnov_em <- bind_rows(list(Known = pel_em, Novel = nov_em), .id = "food")

pelnov_em %>% 
  mutate(contrast = recode(contrast, "Dom - Sub" = "Dom / Sub", "Mid - Sub" = "Mid / Sub", "Dom - Mid" = "Dom / Mid")) %>% 
  mutate(contrast = factor(contrast, levels = c("Dom / Sub", "Mid / Sub", "Dom / Mid"))) %>% 
  group_by(contrast, food) %>% 
  mutate(`q2.5` = quantile(.value, probs = 0.025),
         `q97.5` = quantile(.value, probs = 0.975),
         touch_1 = (`q2.5` <= 1 & `q97.5` >= 1)) %>% 
  ggplot(aes(y = contrast, x = .value, color = touch_1)) +
  stat_halfeyeh(.width = 0.95) +
  geom_vline(xintercept = 1, lty = 2, alpha = 0.3) +
  theme_minimal() +
  xlab("Ratio of bites taken") +
  ylab("Contrasts between dominance ranks") +
  facet_wrap(vars(food)) +
  coord_cartesian(xlim = c(0.5, 1.75)) +
  theme(text = element_text(size = 14), strip.text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.spacing = unit(0.3, "inches"), legend.position = "none") +
  scale_x_continuous(breaks = c(0.5, 1, 1.5)) +
  scale_color_manual("95% CI\noverlaps 1", values = c("springgreen4", "red"))

ggsave("figures/pel_nov_bites_emmeans_contrasts.jpg", width = 5, height = 5)

pf_em <- pel_first %>% 
  emmeans(~ rankformula, type = "response") %>% 
  contrast(method = "pairwise") %>% 
  gather_emmeans_draws() %>% 
  mutate(.value = exp(.value)) %>% # this puts them back onto the odds ratio instead of log.odds ratio scale
  filter(!str_detect(contrast, ".*Unk.*")) %>% 
  mutate(contrast = recode(contrast, "Dom - Sub" = "Dom / Sub", "Mid - Sub" = "Mid / Sub", "Dom - Mid" = "Dom / Mid")) %>% 
  mutate(contrast = factor(contrast, levels = c("Dom / Sub", "Mid / Sub", "Dom / Mid"))) 

nf_em <- nov_first %>% 
  emmeans(~ rankformula, type = "response") %>% 
  contrast(method = "pairwise") %>% 
  gather_emmeans_draws() %>% 
  mutate(.value = exp(.value)) %>% # this puts them back onto the odds ratio instead of log.odds ratio scale
  filter(!str_detect(contrast, ".*Unk.*")) %>% 
  mutate(contrast = recode(contrast, "Dom - Sub" = "Dom / Sub", "Mid - Sub" = "Mid / Sub", "Dom - Mid" = "Dom / Mid")) %>% 
  mutate(contrast = factor(contrast, levels = c("Dom / Sub", "Mid / Sub", "Dom / Mid"))) 

pnf_em <- bind_rows(list(Known = pf_em, Novel = nf_em), .id = "food")

pnf_em %>% 
  mutate(contrast = recode(contrast, "Dom - Sub" = "Dom / Sub", "Mid - Sub" = "Mid / Sub", "Dom - Mid" = "Dom / Mid")) %>% 
  mutate(contrast = factor(contrast, levels = c("Dom / Sub", "Mid / Sub", "Dom / Mid"))) %>% 
  group_by(contrast, food) %>% 
  mutate(`q2.5` = quantile(.value, probs = 0.025),
         `q97.5` = quantile(.value, probs = 0.975),
         touch_1 = (`q2.5` <= 1 & `q97.5` >= 1)) %>% 
  ggplot(aes(y = contrast, x = .value, color = touch_1)) +
  stat_halfeyeh(.width = 0.95) +
  geom_vline(xintercept = 1, lty = 2, alpha = 0.3) +
  theme_minimal() +
  xlab("Odds ratio of eating first") +
  ylab("Contrasts between dominance ranks") +
  facet_wrap(vars(food)) +
  coord_cartesian(xlim = c(0, 5)) +
  theme(text = element_text(size = 14), strip.text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.spacing = unit(0.3, "inches"), legend.position = "none") +
  scale_color_manual("95% CI\noverlaps 1", values = c("springgreen4", "red"))

ggsave("figures/pel_nov_first_emmeans_contrasts.jpg", width = 5, height = 5)

nov_bites %>% 
  emmeans(~ trial_f) %>%
  contrast(method = "pairwise") %>% 
  gather_emmeans_draws() %>%
  mutate(.value = exp(.value)) %>%  # this puts them back onto the odds ratio instead of log.odds ratio scale
  mutate(contrast = str_replace(contrast, "-", "/")) %>% 
  mutate(contrast = str_replace(contrast, "6", "Shrimp")) %>% 
  mutate(contrast = str_replace(contrast, "7", "Glass")) %>% 
  mutate(contrast = str_replace(contrast, "8", "Wood")) %>% 
  mutate(contrast = str_replace(contrast, "9", "Virgin Plastic")) %>% 
  mutate(contrast = str_replace(contrast, "10", "Biofouled Plastic")) %>% 
  group_by(contrast) %>% 
  mutate(`q2.5` = quantile(.value, probs = 0.025),
         `q97.5` = quantile(.value, probs = 0.975),
         touch_1 = (`q2.5` <= 1 & `q97.5` >= 1)) %>% 
  ggplot(aes(y = contrast, x = .value, color = touch_1)) +
  stat_halfeyeh(.width = 0.95) +
  geom_vline(xintercept = 1, lty = 2, alpha = 0.3) +
  theme_minimal() +
  xlab("Ratio of bites taken") +
  ylab("Contrasts between trials") +
  coord_cartesian(xlim = c(0, 4)) +
  theme(text = element_text(size = 14), strip.text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.spacing = unit(0.3, "inches"), legend.position = "none") +
  scale_color_manual("95% CI\noverlaps 1", values = c("springgreen4", "red"))

ggsave("figures/nov_bite_foodtypes_emmeans_contrasts.jpg", width = 5, height = 5)
  

# parameter plots ---------------------------------------------------------

# TODO: get rid of Intercept, rankformula betas here
pel_bites %>%
  gather_draws(`b_.*`, regex = TRUE) %>%
  filter(.variable == "b_weight" | .variable == "b_group_size") %>% 
  ggplot(aes(y = .variable, x = .value)) +
  geom_halfeyeh() +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.7)


# TODO: get rid of Intercept, rankformula betas here
nov_bites %>%
  gather_draws(`b_.*`, regex = TRUE) %>%
  filter(.variable %in% c("b_weight", "b_group_size", "b_pellet_bites")) %>% 
  ggplot(aes(y = .variable, x = .value)) +
  geom_halfeyeh() +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.7)

# TODO: read this https://kevinstadler.github.io/blog/bayesian-ordinal-regression-with-random-effects-using-brms/ to figure out what's up with the 3 intercepts. maybe this too https://web.archive.org/web/20170714110045/https://cran.r-project.org/web/packages/ordinal/vignettes/clm_intro.pdf

pel_first %>%
  gather_draws(`b_.*`, regex = TRUE) %>%
  ggplot(aes(y = .variable, x = .value)) +
  geom_halfeyeh() +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.7)

nov_first %>%
  gather_draws(`b_.*`, `sim.*`, regex = TRUE) %>%
  ggplot(aes(y = .variable, x = .value)) +
  geom_halfeyeh() +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.7)

nov_first %>% get_variables()

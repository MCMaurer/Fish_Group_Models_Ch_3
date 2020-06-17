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

d <- d %>% 
  group_by(group, trial, date, tank, observer, group_size) %>% 
  summarise(chase_sum = sum(chase))

d %>% 
  ggplot(aes(x = chase_sum)) +
  geom_histogram()

d %>% 
  ggplot(aes(x = trial, y = chase_sum, group = group)) +
  geom_line() +
  stat_smooth(geom='line', alpha=0.75, color = "red", se=FALSE, method = "lm") +
  facet_wrap(vars(group))

# TODO: looks like there aren't really temporal trends going on here, I think the trials are more or less independent, but it's worth checking for any temporal autocorrelation here

# TODO: read up on the offset function here and make sure I'm using it correctly
model <- brm(data = d, family = zero_inflated_negbinomial,
             bf(chase_sum ~ offset(log(group_size)) + group_size + trial),
             prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                       set_prior("normal(0, 1)", class = "b")),
             iter = 5000, warmup = 1000, chains = 3, cores = future::availableCores(),
             control = list(adapt_delta = 0.98, max_treedepth = 15))

marginal_effects(model)

conditional_effects(model)

launch_shinystan(model)

waic(typ_int, model)

# save model
saveRDS(model, "")

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

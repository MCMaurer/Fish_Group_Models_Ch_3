library(tidyverse)
library(readxl)
if(!("devtools" %in% installed.packages())){
  install.packages("devtools")
}
if(!("MCMsBasics" %in% installed.packages())){
  devtools::install_github("MCMaurer/MCMsBasics")
}
library(MCMsBasics)

theme_set(minimal_ggplot_theme())

d <- read_xlsx("data/raw_data/Ch 3 Data_Updated rank IDS (changing unk to dom).xlsx") %>% 
  nice_names()

d <- d %>% 
  mutate(novel_bites = as.numeric(novel_bites),
         novel_order = as.numeric(novel_order),
         group = as.character(group),
         tank = as.character(tank)) %>% 
  select(group, fish_id, group_size, chase, chased, in_refuge, rankformula, pellet_bites, pellet_order, novel_bites, novel_order, trial, weight, length, observer, tank, date, time)

d %>% 
  filter(fish_id == "12_BY")

# TODO: check on why this fish ID was duplicated
d$fish_id[d$fish_id == "12_BY" & d$group_size == 4] <- "12_BY_2"

d %>% 
  filter(fish_id == "12_BY")

d %>% write_rds("data/cleaned_data/ch_3_data_cleaned.rds")

d %>% ggskim()

d %>% .$fish_id %>% unique() %>% length()

d %>% .$group %>% unique() %>% length()

d %>% 
  ggplot(aes(x = trial, y = rankformula, group = fish_id, color = as.factor(group_size))) +
  geom_line() +
  facet_wrap(vars(fish_id)) +
  minimal_ggplot_theme(gridlines = T) +
  theme(legend.position = "bottom")

d %>% 
  ggplot(aes(x = chase, y = chased)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(method = "lm") +
  scale_x_quantile(val = d$chase) +
  scale_y_quantile(val = d$chased)


d %>% 
  group_by(group, date) %>% 
  summarise(group_size = max(group_size),
            chase = sum(chase)/group_size) %>% 
  ggplot(aes(x = group_size, y = chase, group = group_size)) +
  geom_violin()

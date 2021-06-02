# To Do

## List of Questions/Models

1. Are there differences in overall aggression between groups of different sizes?
    - total group chases per day ~ group size + trial + (1|group_ID)
2. Is there individual consistency in total aggression?
    - individual chases per day ~ length + weight + trial + group_size
3. Is there consistency in individual representation in overall group aggression? Ie is there consistency in ind_chases / group_chases per day?
4. **THIS IS PRIORITY** Is food consumption determined by dominance rank?
    - pellet_order ~ rankformula + group_size + weight + (1|group) + (1|fish_id)
    - pellet_bites ~ rankformula +
    - novel_order ~ rankformula +
    - novel_bites ~ rankformula +
5. Is there an imbalance within groups in terms of chases and bites? Like, do 1 or 2 individuals do the majority of the chases or take the majority of bites? Need to check this against
6. Do fish reach some stable point in the group hierarchy? You could model this by looking at each fish's ranks at each time point, and calculating whether they change between time points. Whether or not they change is now the response variable, and you look at that over time and between group sizes. You could even try to look at whether they move up or down. You could ask what their previous rank was and whether that had an effect. Like, are you less likely to move from dominant than from submissive or middle?
        - could be binomial, either change or not, or could be categorical (+1, 0, -1)
6. Use the dominant individual's bite rate as a predictor of the bite rate of the other individuals- do the other group members feed more if the dominant feeds more?
7. pel_bites as a predictor for nov_bites on the days that nov_bites are measured. same for order

**issue**: might need to look at bite order *within* group sizes, since order is inherently tied to group size- you cannot be the 4th to eat in a group of 2 or 3

## For ABS

List of priorities:

1. pel_bites
    - pel_bite_zinb_ar
2. nov_bites, maybe use pel_bites as a pred
    - nov_bite_pois_trial_groupsize_ar
    - could refit this one with pel_bites as pred
3. pel_order
4. nov_order, maybe use pel_order as a pred
5. group variation in pel_bites, predicted by mean_weight, per_cap_chases, group_size
6. group variation in nov_bites, same preds


- Could add group_size as a predictor to bite models, even with offset
- could







# From Files

#### via todo.fish

./model_scripts/group_chases_offset_zinb.R:31:# TODO: looks like there aren't really temporal trends going on here, I think the trials are more or less independent, but it's worth checking for any temporal autocorrelation here 

./model_scripts/group_chases_offset_zinb.R:33:# TODO: read up on the offset function here and make sure I'm using it correctly 

./model_scripts/pel_ord_models.R:164:# TODO: could you nest fish_id within group within group_size? 

./model_scripts/pel_ord_models.R:165:# TODO: might just need separate models for each group size 

./scripts/data_cleaning.R:28:# TODO: check on why this fish ID was duplicated 

./scripts/model_analysis_and_plots.R:9:# TODO: the bite models should NOT use group_size as an offset! bites are measured at the individual level, it does not make sense to model them as per-capita!! 

./scripts/model_analysis_and_plots.R:19:# TODO: refit pel_bites and nov_bites models using pel_ord and nov_ord as predictors, and remove bites from ord models.  

./scripts/model_analysis_and_plots.R:20:# TODO: eventually should also refit bites models using group_size as a predictor in addition to using it as an offset. so that would mean we care about the effect of group size on per-capita bite rate 

./scripts/model_analysis_and_plots.R:155:# TODO: get rid of Intercept, rankformula betas here 

./scripts/model_analysis_and_plots.R:163:# TODO: get rid of Intercept, rankformula betas here 

./scripts/model_analysis_and_plots.R:170:# TODO: read this https://kevinstadler.github.io/blog/bayesian-ordinal-regression-with-random-effects-using-brms/ to figure out what's up with the 3 intercepts. maybe this too https://web.archive.org/web/20170714110045/https://cran.r-project.org/web/packages/ordinal/vignettes/clm_intro.pdf 


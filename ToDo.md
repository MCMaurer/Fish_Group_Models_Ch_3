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




# From Files

#### via todo.fish

./model_scripts/group_chases_offset_zinb.R:31:# TODO: looks like there aren't really temporal trends going on here, I think the trials are more or less independent, but it's worth checking for any temporal autocorrelation here 

./model_scripts/group_chases_offset_zinb.R:33:# TODO: read up on the offset function here and make sure I'm using it correctly 


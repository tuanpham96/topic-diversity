topic-diversity

Some current notes

- To run simulation scripts in the `scripts` folder (`scripts/sim-*` files), move to main folder first, this is because they were written to be run from main folder, so the data file paths in the `params_df` are saved as `data/XXX/data-001234.rds` instead of `../data/XXX/data-001234.rds`. Then they were moved to the `scripts` folder for organization purposes. This does need improvement in the future. 

- Watch as new files come in (move to the intended `data/XXX` folder)

``` bash
watch -n30 --color 'ls -l data* | wc -l ; exa -lhrn@ -snew --no-user --no-permissions' # with number of files on top
watch -n 60 'exa -lhrn@ -snew --no-user --no-permissions' # only watch files 
```

- For update functions:
- `update_bipartite_topicagent` and `update_learnt_topic` are quite manual and slow but rid off duplicates and already learnt subjects
- `update_via_matmul` is much faster but the beta route doesn't choose a friend of aoi before hand 
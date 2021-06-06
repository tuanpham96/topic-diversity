Watch as new files come in 

``` bash
watch -n30 --color 'ls -l data* | wc -l ; exa -lhrn@ -snew --no-user --no-permissions'
watch -n 60 'exa -lhrn@ -snew --no-user --no-permissions'
```

For update functions:
- `update_bipartite_topicagent` and `update_learnt_topic` are quite manual and slow but rid off duplicates and already learnt subjects
- `update_via_matmul` is much faster but the beta route doesn't choose a friend of aoi before hand 
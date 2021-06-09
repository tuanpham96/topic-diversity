topic-diversity

General description:
- `src`: 
  - `topic_discovery.R`: definitions for `TopicDiscovery` class (initialization and simulation)
  - `topic_analysis.R`: functions for running diversity analysis and time analysis 
- `scripts`:
  - `rmd2rscript.R`: convert `Rmd` files in this folder to `R` scripts to run non-interactively 
  - `sim-block.R[md]`: simulation with **block/group** models (i.e. using `sample_sbm`)
  - `sim-nonblock.R[md]`: simulation with **nonblock** models (i.e. using `sample_[pa,gnp,smallworld]`)
  - `analyze-diversity-through-time.R[md]`: topic diversity time analysis scripts 
  
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

\begin{align}

P &= 
{\color{blue}
\alpha \psi\left(\left[\left[T\tau\right]_{\star} - \tau \right]_{\star}\right)} + 
{\color{red}
\beta \psi\left(\left[\left[\tau A\right]_{\star} - \tau \right]_{\star}\right)}

\\

\tau(t+1) &\leftarrow \tau(t) + \mathrm{sample}(P)

\\

 
\left[x\right]_{\star} &= 1 \text{ if } x > 0, 0 \text{ otherwise}

\\ 


\psi(X) &\text{ as column norm. for matrix } X
\end{align}


\begin{align}
A &: \text{adj. mat.} \textbf{ agent graph} \\
T &: \text{adj. mat.} \textbf{ topic graph} \\
\tau &: \text{adj. mat.} \textbf{ learnt topics}
\end{align}


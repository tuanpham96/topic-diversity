# Topic discovery and diversity

## General description

- `docs`:
  - [`Pham-CNA2021.pdf`](docs/Pham-CNA2021.pdf) describes the model and results
  - [`CNA21-242_Pham.pdf`](docs/CNA21-242_Pham.pdf) is the presentation file for [CNA21](https://complexnetworks.org/).
  - [`future-considerations.md`](docs/future-considerations.md) describes some future considerations and directions for the models and analyses
- `feedbacks` contains all the comments and feedbacks received from multiple people (listed in the [report's acknowledgement section](docs/Pham-CNA2021.pdf))
- `src`:
  - `topic_discovery.R`: definitions for `TopicDiscovery` class (initialization and simulation)
  - `topic_analysis.R`: functions for running diversity analysis and time analysis
- `scripts`:
  - `rmd2rscript.R`: convert `Rmd` files in this folder to `R` scripts to run non-interactively
  - `sim-block.R[md]`: simulation with **block/group** models (i.e. using `sample_sbm`)
  - `sim-nonblock.R[md]`: simulation with **nonblock** models (i.e. using `sample_[pa,gnp,smallworld]`)
  - `analyze-diversity-through-time.R[md]`: topic diversity time analysis scripts
  - `vis-demo.Rmd`: generate some demo plots about model parameters
  - `vis-analysis.Rmd`: analysis plots for all simulations
- `svproc/cnasubm`: `latex` files to generate CNA21 submission, along with necessary figure and bib files. The final document is also in [`docs/Pham-CNA2021.pdf`](docs/Pham-CNA2021.pdf).
- `data`: the data and analyses generated from scripts can be downloaded at <https://doi.org/10.6084/m9.figshare.17086832.v1>

## Notes

- To run simulation/visualization scripts in the `scripts` folder (`scripts/sim-*` files), move to main folder first, this is because they were written to be run from main folder, so the data file paths in the `params_df` are saved as `data/XXX/data-001234.rds` instead of `../data/XXX/data-001234.rds`. Then they were moved to the `scripts` folder for organization purposes. This does need improvement in the future.

- For update functions:
  - `update_bipartite_topicagent` and `update_learnt_topic` are quite manual and slow but rid off duplicates and already learnt subjects
  - `update_via_matmul` is much faster but the beta route doesn't choose a friend of aoi before hand

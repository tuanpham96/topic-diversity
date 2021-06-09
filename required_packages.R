required_pkgs <- c(
  'tidyverse',
  'reshape2',
  'purrr',
  'bipartite',
  'igraph',
  'pbmcapply',
  'gtools',
  'magrittr',
  'progress',
  'BiocManager',
  'moments',
  'jacpop',
  'rlist',
  'latex2exp', # may not really need
  'svglite', # may not really need
  'ggformula' # may not really need
)

 
install.packages(required_pkgs)

# Additional packages 
BiocManager::install("microbiome")

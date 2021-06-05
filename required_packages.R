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
  'jacpop'
)
 
install.packages(required_pkgs)

# Additional packages 
BiocManager::install("microbiome")

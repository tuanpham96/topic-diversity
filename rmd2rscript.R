# convert R markdown files to R scripts in order to run straight on non-interactive terminal 
# this is needed especially to preserve memory by not running GUI; and also to run on server if needed 
knitr::purl('sim-nonblock.Rmd', 'sim-nonblock.R')
knitr::purl('sim-block.Rmd', 'sim-block.R')
knitr::purl('analyze-diversity-through-time.Rmd', 'analyze-diversity-through-time.R')

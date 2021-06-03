## ----message=F---------------------------------------------------------------------------------------------------
library(tidyverse)
library(reshape2)
library(purrr)

library(bipartite)
library(igraph)

library(pbmcapply)
library(magrittr)
source('src/topic_discovery.R')

set.seed(1019) # for reproducibility of plotting + cluster assigments 


## ----------------------------------------------------------------------------------------------------------------
num_blocks <- 10 # for both 
num_trials <- 10
data_path <- 'data/block-models'
dir.create(data_path, showWarnings = F)
nworkers <- round(parallel::detectCores()/2)


## ----------------------------------------------------------------------------------------------------------------
intra_generators <- list(
  SBM_1 = partial(make_sbmgen_partial,num_blocks=num_blocks,p_within=0.1,p_between=0.001),
  SBM_2 = partial(make_sbmgen_partial,num_blocks=num_blocks,p_within=0.1,p_between=0.004),
  SBM_3 = partial(make_sbmgen_partial,num_blocks=num_blocks,p_within=0.1,p_between=0.016),
  SBM_4 = partial(make_sbmgen_partial,num_blocks=num_blocks,p_within=0.1,p_between=0.064)
)

inter_initiators <- list(
  GROUP_1 = list(type="groups", params=list(p_same = 0.1)),
  GROUP_2 = list(type="groups", params=list(p_same = 0.2)),
  GROUP_3 = list(type="groups", params=list(p_same = 0.4)),
  GROUP_4 = list(type="groups", params=list(p_same = 0.8))
)

groupvec_generator <- list(
  agent = partial(make_groupvec, num_blocks = num_blocks),
  topic = partial(make_groupvec, num_blocks = num_blocks)
)

all_intra_gens <- names(intra_generators)
all_inter_ints <- names(inter_initiators)


## ----------------------------------------------------------------------------------------------------------------
# create paramdf 
param_df <- expand_grid(
  n_t         = 1000, 
  n_a         = 200,
  tau_0       = 5,
  p_alpha     = c(0.1, 0.3, 0.5, 0.7, 0.9), 
  tau_max     = 100,  
  model_agent = all_intra_gens,
  model_topic = all_intra_gens,
  inter_init  = all_inter_ints,
  trial_id    = 1:num_trials
) 

param_df$ind <- 1:nrow(param_df)
param_df <- param_df %>% mutate(
  data_file = file.path(data_path, sprintf('data-%06d.rds', ind)),
  proc_file = file.path(data_path, sprintf('proc-%06d.rds', ind))
)

param_df

# save parameters 
sim_params_config <- list(
  params = param_df,
  intra_generators = intra_generators, 
  inter_initiators = inter_initiators
)
  
saveRDS(sim_params_config, file.path(data_path, 'params.rds'))




## ----------------------------------------------------------------------------------------------------------------
cur_data <- list.files(path = data_path, pattern = 'data.*\\.rds$') %>% c
cur_ind <- regmatches(cur_data, gregexpr("[[:digit:]]+", cur_data)) %>% as.numeric()
remainder_ind <- setdiff(param_df$ind, cur_ind)
# remainder_ind
# length(remainder_ind)


## ----------------------------------------------------------------------------------------------------------------
# indlist_to_run <- remainder_ind # when interrupted or restart
indlist_to_run <- param_df$ind


## ----------------------------------------------------------------------------------------------------------------
run_model_each <- function(comb_ind) {
  sim_param <- param_df[comb_ind,]
  data_file_name <- sim_param$data_file
  
  model_gen <- list(
    agent = intra_generators[[sim_param$model_agent]],
    topic = intra_generators[[sim_param$model_topic]]
  )
  
  td <- TopicDiscovery$new(
    n_a          = sim_param$n_a,
    n_t          = sim_param$n_t,
    p_alpha      = sim_param$p_alpha,
    tau_max      = sim_param$tau_max,
    tau_0        = sim_param$tau_0,
    mdl_gen      = model_gen,
    groups       = groupvec_generator, 
    inter_init   = inter_initiators[[sim_param$inter_init]],
    info         = sim_param,
    colors       = F
  )

  
  td$update_via_matmul(show_progress = F)
  td$save_to_file(data_file_name)
}

pbmclapply(indlist_to_run, 
           run_model_each,
           mc.cores = nworkers, 
           mc.set.seed = TRUE,
           mc.preschedule	= TRUE,
           ignore.interactive = TRUE)



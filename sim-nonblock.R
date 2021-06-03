## ----message=F---------------------------------------------------------------------------------------------------
library(tidyverse)
library(reshape2)
library(purrr)

library(bipartite)
library(igraph)

library(pbmcapply)
library(gtools)

library(magrittr)
source('src/topic_discovery.R')

set.seed(284) # for reproducibility of plotting + cluster assigments 


## ----------------------------------------------------------------------------------------------------------------
num_trials <- 10
data_path <- 'data/nonblock-models'
dir.create(data_path, showWarnings = F)
nworkers <- round(parallel::detectCores()/2)


## ----------------------------------------------------------------------------------------------------------------
intra_generators <- list(
  PA_1 = partial(sample_pa,m=2,power=0.80,directed=F),
  PA_2 = partial(sample_pa,m=2,power=1.00,directed=F),
  PA_3 = partial(sample_pa,m=2,power=1.20,directed=F),
  ER_1 = partial(sample_gnp,p=0.010,directed=F), 
  ER_2 = partial(sample_gnp,p=0.050,directed=F), 
  ER_3 = partial(sample_gnp,p=0.100,directed=F), 
  WS_1 = partial(sample_smallworld,dim=1,nei=5,p=0.001),
  WS_2 = partial(sample_smallworld,dim=1,nei=5,p=0.004),
  WS_3 = partial(sample_smallworld,dim=1,nei=5,p=0.160),
  WS_4 = partial(sample_smallworld,dim=1,nei=5,p=0.640)
)

inter_initiators <- list(
  SOFTMAX_1 = list(type="softmax", params=list(beta_softmax = -0.2)),
  SOFTMAX_2 = list(type="softmax", params=list(beta_softmax = 0)),
  SOFTMAX_3 = list(type="softmax", params=list(beta_softmax = 0.2))
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
  tau_max     = c(25, 50, 100),  
  model_agent = all_intra_gens,
  model_topic = all_intra_gens,
  inter_init  = all_inter_ints,
  trial_id    = 1:num_trials
) %>% filter(model_agent == model_topic) 

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



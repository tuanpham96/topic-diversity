library(tidyverse)
library(reshape2)
library(purrr)
library(bipartite)
library(igraph)
library(pbmcapply)

source('src/topic_discovery.R')

set.seed(3327) # for reproducibility of plotting + cluster assigments 

# define general 
n_t <- 1000
n_a <- 200
tau_0 <- 2
setdiff_peropt <- TRUE

data_path <- 'data/test'


# only run the models of the same kind 
model_generation_lists = list(
  pa_0 = partial(sample_pa,power=0.50,directed=F),
  pa_1 = partial(sample_pa,power=1.00,directed=F)
)

all_modelgens <- names(model_generation_lists)

# create parameters data frame
param_df <- expand_grid(
  p_alpha = c(0.1, 0.5, 0.9), 
  tau_max = c(200), 
  model_agent = all_modelgens,
  model_topic = all_modelgens,
  num_trials = 1:5
) %>% filter(model_agent == model_topic)
param_df$ind <- 1:nrow(param_df)
param_df

# save parameters 
run_params_config <- list(
  general = list(n_t = n_t, n_a = n_a, tau_0 = tau_0, setdiff_peropt = setdiff_peropt),
  params = param_df,
  models = model_generation_lists)

saveRDS(run_params_config, file.path(data_path, 'params.RDS'))



# run one model 
run_model_each <- function(comb_ind) {
  data_file_name <- file.path(data_path, sprintf('data-%06d.RDS', comb_ind))
  model_gen <- list(
    agent = model_generation_lists[[param_df[comb_ind,]$model_agent]],
    topic = model_generation_lists[[param_df[comb_ind,]$model_topic]])
  td <- TopicDiscovery$new(
    n_a,
    n_t,
    param_df[comb_ind,]$p_alpha,
    tau_max = param_df[comb_ind, ]$tau_max,
    tau_0 = tau_0,
    mdl_gen = model_gen
  )
  td$update_via_matmul(show_progress = F)
  td$save_to_file(data_file_name)
}

# run parallel 
indlist_to_run <- param_df$ind
pbmclapply(indlist_to_run, run_model_each,
         mc.cores = 4, 
         mc.set.seed = TRUE,
         mc.preschedule	= TRUE)






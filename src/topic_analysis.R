library(tidyverse)
library(reshape2)
library(bipartite)
library(igraph)

analysis_through_time <- function(td, samp_time = -1,
                                  analysis_function, analysis_arguments = list(),
                                  bind_info = TRUE) {
  # samp_time: sampling time 
  #   (`samp_time < 1`) or (`samp_time >= td$max_nsteps`): only use `t_onset = {0, td$max_nsteps}`
  #   otherwise subsampled using the given `samp_time` using`seq(from = 0, to = td$max_nsteps, by = samp_time)`
  
  label_list <- list()
  
  # parse sampling time 
  if ((samp_time < 1) | (samp_time >= td$max_nsteps)) {
    time_vec <- c(0, td$max_nsteps)
  }
  else {
    time_vec <- seq(from = 0, to = td$max_nsteps, by = samp_time)
  }

  # perform analysis by filtering the "onset" field in the edges data frames of the graph `td$G`
  time_analysis_df <- lapply(time_vec, function(t_on) {
    td_sub <- td$clone(deep = TRUE)
    
    td_sub$G <- graph.data.frame(
      as_data_frame(td_sub$G, 'edges') %>% filter(onset <= t_on),
      directed = FALSE,
      vertices = as_data_frame(td_sub$G, 'vertices')
    )
    
    div_metric_sub <- do.call(analysis_function, c(list(td = td_sub), analysis_arguments))
    
    if (length(label_list) == 0) {
      label_list <-  div_metric_sub$labels
    }
    
    return(div_metric_sub$values %>% as_tibble() %>% mutate(t = t_on))
    
  }) %>% bind_rows()

  
  if (bind_info) {
    time_analysis_df <- time_analysis_df %>% bind_cols(td$info)
  }
  
  return(list(df = time_analysis_df, label = label_list))
}


analyze_diversity <- function(td,
                              nrep_robustness = 5, 
                              valuetype_robustness = 'degree') {
  
  calculate_entropy <- purrr::partial(vegan::diversity, index='shannon',  base = 2)
  
  # get adjmat and distmat 
  all_mats <- td$get_separate_matrices()
  topic_distmat <- all_mats$T %>% 
    graph_from_adjacency_matrix(mode='undirected') %>% 
    distances(mode = 'all')
  topic_distmat[is.infinite(topic_distmat)] <- NA # so don't consider Inf values 
  
  # agent degree in agent graph
  agent_degree <- colSums(td$get_separate_matrices()$A)
  
  if (valuetype_robustness == 'degree') {
    robustness_valuevec <- agent_degree
  } else {
    stop('Currently only support `valuetype_robustness = "degree"` as removal value for robustness calculation')
  }
  
  # node data frames 
  node_df <- as_data_frame(td$G, 'vertices')
  topic_nodedf <- node_df %>% filter(role == 'topics') %>% 
    rename(topic_group = group) %>% 
    select(name,topic_group)
  agent_nodedf <- node_df %>% filter(role == 'agents') %>% 
    rename(agent_group = group) %>%
    select(name,agent_group)
  
  has_group <- !(topic_nodedf$topic_group[1] %>% is.na)
  
  # learnt topics data frame
  learnt_topics <- all_mats$TAU %>% melt %>% 
    filter(value > 0) %>%
    rename(topic=Var1, agent=Var2) %>%
    select(topic, agent) %>% 
    left_join(topic_nodedf, by = c("topic" = "name")) %>% 
    left_join(agent_nodedf, by = c("agent" = "name"))
  
  summarized_by_topics <- learnt_topics %>% 
    select(topic, topic_group) %>% group_by(topic, topic_group) %>%
    summarise(count = length(topic))
  
  # create lists
  label_list <- list()
  value_list <- list()
  
  # number of topics 
  label_list$N_T <- 'number of topics' 
  value_list$N_T <- nrow(summarized_by_topics)
  
  # topic population entropy 
  label_list$H_p <- 'topic population entropy'
  value_list$H_p <- calculate_entropy(summarized_by_topics$count)
  
  # topic group population entropy
  label_list$H_gp <- 'topic group population entropy'
  value_list$H_gp <- NA
  
  if (has_group) {
    summarized_by_topicgroups <- summarized_by_topics %>%
      group_by(topic_group) %>% 
      summarise(count = sum(count)) 
    value_list$H_gp <- calculate_entropy(summarized_by_topicgroups$count)
  } 
  
  # topic-agent group population joint entropy
  label_list$H_gg <- 'topic-agent group population joint entropy'
  value_list$H_gg <- NA
  
  if (has_group) {
    topicgroup_agentgroup <- learnt_topics %>% 
      select(topic_group, agent_group) %>%
      group_by(topic_group, agent_group) %>% 
      summarise(count = n())
    value_list$H_gg <- calculate_entropy(topicgroup_agentgroup$count)
  } 
  
  # mean topic group individual entropy 
  label_list$H_gi <- 'mean topic group individual-agent entropy'
  value_list$H_gi <- NA
  
  if (has_group) {
    peragent_topicgroupentropy <- learnt_topics %>% 
      group_by(agent, topic_group) %>%
      summarise(count = n()) %>% 
      group_by(agent) %>% 
      summarise(ent = calculate_entropy(count))
    
    value_list$H_gi <- mean(peragent_topicgroupentropy$ent, na.rm = TRUE)
  } 
  
  # evenness 
  label_list$E_T <- 'topic Camargo evenness'
  value_list$E_T <- microbiome::evenness(summarized_by_topics$count)$camargo
  
  # rarity 
  label_list$LMS_T <- 'topic log-modulo-skewness (rare index)'
  value_list$LMS_T <- microbiome::log_modulo_skewness(summarized_by_topics$count)
  
  # robustness 
  # note: `TAU [topic x agent]`, and `second.extinct` uses `web [lower x higher]` so remove `agent = higher` 
  #   random removal 
  label_list$R_T_random <- 'topic robustness (random removal of agent)'
  value_list$R_T_random <- calculate_robustness(all_mats$TAU, type = 'random', nrep = nrep_robustness)
  
  #   targeted removal by degree
  label_list$R_T_deghigh <- 'topic robustness (removal of agent with highest degree in agent graph first)'
  value_list$R_T_deghigh <- calculate_robustness(all_mats$TAU, type = 'remove-most-first',
                                                values = agent_degree, nrep = nrep_robustness)
  
  label_list$R_T_deglow <- 'topic robustness (removal of agent with lowest degree in agent graph first)'
  value_list$R_T_deglow <- calculate_robustness(all_mats$TAU, type = 'remove-least-first',
                                                 values = agent_degree, nrep = nrep_robustness)

  # subgraph statistics 
  # d_g: mean distance of the distance matrix subsampled from the topic list
  # d_s: mean distance of the induced subgraph from the topic list
  # c_s_glob: global transitivity/clustering coefficient from induced subgraph 
  # c_s_avr: average transitivity/clustering coefficient from induced subgraph 
  # n_cc: number of connected component
  
  substat_prefixes <- c(
    d_g = 'mean distance from topic graph distance matrix from subsampled topics ',
    d_s = 'mean distance of the induced topic subgraph',
    c_s_glob = 'global clustering coefficient from induced topic subgraph',
    c_s_avr = 'average clustering coefficient from induced topic subgraph',
    n_cc = 'number of connected component of induced topic subgraph'
  )
  
  substat_suffixes <- c(
    mean = 'average', 
    med = 'median', 
    sd = 'standard deviation', 
    skew = 'skewness'
  )
  
  substat_namedesc <- expand_grid(prefix = names(substat_prefixes), 
                                  suffix = names(substat_suffixes)) %>%
    mutate(label = sprintf('%s_%s', prefix, suffix), 
           description = sprintf('%s (%s)', substat_prefixes[prefix], substat_suffixes[suffix])) %>% 
    select(label, description) %>% 
    spread(label, description) %>% 
    as.list()
  
  label_list <- c(label_list, substat_namedesc)
  
  substats_values <-
    group_by(learnt_topics, agent) %>%
    summarise(
      d_g = mean(topic_distmat[topic, topic], na.rm = TRUE),
      d_s = induced_subgraph(td$G, topic) %>% mean_distance(directed=FALSE,unconnected=TRUE),   
      c_s_glob = induced_subgraph(td$G, topic) %>% transitivity(type='global'), 
      c_s_avr = induced_subgraph(td$G, topic) %>% transitivity(type='average'),
      n_cc = components(induced_subgraph(td$G, topic))$no
    ) %>%
    select(-agent) %>% 
    summarise_all(list(
      mean=partial(mean, na.rm=TRUE),
      med = partial(median, na.rm = TRUE),
      sd = partial(sd, na.rm = TRUE),
      skew = partial(moments::skewness, na.rm = TRUE))
    ) %>% 
    as.list()
  
  value_list <- c(value_list, substats_values)
  
  # Jaccard similarity list of topics between agents
  label_list$Js_T <- 'mean pairwise Jaccard similarity of topics between agents'
  
  sink("/dev/null") # the following function outputs some messages, best to suppress
  Js_pw <- jacpop::generate_pw_jaccard(t(all_mats$TAU), n.pcs = NULL, plot_it = FALSE)$Jac
  value_list$Js_T <- mean(Js_pw[upper.tri(Js_pw)], na.rm = TRUE)
  sink()
  
  return(list(
    labels = label_list,
    values = value_list
  ))
}


auc_trapez <- function(x, y) {
  # area under the curve of (x,y=f(x)) vectors using trapezoid method
  # in other word AUC = sum(diff(x) * (y(1:end-1) + y(2:end)))/2
  if (length(x) != length(y)) {
    stop('Length of x and y have to be the same')
  }
  
  n <- length(x)
  dx <- x[2:n] - x[1:(n-1)]
  sy <- 0.5*(y[2:n] + y[1:(n-1)])
  return(sum(dx * sy))
} 

cum_ntopics <- function(M, i) {
  # cumulative number of nonzero rows (topics) based on `1:i`
  if (i > 1) {
    return( sum(rowSums(M[,1:i]) > 0) )
  } else if (i == 1) {
    return( sum(M[,1:i] > 0) )
  } else {
    return(0)
  }
} 

calculate_robustness <- function(M, type = 'random', values = NA, nrep = 10) {
  # type: either 'random' or 'remove-most-first' or 'remove-least-first'
  # values: vector indicating "agents" values (for example degree), 
  #         if `type = 'random'` will ignore 
  #         if `type = 'remove-most-first'` will remove the most of the values first 
  #         if `type = 'remove-least-first'` will remove the least of the values first 
  # nrep: number of repetitions for all methods (because values could be have repeating values)
  num_topics <- dim(M)[1]
  num_agents <- dim(M)[2]
  
  removal_type <- ifelse(type=='random', type, 'basedonval')
  if (is.na(values) %>% all %>% not) {
    unq_vals <- switch(
      type, 
      'remove-least-first' = unique(values) %>% sort(decreasing = TRUE), 
      'remove-most-first' = unique(values) %>% sort(decreasing = FALSE),
    )
  } else if ((removal_type == 'basedonval') & length(values) != num_agents) {
    stop('Need a value vector with length as same number of agents (ncol of matrix) for nonrandom types')
  }
  
  auc_robustness <- sapply(1:nrep, function(.) {
    # rand_order_vec: the first element in this vector is removed last
    if (removal_type == 'random') {
      rand_order_vec <- sample(num_agents)
    } else if (removal_type == 'basedonval') {
      rand_order_vec <- lapply(unq_vals, function(u) {
        which(values == u) %>% as.vector() %>% sample
      }) %>% unlist
    } 
    
    M_randorder <- M[,rand_order_vec]
    rem_ntopics <- sapply(0:num_agents, function(i){ cum_ntopics(M_randorder, i) })
    vec_x <- (0:num_agents)/num_agents
    vec_y <- rem_ntopics/num_topics
    return(auc_trapez(vec_x,vec_y))
  }) %>% mean(na.rm = TRUE)
  
  return(auc_robustness)
}



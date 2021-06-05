library(tidyverse)
library(reshape2)
library(bipartite)
library(igraph)

analyze_diversity <- function(td, nrep_2ndextinct=5) {
  
  calculate_entropy <- purrr::partial(vegan::diversity, index='shannon',  base = 2)
  
  # get adjmat and distmat 
  all_mats <- td$get_separate_matrices()
  topic_distmat <- all_mats$T %>% 
    graph_from_adjacency_matrix(mode='undirected') %>% 
    distances(mode = 'all')
  topic_distmat[is.infinite(topic_distmat)] <- NA # so don't consider Inf values 
  
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
    
    value_list$H_gi <- mean(peragent_topicgroupentropy$ent)
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
  value_list$R_T_random <- all_mats$TAU %>% 
    bipartite::second.extinct(participant = 'higher', method = 'random', nrep = nrep_2ndextinct) %>% 
    bipartite::robustness()
  
  #   targeted removal by degree
  label_list$R_T_degree <- 'topic robustness (removal of agent by degree rank)'
  value_list$R_T_degree <- all_mats$TAU %>% 
    bipartite::second.extinct(participant = 'higher', method = 'degree', nrep = nrep_2ndextinct) %>% 
    bipartite::robustness()
  
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
    summarise_all(
      list(mean=mean,
           med=median,
           sd=sd,
           skew=moments::skewness)
      ) %>% 
    as.list()
  
  value_list <- c(value_list, substats_values)
  
  # Jaccard similarity list of topics between agents
  label_list$Js_T <- 'mean pairwise Jaccard similarity of topics between agents'
  Js_pw <- jacpop::generate_pw_jaccard(t(all_mats$TAU), n.pcs = NULL, plot_it = FALSE)$Jac
  value_list$Js_T <- mean(Js_pw[upper.tri(Js_pw)], na.rm = TRUE)
  
  return(list(
    labels = label_list,
    values = value_list
  ))
}

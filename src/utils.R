
initialize_unipartite <- function(n_a,n_t,tau_0,
                                  mdl_gen=list(
                                    agent = purrr::partial(sample_pa,power=1,m=1,directed=F),
                                    topic = purrr::partial(sample_pa,power=1,m=1,directed=F)
                                  ),
                                  colors=list(
                                    agent_vert = rgb(1,0,0,0.8), topic_vert = rgb(0.1,0.2,0.9,0.8), 
                                    agent_edge = rgb(1,0,0,0.6), topic_edge = rgb(0.1,0.2,0.9,0.6), 
                                    init_cross = rgb(0,0,0,0.3), new_cross = rgb(0.3,0.7,0.3,0.8)
                                    )
                                  ) {
  g_A <- mdl_gen$agent(n_a) 
  g_T <- mdl_gen$topic(n_t)
  
  V(g_A)$name <- paste("a",1:n_a,sep="")
  V(g_A)$label <- 1:n_a
  V(g_A)$role <- 'agents'
  V(g_A)$role_id <- 1
  
  V(g_T)$name <- paste("t",1:n_t,sep="")
  V(g_T)$label <- 1:n_t
  V(g_T)$role <- 'topics'
  V(g_T)$role_id <- 2
  
  
  E_Tau0 <- data.frame(from=sample(V(g_T)$name, tau_0*n_a,replace=T), 
                       to=rep(V(g_A)$name,tau_0))
  
  V(g_A)$color <- colors$agent_vert
  V(g_T)$color <- colors$topic_vert
  
  E(g_A)$color <- colors$agent_edge
  E(g_T)$color <- colors$topic_edge
  E_Tau0$color <- colors$init_cross

  
  E_G <- rbind(as_data_frame(g_A, 'edges'), as_data_frame(g_T, 'edges'))
  V_G <- rbind(as_data_frame(g_A, 'vertices'), as_data_frame(g_T, 'vertices'))
  V_G$label.cex <- 1
  V_G$node_id <- 1:(n_a+n_t)
  G <- rbind(E_G, E_Tau0) %>% 
    graph.data.frame(vertices=V_G,directed=F)
  E(G)$onset <- 0
  return(G)
}

get_separate_matrices <- function(G) {
  M <- as_adjacency_matrix(G) %>% as.matrix()
  agents_ind <- V(G)$role == "agents"
  A_adj <- M[agents_ind, agents_ind]
  T_adj <- M[!agents_ind, !agents_ind]
  TAU_adj <- M[!agents_ind, agents_ind]
  return(list(A=A_adj,T=T_adj,TAU=TAU_adj))
}

colNorm <- function(A) {
  num_rows <- nrow(A)
  cs_A <- colSums(A) %>% rep(num_rows) %>%
    matrix(nrow=num_rows,byrow=T)
  return(A/cs_A)
}

calc_bipartite_mat <- function(P_adj,A_adj,T_adj,p_alpha,p_beta,
                               n_steps=1,save_every=NA) {
  if (is.na(save_every)) {
    save_every <- n_steps
  } 
  
  P_results <- c()
  for (n in 1:n_steps) {
    P_adj <- p_alpha * colNorm(T_adj %*% P_adj) +
             p_beta * colNorm(P_adj %*% A_adj)
    if (n %% save_every == 0) {
      P_results <- abind::abind(P_results, P_adj)
    }
  }
  
  return(P_results)
}

filter_noderole <- function(obj, sel_role) {
  return(obj[obj$role == sel_role])
}

quicksample <- function(X) {
  if (length(X) > 0) {
    return(sample(X, 1))
  } else {
    return(NA)
  }
}

update_learnt_topics <- function(G, agent_of_interest, 
                                 p_alpha, p_beta, tau_max = NA,
                                 setdiff_peropt = T) {
  topics_of_aoi <- neighbors(G, agent_of_interest) %>% filter_noderole("topics") %>% unique
  num_top_aoi <- length(topics_of_aoi)
  
  if (!is.na(tau_max) & num_top_aoi >= tau_max) {return(NA)}
  
  rnd_num <- runif(1)
  
  if (rnd_num < p_alpha) {
    possible_topics <- neighborhood(G, nodes=topics_of_aoi, order=1, mindist=1) %>%
      lapply(function(.) filter_noderole(., "topics")) %>% 
      lapply(function(.) .$name) %>% unlist 
  } else if (rnd_num <= p_alpha + p_beta) {
    neighs_of_aoi <- neighbors(G, agent_of_interest) %>% filter_noderole("agents") 
    if (length(neighs_of_aoi) == 0) {return(NA)}
    
    possible_topics <- neighbors(G, sample(neighs_of_aoi)) %>% filter_noderole("topics")
    possible_topics <- possible_topics$name  
  } else {
    return(NA)
  }
  
  possible_topics <- unique(possible_topics)
  if (setdiff_peropt) {
    new_topic <- possible_topics %>% setdiff(topics_of_aoi$name) %>% quicksample
  } else {
    new_topic <- possible_topics %>% quicksample %>% setdiff(topics_of_aoi$name)
  }
  
  if (is.null(new_topic) | length(new_topic) == 0) {return(NA)}
  
  return(new_topic)
}

update_bipartite_topicagent <- function(G, max_nsteps, 
                                        p_alpha, p_beta, tau_max,
                                        setdiff_peropt, new_color=NA) {
  pb <- progress::progress_bar$new(
    format = "(:spin) [:bar] :percent [Elapsed: :elapsedfull || ETA: :eta]",
    total = max_nsteps,
    complete = "=",   # Completion bar character
    incomplete = "-", # Incomplete bar character
    current = ">",    # Current bar character
    clear = FALSE,    # If TRUE, clears the bar when finish
    width = 100)      # Width of the progress bar
  
  agents_nodes <- filter_noderole(V(G),"agents")$name
  for (ni in 1:max_nsteps) {
    all_new_topics <- sapply(agents_nodes, function(aoi) {
      update_learnt_topics(G, aoi, p_alpha, p_beta, tau_max, setdiff_peropt)
    })
    
    new_topics_df <- all_new_topics[!is.na(all_new_topics)] %>%
      data.frame() %>% rename(to=".") %>% rownames_to_column(var="from")
    
    edge_seq <- new_topics_df %>% as.matrix() %>% t %>% as.vector()
    G <- add_edges(G, edge_seq, onset=ni, color=new_color)
    
    pb$tick()
  }
  return(G)
}
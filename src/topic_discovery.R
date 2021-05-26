library(R6)
library(tidyverse)
library(igraph)

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

colNorm <- function(A) {
  num_rows <- nrow(A)
  cs_A <- colSums(A) %>% rep(num_rows) %>%
    matrix(nrow=num_rows,byrow=T)
  return(A/cs_A)
}

TopicDiscoveryConfig <- R6Class(
  "TopicDiscoveryConfig",
  portable = FALSE,
  public = list(
    n_t = NULL,
    n_a = NULL,
    tau_max = NULL,
    tau_0 = NULL,
    p_alpha = NULL,
    p_beta = NULL,
    setdiff_peropt = NULL,
    max_nsteps = NULL,
    mdl_gen = NULL,
    colors = NULL,
    initialize = function(n_a,
                          n_t,
                          p_alpha,
                          tau_max,
                          p_beta = NA,
                          tau_0 = 1,
                          setdiff_peropt = TRUE,
                          max_nsteps = NA) {
      self$n_a <- n_a
      self$n_t <- n_t
      
      self$p_alpha <- p_alpha
      self$p_beta <- ifelse(is.na(p_beta), 1 - p_alpha, p_beta)
      
      self$tau_0 <- tau_0
      self$tau_max <- tau_max
      
      self$setdiff_peropt <- setdiff_peropt
      self$max_nsteps <-
        ifelse(is.na(max_nsteps), tau_max * 1.1, max_nsteps)
    },
    initialize_modelgeneration = function(mdl_gen = list(
      agent = purrr::partial(igraph::sample_pa, directed = F),
      topic = purrr::partial(igraph::sample_pa, directed = F)
    )) {
      self$mdl_gen <- mdl_gen
    },
    initialize_colors = function(colors = list(
      agent_vert = rgb(1, 0, 0, 0.8),
      topic_vert = rgb(0.1, 0.2, 0.9, 0.8),
      agent_edge = rgb(1, 0, 0, 0.6),
      topic_edge = rgb(0.1, 0.2, 0.9, 0.6),
      init_cross = rgb(0, 0, 0, 0.3),
      new_cross = rgb(0.3,  0.7, 0.3, 0.8)
    )) {
      self$colors <- colors
    }
  )
)

TopicDiscovery <- R6Class(
  "TopicDiscovery",
  inherit = TopicDiscoveryConfig,
  portable = FALSE,
  public = list(
    G = NULL,
    use_colors = NULL, 
    initialize = function(n_a,
                          n_t,
                          p_alpha,
                          tau_max,
                          p_beta = NA,
                          tau_0 = 1,
                          setdiff_peropt = TRUE,
                          max_nsteps = NA,
                          mdl_gen = NA,
                          colors = NA) {
      super$initialize(n_a,
                       n_t,
                       p_alpha,
                       tau_max,
                       p_beta,
                       tau_0,
                       setdiff_peropt,
                       max_nsteps)
      
      if (is.na(mdl_gen) %>% all) {
        super$initialize_modelgeneration()
      } else {
        super$initialize_modelgeneration(mdl_gen)
      }
      
      if (is.na(colors) %>% all) {
        self$use_colors <- FALSE
      } else {
        self$use_colors <- TRUE
        if (all(colors)) {
          super$initialize_colors()
        } else {
          super$initialize_colors(colors)
        }
      }
      
      initialize_graph()
    },
    initialize_graph = function() {
      g_A <- mdl_gen$agent(n_a)
      g_T <- mdl_gen$topic(n_t)
      
      V(g_A)$name <- paste("a", 1:n_a, sep = "")
      V(g_A)$label <- 1:n_a
      V(g_A)$role <- 'agents'
      V(g_A)$role_id <- 1
      
      V(g_T)$name <- paste("t", 1:n_t, sep = "")
      V(g_T)$label <- 1:n_t
      V(g_T)$role <- 'topics'
      V(g_T)$role_id <- 2
      
      # the first method might have duplicates so switch to the second one instead to maintain still same tau_0
      # E_Tau0 <- data.frame(from = sample(V(g_T)$name, tau_0 * n_a, replace = T), to = rep(V(g_A)$name, tau_0))
      E_Tau0 <- lapply(V(g_A)$name, function(agent_name) {
        list(from = sample(V(g_T)$name, tau_0, replace = F),
             to = rep(agent_name, tau_0))
      }) %>% bind_rows() 
      
      if (use_colors) {
        V(g_A)$color <- colors$agent_vert
        V(g_T)$color <- colors$topic_vert
        
        E(g_A)$color <- colors$agent_edge
        E(g_T)$color <- colors$topic_edge
        E_Tau0$color <- colors$init_cross
      }
      
      E_G <- rbind(igraph::as_data_frame(g_A, 'edges'), igraph::as_data_frame(g_T, 'edges'))
      V_G <- rbind(igraph::as_data_frame(g_A, 'vertices'), igraph::as_data_frame(g_T, 'vertices'))
      
      V_G$label.cex <- 1
      V_G$node_id <- 1:(n_a + n_t)
      G <<- rbind(E_G, E_Tau0) %>% graph.data.frame(vertices = V_G, directed = F)
      E(G)$onset <<- 0
    },
    get_separate_matrices = function() {
      M <- as_adjacency_matrix(G) %>% as.matrix()
      agents_ind <- V(G)$role == "agents"
      A_adj <- M[agents_ind, agents_ind]
      T_adj <- M[!agents_ind, !agents_ind]
      TAU_adj <- M[!agents_ind, agents_ind]
      return(list(A = A_adj, T = T_adj, TAU = TAU_adj))
    },
    update_learnt_topics = function(agent_of_interest) {
      topics_of_aoi <- neighbors(G, agent_of_interest) %>% filter_noderole("topics") %>% unique
      num_top_aoi <- length(topics_of_aoi)
      
      if (num_top_aoi >= tau_max) {
        return(NA)
      }
      
      rnd_num <- runif(1)
      
      if (rnd_num < p_alpha) {
        possible_topics <- neighborhood(G, nodes = topics_of_aoi, order = 1, mindist = 1) %>%
          lapply(function(.) filter_noderole(., "topics")) %>%
          lapply(function(.) .$name) %>% unlist
      } else if (rnd_num <= p_alpha + p_beta) {
        neighs_of_aoi <- neighbors(G, agent_of_interest) %>% filter_noderole("agents")
        if (length(neighs_of_aoi) == 0) {
          return(NA)
        }
        possible_topics <- neighbors(G, sample(neighs_of_aoi)) %>% filter_noderole("topics")
        possible_topics <- possible_topics$name
      } else {
        return(NA)
      }
      
      possible_topics <- unique(possible_topics)
      if (setdiff_peropt) {
        new_topic <-
          possible_topics %>% setdiff(topics_of_aoi$name) %>% quicksample
      } else {
        new_topic <- possible_topics %>% quicksample %>% setdiff(topics_of_aoi$name)
      }
      
      if (is.null(new_topic) | length(new_topic) == 0) {
        return(NA)
      }
      
      return(new_topic)
      
    },
    update_bipartite_topicagent = function(show_progress=TRUE) {
      if (show_progress) { 
        pb <- progress::progress_bar$new(
          format = "(:spin) [:bar] :percent [Elapsed: :elapsedfull || ETA: :eta]",
          total = max_nsteps, complete = "=", incomplete = "-", current = ">",
          clear = FALSE, width = 100
        )
      }
      
      agents_nodes <- filter_noderole(V(G), "agents")$name
      for (ni in 1:max_nsteps) {
        all_new_topics <- sapply(agents_nodes, function(aoi) {
          update_learnt_topics(aoi)
        })
        
        new_topics_df <- all_new_topics[!is.na(all_new_topics)] %>%
          data.frame() %>% rename(to = ".") %>% rownames_to_column(var = "from")
        
        edge_seq <- new_topics_df %>% as.matrix() %>% t %>% as.vector()
        
        if (use_colors) {
          G <<- add_edges(G, edge_seq, onset = ni, color = colors$new_cross)
        } else {
          G <<- add_edges(G, edge_seq, onset = ni)
        }
        
        if (show_progress) {
          pb$tick()
        }
      }
    },
    update_via_matmul = function(show_progress = TRUE) {
      if (show_progress) {
        pb <- progress::progress_bar$new(
          format = "(:spin) [:bar] :percent [Elapsed: :elapsedfull || ETA: :eta]",
          total = max_nsteps,
          complete = "=",
          incomplete = "-",
          current = ">",
          clear = FALSE,
          width = 100
        )
      }
      
      M <- as_adjacency_matrix(G) %>% as.matrix()
      agents_ind <- V(G)$role == "agents"
      A_mat <- M[agents_ind, agents_ind]
      T_mat <- M[!agents_ind, !agents_ind]
      
      
      for (ni in 1:max_nsteps) {
        M <- as_adjacency_matrix(G) %>% as.matrix()
        TAU_mat <- M[!agents_ind, agents_ind]
        
        new_topics_df <-
          p_alpha * colNorm(1 * (((T_mat %*% TAU_mat > 0) - TAU_mat) > 0)) +
          p_beta  * colNorm(1 * (((TAU_mat %*% A_mat > 0) - TAU_mat) > 0))
        
        new_topics_df <- melt(new_topics_df, c("to", "from")) %>%
          rename(prob = value) %>% filter(prob > 0) %>%
          group_by(from) %>% slice_sample(weight_by = prob) %>%
          select(to, from)
        
        edge_seq <-
          new_topics_df %>% as.matrix() %>% t %>% as.vector()
        
        if (use_colors) {
          G <<- add_edges(G, edge_seq, onset = ni, color = colors$new_cross)
        } else {
          G <<- add_edges(G, edge_seq, onset = ni)
        }
        
        if (show_progress) {
          pb$tick()
        }
      }
    },
    save_to_file = function(file_name) {
      saveRDS(self, file=file_name)
    }
  )
)

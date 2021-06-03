library(R6)
library(tidyverse)
library(magrittr)
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

make_sbmgen_partial <- function(num_vertices, num_blocks, p_within, p_between) {
  P <- matrix(rep(p_between,num_blocks^2), num_blocks, num_blocks)
  diag(P) <- p_within
  per_block_size <- round(num_vertices/num_blocks)
  block_sizes <- rep(per_block_size, num_blocks)
  block_sizes[num_blocks] <- num_vertices - (num_blocks - 1) * per_block_size
  return(sample_sbm(num_vertices, pref.matrix=P, block.sizes=block_sizes, directed = FALSE, loops = FALSE))
}

make_groupvec <- function(num_vertices, num_blocks) {
  per_block_size <- round(num_vertices/num_blocks)
  group_vec <- rep(1:num_blocks, rep(per_block_size,num_blocks))
  group_vec <- group_vec[1:num_vertices]
  return(group_vec)
}


TopicDiscoveryConfig <- R6Class(
  "TopicDiscoveryConfig",
  portable = FALSE,
  public = list(
    n_a = NULL,             # number of agents    
    n_t = NULL,             # number of topics            
    tau_max = NULL,         # topic capacity per agent          
    tau_0 = NULL,           # initial number of topics. Set to `1` by default     
    p_alpha = NULL,         # alpha probability, learning through related topics (rabbit hole)     
    p_beta = NULL,          # beta probability, learning through friends' suggestions (recommender). If not set, use `1-p_alpha`    
    setdiff_peropt = NULL,  # setdiff option for update (only for manual update in `TopicDiscovery::update_bipartite_topicagent`, not used in `TopicDiscovery::update_via_matmul`)                      
    max_nsteps = NULL,      # maximum number of steps to update. If not set, use `1.2 * tau_max`              
    mdl_gen = NULL,         # model generation methods for intralayer networks, in form `list(agent = <function>, topic = <function>)`. If not set, use default (see `TopicDiscoveryConfig::initialize_modelgeneration`, and `TopicDiscovery::initialize_intralayer`)         
    groups = NULL,          # group assignments for intralayer networks if needed, in form `list(agent = <vector>, topic = <vector>)`. If not set, use default (see `TopicDiscoveryConfig::initialize_groupassignments`, and `TopicDiscovery::initialize_intralayer`)      
    inter_init = NULL,      # interlayer initialization method, in form `list(type = <string>, params = <list>)`. The `type` could only be "random", "softmax", "groups" for now (see `TopicDiscoveryConfig::initialize_groupassignments`, `TopicDiscovery::initialize_interlayer`)    
    info = NULL,            # further information and description to be passed and saved in the object        
    colors = NULL,          # colors to assign vertices and edges in graphs, either `NA`, `TRUE/FALSE`, or a `list`. See `TopicDiscoveryConfig::initialize_colors` for more details  
    use_colors = NULL,      # whether to use color when initializing `TopicDiscovery` object, see how this is set in `TopicDiscoveryConfig::initialize_colors`
    initialize = function(n_a, n_t, p_alpha, tau_max,
                          p_beta = NA, tau_0 = 1, setdiff_peropt = TRUE, max_nsteps = NA,
                          mdl_gen = NA, groups = NA, inter_init = NA,
                          colors = NA, info = NA) {

      self$n_a <- n_a
      self$n_t <- n_t
      
      self$p_alpha <- p_alpha
      self$p_beta <- ifelse(is.na(p_beta), 1 - p_alpha, p_beta)
      
      self$tau_0 <- tau_0
      self$tau_max <- tau_max
      
      self$setdiff_peropt <- setdiff_peropt
      self$max_nsteps <- ifelse(is.na(max_nsteps), tau_max * 1.2, max_nsteps)
      
      self$initialize_modelgeneration(mdl_gen)
      
      self$initialize_groupassignments(groups)
      
      self$initialize_interlayerinitalization(inter_init)
      
      self$initialize_colors(colors)
      
      self$info <- info
    },
    initialize_setordefault = function(fieldname, value, default) {
      if (is.na(value) %>% all) {
        self[[fieldname]] <- default
      } else {
        self[[fieldname]] <- value
      }
    },
    initialize_modelgeneration = function(val) {
      def_mdlgen <- list(
        agent = purrr::partial(igraph::sample_pa, directed = F),
        topic = purrr::partial(igraph::sample_pa, directed = F)
      )
      initialize_setordefault('mdl_gen', val, def_mdlgen)
    },   
    initialize_interlayerinitalization = function(val) {
      def_interinit <- list(type="random", params=list())
      initialize_setordefault('inter_init', val, def_interinit)
    },
    initialize_groupassignments = function(val) {
      def_groups <- list(agent = NA, topic = NA)
      if (is.na(val) %>% all) {
        self$groups <- def_groups
      } else if (is.list(val)) {
        if (!(setequal(names(val), names(def_groups)))) {
          stop('If `groups` is provided as a list, then need to be form `list(agent = <NA or vector or function>, topic = = <NA or vector or function>)')
        }
        assign_group('agent', val$agent, n_a)
        assign_group('topic', val$topic, n_t)
      } else {
        stop('Invalid input, need to be a list or just `NA`')
      }
    },
    assign_group = function(role, group_val, num_vertices) {
      if (is.na(group_val)) {
        self$groups[[role]] <- NA
      } else if (is.vector(group_val)) {
        if (length(group_val) != num_vertices) {
          stop(sprintf('Invalid group vector input for "%s" as the length of vector is not the same as number of vertices ', role))
        }
        self$groups[[role]] <- group_val
      } else if (is.function(group_val)) {
        self$groups[[role]] <- group_val(num_vertices)
      } else {
        stop(sprintf('Invalid input for "%s", could either be `NA`, a vector or a function', role))
      }
    },
    initialize_colors = function(val) {
      # If `val` is `NA` or `FALSE` then don't use colors, i.e. `use_colors` set to `FALSE`
      # If not set `use_colors = TRUE`, check `val == TRUE` then use `def_colors`
      # If `val` is a list, use `def_colors` for missing values
      def_colors <- list(
        agent_vert = rgb(1.0, 0.0, 0.0, 0.8),
        topic_vert = rgb(0.1, 0.2, 0.9, 0.8),
        agent_edge = rgb(1.0, 0.0, 0.0, 0.6),
        topic_edge = rgb(0.1, 0.2, 0.9, 0.6),
        init_cross = rgb(0.0, 0.0, 0.0, 0.3),
        new_cross  = rgb(0.3, 0.7, 0.3, 0.8)
      )
      
      self$colors <- def_colors
      
      if (is.na(val) %>% all) {
        self$use_colors <- FALSE
      } else {
        if (is.logical(val)) {
          self$use_colors <- val
        } else if (is.list(val)) {
          same_fields <- intersect(names(def_colors), names(val))
          for (fn in same_fields) {
            self$colors[[fn]] <- val[[fn]]
          }
        } else {
          stop('Invalid `colors` input, either `NA`, a list or a logical')
        } 
      }
    }
  )
)

TopicDiscovery <- R6Class(
  "TopicDiscovery",
  inherit = TopicDiscoveryConfig,
  portable = FALSE,
  public = list(
    G = NULL,
    initialize = function(...) {
      super$initialize(...)
      initialize_graph()
    },
    initialize_intralayer = function(mdlgen_fun, num_vertices, prefix_name, role_name, role_number_id,
                                     group_vec = NA, color_vertex = NA, color_edge = NA) {
      g <- mdlgen_fun(num_vertices)
      V(g)$name <- paste(prefix_name, 1:num_vertices, sep = "")
      V(g)$label <- 1:num_vertices
      V(g)$role <- role_name
      V(g)$role_id <- role_number_id
      V(g)$group <- group_vec
      
      if (use_colors) {
        V(g)$color <- color_vertex
        E(g)$color <- color_edge
      }
      return(g)
    }, 
    initialize_interlayer = function(init_gen, agent_graph, topic_graph, edge_color) {
      init_type <- init_gen$type
      init_params <- init_gen$params
      if (is.null(init_params)) {
        init_params <- list()
      }
      
      common_param <- list(agent_graph=agent_graph, topic_graph=topic_graph)
      
      E_Tau0 <- switch(
        init_type,
        "random"  = do.call(init_inter_random,  common_param),
        "softmax" = do.call(init_inter_softmax, c(common_param, init_params)),
        "groups"  = do.call(init_inter_groups,  c(common_param, init_params)),
        stop('The type for interlayer initialization can only be "random", "softmax" or "groups"!')
      )
      if (use_colors) {
        E_Tau0$color <- edge_color
      }
      
      return(E_Tau0)
    },
    init_inter_random  = function(agent_graph, topic_graph) {
      topic_names <- V(topic_graph)$name
      E_Tau0 <- lapply(V(agent_graph)$name, function(agent_name) {
        list(from = sample(topic_names, tau_0, replace = F),
             to = rep(agent_name, tau_0))
      }) %>% bind_rows() 
      return(E_Tau0)
    },
    init_inter_softmax = function(agent_graph, topic_graph, value_func = igraph::degree, beta_softmax = 0) {
      # apply softmax on a certain valued function
      P_select_topic <- data.frame(f = value_func(topic_graph)) %>% 
        rownames_to_column(var = 'topic') %>% 
        mutate(f = exp(beta_softmax * f)) %>% 
        mutate(p = f/sum(f))
      
      E_Tau0 <- lapply(V(agent_graph)$name, function(agent_name) {
        list(from = slice_sample(P_select_topic, n = tau_0, weight_by = p, replace=F)$topic,
             to = rep(agent_name, tau_0))
      }) %>% bind_rows() 
      
      return(E_Tau0)
    },
    init_inter_groups  = function(agent_graph, topic_graph, p_same = 0.5) {
      # basically corresponding groups
      if (is.na(groups) %>% all) {
        stop('The object needs to have groups defined in both agent and topic graphs')
      }
      
      unq_agent_groups <- unique(groups$agent) %>% sort
      unq_topic_groups <- unique(groups$topic) %>% sort
      if (!(identical(unq_agent_groups, unq_topic_groups))) {
        stop('The group ids of agents and topics do not match for initializing with group correspondence')
      }
      
      agent_nodedf <- as_data_frame(agent_graph, 'vertices') %>% select(c('name', 'group'))
      topic_nodedf <- as_data_frame(topic_graph, 'vertices') %>% select(c('name', 'group'))
      
      E_Tau0 <- lapply(agent_nodedf$name, function(agent_name) {
        aoi_group <- agent_nodedf$group[agent_nodedf$name == agent_name]
        
        k_same <- topic_nodedf$group[topic_nodedf$group == aoi_group] %>% length()
        k_diff <- n_t - k_same
        
        p_select_topic <- topic_nodedf %>% 
          mutate(p = if_else(group == aoi_group, 
                             p_same/k_same, 
                             (1-p_same)/k_diff))
        
        return(list(
          from = slice_sample(p_select_topic, n = tau_0, weight_by = p, replace=F)$name,
          to = rep(agent_name, tau_0))
        )
      }) %>% bind_rows()
      
      return(E_Tau0)
    },
    initialize_graph = function() {
      g_A <- initialize_intralayer(
        mdlgen_fun = mdl_gen$agent,
        num_vertices = n_a,
        prefix_name = 'a',
        role_name = 'agents',
        role_number_id = 1,
        group_vec = groups$agent,
        color_vertex = colors$agent_vert,
        color_edge = colors$agent_edge
      )
      
      g_T <- initialize_intralayer(
        mdlgen_fun = mdl_gen$topic,
        num_vertices = n_t,
        prefix_name = 't',
        role_name = 'topics',
        role_number_id = 2,
        group_vec = groups$topic,
        color_vertex = colors$topic_vert,
        color_edge = colors$topic_edge
      )
      
      E_Tau0 <- initialize_interlayer(
        init_gen = inter_init,
        agent_graph = g_A,
        topic_graph = g_T,
        edge_color = colors$init_cross
      )

      E_G <- rbind(igraph::as_data_frame(g_A, 'edges'), igraph::as_data_frame(g_T, 'edges'))
      V_G <- rbind(igraph::as_data_frame(g_A, 'vertices'), igraph::as_data_frame(g_T, 'vertices'))
      
      V_G$label.cex <- 1
      V_G$node_id <- 1:(n_a + n_t)
      
      G <<- rbind(E_G, E_Tau0) %>% 
        graph.data.frame(vertices = V_G, directed = F) %>%
        igraph::simplify()
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
    create_progressbar = function() {
      pb <- progress::progress_bar$new(
        format = "(:spin) [:bar] :percent [Elapsed: :elapsedfull || ETA: :eta]",
        total = max_nsteps,
        complete = "=",
        incomplete = "-",
        current = ">",
        clear = FALSE,
        width = 100
      )
      return(pb)
    },
    update_bipartite_topicagent = function(show_progress=TRUE) {
      if (show_progress) {
        pb <- create_progressbar()
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
        pb <- create_progressbar()
      }
      
      M <- as_adjacency_matrix(G) %>% as.matrix()
      agents_ind <- V(G)$role == "agents"
      A_mat <- M[agents_ind, agents_ind]
      T_mat <- M[!agents_ind, !agents_ind]
      
      
      for (ni in 1:max_nsteps) {
        M <- as_adjacency_matrix(G) %>% as.matrix()
        TAU_mat <- M[!agents_ind, agents_ind]
        
        # new topic probabilities 
        new_topics_df <-
          p_alpha * colNorm(1 * (((T_mat %*% TAU_mat > 0) - TAU_mat) > 0)) +
          p_beta  * colNorm(1 * (((TAU_mat %*% A_mat > 0) - TAU_mat) > 0))
        
        # new topic sampling
        new_topics_df <- 
          melt(new_topics_df, c("to", "from")) %>%
          rename(prob = value) %>% filter(prob > 0) %>%
          group_by(from) %>% slice_sample(weight_by = prob) %>%
          select(to, from)
        
        # select only the agents with less than `tau_max` topics 
        agents_to_update <- 
          data.frame(curr_ntopic=colSums(TAU_mat)) %>%
          rownames_to_column(var = 'agent') %>% 
          filter(curr_ntopic < tau_max) 
        
        new_topics_df <- new_topics_df %>% 
          filter(from %in% agents_to_update$agent)
        
        # turn to edge sequences to be updated inside the graph object 
        edge_seq <-
          new_topics_df %>% as.matrix() %>% t %>% as.vector()
        
        if (use_colors) {
          G <<- add_edges(G, edge_seq, onset = ni, color = colors$new_cross) %>% igraph::simplify()
        } else {
          G <<- add_edges(G, edge_seq, onset = ni) %>% igraph::simplify()
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

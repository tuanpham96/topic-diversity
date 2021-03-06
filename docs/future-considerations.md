# Future considerations

- [ ] Need to update based on comments

## Current list

- Wandering (serendipity) and forgetting probs and null models when setting either `alpha` or `beta` to 0
- Heterogenity of `alpha` instead of setting one constant across all agents
- Certain cost of learning, e.g. learning is easier for "closer" topics, and takes more time with high-degree topics (e.g. longer Wikipedia page)
- Topic capacity in terms of information of the sub-knowledge tree
- Consider the knowledge trees of individuals and analysis of such, i.e. remember the paths of learning/discovery in addition to only the topic nodes
- Dependency of knowledge (directed) and strength of knowledge (weights; can consider forgetting and refinement to increase and decrease)
- Learning through friends might need a primer, i.e. a certain topic to come up first connected to both agents, new topic needs to be neighbor of the common topic and the friend
- Specialist and generalist specification and their distributions in different settings
- Overlap of topics `Js_T` (Hamming similarity) of neighboring agents (or within certain distance) versus just random pairs of agents
- Local diversity (consider topics of only neighboring agents or agents within certain distance) as a function of distance between agents
- Nestedness of the bipartite graph
- Persistent homology of the whole graph or the projected topic graph
- Modularity (and number of modules) of the bipartite or projected topic graph
- Growing and dynamic intralayer net with node/edge addition/removal, and edge reorganization to simulate changing friendship/human network and evolving knowledge graph
- Consider multilayer nets, e.g. analysis in time as layers, or inclusion of more layers in addition to just agents and topics (need to come up with examples)
- Relax some of the Heaviside step nonlinearity in the update equations and in `update_via_matmul`
- Change the size of the topic and agent networks, e.g. (a) fix `n_a` but change `n_t` or (b) fix `n_a + n_t` but change their ratios
- **Data**:
  - Use citation/paper networks, with authors as agents (assuming no agent groups), papers (or keywords) as topics, fields/subfields as groups of topics
  - *Guessing `alpha`*: assume intralayer networks as static but initialize interlayer network at a certain year, simulate with different `alpha` then calculate the likelihood of the evolved network to different actual years with matching density of the bipartite graph. This hopefully examines the tendency to self-learn or learn from friends at different times
  - *Data as initial seed*: use the empirical network as initialzed state, then simulate with different `alpha` and compare the different outcomes on topic diversity, modularity, ... Also compare with the results using the max-likelihood `alpha` from above (or projected `alpha` if MLE `alpha` seems to change with time)


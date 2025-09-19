perron_lower_bound <- function(list_models,
                               reward,
                               disc,
                               alphas=NA){
  ##list_models: list like object of possible models
  ##reward: reward function
  ##disc: discount factor
  ##alphas: list like object of alpha vetors, action, observation, index.

  ## returns alphas incremented with the alpha vectors, corresponding action, observation and index

  ## Calculate lower bound####
  vectors <- c()
  actions <- c()
  observations <- c()
  for (index_MDP_test in seq_along(list_models)){
    #solve MDP
    policy_opt <- mdp_value_iteration(list_models[[index_MDP_test]],
                                      reward,
                                      disc)
    #update vectors
    actions <- c(actions, policy_opt$policy)
    observations <- c(observations, seq(nrow(reward)))
    vectors_test <- matrix(0, ncol=nrow(reward), nrow=length(list_models))
    #apply policy of index_MDP_test to index_MDP_true
    for (index_MDP_true in seq_along(list_models)){

      solution_test <- mdp_eval_policy_matrix(list_models[[index_MDP_true]],
                                              reward,
                                              disc,
                                              policy_opt$policy)
      vectors_test[index_MDP_true,] <- solution_test
    }
    vectors <- cbind(vectors,vectors_test)
  }

  if (is.na(alphas)[1]){
    alphas <- list(vectors = vectors,
                   action = actions,
                   obs = observations,
                   index = seq(ncol(vectors)))
  } else {
    alphas$vectors <- cbind(alphas$vectors, vectors)
    alphas$action <- c(alphas$action, actions)
    alphas$obs <- c(alphas$obs, observations)
    alphas$index <- seq(ncol(alphas$vectors))
  }

  return(alphas)
}

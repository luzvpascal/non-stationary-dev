update_belief_non_stat <- function(transition_ecosystem,
                                   transition_models,
                                   current_state,
                                   next_state,
                                   current_action,
                                   current_belief_mod) {
  if (length(current_belief_mod) == 1) {
    return(1)
  }

  # Extract the relevant transition probabilities for the current state and action
  ecosystem_transitions <- sapply(transition_ecosystem, function(matrix) {
    matrix[current_state, next_state, current_action]
  })

  # Compute next belief using matrix multiplication
  next_belief <- unlist(ecosystem_transitions *
                          (matrix(current_belief_mod, nrow=1) %*%
                             transition_models[,,current_action]))[1,]

  # Normalize the belief
  next_belief <- next_belief / sum(next_belief)

  return(next_belief)
}


trajectory_non_stat <- function(state_prior_eco,
                                Tmax,
                                initial_belief_state,
                                transition_ecosystem,
                                transition_models,
                                true_transition_ecosystem,
                                reward,
                                alpha_momdp,
                                disc = gamma,
                                optimal_policy = TRUE,
                                naive_policy = NA,
                                alpha_indexes=FALSE) {

  #inputs
  # state_prior: index of observable variable
  # Tmax: horizon considered in the simulated trajectory

  # initial_belief_state: vector, prior on partially observable models
  # tr_mdp: transition function of the real mdp on which hmMDP policy is tested (X, X, A)
  # rew_mdp: reward function of the reefs. matrix of dim (X, A)
  # tr_momdp: transition function of the hmMDP as returned by transition_hmMDP (X.Y,X.Y,A)
  # obs_momdp:observation function as returned by obs_hmMDP(X.Y,X,A)

  # alpha_momdp: solution list of alpha vectors/actions/obs as returned by read_policyx2
  # disc = discount factor
  # optimal_policy : boolean indicating if we are using the optimal policy of the hmmdp or
  # a naive policy

  #alpha_indexes: boolean indicating if simulation returns indexes of used alpha vectors

  #function:
  # simulated n_it trajectories to compute the expected sum of discounted rewards
  # when using the optimal policy of a hmMDP

  #output: data.frame
  # if average: return a vector of expected sum of discounted rewards for each time step
  # else : return a concatenation of data.frames as returned by trajectory

  # initialise Num_mod and Num_state
  Num_mod <- length(initial_belief_state)

  #number of states
  Num_s_eco <- dim(transition_ecosystem[[1]])[1]
  Num_a <- dim(transition_ecosystem[[1]])[3]
  #initialise sequence of actions and rewards
  actions <- c()
  V <- c(0) #initial reward is 0

  if (alpha_indexes){indexes <- c()}

  state_eco <- state_prior_eco
  mod_probs <- matrix(initial_belief_state, nrow = 1)
  for (i in seq(Tmax)) {
    if (optimal_policy){
      output <- interp_policy2(mod_probs[i,],
                               obs = state_eco[i],
                               alpha = alpha_momdp$vectors,
                               alpha_action = alpha_momdp$action,
                               alpha_obs = alpha_momdp$obs,
                               alpha_index = alpha_momdp$index)

      actions <- c(actions, output[[2]][1])
      if (alpha_indexes){indexes <- c(indexes, output[[3]][1])}

    } else {
      act <- naive_policy(state[i], i)
      actions <- c(actions, act)
    }

    #update reward
    if (i == 1){
      V <- reward[state_eco[i], actions[i]]
    } else {
      V <- c(V, V[i-1] + disc**(i-1)*reward[state_eco[i], actions[i]])
    }

    #next observation given belief, action and obs
    set.seed(as.integer((as.double(Sys.time()) *i*1000 + Sys.getpid())%%2^31))

    prob_dist <-  c(true_transition_ecosystem[state_eco[i],,actions[i]])

    state_eco <- c(state_eco,
                   sample(seq(Num_s_eco), size=1, replace = TRUE,
                          prob =prob_dist))
    #update beliefs
    belief_state <- update_belief_non_stat(transition_ecosystem,
                                           transition_models,
                                           state_eco[i],
                                           state_eco[i+1],
                                           actions[i],
                                           mod_probs[i,])
    mod_probs <- rbind(mod_probs, belief_state)
  }

  data_output <- data.frame(state_eco=state_eco)
  data_output$value <-  c(V, V[length(V)])
  data_output$action <- c(actions, 0)
  data_output$time <- seq(0, Tmax)
  if (alpha_indexes){data_output$indexes <- c(indexes,0)}

  return(list(data_output=data_output,
              mod_probs = mod_probs))
}

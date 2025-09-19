update_belief_tech <- function(transition_tech,
                               current_state_tech,
                               next_state_tech,
                               current_action,
                               current_belief_tech){
  next_belief <- rep(0, length(current_belief_tech))
  for (mod in seq(length(transition_tech))){
    next_belief[mod] <- transition_tech[[mod]][current_state_tech,next_state_tech,current_action]*current_belief_tech[mod]
  }
  next_belief <- next_belief/sum(next_belief)
  return(next_belief)
}

trajectory_non_stationary_rewards <- function(state_prior_tech,
                                               Tmax,
                                               initial_belief_state_tech,
                                               transition_tech,
                                               true_model_tech,
                                               reward_non_stat,
                                               alpha_momdp,
                                               disc = 0.95,
                                               non_stationary_strategy = TRUE,
                                               alpha_indexes=FALSE) {

  #inputs
  # state_prior_tech: index of initial state technology (1 idle 2 ready) observable variable
  # Tmax: horizon considered in the simulated trajectory

  # initial_belief_state_tech: vector, prior on partially observable models tech (e.g. success failure)

  # transition_tech: list object of transition function for the tech
  # true_model_tech: transition function of the real mdp on which hmMDP policy is tested (X, X, A)
  # reward_non_stat: reward function of the technology non-stationary matrix of dim (X*T, A)

  # alpha_momdp: solution list of alpha vectors/actions/obs as returned by read_policyx2
  # disc = discount factor
  # non_stationary_strategy : boolean indicating if we are using the optimal non_stationary_strategy or a stationary strategy

  #alpha_indexes: boolean indicating if simulation returns indexes of used alpha vectors

  #function:
  # simulated n_it trajectories to compute the expected sum of discounted rewards
  # when using the optimal policy of a hmMDP

  # initialise Num_mod and Num_state
  Num_mod_tech <- length(initial_belief_state_tech)

  #number of states
  Num_s_tech <- dim(transition_tech[[1]])[1]
  Num_a <- dim(transition_tech[[1]])[3]
  max_time <- nrow(reward_non_stat)/Num_s_tech

  #initialise sequence of actions and rewards
  actions <- c()

  if (alpha_indexes){indexes <- c()}

  state_tech <- state_prior_tech

  mod_probs_tech <- matrix(initial_belief_state_tech, nrow = 1)

  for (i in seq(Tmax)) {
    #compute next best action0
    if (non_stationary_strategy){
      output <- interp_policy2(mod_probs_tech[i,],
                               obs = tuple_to_index(min(i, max_time), state_tech[i]),
                               alpha = alpha_momdp$vectors,
                               alpha_action = alpha_momdp$action,
                               alpha_obs = alpha_momdp$obs,
                               alpha_index = alpha_momdp$index)

      actions <- c(actions, output[[2]][1])
      if (alpha_indexes){indexes <- c(indexes, output[[3]][1])}

    } else {
      output <- interp_policy2(mod_probs_tech[i,],
                               obs = state_tech[i],
                               alpha = alpha_momdp$vectors,
                               alpha_action = alpha_momdp$action,
                               alpha_obs = alpha_momdp$obs,
                               alpha_index = alpha_momdp$index)

      actions <- c(actions, output[[2]][1])
      if (alpha_indexes){indexes <- c(indexes, output[[3]][1])}
    }

    #update reward
    #update reward
    if (i==1){
      V <- reward_non_stat[state_tech[i], actions[i]]
      r <- reward_non_stat[state_tech[i], actions[i]]
    } else {
      V <- c(V, V[i-1] + disc**(i-1)*reward_non_stat[tuple_to_index(min(i, max_time), state_tech[i]), actions[i]])
      r <- c(r, reward_non_stat[tuple_to_index(min(i, max_time), state_tech[i]), actions[i]])
    }


    #next observation given belief, action and obs
    set.seed(as.integer((as.double(Sys.time()) *i*1000 + Sys.getpid())%%2^31))

    state_tech <- c(state_tech,
                    sample(seq(Num_s_tech), size=1, replace = TRUE,
                           prob = c(true_model_tech[state_tech[i], , actions[i]])))
    ## beliefs ####
    belief_state <- update_belief_tech(transition_tech,
                                       state_tech[i],
                                       state_tech[i+1],
                                       actions[i],
                                       mod_probs_tech[i,])

    mod_probs_tech <- rbind(mod_probs_tech, belief_state)
  }

  data_output <- data.frame(state_tech=state_tech)
  data_output$value <- c(V, V[Tmax]+disc**(Tmax)*r[Tmax]/(1-disc))#infinite rewards
  data_output$r <- c(r, r[Tmax])
  data_output$action <- c(actions, 0)
  data_output$time <- seq(0, Tmax)
  if (alpha_indexes){data_output$indexes <- c(indexes,0)}

  return(list(data_output=data_output,
              mod_probs_tech=mod_probs_tech))
}

sim_non_stationary_rewards_fixed_mdp <- function(state_prior_tech,
                                                 Tmax,
                                                 initial_belief_state_tech,
                                                 transition_tech,
                                                 true_model_tech,
                                                 reward_non_stat,
                                                 alpha_momdp,
                                                 disc = 0.95,
                                                 non_stationary_strategy = TRUE,
                                                 alpha_indexes=FALSE,
                                                 n_it = 100,
                                                 average=TRUE){
  #inputs
  #inputs
  # state_prior_tech: index of initial state technology (1 idle 2 ready) observable variable
  # Tmax: horizon considered in the simulated trajectory

  # initial_belief_state_tech: vector, prior on partially observable models tech (e.g. success failure)

  # transition_tech: list object of transition function for the tech
  # true_model_tech: transition function of the real mdp on which hmMDP policy is tested (X, X, A)
  # reward_non_stat: reward function of the technology non-stationary matrix of dim (X*T, A)

  # alpha_momdp: solution list of alpha vectors/actions/obs as returned by read_policyx2
  # disc = discount factor
  # non_stationary_strategy : boolean indicating if we are using the optimal non_stationary_strategy or a stationary strategy
  # a naive policy

  #alpha_indexes: boolean indicating if simulation returns indexes of used alpha vectors

  # n_it = number of iterations, number of times a trajectory is computed
  # average : bool, average the value function among all iterations

  #function:
  # simulated n_it trajectories to compute the expected sum of discounted rewards
  # when using the optimal policy of a hmMDP

  #output: data.frame
  # if average: return a vector of expected sum of discounted rewards for each time step
  # else : return a concatenation of data.frames as returned by trajectory

  #create a list of dataframes as returned by trajectory
  # cores=parallel::detectCores()
  cores <- parallelly::availableCores()
  cl <- parallel::makeCluster(cores[1]-2) #not to overload your computer
  doParallel::registerDoParallel(cl)
  list_results <- foreach::foreach(i=1:n_it,
                                   .export=c('trajectory_non_stationary_rewards',
                                             'update_belief_tech',
                                             'interp_policy2',
                                             'tuple_to_index')) %dopar% {
                                               trajectory_non_stationary_rewards(state_prior_tech,
                                                                                 Tmax,
                                                                                 initial_belief_state_tech,
                                                                                 transition_tech,
                                                                                 true_model_tech,
                                                                                 reward_non_stat,
                                                                                 alpha_momdp,
                                                                                 disc,
                                                                                 non_stationary_strategy,
                                                                                 alpha_indexes)$data_output
                                             }
  parallel::stopCluster(cl)

  #rbind all dataframes
  list_results <- dplyr::bind_rows(list_results, .id = "simulation")
  if (average){
    list_results <-list_results %>%
      group_by(time)%>%
      summarize(mean(value),
                sd(value))
    return(list_results)
  } else {
    return(list_results)
  }
}

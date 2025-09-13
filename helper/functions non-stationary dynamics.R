p_dev_linear_increase <- function(nu, kmin, k, pdev_0 = 0){
  #nu: slope of increase of pdev
  #kmin: after kmin investments
  #k: number of previous investments
  #pdev_0: intial value of pdev

  #returns linear increase of pdev, starting at pdev_0, with slope nu, after kmin invesmtments
  #bounded between 0 and 1

  p_dev <- pdev_0 + nu*pmax((k-kmin),0)
  return(pmin(p_dev, 1))
}

p_idle_idle_vector_function <- function(params, case_study){
  #builds a reward function for the development problem
  #2 states: idle ready
  #params parameters for each case study as a list
  #case_study:
  #"A": linear increase of pdev after k_min investments
    #params:
    #Rbau_0 = 0.9
    #Rdep_0 = 1
    #Cdev: cost of development constant
    #nu: slope of pdev
    #kmin: number of minimum investments
    #horizon: length of decision problem (from 0 to horizon-1)
    #initial_belief:success or failure
  if (case_study == "A"){
    #linear increase or decrease of rewards
    nu <- params$nu #negative in both case studies
    kmin <- params$kmin #negative for C, positive for D

    time_steps <- seq(params$horizon)

    pdev_vec <- p_dev_linear_increase(nu, kmin, time_steps)

    return(1-pdev_vec)
  }
}

transition_between_models <- function(n, alpha) {
  #n is the number of models (including failure)
  #alpha the transition rate between models
  # e.g with 3 success models
  # [1-alpha, alpha, 0]
  # [0, 1-alpha, alpha]
  # [0 , 0, 1]

  mat <- matrix(0, n, n)  # Start with an n x n matrix of zeros
  # Set first and last rows
  mat[n, n] <- 1

  # Set the main diagonal and the elements directly above and below it for middle rows
  if (n >=3){
    for (i in 1:(n - 1)) {
      mat[i, i] <- 1 - alpha           # Main diagonal
      mat[i, i + 1] <- alpha    # Above main diagonal
    }
  }
  return(mat)
}

transition_between_models_with_failure <- function(n, alpha) {
  #n is the number of models (including failure)
  #alpha the transition rate between models
  # e.g with 3 success models
  # [1-alpha, alpha, 0]
  # [0, 1-alpha, alpha]
  # [0 , 0, 1]

  mat <- matrix(0, n, n)  # Start with an n x n matrix of zeros
  # Set first and last rows
  mat[n, n] <- 1
  mat[n-1, n-1] <- 1

  # Set the main diagonal and the elements directly above and below it for middle rows
  if (n >=3){
    for (i in 1:(n - 2)) {
      mat[i, i] <- 1 - alpha           # Main diagonal
      mat[i, i + 1] <- alpha    # Above main diagonal
    }
  }
  return(mat)
}

transition_between_models_by_action <- function(n, alpha){
  transition_between_models_surrender <- transition_between_models(n, 0)
  transition_between_models_invest <- transition_between_models(n, alpha)

  transition_between_models <- array(c(transition_between_models_surrender,
                                       transition_between_models_invest),
                                     dim=c(n,n,2))
}

transition_between_models_by_action_with_failure <- function(n, alpha){
  transition_between_models_surrender <- transition_between_models_with_failure(n, 0)
  transition_between_models_invest <- transition_between_models_with_failure(n, alpha)

  transition_between_models <- array(c(transition_between_models_surrender,
                                       transition_between_models_invest),
                                     dim=c(n,n,2))
}

transition_at_each_investment <- function(p_idle_idle_vector){
  #returns a list like object
  #where each element k is the transition function after k investments
  #and the last element is the failure scenario
  all_models <- list()
  for (index_val in seq_along(p_idle_idle_vector)){
    all_models[[index_val]] <- transition_function_P1(p_idle_idle_vector[index_val])
  }
  return(all_models)
}

combined_transition <- function(transition_models, transition_function_index){
  N_actions <- dim(transition_function_index[[1]])[3]
  N_tech_states <- dim(transition_function_index[[1]])[1]
  N_models <- dim(transition_models)[1]

  transition_function <- array(0, dim=c(N_tech_states*N_models,N_tech_states*N_models,N_actions))
  # Loop over each model
  for (t in 1:N_models) {
    for (action_index in seq(N_actions)){
      transition_function_action_index <- transition_function_index[[t]][,,action_index]
      # Loop over each ending time
      time_prob <- transition_models[t, ,action_index]
      for (p in which(time_prob>0)) { #FOR EACH TIME STEP THAT HAS A POSITIVE TRANSITION PROBABILITY
        for (i in 1:N_tech_states) {
          # Loop over each ending ecosystem state
          start_index <- tuple_to_index(t, i, N_tech_states)
          for (j in 1:N_tech_states) {
            # Compute the combined index for the larger transition matrix
            end_index <- tuple_to_index(p, j, N_tech_states)

            # Get the ecosystem transition probability
            state_prob <- transition_function_action_index[i, j]

            # Set the combined transition probability
            transition_function[start_index, end_index, action_index] <- state_prob * time_prob[p]
          }
          transition_function[start_index, , action_index] <- transition_function[start_index, , action_index]/sum(transition_function[start_index, , action_index])
        }
      }
    }
  }

  return(transition_function)
}

transition_function_non_stationary_dynamics <- function(params, case_study){

  #construct the transition function

  #transition between models for each action
  transition_models <- transition_between_models_by_action(params$horizon, 1) #only the transition function under success

  # transition function for each model
  p_idle_idle_vector <- p_idle_idle_vector_function(params, case_study)

  transition_function_index <- transition_at_each_investment(p_idle_idle_vector)

  combined_transition_index <- combined_transition(transition_models, transition_function_index)

  return(list(transition_models=transition_models,
              p_idle_idle_vector=p_idle_idle_vector,
              transition_function_index=transition_function_index,
              combined_transition_index=combined_transition_index))
}

# Function to convert a tuple (t, i) to an index####
tuple_to_index <- function(t, i, N_tech_states=2) {
  # t: time step 1 to horizon
  # i: tech state (e.g. 1 for idle 2 for ready)
  # N_tech_states (2 by default)
  return((t - 1) * N_tech_states + i)
}

# Function to convert an index back to a tuple (t, i)####
index_to_tuple <- function(index, N_tech_states=2) {
  # t: model
  # i: tech state (e.g. 1 for idle 2 for ready)
  # N_tech_states (2 by default)

  t <- ((index - 1) %/% N_tech_states) + 1
  i <- ((index - 1) %% N_tech_states) + 1
  return(c(t, i))
}

index_to_year <- function(index, N_tech_states=2){
  # t: time step 1 to horizon
  # i: tech state (e.g. 1 for idle 2 for ready)
  # N_tech_states (2 by default)

  t <- index_to_tuple(index, N_tech_states)
  return(t[1])
}

index_to_eco <- function(index, N_tech_states){
  # t: time step 1 to horizon
  # i: tech state (e.g. 1 for idle 2 for ready)
  # N_tech_states (2 by default)

  t <- index_to_tuple(index, N_tech_states)
  return(t[2])
}


voi_non_stationary_dynamics <- function(params, case_study){
  #calculates the value of modelling non-stationarity for a given set of parameters
  #and case study
  #builds a reward function for the development problem
  #2 states: idle ready
  #params parameters for each case study as a list
  #case_study:
  #"A": linear increase of pdev after k_min investments
    #params:
    #Rbau_0 = 0.9
    #Rdep_0 = 1
    #Cdev: cost of development constant
    #nu: slope of pdev
    #kmin: number of minimum investments
    #horizon: length of decision problem (from 0 to horizon-1)
    #initial_belief:success or failure

  ####################################
  #calculate value non-stationary ####
  ####################################

  PR_full <- transition_function_non_stationary_dynamics(params, case_study)
  PR <- PR_full$combined_transition_index
  transition_models <- PR_full$transition_models
  REW <- reward_non_stationary(params, case_study)

  sol_MDP_non_stat <- mdp_finite_horizon(PR, REW, gamma, params$horizon, h=rep(0, nrow(REW)))
  value_opt <- sol_MDP_non_stat$V[1,1]

  #calculate value uncertainty
  p_idle_idle_vector <- PR_full$p_idle_idle_vector
  # p_idle_idle_vector <- mean(p_idle_idle_vector) #also testing the average value
  p_idle_idle_vector <- c(p_idle_idle_vector, mean(p_idle_idle_vector)) #also testing the average value
  times_VOI <- seq(length(p_idle_idle_vector))

  max_values_VOI <- 0
  for (t in times_VOI){
    p_idle_idle_vector_t <- rep(p_idle_idle_vector[t], params$horizon)

    transition_t  <- transition_at_each_investment(p_idle_idle_vector_t)
    combined_transition_index <- combined_transition(transition_models, transition_t)

    sol_t <- mdp_finite_horizon(combined_transition_index, REW, gamma, params$horizon, h=rep(0, nrow(REW)))
    policy_t <- sol_t$policy[,1]
    value_policy_t <- mdp_eval_policy_iterative(PR, REW, gamma, c(policy_t),
                                                V0 = rep(0, nrow(REW)),
                                                max_iter = params$horizon,
                                                epsilon=0.01)

    values_VOI <- value_policy_t[1,1]

    if (values_VOI>max_values_VOI){
      max_values_VOI <- values_VOI
      tr_sol <- transition_t
    }

  }
  #return the value of information
  return(list(opt=value_opt,
              stat=max_values_VOI,
              tr_sol=tr_sol,
              PR_full=PR_full,
              REW=REW))
}

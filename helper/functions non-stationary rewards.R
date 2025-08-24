reward_instant <- function(Rbau, Rdep, Cdev){
  #builds a reward function for the development problem
  #2 states: idle ready
  #Rbau baseline benefits (Do nothing)
  #Rdep linearly decreases with slope beta
  #Cdev cost of development constant

  rew = matrix(c(Rbau, Rbau - Cdev, Rbau, Rdep), nrow=2, byrow=TRUE)
  return(rew)
}

Reward_linear_time <- function(R_0, time, alpha){
  #linear decline of R, bounded by 0 (ecosystem death) and 1
  #R : initial reward
  #time: time step (as int or vector)
  #alpha: slope
  # return(R_0 + time*alpha)
  R <- pmax(Rbau_0 + time*alpha,0) #bound by 0
  R <- pmin(R,1)#bound by 1
  return(R)
}

#variable Reward function ####
reward_non_stationary <- function(params, case_study){
  #builds a reward function for the development problem
  #2 states: idle ready
  #params parameters for each case study as a list
  #case_study:
    #"C": linear decrease of Rbau and Rdep
      #params:
        #Rbau_0 = 1 linearly decreases with slope alpha
        #Rdep_0 = 1 linearly decreases with slope beta
        #Cdev: cost of development constant
        #alpha: slope of Rbau
        #beta: slope of Rdep
        #horizon time steps (from 0 to horizon-1)
    #"D": linear increase of Rdep and decrease of Rbau
      #params:
        #Rbau_0 = 1 linearly decreases with slope alpha
        #Rdep_0 = 0 linearly increases with slope beta
        #Cdev: cost of development constant
        #alpha: slope of Rbau
        #beta: slope of Rdep
        #horizon time steps (from 0 to horizon-1)
    #"E": seasonal changes in Rbau and Rdep #no need to put the horizon
      #params:
        #Rbau = vector of values 1 for no heatwave 0 for heatwave
        #Rdep = #vector 0.8 for no heatwave 0.2 for heatwave
        #Cdev: cost of development (constant)
        #p_heat: probability heat stays heat
        #p_cool: probability cool stays cool


  if (case_study == "C" | case_study == "D" ){
    #linear increase or decrease of rewards
    Rbau_0 <- params$Rbau_0 #1 for both case studies
    Rdep_0 <- params$Rdep_0 #1 for C and 0 for D
    Cdev <- params$Cdev #constant
    horizon <- params$horizon
    alpha <- params$alpha #negative in both case studies
    beta <- params$beta #negative for C, positive for D

    rew = reward_instant(Rbau_0,#at time 0
                         Rdep_0,
                         Cdev)

    for (t in seq(horizon-2)){ #
      reward_t = reward_instant(Reward_linear_time(Rbau_0, t, alpha),
                                Reward_linear_time(Rdep_0, t, beta),
                                Cdev)
      rew = rbind(rew, reward_t)
    }
    #for the last time step set the rewards to 0
    reward_t = reward_instant(0, 0, Cdev)
    rew = rbind(rew, reward_t)

    return(rew)
  } else if (case_study == "E"){
    Rbau <- params$Rbau #vector 1 for no heatwave 0 for heatwave
    Rdep <- params$Rdep #vector 0.8 for no heatwave 0.2 for heatwave

    Cdev <- params$Cdev #constant


    rew = matrix()

    for (k in seq(length(Rbau))){
      reward_instant(Rbau[k],#at time 0
                     Rdep[k],
                     Cdev)
      rew <- rbind(rew,
                   reward_instant)
      }

    return(rew)
    }

}

## transition function times states ####
transition_function_times <- function(params, case_study){
  if (case_study == "C"|case_study == "D"){
    horizon <- params$horizon
    mat <- diag(horizon-1)
    mat <- cbind(rep(0,horizon-1), mat)
    mat <- rbind(mat, c(rep(0,horizon-1),1))
    return(mat)
  } else if (case_study == "E"){

    p_heat <- params$p_heat #probability heat stays heat
    p_cool <- params$p_cool #probability cool stays cool
    mat <- matrix(c(p_heat, 1-p_heat, 1-p_cool, p_cool), ncol=2, byrow=TRUE)
  }
}

## transition function tuple ####
transition_function_states <- function(transition_times, p_idle_idle, N_tech_states=2){

  #time_states: vector of states for time
  #number of tech states (N_tech_states=2
  #p_idle_idle probability of staying idle after investing

  #construct the transition function
  transition_function_index <- transition_function_P1(p_idle_idle)
  N_actions <- dim(transition_function_index)[3]
  N_times <- dim(transition_times)[1]

  transition_function <- array(0, dim=c(N_tech_states*N_times,N_tech_states*N_times,N_actions))
  for (action_index in seq(N_actions)){
    transition_function_action_index <- transition_function_index[,,action_index]
#
#     transition_function_action_list <- rep(list(transition_function_action_index), N_times-1)
#     transition_function_action <- .bdiag(transition_function_action_list)
#
#     transition_function_action <- cbind(matrix(0,ncol=N_tech_states, nrow=nrow(transition_function_action)), transition_function_action)
#     transition_function_action <- rbind(transition_function_action,matrix(0,nrow=N_tech_states, ncol=ncol(transition_function_action)))
#
#     transition_function_action[seq(nrow(transition_function_action)-1,nrow(transition_function_action)),
#                            seq(ncol(transition_function_action)-1,ncol(transition_function_action))
#     ] <- transition_function_action_index

    # Loop over each starting time
    for (t in 1:N_times) {
      # Loop over each ending time
      time_prob <- transition_times[t, ]
      for (p in which(time_prob>0)) {
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

    # transition_function[,,action_index] <- array(transition_function_action, dim=c(N_tech_states*N_times,N_tech_states*N_times,1))
    }
  }

  return(transition_function)
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
  # t: time step 1 to horizon
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



voi_non_stationary_rewards <- function(p_idle_idle, Rbau_0, Rdep_0, alpha, beta, horizon, Cdev){
  #calculate value non-stationary
  PR <- transition_function_states(horizon, p_idle_idle)
  REW <- reward_non_stationary(Rbau_0, Rdep_0, alpha, beta, horizon, Cdev)

  sol_MDP_non_stat <- mdp_finite_horizon(PR, REW, gamma, horizon, h=rep(0, nrow(REW)))
  value_opt <- sol_MDP_non_stat$V[1,1]

  #calculate value uncertainty
  times_VOI <- seq(horizon)-1
  values_VOI <- c()
  reward_0 <- matrix(0, nrow=2,ncol = 2)#two states two actions
  for (t in times_VOI){
    reward_t <- reward_instant(Rbau_time(Rbau_0, t, alpha),
                               Rdep_time(Rdep_0, t, beta),
                               Cdev)
    reward_t <- matrix(rep(c(t(reward_t)), horizon-1), ncol = 2,byrow=TRUE)#two actions
    reward_t <- rbind(reward_t, reward_0)
    sol_t <- mdp_finite_horizon(PR, reward_t, gamma, horizon, h=rep(0, nrow(REW)))
    policy_t <- sol_t$policy[,1]

    value_policy_t <-mdp_eval_policy_iterative(PR, REW, gamma, policy_t)
    values_VOI <- c(values_VOI, value_policy_t[1,1])
  }
  #return the value of information
  return(c(value_opt, max(values_VOI)))
}

reward_instant <- function(Rbau, Rdep, Cdev){
  #builds a reward function for the development problem
  #2 states: idle ready
  #Rbau baseline benefits (Do nothing)
  #Rdep linearly decreases with slope beta
  #Cdev cost of development constant

  rew = matrix(c(Rbau, Rbau - Cdev, Rbau, Rdep), nrow=2, byrow=TRUE)
  return(rew)
}

#variable Reward function ####
reward_non_stationary <- function(Rbau_0, Rdep_0, alpha, beta, horizon, Cdev){
  #builds a reward function for the development problem
  #2 states: idle ready
  #horizon time steps (from 1 to horizon)
  #Rbau linearly increases with slope alpha
  #Rdep linearly increases with slope beta
  #cost of development constant Cdev
  rew = reward_instant(Rbau_0,
                       Rdep_0,
                       Cdev)
  for (t in seq(horizon-2)){ #
    reward_t = reward_instant(max(Rbau_0 + t*alpha,0),#cannot go below 0?
                              Rdep_0 + t*beta,
                              Cdev)
    rew = rbind(rew, reward_t)
  }
  #for the last time step set the rewards to 0
  reward_t = reward_instant(0, 0, Cdev)
  rew = rbind(rew, reward_t)

  return(rew)
}


## transition function times states ####
transition_function_times <- function(time_states){
  mat <- diag(length(time_states)-1)
  mat <- cbind(rep(0,length(time_states)-1), mat)
  mat <- rbind(mat, c(rep(0,length(time_states)-1),1))
  return(mat)
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

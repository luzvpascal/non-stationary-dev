transition_reward <- function(reward_list, perfect_obs=TRUE, weight_true=0.8){
  #inputs: inputs transition function in compact manner as returned by build_hmMDP()
    #reward_list: list of matrices of size [s*t,a]
    #checks which values are the same
  #perfect_obs: bool determining if the reward is perfectly observable
  #if FALSE, 0.8 weight on true reward, uniform on the others
  Num_s <- dim(reward_list[[1]])[1]#number of fully observable states
  Num_a <- dim(reward_list[[1]])[2]#number of actions
  Num_mod <- length(reward_list) #number of models

  ## find the number of different observations ####
  list_values <- unlist(reward_list)
  list_obs <- unique(list_values)
  Num_obs <- length(list_obs)

  ## perfect obs case ####
  rew_transition <- list()
  if (perfect_obs){
    for (tr_id in seq(Num_mod)){
      rew_transition[[tr_id]] <- array(0, dim=c(Num_s,Num_obs,Num_a))
      for (id_action in seq(Num_a)){
        for (id_state in seq(Num_s)){
          rew_transition[[tr_id]][id_state,,id_action] <- as.numeric(list_obs == reward_list[[tr_id]][id_state,id_action])
        }
      }
    }
  } else {
    for (tr_id in seq(Num_mod)){
      rew_transition[[tr_id]] <- array(0, dim=c(Num_s,Num_obs,Num_a))
      for (id_action in seq(Num_a)){
        for (id_state in seq(Num_s)){
          tr_true <- as.numeric(list_obs == reward_list[[tr_id]][id_state,id_action])
          tr_false <- (1-tr_true)/(length(tr_true)-1)

          rew_transition[[tr_id]][id_state,,id_action] <-tr_true*weight_true+tr_false*(1-weight_true)
        }
      }
    }
  }

  return(list(rew_transition,
              list_obs))
}

belief_tech <- function(current_belief, Num_mod_tech){
  #belief in tech model
  m <- matrix(current_belief, nrow = Num_mod_tech)
  tech_belief <- c(apply(m, 1, sum))
  return(tech_belief)
}

belief_mod <- function(current_belief, Num_mod_tech){
  #belief in rew model
  m <- matrix(current_belief, nrow = Num_mod_tech)
  mod_belief <- c(apply(m, 2, sum))
  return(mod_belief)
}

belief <- function(belief_tech,belief_mod){
  a <- matrix(belief_tech, ncol=1)
  b <- matrix(belief_mod, nrow=1)
  belief <- c(a%*%b) #double check
  return(belief)
}


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

update_belief_mod <- function(transition_rew,
                              current_state_tech,
                              current_action,
                              next_state_rew,
                              current_belief_mod){
  next_belief <- rep(0, length(current_belief_mod))
  for (mod in seq(length(transition_rew))){
    next_belief[mod] <- transition_rew[[mod]][current_state_tech,next_state_rew,current_action]*current_belief_mod[mod]
  }
  next_belief <- next_belief/sum(next_belief)
  return(next_belief)
}

update_belief_uncertain_rew <- function(transition_tech,
                          transition_rew,
                          current_state_tech,
                          next_state_tech,
                          next_state_rew,
                          current_action,
                          current_belief_tech,
                          current_belief_mod){

  next_belief_mod <- update_belief_mod(transition_rew,
                                       current_state_tech,
                                       current_action,
                                       next_state_rew,
                                       current_belief_mod)

  next_belief_tech <- update_belief_tech(transition_tech,
                                         current_state_tech,
                                         next_state_tech,
                                         current_action,
                                         current_belief_tech)
  next_belief <- belief(next_belief_tech, next_belief_mod)
  return(next_belief)
}

factored_state <- function(state_tech, state_rew,
                           Num_s_tech, Num_s_rew){
  return(state_tech + Num_s_tech*(state_rew-1))
}

trajectory_uncertain_rewards <- function(state_prior_tech,
                                         state_prior_rew,
                                         Tmax,
                                         initial_belief_state_tech,
                                         initial_belief_state_rew,
                                         transition_tech,
                                         transition_rew,
                                         reward_list,
                                         true_model_tech,
                                         true_model_rew,
                                         true_rew,
                                         alpha_momdp,
                                         disc = 0.95,
                                         optimal_policy = TRUE,
                                         naive_policy = NA,
                                         alpha_indexes=FALSE) {

  #inputs
  # state_prior_tech: index of initial state technology (1 idle 2 ready) observable variable
  # state_prior_rew: index of initial state reward (depending on possible observations)
  # Tmax: horizon considered in the simulated trajectory

  # initial_belief_state_tech: vector, prior on partially observable models tech (e.g. success failure)
  # initial_belief_state_rew: vector, prior on partially observable models reward (e.g. beneficial, not beneficial)

  # transition_tech: list object of transition function for the tech
  # transition_rew: list object of transition function for the tech
  # reward_list: list object of alterantive rewards

  # true_model_tech: transition function of the real mdp on which hmMDP policy is tested (X, X, A)
  # true_model_rew: reward function of the reefs. matrix of dim (X, A)

  # alpha_momdp: solution list of alpha vectors/actions/obs as returned by read_policyx2
  # disc = discount factor
  # optimal_policy : boolean indicating if we are using the optimal policy of the hmmdp or
  # a naive policy

  #alpha_indexes: boolean indicating if simulation returns indexes of used alpha vectors

  #function:
  # simulated n_it trajectories to compute the expected sum of discounted rewards
  # when using the optimal policy of a hmMDP

  # initialise Num_mod and Num_state
  Num_mod_tech <- length(initial_belief_state_tech)
  Num_mod_rew <- length(initial_belief_state_rew)

  #number of states
  Num_s_tech <- dim(transition_tech[[1]])[1]
  Num_s_rew <- dim(transition_rew[[1]])[2]
  Num_s <- Num_s_tech*Num_s_rew
  Num_a <- dim(transition_tech[[1]])[3]

  #initialise sequence of actions and rewards
  actions <- c()

  if (alpha_indexes){indexes <- c()}

  state_tech <- state_prior_tech
  state_rew <- state_prior_rew

  state_current <- factored_state(state_tech,
                                  state_rew,
                                  Num_s_tech,
                                  Num_s_rew)

  mod_probs_tech <- matrix(initial_belief_state_tech, nrow = 1)
  mod_probs_rew <- matrix(initial_belief_state_rew, nrow = 1)
  mod_probs <- matrix(belief(initial_belief_state_tech, initial_belief_state_rew), nrow = 1)

  for (i in seq(Tmax)) {
    #compute next best action0
    if (optimal_policy){
      output <- interp_policy2(mod_probs[i,],
                               obs = state_current[i],
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
    #update reward
    if (i==1){
      V <- true_rew[state_tech[i], actions[i]]
      r <- true_rew[state_tech[i], actions[i]]
    } else {
      V <- c(V, V[i-1] + disc**(i-1)*true_rew[state_tech[i], actions[i]])
      r <- c(r, true_rew[state_tech[i], actions[i]])
    }


    #next observation given belief, action and obs
    set.seed(as.integer((as.double(Sys.time()) *i*1000 + Sys.getpid())%%2^31))

    state_tech <- c(state_tech,
                    sample(seq(Num_s_tech), size=1, replace = TRUE,
                           prob = c(true_model_tech[state_tech[i], , actions[i]])))

    state_rew <- c(state_rew,
                    sample(seq(Num_s_rew), size=1, replace = TRUE,
                           prob = c(true_model_rew[state_tech[i], , actions[i]])))

    state_current <- c(state_current,
                       factored_state(state_tech[i+1],
                                      state_rew[i+1],
                                      Num_s_tech,
                                      Num_s_rew))
    ## beliefs ####


    #update beliefs ####
    belief_state <- update_belief_uncertain_rew(transition_tech,
                                  transition_rew,
                                  state_tech[i],
                                  state_tech[i+1],
                                  state_rew[i+1],
                                  actions[i],
                                  mod_probs_tech[i,],
                                  mod_probs_rew[i,]
                                  )
    mod_probs <- rbind(mod_probs, belief_state)

    #belief tech
    belief_tech_next <- belief_tech(mod_probs[i+1,], Num_mod_tech)
    #belief rew
    belief_rew_next <- belief_mod(mod_probs[i+1,], Num_mod_tech)

    mod_probs_tech <- rbind(mod_probs_tech, belief_tech_next)
    mod_probs_rew <- rbind(mod_probs_rew, belief_rew_next)
  }

  data_output <- data.frame(state_tech=state_tech)
  data_output$state_rew <- state_rew
  data_output$state_current <- state_current
  data_output$value <- c(V, V[Tmax])
  data_output$r <- c(r, r[Tmax])
  data_output$action <- c(actions, 0)
  data_output$time <- seq(0, Tmax)
  if (alpha_indexes){data_output$indexes <- c(indexes,0)}

  return(list(data_output=data_output,
              mod_probs_tech=mod_probs_tech,
              mod_probs_rew=mod_probs_rew,
              mod_probs = mod_probs))
}

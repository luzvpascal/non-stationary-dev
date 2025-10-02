reward_instant <- function(Rbau, Rdep, Cdev){
  #builds a reward function for the development problem
  #2 states: idle ready
  #Rbau baseline benefits (Do nothing)
  #Rdep linearly decreases with slope beta
  #Cdev cost of development constant

  rew = matrix(c(Rbau, Rbau - Cdev, Rbau, Rdep), nrow=2, byrow=TRUE)
  return(rew)
}

Reward_linear_time <- function(R, time, alpha){
  #linear decline of R, bounded by 0 (ecosystem death) and 1
  #R : initial reward
  #time: time step (as int or vector)
  #alpha: slope
  # return(R + time*alpha)
  R1 <- pmax(R + time*alpha,0) #bound by 0
  R1 <- pmin(R1,1)#bound by 1
  return(R1)
}


Reward_linear_DeltaR_time <- function(R, time, alpha){
  #linear decline of R, bounded by 0 (ecosystem death) and 1
  #R : initial reward
  #time: time step (as int or vector)
  #alpha: slope
  # return(R + time*alpha)
  R1 <- pmax(R + time*alpha,-1) #bound by 0
  R1 <- pmin(R1,1)#bound by 1
  return(R1)
}


Reward_logistic_time <- function(R, time, mu, phi){
  #linear decline of R, bounded by 0 (ecosystem death) and 1
  #R : initial reward
  #time: time step (as int or vector)
  # phi: time step of inflection
  # mu: decline rate : if positive decline, if negative increase
  # return(R + time*alpha)
  R1 <- R/(1+exp(mu*(time-phi)))
  return(R1)
}

#variable Reward function ####
reward_non_stationary <- function(params){
  #builds a reward function for the development problem
  #2 states: idle ready
  #params parameters for each case study as a list
      #params:
        #Rbau = 1 linearly decreases with slope alpha
        #Rdep = 1 linearly decreases with slope beta
        #Cdev: cost of development constant
        #alpha: slope of Rbau
        #beta: slope of Rdep
        #horizon: length of decision problem (from 0 to horizon-1)
        #p_idle_idle: probability of staying idle
        #initial_belief:success or failure
    if (params$type=="linear"){
    #linear increase or decrease of rewards
    Rbau <- params$Rbau
    Rdep <- params$Rdep
    Cdev <- params$Cdev #constant
    horizon <- params$horizon
    alpha <- params$alpha
    beta <- params$beta

    for (t in seq(0,horizon-1)){ #
    # for (t in seq(0,horizon-2)){ #
      reward_t = reward_instant(Reward_linear_time(Rbau, t, alpha),
                                Reward_linear_time(Rdep, t, beta),
                                Cdev)
      if (t==0){
        rew <- reward_t
      } else{
        rew <- rbind(rew,
                     reward_t)
      }
    }

    return(rew)
  } else if (params$type=="linear_deltaR"){
    #linear increase or decrease of rewards
    Rbau <- params$Rbau
    Rdep <- params$Rdep
    Cdev <- params$Cdev #constant
    horizon <- params$horizon
    alpha <- params$alpha
    beta <- params$beta

    for (t in seq(0,horizon-1)){ #
      # for (t in seq(0,horizon-2)){ #
      reward_t = reward_instant(Reward_linear_DeltaR_time(Rbau, t, alpha),
                                Reward_linear_DeltaR_time(Rdep, t, beta),
                                Cdev)
      if (t==0){
        rew <- reward_t
      } else{
        rew <- rbind(rew,
                     reward_t)
      }
    }
    # reward_t = reward_instant(0,
    #                           0,
    #                           0)
    # rew <- rbind(rew,
    #              reward_t)
    return(rew)
  } else if (params$type=="season"){
    Rbau <- params$Rbau #vector 1 for no heatwave 0 for heatwave
    Rdep <- params$Rdep #vector 0.8 for no heatwave 0.2 for heatwave

    Cdev <- params$Cdev #constant

    for (k in seq(length(Rbau))){
      r_k <- reward_instant(Rbau[k],#at time 0
                           Rdep[k],
                           Cdev)
      if (k==1){
        rew <- r_k
      } else{
      rew <- rbind(rew,
                   r_k)
      }
    }
    return(rew)
  } else if (params$type=="logistic"){
      #linear increase or decrease of rewards
      Rbau <- params$Rbau #1 for both case studies
      Rdep <- params$Rdep #1 for both case studies
      Cdev <- params$Cdev #constant
      horizon <- params$horizon
      mu_bau <- params$mu_bau #constant : 0.5 for F and G
      mu_dep <- params$mu_dep #constant : 0.5 for F and  -0.5 for G
      phi_bau <- params$phi_bau #time step between 0 and horizon
      phi_dep <- params$phi_dep #time step between 0 and horizon

      rew = reward_instant(Rbau,#at time 0
                           Rdep,
                           Cdev)

      for (t in seq(0, horizon-1)){ #
        reward_t = reward_instant(Reward_logistic_time(Rbau, t, mu_bau, phi_bau),
                                  Reward_logistic_time(Rdep, t, mu_dep, phi_dep),
                                  Cdev)
        if (t==1){
          rew <- reward_t
        } else{
          rew <- rbind(rew,
                       reward_t)
        }
      }
      # #for the last time step set the rewards to 0
      # reward_t = reward_instant(0, 0, Cdev)
      # rew = rbind(rew, reward_t)

      return(rew)
    }

}

#reward function with uncertainty ####
reward_non_stationary_wrapper <- function(params) {
  # Identify which parameter is a vector
  param_list <- list(
    Rbau = params$Rbau,
    Rdep = params$Rdep,
    alpha  = params$alpha,
    beta   = params$beta
  )

  lens <- sapply(param_list, length)
  varying_param <- names(param_list)[lens > 1]

  if (length(varying_param) > 1) {
    stop("Only one parameter can vary at a time.")
  }

  if (length(varying_param) == 0) {
    # No variation, just call the base function
    return(list(reward_non_stationary(params)))
  }

  # Loop over values of the varying parameter
  varname <- varying_param
  values <- param_list[[varname]]

  results <- vector("list", length(values))
  names(results) <- paste0(varname, "_", values)

  for (i in seq_along(values)) {
    params[[varname]] <- values[i]  # replace with single value
    results[[i]] <- reward_non_stationary(params)
  }

  return(results)
}

## extract reward trajectories ####
extract_trajectories <- function(rew, horizon, case_study_name = "unknown") {
  time <- seq(0, horizon - 1)

  if (is.matrix(rew)) {
    # Single trajectory
    Rbau_plot <- rew[seq(1, 2*(horizon-1), by=2), 1]
    Rdep_plot <- rew[seq(2, 2*(horizon-1), by=2), 2]
    df <- data.frame(
      time = time,
      case_study = case_study_name,
      Rbau = Rbau_plot,
      Rdep = Rdep_plot
    )
    return(df)
  }

  if (is.list(rew)) {
    df <- data.frame(time = time, case_study = case_study_name)

    for (i in seq_along(rew)) {
      mat <- rew[[i]]
      Rbau_i <- mat[seq(1, 2*(horizon), by=2), 1]
      Rdep_i <- mat[seq(2, 2*(horizon), by=2), 2]

      df[[paste0("Rbau_", i)]] <- Rbau_i
      df[[paste0("Rdep_", i)]] <- Rdep_i
    }
    return(df)
  }

  stop("rew must be a matrix or a list of matrices")
}
## transition function times states ####
transition_function_times <- function(params){
  # if (case_study == "A"|case_study == "B"|case_study == "C"|case_study == "D"|case_study == "F"|case_study == "G"){
    horizon <- params$horizon
    if (horizon == 1){
      return(matrix(1, ncol=1,nrow=1))
    }
    mat <- diag(horizon-1)
    mat <- cbind(rep(0,horizon-1), mat)
    mat <- rbind(mat, c(rep(0,horizon-1),1))
    return(mat)
  # } else if (case_study == "E"){
  #   p_heat <- params$p_heat #probability heat stays heat
  #   p_cool <- params$p_cool #probability cool stays cool
  #   mat <- matrix(c(p_heat, 1-p_heat, 1-p_cool, p_cool), ncol=2, byrow=TRUE)
  # }
}

## transition function tuple ####
transition_function_states <- function(params){

  #construct the transition function
  transition_times <- transition_function_times(params)
  transition_function_index <- transition_function_P1(params$p_idle_idle)
  N_actions <- dim(transition_function_index)[3]
  N_tech_states <- dim(transition_function_index)[1]
  N_times <- dim(transition_times)[1]

  transition_function <- array(0, dim=c(N_tech_states*N_times,N_tech_states*N_times,N_actions))
  for (action_index in seq(N_actions)){
    transition_function_action_index <- transition_function_index[,,action_index]
    # Loop over each starting time
    for (t in 1:N_times) {
      # Loop over each ending time
      time_prob <- transition_times[t, ]
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

update_params_from_row <- function(params, row_values, params_voi_names) {
  # Extract base names (remove _1, _2 suffixes if present)
  base_names <- gsub("_[0-9]+$", "", params_voi_names)

  # For each unique base name, collect values into either scalars or vectors
  for (bn in unique(base_names)) {
    idx <- which(base_names == bn)
    values <- as.numeric(row_values[idx])

    if (length(values) == 1) {
      params[[bn]] <- values
    } else {
      params[[bn]] <- values
    }
  }

  return(params)
}

voi_static_non_stationary_rewards <- function(params){
  ####################################
  #calculate value non-stationary ####
  ####################################

  PR <- transition_function_states(params)
  REW <- reward_non_stationary(params)

  sol_MDP_non_stat <- mdp_value_iteration(PR, REW, gamma)
  value_opt <- mdp_eval_policy_iterative(PR, REW, gamma, sol_MDP_non_stat$policy)[1]

  #calculate value uncertainty
  stat_strategies <- expand.grid(idle=c(1,2),ready=c(1,2))#only 4 strategies
  # times_VOI <- seq(nrow(REW)/2) #divided by 2 states
  max_values_VOI <- 0
  # reward <- matrix(0, nrow=2,ncol = 2)#two states two actions
  # for (t in times_VOI){
  for (t in seq(nrow(stat_strategies))){
    stat_strat <- unlist(unname(c(stat_strategies[t,])))
    policy_t <- rep(stat_strat, params$horizon)

    value_policy_t <- mdp_eval_policy_iterative(PR, REW, gamma, c(policy_t))

    values_VOI <- value_policy_t[1]

    if (values_VOI>max_values_VOI){
      max_values_VOI <- values_VOI
      policy_sol <- policy_t
    }

  }
  #return the value of information
  return(list(opt=value_opt,
              stat=max_values_VOI,
              policy_sol=policy_sol,
              PR=PR,
              REW=REW))
}

get_Tmax <- function(params,
                     write_hmMDP,
                     solve_hmMDP,
                     file_pomdpx,
                     file_policyx){
  #solve the non-stationary problem
  outputs <- solving_uncertain_nonstat_rewards_POMDP(params,
                                                     write_hmMDP,
                                                     solve_hmMDP,
                                                     file_pomdpx,
                                                     file_policyx)

  alpha_non_stat <- read_policyx2(file_policyx)
  #get optimal value
  value_non_stat <- interp_policy2(params$initial_belief,
                                   obs = 1,
                                   alpha = alpha_non_stat$vectors,
                                   alpha_action = alpha_non_stat$action,
                                   alpha_obs = alpha_non_stat$obs,
                                   alpha_index = alpha_non_stat$index)[[1]]

  trajectory_non_stat_failure <- (trajectory_non_stationary_rewards(state_prior_tech=1,
                                                                    # Tmax=params$horizon,
                                                                    Tmax=Tmax_SIM,
                                                                    initial_belief_state_tech=params$initial_belief,
                                                                    transition_tech=outputs$TR_FUNCTION,
                                                                    true_model_tech=outputs$TR_FUNCTION[[2]],#failure
                                                                    reward_non_stat=outputs$REW[[1]],
                                                                    alpha_momdp=alpha_non_stat,
                                                                    disc = gamma,
                                                                    non_stationary_strategy = TRUE,
                                                                    alpha_indexes = TRUE))

  #get the optimal Tmax
  actions <- (trajectory_non_stat_failure$data_output$action[-(Tmax_SIM+1)]-1)
  if (sum(actions)>0){
    r <- rle(actions)
    Tmax <- r$lengths[r$values == 1] # Lengths of consecutive 1s
    start <- cumsum(r$lengths)[r$values == 1] - Tmax   # Start positions of consecutive 1s
    # if (length(start)>1){
    #   return(0)
    # }
  } else {
    Tmax <- 0 # Lengths of consecutive 1s
    start <- 0 # Start positions of consecutive 1s
  }
  return(list(result=data.frame(value_non_stat=value_non_stat,
                    Tmax=sum(Tmax),
                    Tmax_len = length(Tmax),
                    start=min(start)),
              outputs=outputs))

}

get_Tmax_stationary <- function(params_stationary,
                                reward_POMDP,
                                outputs,
                                solve_hmMDP,
                                file_name_pomdpx,
                                file_name_policyx){
  #params_stationary list object of stationary params
  #reward_POMDP: reward function of the stationarty pb
  #outputs: parameters of the true non-stationarty pb (outputs of solving_uncertain_nonstat_rewards_POMDP)
  # solve_hmMDP: bool
  # file_name_pomdpx: file name
  # file_name_policyx: file name

  outputs_stat <- solving_POMDP(params_stationary$p_idle_idle,
                                params_stationary$initial_belief,
                                reward_POMDP,
                                solve_hmMDP,
                                file_name_pomdpx,
                                file_name_policyx)

  alpha_momdp <- read_policyx2(file_name_policyx) #stationary strategy

  #apply the stationary strategy to the non-stationary problem success
  value_stat_success <- sim_non_stationary_rewards_fixed_mdp(state_prior_tech=1,
                                                             Tmax=Tmax_SIM,
                                                             initial_belief_state_tech=params$initial_belief,
                                                             transition_tech=outputs$TR_FUNCTION,
                                                             true_model_tech=outputs$TR_FUNCTION[[1]],#success
                                                             reward_non_stat=outputs$REW[[1]],
                                                             alpha_momdp=alpha_momdp,
                                                             disc = gamma,
                                                             non_stationary_strategy = FALSE,
                                                             average=TRUE,
                                                             n_it = 5000)
  value_stat_success <- value_stat_success$`mean(value)`[Tmax_SIM+1]
  #apply the stationary strategy to the non-stationary problem failure
  trajectory_failure <- (trajectory_non_stationary_rewards(state_prior_tech=1,
                                                           Tmax=Tmax_SIM,
                                                           initial_belief_state_tech=params$initial_belief,
                                                           transition_tech=outputs$TR_FUNCTION,
                                                           true_model_tech=outputs$TR_FUNCTION[[2]],#failure
                                                           reward_non_stat=outputs$REW[[1]],
                                                           alpha_momdp=alpha_momdp,
                                                           disc = gamma,
                                                           non_stationary_strategy = FALSE))
  value_stat_failure <- trajectory_failure$data_output$value[Tmax_SIM+1]
  # average stat value
  value_stat <- sum(c(value_stat_success, value_stat_failure)*params$initial_belief)

  ## get the Tmax ####
  #Tmax stationary strategy
  actions <- (trajectory_failure$data_output$action[-(Tmax_SIM+1)]-1)
  if (sum(actions)>0){
    r <- rle(actions)
    Tmax <- r$lengths[r$values == 1] # Lengths of consecutive 1s
    start <- cumsum(r$lengths)[r$values == 1] - Tmax   # Start positions of consecutive 1s
  } else {
    Tmax <- 0 # Lengths of consecutive 1s
    start <- 0 # Start positions of consecutive 1s
  }
  return(list(value_stat=value_stat,
              Tmax=Tmax,
              start=start))
}

voi_non_stationary_rewards <- function(params,
                                       write_hmMDP,
                                       solve_hmMDP,
                                       file_name_pomdpx,
                                       file_name_policyx,
                                       run_voi
                                       ){
  ############################################
  #calculate value non-stationary rewards ####
  ############################################
  #evaluate the value of modelling non-stationary rewards
  #only for case studies without uncertainty about reward trajectory

  output_list <- get_Tmax(params,
                     write_hmMDP,
                     solve_hmMDP,
                     paste0(file_name_pomdpx,".pomdpx"),
                     paste0(file_name_policyx, ".policyx"))

  result <- output_list$result
  outputs <- output_list$outputs

  if (run_voi){
    #stationary params list
    params_stationary <- params
    params_stationary$horizon <- 1
    params_stationary$alpha <- 0
    params_stationary$beta <- 0

    best_value_stat <- -Inf
    best_t_stat <- 0

    # for each time step, get the reward function (might be uncertain), solve the stationary POMDP
    # time_steps <- c(1, round(params$horizon/2), params$horizon)
    # time_steps <- seq(params$horizon)
    time_steps <- c(1, params$horizon)
    # time_steps <- c(seq(1, params$horizon,10), params$horizon)
    all_names <- c(time_steps, "avg","integral")

    Tmax_names <- paste0("Tmax_", all_names)
    start_names <- paste0("start_", all_names)
    values_names <- paste0("value_", all_names)

    Tmax_values <- rep(0, length(all_names))
    start_values <- rep(0, length(all_names))
    values <- rep(0, length(all_names))

    #for all time steps####
    for (t in time_steps){
      params_stationary$Rbau <-  unique(unname(sapply(outputs$REW, function(x) x[tuple_to_index(t, 1), 1])))
      params_stationary$Rdep <-  (unname(sapply(outputs$REW, function(x) x[tuple_to_index(t, 2), 2])))

      reward_POMDP <- reward_non_stationary_wrapper(params_stationary)[[1]]

      stationary_output <- get_Tmax_stationary(params_stationary,
                                      reward_POMDP,
                                      outputs,
                                      solve_hmMDP,
                                      paste0(file_name_pomdpx,"_stationary_",t, ".pomdpx"),
                                      paste0(file_name_policyx,"_stationary_",t, ".policyx"))
      #compare
      Tmax_values[which(t==time_steps)] <- stationary_output$Tmax
      start_values[which(t==time_steps)] <- stationary_output$start
      values[which(t==time_steps)] <- stationary_output$value_stat

      if (stationary_output$value_stat>best_value_stat){
        best_value_stat <- stationary_output$value_stat
        best_t_stat <- t
        Tmax_stat <- stationary_output$Tmax
        start_stat <- stationary_output$start
      }
    }

    #extract trajectories
    df <- extract_trajectories(outputs$REW, params$horizon, case_study_name)
    df_last <- df %>%
      filter(time==max(time))%>%
      reframe(time=time+1,
              case_study =case_study ,
              Rbau_1 = Rbau_1,
              Rdep_1=Rdep_1)

    df <- rbind(df,df_last)


    df <- df %>%
      mutate(deltaR=Rdep_1-Rbau_1,
             deltaR_gamma=ifelse(time==max(time),
                                 deltaR*gamma**time/(1-gamma),
                                 deltaR*gamma**time))

    average_slope <- mean(df$deltaR-lag(df$deltaR), na.rm=TRUE)

    df <- df %>%
      reframe(mean_deltaR=mean(deltaR),
              integral_gamma=sum(deltaR_gamma)*(1-gamma),
              diff_max=max(deltaR)-min(deltaR),
              end_deltaR = deltaR[time == max(time)])
    df$average_slope <- average_slope

    #solve POMDP with average reward ####
    params_stationary$Rbau <- 0
    params_stationary$Rdep <- df$mean_deltaR

    reward_POMDP <- reward_non_stationary_wrapper(params_stationary)[[1]]

    stationary_output <- get_Tmax_stationary(params_stationary,
                                             reward_POMDP,
                                             outputs,
                                             solve_hmMDP,
                                             paste0(file_name_pomdpx,"_average.pomdpx"),
                                             paste0(file_name_policyx,"_average.policyx"))
    #compare
    Tmax_values[length(time_steps)+1] <- stationary_output$Tmax
    start_values[length(time_steps)+1] <- stationary_output$start
    values[length(time_steps)+1] <- stationary_output$value_stat

    if (stationary_output$value_stat>best_value_stat){
      best_value_stat <- stationary_output$value_stat
      best_t_stat <- t
      Tmax_stat <- stationary_output$Tmax
      start_stat <- stationary_output$start
    }
    # #for integral annualized reward ####
    # params_stationary$Rbau <- 0
    # params_stationary$Rdep <- df$integral_gamma
    #
    # reward_POMDP <- reward_non_stationary_wrapper(params_stationary)[[1]]
    #
    # stationary_output <- get_Tmax_stationary(params_stationary,
    #                                          reward_POMDP,
    #                                          outputs,
    #                                          solve_hmMDP,
    #                                          paste0(file_name_pomdpx,"_integral.pomdpx"),
    #                                          paste0(file_name_policyx,"_integral.policyx"))
    # #compare
    # Tmax_values[length(time_steps)+2] <- stationary_output$Tmax
    # start_values[length(time_steps)+2] <- stationary_output$start
    # values[length(time_steps)+2] <- stationary_output$value_stat
    #
    # if (stationary_output$value_stat>best_value_stat){
    #   best_value_stat <- stationary_output$value_stat
    #   best_t_stat <- t
    #   Tmax_stat <- stationary_output$Tmax
    #   start_stat <- stationary_output$start
    # }

    #return the value of information
    result$value_stat=best_value_stat
    result$best_t_stat=best_t_stat
    result$Tmax_stat=Tmax_stat
    result$start_stat=start_stat

    data_Tmax <-data.frame(name=c(Tmax_names,start_names,values_names),
                           value=c(Tmax_values,start_values,values))

    data_Tmax <- data_Tmax %>%
      pivot_wider()

    result <- merge(result, df)
    result <- merge(result, data_Tmax)

    ## analytical approximation ####
    bIS_mean <- belief_invest_to_surrender(params$Cdev,
                                      params$p_idle_idle,
                                      0,
                                      result$mean_deltaR,
                                      gamma
    )

    Tmax_mean_an <- max_years(params$initial_belief[1],
                         params$p_idle_idle,
                         bIS_mean)

    result$Tmax_mean_an <- Tmax_mean_an

    ## analytical approximation ####
    # bIS_integral <- belief_invest_to_surrender(params$Cdev,
    #                                        params$p_idle_idle,
    #                                        0,
    #                                        result$integral_gamma,
    #                                        gamma
    # )
    #
    # Tmax_integral_an <- max_years(params$initial_belief[1],
    #                           params$p_idle_idle,
    #                           bIS_integral)
    #
    # result$Tmax_integral_an <- Tmax_integral_an
  }
  return(result)

}

voi_model_uncertainty_rewards <- function(params,
                                       write_hmMDP,
                                       solve_hmMDP,
                                       file_name){
  #####################################################
  #calculate value of model uncertainty in rewards ####
  #####################################################

  #solve the complete non-stationary problem
  outputs <- solving_uncertain_nonstat_rewards_POMDP(params,
                                                     write_hmMDP,
                                                     solve_hmMDP,
                                                     paste0(file_name, ".pomdpx"),
                                                     paste0(file_name, ".policyx"))

  alpha_non_stat <- read_policyx2(paste0(file_name, ".policyx"))
  #get optimal value
  value_non_stat <- interp_policy2(params$initial_belief,
                                   obs = 1,
                                   alpha = alpha_non_stat$vectors,
                                   alpha_action = alpha_non_stat$action,
                                   alpha_obs = alpha_non_stat$obs,
                                   alpha_index = alpha_non_stat$index)[[1]]

  trajectory_non_stat_failure <- (trajectory_non_stationary_rewards(state_prior_tech=1,
                                                                    Tmax=Tmax_SIM,
                                                                    initial_belief_state_tech=params$initial_belief,
                                                                    transition_tech=outputs$TR_FUNCTION,
                                                                    true_model_tech=outputs$TR_FUNCTION[[2]],#failure
                                                                    reward_non_stat=outputs$REW[[1]],
                                                                    alpha_momdp=alpha_non_stat,
                                                                    disc = gamma,
                                                                    non_stationary_strategy = TRUE,
                                                                    alpha_indexes = TRUE))

  #get the optimal Tmax
  actions <- (trajectory_non_stat_failure$data_output$action[-(Tmax_SIM+1)]-1)
  if (sum(actions)>0){
    r <- rle(actions)
    Tmax <- r$lengths[r$values == 1] # Lengths of consecutive 1s
    start <- cumsum(r$lengths)[r$values == 1] - Tmax   # Start positions of consecutive 1s
    # if (length(start)>1){
    #   return(0)
    # }
  } else {
    Tmax <- 0 # Lengths of consecutive 1s
    start <- 0 # Start positions of consecutive 1s
  }

  # for each reward function, solve the POMDP, and apply the strategy to the alternative functions
  value_performance_best <-0
  for (r_index in seq_along(outputs$REW)){
    values_performance_index <- c()

    #solve POMDP without uncertainty in non-stationary rewards
    solving_POMDP_non_stationary_rewards(outputs$TR_FUNCTION,
                                         outputs$TR_FUNCTION_TIMES,
                                         params$initial_belief,
                                         outputs$REW[[r_index]],
                                         write_hmMDP,
                                         solve_hmMDP,
                                         paste0(file_name,"_reward_",r_index, ".pomdpx"),
                                         paste0(file_name,"_reward_",r_index, ".policyx"))


    alpha_momdp <- read_policyx2(paste0(file_name,"_reward_",r_index, ".policyx")) #non-stationary strategy

    opt_value <- interp_policy2(params$initial_belief,
                                obs = 1,
                                alpha = alpha_non_stat$vectors,
                                alpha_action = alpha_non_stat$action,
                                alpha_obs = alpha_non_stat$obs,
                                alpha_index = alpha_non_stat$index)[[1]]

    values_performance_index <- c(values_performance_index,opt_value)

    for (r_index_true in seq_along(outputs$REW)){
      if (r_index_true != r_index){
        #apply the strategy to the other possible reward functions

        #apply the stationary strategy to the non-stationary problem success
        value_success <- sim_non_stationary_rewards_fixed_mdp(state_prior_tech=1,
                                                             Tmax=Tmax_SIM,
                                                             initial_belief_state_tech=params$initial_belief,
                                                             transition_tech=outputs$TR_FUNCTION,
                                                             true_model_tech=outputs$TR_FUNCTION[[1]],#success
                                                             reward_non_stat=outputs$REW[[r_index_true]],
                                                             alpha_momdp=alpha_momdp,
                                                             disc = gamma,
                                                             non_stationary_strategy = TRUE,
                                                             average=TRUE,
                                                             n_it = 5000)
        value_success <- value_success$`mean(value)`[Tmax_SIM+1]
        #value failure
        trajectory_failure <- (trajectory_non_stationary_rewards(state_prior_tech=1,
                                                                 Tmax=Tmax_SIM,
                                                                 initial_belief_state_tech=params$initial_belief,
                                                                 transition_tech=outputs$TR_FUNCTION,
                                                                 true_model_tech=outputs$TR_FUNCTION[[2]],#failure
                                                                 reward_non_stat=outputs$REW[[r_index_true]],
                                                                 alpha_momdp=alpha_momdp,
                                                                 disc = gamma,
                                                                 non_stationary_strategy = TRUE))
        value_failure <- trajectory_failure$data_output$value[Tmax_SIM+1]

        #value application
        value_true_model <- sum(c(value_stat_success, value_stat_failure)*params$initial_belief)

        #store value in values_performance_index
        values_performance_index <- c(values_performance_index, value_true_model)
      }
    }

    values_performance <- mean(values_performance_index)
    #compare
    if (values_performance>value_performance_best){
      value_performance_best <- values_performance
      best_reward <- r_index

      #Tmax stationary strategy
      actions <- (trajectory_failure$data_output$action[-(Tmax_SIM+1)]-1)
      if (sum(actions)>0){
        r <- rle(actions)
        Tmax_rew <- r$lengths[r$values == 1] # Lengths of consecutive 1s
        start_rew <- cumsum(r$lengths)[r$values == 1] - Tmax_stat   # Start positions of consecutive 1s
      } else {
        Tmax_rew <- 0 # Lengths of consecutive 1s
        start_rew <- 0 # Start positions of consecutive 1s
      }
    }
  }

  #return the value of information
  return(data.frame(value_non_stat=value_non_stat,
                    Tmax=Tmax,
                    start=start,
                    value_performance_best=value_performance_best,
                    best_reward=best_reward,
                    Tmax_rew=Tmax_rew,
                    start_rew=start_rew))
}

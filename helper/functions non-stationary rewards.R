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
    # for (t in seq(horizon-2)){ #
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

## reward trajectories ####
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

voi_non_stationary_rewards <- function(params){
  ####################################
  #calculate value non-stationary ####
  ####################################
  #non-stationary reward function
  REW <- reward_non_stationary(params)


  #models for technology development ####
  models_tech <- list()
  params_fail <- params
  params_fail$pdev <- 0
  params_fail$p_idle_idle <- 1-params_fail$pdev

  models_tech[[1]] <- transition_function_P1(params$p_idle_idle)
  models_tech[[2]] <- transition_function_P1(params_fail$p_idle_idle)

  #make transition function times
  transition_times <- transition_function_times(params, case_study_name)

  #whole transition function
  models <- list()
  models[[1]] <- transition_function_states(params, case_study_name)
  models[[2]] <- transition_function_states(params_fail, case_study_name)

  ##
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

plot_voi_result <- function(df,params) {
  # grab first two column names dynamically
  xcol <- names(df)[1]
  ycol <- names(df)[2]

  plot <- df %>%
    mutate(EVPI = (opt - stat)*100 / opt) %>%
    ggplot(aes(
      x = abs(.data[[xcol]]),
      y = abs(.data[[ycol]]),
      fill = EVPI
    )) +
    # geom_raster(interpolate = TRUE) +
    # geom_point() +
    geom_tile() +
    scale_fill_gradient(low = "lightblue", high = "blue") +
    labs(
      fill = TeX("Value of modelling\nnon-stationary rewards (%)")
    )
  if (params$log_scale_x){
    plot <- plot+
      scale_x_log10()
  }

  if (params$log_scale_y){
    plot <- plot+
      scale_y_log10()
  }
  return(plot)
}

plot_Tmax_opt <- function(df,params) {
  # grab first two column names dynamically
  xcol <- names(df)[1]
  ycol <- names(df)[2]

  plot <- df %>%
    mutate(Tmax_diff = (Tmax_opt)) %>%
    ggplot(aes(
      x = abs(.data[[xcol]]),
      y = abs(.data[[ycol]]),
      fill = Tmax_diff
    )) +
    geom_raster() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(
      fill = TeX("Optimal $T_{max}$")
    )
  if (params$log_scale_x){
    plot <- plot+
      scale_x_log10()
  }

  if (params$log_scale_y){
    plot <- plot+
      scale_y_log10()
  }
  return(plot)
}

plot_Tmax_stat <- function(df,params) {
  # grab first two column names dynamically
  xcol <- names(df)[1]
  ycol <- names(df)[2]

  plot <- df %>%
    mutate(Tmax_diff = (Tmax_stat)) %>%
    ggplot(aes(
      x = abs(.data[[xcol]]),
      y = abs(.data[[ycol]]),
      fill = Tmax_diff
    )) +
    geom_raster() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(
      fill = TeX("$T_{max}$ stationary")
    )
  if (params$log_scale_x){
    plot <- plot+
      scale_x_log10()
  }

  if (params$log_scale_y){
    plot <- plot+
      scale_y_log10()
  }
}

plot_Tmax_diff <- function(df,params) {
  # grab first two column names dynamically
  xcol <- names(df)[1]
  ycol <- names(df)[2]

  plot <- df %>%
    mutate(Tmax_diff = (Tmax_opt - Tmax_stat)) %>%
    ggplot(aes(
      x = abs(.data[[xcol]]),
      y = abs(.data[[ycol]]),
      fill = Tmax_diff
    )) +
    geom_raster() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(
      fill = TeX("Difference between $T_{max}$")
    )
  if (params$log_scale_x){
    plot <- plot+
      scale_x_log10()
  }

  if (params$log_scale_y){
    plot <- plot+
      scale_y_log10()
  }
  return(plot)
}

plot_start_invest_result <- function(df,params) {
  # grab first two column names dynamically
  xcol <- names(df)[1]
  ycol <- names(df)[2]

  plot <- df %>%
    mutate(start_diff = (Tmax_opt_start)) %>%
    # mutate(start_diff = (Tmax_opt_start-Tmax_stat_start)) %>%
    ggplot(aes(
      x = abs(.data[[xcol]]),
      y = abs(.data[[ycol]]),
      fill = start_diff
    )) +
    # geom_raster(interpolate = TRUE) +
    geom_raster() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(
      # x = TeX("Slope change baseline rewards $\\alpha$"),
      # y = TeX("Slope change deployment rewards $\\beta$"),
      fill = TeX("Difference between\nstart times")
    )
  if (params$log_scale_x){
    plot <- plot+
      scale_x_log10()
  }

  if (params$log_scale_y){
    plot <- plot+
      scale_y_log10()
  }
  return(plot)
}

plot_value_result <- function(df,params) {
  # grab first two column names dynamically
  xcol <- names(df)[1]
  ycol <- names(df)[2]

  df %>%
    mutate(rEVPI = (V_opt-V_stat)*100/V_opt) %>%
    ggplot(aes(
      x = abs(.data[[xcol]]),
      y = abs(.data[[ycol]]),
      fill = rEVPI
    )) +
    # geom_raster(interpolate = TRUE) +
    geom_raster() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(
      # x = TeX("Slope change baseline rewards $\\alpha$"),
      # y = TeX("Slope change deployment rewards $\\beta$"),
      fill = TeX("rEVPI (%)")
    )
  if (params$log_scale_x){
    plot <- plot+
      scale_x_log10()
  }

  if (params$log_scale_y){
    plot <- plot+
      scale_y_log10()
  }
  return(plot)
}

source("non-stationary dynamics/case studies parameters.R")
results <- list()
# write_hmMDP <- FALSE
# solve_hmMDP <- FALSE
write_hmMDP <- TRUE
solve_hmMDP <- TRUE
start <- Sys.time()
for (index_case_study in seq_along(CASE_STUDIES)){

  case_study_name <- names(CASE_STUDIES)[index_case_study]
  params_case_study <- CASE_STUDIES[[index_case_study]]
  results_case_study <- params_case_study$case_studies
  params_voi_names <- names(results_case_study)

  results_case_study$opt <- 0
  results_case_study$stat <- 0
  results_case_study$Tmax_opt <- 0
  results_case_study$Tmax_opt_start <- 0
  results_case_study$Tmax_stat <- 0
  results_case_study$Tmax_stat_start <- 0
  results_case_study$V_opt <- 0
  results_case_study$V_stat <- 0

  for (k in seq(params_case_study$N_case_studies)){
    params <- params_case_study

    for (par_name_index in seq_along(params_voi_names)){
      par_name <- params_voi_names[par_name_index]
      params[[par_name]] <- results_case_study[k,par_name_index]
    }

    #calculate VOI for each case study ####
    res_voi <- voi_non_stationary_dynamics(params, case_study_name)

    results_case_study$opt[k] <- res_voi$opt
    results_case_study$stat[k] <- res_voi$stat

    # solve complete POMDP ####
    #make the list of models for the technology
    models_tech <- res_voi$PR_full$transition_function_index
    models_tech[[length(models_tech)+1]] <- transition_function_P1(1)#add the failure transition

    #transition between models
    transition_models <- transition_between_models_by_action_with_failure(length(models_tech),1)
    #make transition function between time steps
    transition_times <- transition_function_times(params, case_study_name)

    #whole transition function
    models_tech_time <- list()
    for (index_model in seq(length(models_tech))){
      list_models <- rep(list(models_tech[[index_model]]),params$horizon)

      models_tech_time[[index_model]] <- combined_transition(array(transition_times, dim=c(horizon, horizon, 2)),
                                                   list_models)
    }
    transition_momdp <- combined_transition(transition_models, models_tech_time)
    #observation function momdp
    observation_momdp <- obs_hmMDP(models_tech_time)

    #real dynamics
    tr_mdp_real <- models_tech_time[[length(models_tech_time)]] #failure

    #solve POMDP
    solving_POMDP_non_stationary_dynamics(models_tech,
                                          transition_models,
                                          transition_times,
                                          params$initial_belief,
                                          res_voi$REW,
                                          write_hmMDP,
                                          solve_hmMDP,
                                          paste0("res/pomdpx/", case_study_name, "_",k,".pomdpx"),
                                          paste0("res/policyx/", case_study_name, "_",k,".policyx"))
    #getting the Tmax
    alpha_vectors <- read_policyx2(paste0("res/policyx/", case_study_name, "_",k,".policyx")) #alpha vectors

    B_PAR <- rep(0, length(models_tech))

    B_PAR[1] <- params$initial_belief[1]
    B_PAR[length(models_tech)] <- params$initial_belief[2]

    tab_ <- trajectory(state_prior= 1,
                       Tmax = params$horizon,
                       initial_belief_state = B_PAR,
                       tr_mdp = tr_mdp_real,
                       rew_mdp= res_voi$REW,
                       tr_momdp = transition_momdp,
                       obs_momdp = observation_momdp,
                       alpha_momdp = alpha_vectors,
                       disc = gamma,
                       optimal_policy = TRUE,
                       naive_policy = NA,
                       alpha_indexes=FALSE)

    actions <- (tab_$data_output$action[-(horizon+1)]-1)
    if (sum(actions)>0){
      r <- rle(actions)
      len <- r$lengths[r$values == 1] # Lengths of consecutive 1s
      starts <- cumsum(r$lengths)[r$values == 1] - len + 1    # Start positions of consecutive 1s
    } else {
      len <- 0 # Lengths of consecutive 1s
      starts <- 0 # Start positions of consecutive 1s
    }

    results_case_study$Tmax_opt[k] <- len
    results_case_study$Tmax_opt_start[k] <- starts

    ##performance optimal strategy ####
    output <- interp_policy2(B_PAR,
                             obs = 1,
                             alpha = alpha_vectors$vectors,
                             alpha_action = alpha_vectors$action,
                             alpha_obs = alpha_vectors$obs,
                             alpha_index = alpha_vectors$index)

    mean_value_opt <- output[[1]]
    results_case_study$V_opt[k] <- mean_value_opt

    ## solve the approximate POMPD with constant dynamics ####
    stat_models <- res_voi$tr_sol
    stat_models[[length(stat_models)+1]] <- transition_function_P1(1)#add the failure transition


    solving_POMDP_non_stationary_dynamics(stat_models,
                                          transition_models,
                                          transition_times,
                                          params$initial_belief,
                                          res_voi$REW,
                                          write_hmMDP,
                                          solve_hmMDP,
                                          paste0("res/pomdpx/", case_study_name, "_",k,"_approx.pomdpx"),
                                          paste0("res/policyx/", case_study_name, "_",k,"_approx.policyx"))
    #getting the Tmax
    alpha_vectors_approx <- read_policyx2(paste0("res/policyx/", case_study_name, "_",k,"_approx.policyx")) #alpha vectors

    tab_approx <- trajectory(state_prior= 1,
                             Tmax = params$horizon,
                             initial_belief_state = B_PAR,
                             tr_mdp = tr_mdp_real,
                             rew_mdp= res_voi$REW,
                             tr_momdp = transition_momdp,
                             obs_momdp = observation_momdp,
                             alpha_momdp = alpha_vectors_approx,
                             disc = gamma,
                             optimal_policy = TRUE,
                             naive_policy = NA,
                             alpha_indexes=FALSE)

    actions <- (tab_approx$data_output$action[-(horizon+1)]-1)
    if (sum(actions)>0){
      r <- rle(actions)
      len <- r$lengths[r$values == 1] # Lengths of consecutive 1s
      starts <- cumsum(r$lengths)[r$values == 1] - len + 1    # Start positions of consecutive 1s
    } else {
      len <- 0 # Lengths of consecutive 1s
      starts <- 0 # Start positions of consecutive 1s
    }

    results_case_study$Tmax_stat[k] <- len
    results_case_study$Tmax_stat_start[k] <- starts

    # evaluate the performance ####
    tr_mdp_non_stat <- res_voi$PR_full$combined_transition_index
    tr_mdp_non_stat[,,1] <- tr_mdp_real[,,1]
    sim_approx <- sim_mdp_momdp_policy_fixed_mdp(state_prior= 1,
                                                 Tmax = params$horizon,
                                                 initial_belief_state = B_PAR,
                                                 tr_mdp = tr_mdp_non_stat,#true transition function between models if success
                                                 rew_mdp= res_voi$REW, #true rewards
                                                 tr_momdp = transition_momdp,
                                                 obs_momdp = observation_momdp,
                                                 alpha_momdp = alpha_vectors_approx,#solution with approximated rewards
                                                 disc = gamma,
                                                 n_it = 100,
                                                 optimal_policy = TRUE,
                                                 average=TRUE,
                                                 naive_policy = NA)

    mean_value_approx <- (sim_approx[params$horizon] + tab_approx$data_output$value[params$horizon])/2
    results_case_study$V_stat[k] <- mean_value_approx
  }

  results[[case_study_name]] <- results_case_study

}
end <- Sys.time()
print(end-start)
#
saveRDS(results, "case_studies_dynamics.rds")
# #
results <- readRDS("case_studies_dynamics.rds")
plots <- list()
for (index_case_study in seq_along(CASE_STUDIES)){

  case_study_name <- names(CASE_STUDIES)[index_case_study]
  data_case <- results[[case_study_name]]

  plot_VOI <- plot_voi_result(data_case) +
    geom_text(aes(label=round(EVPI,digits=2)))+
    # scale_y_continuous(trans='log10')+
    scale_x_continuous(trans='log10')

  # plot_Tmax <- plot_Tmax_opt(data_case) +
  plot_Tmax <- plot_Tmax_stat(data_case) +
  # plot_Tmax <- plot_Tmax_diff(data_case) +
    geom_text(aes(label=Tmax_diff))+
    # scale_y_continuous(trans='log10') +
    scale_x_continuous(trans='log10')

  plot_start <- plot_start_invest_result(data_case)+
    # scale_y_continuous(trans='log10') +
    scale_x_continuous(trans='log10')

  plot_value <- plot_value_result(data_case)+
    # scale_y_continuous(trans='log10') +
    geom_text(aes(label=round(rEVPI,digits=3)))+
    scale_x_continuous(trans='log10')

  plot_index <- ggarrange(plot_VOI, plot_Tmax,
                          plot_start,plot_value)

  plots[[index_case_study]] <- plot_index
}
plots
# plots[[1]]
# plots[[2]]
# #
# # plots[[3]]
# # plots[[4]]

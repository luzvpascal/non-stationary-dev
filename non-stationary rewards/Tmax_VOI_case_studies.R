source("non-stationary rewards/case studies parameters.R")
results <- list()
write_hmMDP <- FALSE
solve_hmMDP <- FALSE
# write_hmMDP <- TRUE
# solve_hmMDP <- TRUE
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
    res_voi <- voi_non_stationary_rewards(params)

    results_case_study$opt[k] <- res_voi$opt
    results_case_study$stat[k] <- res_voi$stat

    # solve complete POMDP ####
    #make the list of models for the technology
    models_tech <- list()
    params_fail <- params
    params_fail$pdev <- 0
    params_fail$p_idle_idle <- 1-params_fail$pdev

    models_tech[[1]] <- transition_function_P1(params$p_idle_idle)
    models_tech[[2]] <- transition_function_P1(params_fail$p_idle_idle)

    #make transition function times
    transition_times <- transition_function_times(params)

    #whole transition function
    models <- list()
    models[[1]] <- transition_function_states(params)
    models[[2]] <- transition_function_states(params_fail)

    #transition function momdp
    transition_momdp <- transition_hmMDP(models)
    #observation function momdp
    observation_momdp <- obs_hmMDP(models)

    #real dynamics
    tr_mdp_real <- models[[2]] #failure

    #solve POMDP
    solving_POMDP_non_stationary_rewards(models_tech,
                                         transition_times,
                                         params$initial_belief,
                                         res_voi$REW,
                                         write_hmMDP=write_hmMDP,
                                         solve_hmMDP=solve_hmMDP,
                                         paste0("res/pomdpx/non-stationary-rewards/", case_study_name, "_",k,".pomdpx"),
                                         paste0("res/policyx/non-stationary-rewards/", case_study_name, "_",k,".policyx"))
    #getting the Tmax
    alpha_vectors <- read_policyx2(paste0("res/policyx/non-stationary-rewards/", case_study_name, "_",k,".policyx")) #alpha vectors

    tab_ <- trajectory(state_prior= 1,
                       Tmax = params$horizon,
                       initial_belief_state = params$initial_belief,
                       tr_mdp = tr_mdp_real,
                       rew_mdp= res_voi$REW,
                       tr_momdp = transition_momdp,
                       obs_momdp = observation_momdp,
                       alpha_momdp = alpha_vectors,
                       disc = gamma,
                       optimal_policy = TRUE,
                       naive_policy = NA)

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
    output <- interp_policy2(params$initial_belief,
                             obs = 1,
                             alpha = alpha_vectors$vectors,
                             alpha_action = alpha_vectors$action,
                             alpha_obs = alpha_vectors$obs,
                             alpha_index = alpha_vectors$index)

    mean_value_opt <- output[[1]]
    results_case_study$V_opt[k] <- mean_value_opt

    # ## solve the approximate POMPD with constant rewards ####
    # solving_POMDP_non_stationary_rewards(models_tech,
    #                                      transition_times,
    #                                      params$initial_belief,
    #                                      res_voi$rew_sol,
    #                                      write_hmMDP=write_hmMDP,
    #                                      solve_hmMDP=solve_hmMDP,
    #                                      paste0("res/pomdpx/", case_study_name, "_",k,"_approx.pomdpx"),
    #                                      paste0("res/policyx/", case_study_name, "_",k,"_approx.policyx"))
    #
    # #getting the Tmax
    # alpha_vectors_approx <- read_policyx2(paste0("res/policyx/", case_study_name, "_",k,"_approx.policyx")) #alpha vectors
    #
    # tab_approx <- trajectory(state_prior= 1,
    #                    Tmax = horizon,
    #                    initial_belief_state = params$initial_belief,
    #                    tr_mdp = tr_mdp_real,
    #                    rew_mdp= res_voi$REW,
    #                    tr_momdp = transition_momdp,
    #                    obs_momdp = observation_momdp,
    #                    alpha_momdp = alpha_vectors_approx,
    #                    disc = gamma,
    #                    optimal_policy = TRUE,
    #                    naive_policy = NA)
    #
    # actions <- (tab_approx$data_output$action[-(horizon+1)]-1)
    # if (sum(actions)>0){
    #   r <- rle(actions)
    #   len <- r$lengths[r$values == 1] # Lengths of consecutive 1s
    #   starts <- cumsum(r$lengths)[r$values == 1] - len + 1    # Start positions of consecutive 1s
    # } else {
    #   len <- 0 # Lengths of consecutive 1s
    #   starts <- 0 # Start positions of consecutive 1s
    # }
    #
    # results_case_study$Tmax_stat[k] <- len
    # results_case_study$Tmax_stat_start[k] <- starts
    #
    # # evaluate the performance ####
    # sim_approx <- sim_mdp_momdp_policy(state_prior= 1,
    #                             Tmax = horizon+1,
    #                             initial_belief_state = params$initial_belief,
    #                             list_tr_mdp = models,
    #                             rew_mdp= res_voi$REW, #true non-stationary rewards
    #                             tr_momdp = transition_momdp,
    #                             obs_momdp = observation_momdp,
    #                             alpha_momdp = alpha_vectors_approx,#solution with approximated rewards
    #                             disc = gamma,
    #                             n_it = 1000,
    #                             optimal_policy = TRUE,
    #                             naive_policy = NA)
    #
    # mean_value_approx <- sim_approx[horizon]
    # results_case_study$V_stat[k] <- mean_value_approx
  }

  results[[case_study_name]] <- results_case_study

}
end <- Sys.time()
print(end-start)
#
  saveRDS(results, "case_studies_rewards.rds")
# #a
results <- readRDS("case_studies_rewards.rds")
plots <- list()
for (index_case_study in seq_along(CASE_STUDIES)){

  case_study_name <- names(CASE_STUDIES)[index_case_study]
  params_case_study <- CASE_STUDIES[[index_case_study]]
  data_case <- results[[case_study_name]]

  plot_VOI <- plot_voi_result(data_case,params_case_study)

  plot_Tmax <- plot_Tmax_opt(data_case,params_case_study)

  plot_start <- plot_start_invest_result(data_case,params_case_study)

  # plot_value <- plot_value_result(data_case,params_case_study)

  plot_index <- ggarrange(plot_VOI
                          ,
                          plot_Tmax
                          ,
                          plot_start
                          # ,
                          # plot_value
                          )

  plots[[index_case_study]] <- plot_index
}

plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]

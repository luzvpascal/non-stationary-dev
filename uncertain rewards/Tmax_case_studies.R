source("uncertain rewards/case studies parameters.R")
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

  results_case_study$Tmax_opt <- 0
  results_case_study$Tmax_opt_start <- 0

  for (k in seq(params_case_study$N_case_studies)){
    params <- params_case_study

    for (par_name_index in seq_along(params_voi_names)){
      par_name <- params_voi_names[par_name_index]
      params[[par_name]] <- results_case_study[k,par_name_index]
    }

    outputs <- solving_uncertain_rewards_POMDP(params,
                                    write_hmMDP,
                                    solve_hmMDP,
                                    paste0("res/uncertain_rewards/pomdpx/", case_study_name, "_",k,".pomdpx"),
                                    paste0("res/uncertain_rewards/policyx/", case_study_name, "_",k,".policyx"))

    #getting the Tmax
    alpha_vectors <- read_policyx2( paste0("res/uncertain_rewards/policyx/", case_study_name, "_",k,".policyx")) #alpha vectors

    tab_ <- trajectory_uncertain_rewards(state_prior_tech=1,
                                         state_prior_rew=1,
                                         Tmax=Tmax_SIM,
                                         initial_belief_state_tech=outputs$B_PAR,
                                         initial_belief_state_rew=outputs$B_PAR_REW,
                                         transition_tech = outputs$TR_FUNCTION,
                                         transition_rew = outputs$TR_FUNCTION_REW,
                                         reward_list = outputs$REW,
                                         true_model_tech = outputs$TR_FUNCTION[[2]],#failure
                                         true_model_rew = outputs$TR_FUNCTION_REW[[2]],
                                         true_rew = outputs$REW[[2]],
                                         alpha_vectors,
                                         disc = gamma,
                                         optimal_policy = TRUE,
                                         naive_policy = NA,
                                         alpha_indexes=FALSE)


    actions <- (tab_$data_output$action[-(Tmax_SIM+1)]-1)
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
  }

  results[[case_study_name]] <- results_case_study

}
end <- Sys.time()
print(end-start)
#
saveRDS(results, "case_studies_uncertain_rewards.rds")
# #a
# results <- readRDS("case_studies_uncertain_rewards.rds")
plots <- list()
for (index_case_study in seq_along(CASE_STUDIES)){

  case_study_name <- names(CASE_STUDIES)[index_case_study]
  params_case_study <- CASE_STUDIES[[index_case_study]]
  data_case <- results[[case_study_name]]

  plot_Tmax <- plot_Tmax_opt(data_case,params_case_study)+
    geom_text(aes(label = Tmax_opt))

  plot_start <- plot_start_invest_result(data_case,params_case_study)

  # plot_value <- plot_value_result(data_case,params_case_study)

  plot_index <- ggarrange(
                          # plot_VOI
                          # ,
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

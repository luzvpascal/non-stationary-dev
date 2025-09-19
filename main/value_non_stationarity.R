source("main/case_studies_parameters.R")
write_hmMDP <- FALSE
solve_hmMDP <- FALSE
run_voi <- FALSE
# write_hmMDP <- TRUE
# solve_hmMDP <- TRUE
start <- Sys.time()
for (index_case_study in seq_along(CASE_STUDIES_VALUE_NON_STAT)[1]){
  results <- data.frame()
  mean_values <- c()

  case_study_name <- names(CASE_STUDIES_VALUE_NON_STAT)[index_case_study]
  params_case_study <- CASE_STUDIES_VALUE_NON_STAT[[index_case_study]]
  results_case_study <- params_case_study$case_studies
  params_voi_names <- names(results_case_study)

  for (k in seq(params_case_study$N_case_studies)){
    print(k)
    params <- params_case_study
    row <- results_case_study[k,]
    params <- update_params_from_row(params,row, params_voi_names)

    file_name <- paste0("res/uncertain_rewards_nonstat/pomdpx/", case_study_name,"_",k)

    ## value of information ####
    output <- voi_non_stationary_rewards(params,
                                         write_hmMDP,
                                         solve_hmMDP,
                                         run_voi,
                                         file_name)

    if (!is.data.frame(output)){
      print("buuuuuuug")
      break
    }


    ## extract reward trajectories
    REW <- reward_non_stationary_wrapper(params)

    df <- extract_trajectories(REW, params$horizon, case_study_name) %>%
      mutate(deltaR=Rdep_1-Rbau_1)%>%

      summarise(mean(deltaR))

    ## results ####
    results_now <- cbind(row,output)
    results_now$mean_deltaR <- unlist(unname(df[1]))
    results_now$case_study = case_study_name

    results <- rbind(results,
                     results_now)

    end <- Sys.time()
    print(end-start)
  }

  write.csv(results,
            paste0("res/value_non_stat_horizon_",
                   case_study_name,
                    "_",
                    params$horizon,
                    ".csv"), row.names = FALSE)
}

end <- Sys.time()
print(end-start)

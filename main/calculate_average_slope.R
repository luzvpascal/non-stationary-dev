source("main/case_studies_parameters.R")
start <- Sys.time()
for (index_case_study in seq_along(CASE_STUDIES_VALUE_NON_STAT)){

  case_study_name <- names(CASE_STUDIES_VALUE_NON_STAT)[index_case_study]
  params_case_study <- CASE_STUDIES_VALUE_NON_STAT[[index_case_study]]
  results_case_study <- params_case_study$case_studies
  params_voi_names <- names(results_case_study)

  results <- read.csv(
    paste0("res/value_non_stat_horizon_",
           case_study_name,
           "_",
           50,
           ".csv"))

  results$average_slope <- 0

  for (k in seq(params_case_study$N_case_studies)){
    print(k)
    params <- params_case_study
    row <- results_case_study[k,]
    params <- update_params_from_row(params,row, params_voi_names)

    REW <- reward_non_stationary_wrapper(params)

    df <- extract_trajectories(REW, params$horizon, case_study_name) %>%
      mutate(deltaR=Rdep_1-Rbau_1)

    mean_slope <- mean(df$deltaR-lag(df$deltaR), na.rm=TRUE)


    results$average_slope[k] <- mean_slope

    end <- Sys.time()
    print(end-start)
  }

  write.csv(results,
            paste0("res/value_non_stat_horizon_",
                   case_study_name,
                   "_",
                   50,
                   ".csv"), row.names = FALSE)
}

end <- Sys.time()
print(end-start)

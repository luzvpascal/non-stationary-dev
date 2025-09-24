source("main/case_studies_parameters.R")
# write_hmMDP <- FALSE
# solve_hmMDP <- FALSE
# run_voi <- FALSE
write_hmMDP <- TRUE
solve_hmMDP <- TRUE
run_voi <- TRUE
start <- Sys.time()
for (index_case_study in seq_along(CASE_STUDIES_VALUE_NON_STAT)){
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

    file_name_pomdpx <- paste0("res/uncertain_rewards_nonstat/pomdpx/", case_study_name,"_",k)
    file_name_policyx <- paste0("res/uncertain_rewards_nonstat/policyx/", case_study_name,"_",k)

    ## value of information ####
    output <- voi_non_stationary_rewards(params,
                                         write_hmMDP,
                                         solve_hmMDP,
                                         file_name_pomdpx,
                                         file_name_policyx,
                                         run_voi)
    ## results ####
    results_now <- merge(row,output)
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

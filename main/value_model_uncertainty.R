source("main/case_studies_parameters.R")
data_plots <- data.frame()
results <- data.frame()
plots <- list()
# write_hmMDP <- FALSE
# solve_hmMDP <- FALSE
# run_sims <- FALSE
write_hmMDP <- TRUE
solve_hmMDP <- TRUE
run_sims <- TRUE
start <- Sys.time()
for (index_case_study in seq_along(CASE_STUDIES_VALUE_MODEL_UNCERTAINTY)){

  case_study_name <- names(CASE_STUDIES_VALUE_NON_STAT)[index_case_study]
  params_case_study <- CASE_STUDIES_VALUE_NON_STAT[[index_case_study]]
  results_case_study <- params_case_study$case_studies
  params_voi_names <- names(results_case_study)

  for (k in seq(params_case_study$N_case_studies)){
    params <- params_case_study
    row <- results_case_study[k,]
    params <- update_params_from_row(params,row, params_voi_names)

    file_name <- paste0("res/uncertain_rewards_nonstat/pomdpx/", case_study_name,"_",k)

    ## value of information ####
    output <- voi_model_uncertainty_rewards(params,
                                           write_hmMDP,
                                           solve_hmMDP,
                                           file_name)

    if (!is.data.frame(output)){
      print("buuuuuuug")
      break
    }
    ## results ####
    results_now <- cbind(row,output)
    results_now$case_study = case_study_name

    results <- rbind(results,
                     results_now)

  }
}

end <- Sys.time()
print(end-start)

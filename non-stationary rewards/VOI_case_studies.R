source("non-stationary rewards/case studies parameters.R")
results <- list()

start <- Sys.time()
for (index_case_study in seq_along(CASE_STUDIES)){

  case_study_name <- names(CASE_STUDIES)[index_case_study]
  params_case_study <- CASE_STUDIES[[index_case_study]]
  results_case_study <- params_case_study$case_studies
  params_voi_names <- names(results_case_study)

  results_case_study$opt <- 0
  results_case_study$stat <- 0

  for (k in seq(params_case_study$N_case_studies)){
    params <- params_case_study
    for (par_name_index in seq_along(params_voi_names)){
      par_name <- params_voi_names[par_name_index]
      params[[par_name]] <- results_case_study[k,par_name_index]
    }
    res_voi <- voi_non_stationary_rewards(params, case_study_name)

    results_case_study$opt[k] <- res_voi$opt
    results_case_study$stat[k] <- res_voi$stat
  }

  results[[case_study_name]] <- results_case_study
}
end <- Sys.time()
print(end-start)

saveRDS(results, "case_studies_rewards.rds")

plots <- list()
for (index_case_study in seq_along(CASE_STUDIES)){

  case_study_name <- names(CASE_STUDIES)[index_case_study]
  data_case <- results[[case_study_name]]
  params_case_study <- CASE_STUDIES[[index_case_study]]

  plot_VOI <- plot_voi_result(data_case,params_case_study)

  plots[[index_case_study]] <- plot_VOI
}

plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]

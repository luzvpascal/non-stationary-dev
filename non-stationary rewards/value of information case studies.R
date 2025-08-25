source("non-stationary rewards/case studies parameters.R")
results <- list()

start <- Sys.time()
for (index_case_study in seq_along(CASE_STUDIES)[c(2,3)]){

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

    results_case_study$opt[k] <- res_voi[1]
    results_case_study$stat[k] <- res_voi[2]
  }

  results[[case_study_name]] <- results_case_study
}
end <- Sys.time()
print(end-start)
# results[["D"]] %>%
#   # mutate(EVPI = (opt-stat))%>%
#   mutate(EVPI = (opt-stat)/(opt))%>%
#   ggplot(aes(x=alpha, y=beta, fill=EVPI*100))+
#   # ggplot(aes(x=p_heat, y=p_cool, fill=EVPI*100))+
#   geom_raster(interpolate = TRUE)+
#   scale_fill_gradient(low="white",
#                       high="blue")+
#   labs(x = TeX("Slope change baseline rewards $\\alpha$"),
#        y = TeX("Slope change deployment rewards $\\beta$"),
#        fill = TeX("Value of modelling\nnon-stationary rewards")
#   )+
#   # geom_text(aes(label=round(EVPI*100)))+
#   coord_equal()

plotC <- plot_result(results[["C"]])
plotC
plotD <- plot_result(results[["D"]])
plotD
plotE <- plot_result(results[["E"]])
plotE+
 geom_text(aes(label=round(EVPI*100)))
# ggarrange(plotC,
#           plotD,
#           plotE,
#           common.legend = TRUE)

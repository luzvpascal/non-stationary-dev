source("main/case_studies_parameters.R")
copy_plot_params <- function(params) {
  plot_names <- grep("_plot$", names(params), value = TRUE)

  for (pn in plot_names) {
    new_name <- sub("_plot$", "", pn)
    params[[new_name]] <- params[[pn]]
  }

  return(params)
}

data_plots <- data.frame()
results <- data.frame()
plots <- list()
# write_hmMDP <- FALSE
# solve_hmMDP <- FALSE
# run_sims <- FALSE
y_Tmax <- 0.2
write_hmMDP <- TRUE
solve_hmMDP <- TRUE
run_sims <- TRUE
start <- Sys.time()
for (index_case_study in seq_along(CASE_STUDIES)){

  case_study_name <- names(CASE_STUDIES)[index_case_study]
  params_case_study <- CASE_STUDIES[[index_case_study]]
  results_case_study <- params_case_study$case_studies
  params_voi_names <- names(results_case_study)

  params <- params_case_study
  params <- copy_plot_params(params)

  outputs <- solving_uncertain_nonstat_rewards_POMDP(params,
                                                     write_hmMDP,
                                                     solve_hmMDP,
                                             paste0("res/uncertain_rewards_nonstat/pomdpx/", case_study_name, "_plot.pomdpx"),
                                             paste0("res/uncertain_rewards_nonstat/policyx/", case_study_name, "_plot.policyx"))

  #extract rewards trajectories
  df <- extract_trajectories(outputs$REW, params$horizon, case_study_name)
  df <- df %>%
    pivot_longer(!c(time, case_study), names_to = "Reward",
                 values_to = "value")

  data_plots <- rbind(data_plots, df)

  ## get the Tmax ####
  if (run_sims){
    sims_pomdpsim(100,
                10,
                paste0("res/uncertain_rewards_nonstat/pomdpx/", case_study_name, "_plot.pomdpx"),
                # paste0("res/uncertain_rewards_nonstat/pomdpx/", case_study_name, "_failure_plot.pomdpx"),
                paste0("res/uncertain_rewards_nonstat/policyx/", case_study_name, "_plot.policyx"),
                paste0("res/uncertain_rewards_nonstat/sim/", case_study_name, "_plot.txt"))
  }

  sims <- read_sims(paste0("res/uncertain_rewards_nonstat/sim/", case_study_name, "_plot.txt"))
  sims <- sims %>%
    filter(model==2)%>%
    filter(sim==min(sim))

  actions <- (sims$action[-(Tmax_SIM+1)]-1)
  if (sum(actions)>0){
    r <- rle(actions)
    len <- r$lengths[r$values == 1] # Lengths of consecutive 1s
    starts <- cumsum(r$lengths)[r$values == 1] - len   # Start positions of consecutive 1s
  } else {
    len <- 0 # Lengths of consecutive 1s
    starts <- 0 # Start positions of consecutive 1s
  }

  results_now <- data.frame(Tmax=len,
                            start_time=starts,
                            case_study = case_study_name)
  results <- rbind(results,
                   results_now)

  ## plots ####
  plot <- df %>%
    filter(Reward != "Rbau_2") %>%
    ggplot(aes(x = time, y = value, group = Reward, color = Reward,
               linetype=Reward)) +
    scale_color_manual(values=c("salmon",
                                "darkgreen",
                                "darkgreen"))+
    scale_linetype_manual(values = c("solid",
                              "dashed",
                              "dotted"
                              ))+
    geom_line(linewidth = 1) +
    geom_hline(yintercept = -0.1)+
    theme_bw() +
    theme(legend.position = "bottom") +
    # Add Tmax as a horizontal segment at y = 1.2
    geom_segment(
      data = results_now,
      aes(x = start_time, xend = start_time + Tmax-1,
          y = -y_Tmax, yend = -y_Tmax),
      inherit.aes = FALSE,  # prevents inheriting aes from df
      linewidth = 4, col = "darkblue"
    ) +
    geom_text(
      data = results_now,
      aes(x = start_time + Tmax, y = -y_Tmax, label = Tmax),
      inherit.aes = FALSE,
      # size=2,
      hjust = 0,
      col = "black"
    ) +
    lims(y=c(-0.35,1.05))+
    labs(x="Time",
         y="Reward value",
         title = params$label)+
    theme(title = element_text(size=10))+
    scale_y_continuous(breaks = c(-y_Tmax, 0, 0.5,1),
                       labels = c("Tmax", "0.0","0.5","1.0"))

  plots[[index_case_study]] <- plot

}

end <- Sys.time()
print(end-start)
main_plot <- ggarrange(plots[[1]],
          plots[[2]],
          plots[[3]],
          plots[[4]],
          plots[[5]],
          plots[[6]],
          ncol=3,
          nrow=2,
          common.legend = TRUE)
main_plot

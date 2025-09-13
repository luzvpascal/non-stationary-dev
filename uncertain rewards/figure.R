source("uncertain rewards/case studies parameters.R")
data_plots <- data.frame()
results <- data.frame()
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

  params <- params_case_study

  for (par_name_index in seq_along(params_voi_names)){
    par_name <- params_voi_names[par_name_index]
    par_name_plot <- paste0(par_name,"_plot")
    params[[par_name]] <- params[[par_name_plot]]
  }

  outputs <- solving_uncertain_rewards_POMDP(params,
                                             write_hmMDP,
                                             solve_hmMDP,
                                             paste0("res/uncertain_rewards/pomdpx/", case_study_name, "_plot.pomdpx"),
                                             paste0("res/uncertain_rewards/policyx/", case_study_name, "_plot.policyx"))
  # getting the parameters####

  data_now <- data.frame(Rbau=params$Rbau_plot,
                         Rdep1=params$Rdep1_0,
                         Rdep2=params$Rdep2_0,
                         time=seq(length(params$Rdep2_0)),
                         case_study=case_study_name)

  data_plots <- rbind(data_plots, data_now)

  #getting the Tmax
  alpha_vectors <- read_policyx2( paste0("res/uncertain_rewards/policyx/", case_study_name, "_plot.policyx")) #alpha vectors

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


  actions <- (tab_$data_output$action[-(horizon+1)]-1)
  if (sum(actions)>0){
    r <- rle(actions)
    len <- r$lengths[r$values == 1] # Lengths of consecutive 1s
    starts <- cumsum(r$lengths)[r$values == 1] - len + 1    # Start positions of consecutive 1s
  } else {
    len <- 0 # Lengths of consecutive 1s
    starts <- 0 # Start positions of consecutive 1s
  }

  results_now <- data.frame(Tmax=len,
                            start_time=starts,
                            case_study = case_study_name)
  results <- rbind(results,
                   results_now)

}
end <- Sys.time()
print(end-start)
## Rewards trajectories ####
data_plots <- data_plots %>%
  pivot_longer(!c(time, case_study), names_to = "Reward",
               values_to = "value")

plot <- data_plots%>%
  ggplot(aes(x=time, y=value, group = Reward, color=Reward))+
  geom_line(linewidth=1)+
  facet_wrap(~case_study,ncol=1)+
  theme_bw()+
  theme(legend.position = "bottom")

## Tmax trajectories ####
Tmax_plots <- ggplot(results) +
  geom_segment(aes(x = start_time, xend = start_time+Tmax-1,
                   y = 0, yend = 0), linewidth = 4, col="darkgreen") +
  facet_wrap(~case_study,ncol=1) +
  theme_bw() +
  labs(x = TeX("Development time limit $T_{max}$"), y = "")+
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )+
  lims(x=c(0, horizon),
       y=c(-0.2,0.2))+
  geom_text(aes(y = 0.1,
                x = start_time+Tmax-1, label=Tmax))

ggarrange(plot, Tmax_plots, align="hv")

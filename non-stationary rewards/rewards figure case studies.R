source("non-stationary rewards/case studies parameters.R")
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
  res_voi <- reward_non_stationary(params, case_study_name)

  Rbau_plot <- res_voi[seq(1, 2*(horizon-1), 2),1]
  Rdep_plot <- res_voi[seq(2, 2*(horizon-1), 2),2]

  data_now <- data.frame(Rbau=Rbau_plot,
                         Rdep=Rdep_plot,
                         time=seq(length(Rdep_plot)),
                         case_study=case_study_name)

  data_plots <- rbind(data_plots, data_now)

  ## VOI analysis ####
  res_voi <- voi_non_stationary_rewards(params, case_study_name)

  # solve complete POMDP ####
  #make the list of models for the technology
  models_tech <- list()
  params_fail <- params
  params_fail$pdev <- 0
  params_fail$p_idle_idle <- 1-params_fail$pdev

  models_tech[[1]] <- transition_function_P1(params$p_idle_idle)
  models_tech[[2]] <- transition_function_P1(params_fail$p_idle_idle)

  #make transition function times
  transition_times <- transition_function_times(params, case_study_name)

  #whole transition function
  models <- list()
  models[[1]] <- transition_function_states(params, case_study_name)
  models[[2]] <- transition_function_states(params_fail, case_study_name)
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
                                       paste0("res/non-stationary-rewards/pomdpx/", case_study_name, "_plot.pomdpx"),
                                       paste0("res/non-stationary-rewards/policyx/", case_study_name, "_plot.policyx"))
  #getting the Tmax
  alpha_vectors <- read_policyx2(paste0("res/non-stationary-rewards/policyx/", case_study_name, "_plot.policyx")) #alpha vectors

  tab_ <- trajectory(state_prior= 1,
                     Tmax = Tmax_SIM,
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

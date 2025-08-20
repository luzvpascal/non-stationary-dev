source("0 functions/POMDP/write_hmMDP_mcuams.R")
source("0 functions/POMDP/solving tech dev failure observation.R")
source("0 functions/simulations/simulations.R")
## TIME LIMIT TECH DEV ####
res <- data.frame()

## influence of initial belief ####
for (coeffDev_index in coeffs_costDev_heatmap[2]){
  print(coeffDev_index)
  costDev_P1_index <- coeffDev_index*V_max #might be even smaller
  for (value_noRD in coeffs_r_deploy){
    for (r_deploy in coeffs_r_deploy){
      ## setting the value of reward_POMDP####
      reward_POMDP <- matrix(c(value_noRD, value_noRD,value_noRD,
                               value_noRD-costDev_P1_index, r_deploy,value_noRD-costDev_P1_index), ncol=2)

      ## Solving POMDP with high transition between models####
      models <- solving_POMDP_failure_observation(prob_idle_idle,
                                                  prob_idle_idle_failure,
                                                   initial_belief_state_P1,
                                                   reward_POMDP,
                                                   solve_hmMDP,
                                                   file_pomdpx_index="test.pomdpx",
                                                   file_outpolicy_index="test.policyx")

      ## figure out Tmax ####
      all_models <-models$all_models

      alphas <- read_policyx2("test.policyx") #alpha vectors

      #transition function momdp
      transition_momdp <- transition_hmMDP(all_models)
      #observation function momdp
      observation_momdp <- obs_hmMDP(all_models)

      #real dynamics
      tr_mdp_real <-  array(c(diag(3),diag(3)), dim=c(3,3,2))

      tab_ <- trajectory(state_prior=1,
                         # Tmax = 10,
                         Tmax = Tmax_SIM,
                         initial_belief_state = initial_belief_state_P1,
                         tr_mdp = tr_mdp_real,
                         rew_mdp= reward_POMDP,#dont care
                         tr_momdp = transition_momdp,
                         obs_momdp = observation_momdp,
                         alpha_momdp = alphas,
                         disc = gamma,
                         optimal_policy = TRUE,
                         naive_policy = NA)

      results <- tab_$data_output %>%
        mutate(invest=if_else(action >=2, 1,0)) %>%
        group_by(invest) %>%
        summarize(max_invest=max(max(time+1)*invest)) %>%
        select(-invest) %>%
        filter(max_invest==max(max_invest))


      res <- rbind(res, data.frame(value_noRD=value_noRD,
                                   r_deploy = r_deploy,
                                   max_invest=results$max_invest[1],
                                   costDev_P1_index=costDev_P1_index))
    }
  }
}
write.csv(res,
          results_tmax_vs_rdeploy_obs_failure_file, row.names = FALSE)

##############
## FIGURE ####
##############

## map TMAX#####
res <- read.csv(results_tmax_vs_rdeploy_file)
res2 <- read.csv(results_tmax_vs_rdeploy_obs_failure_file)
names(res2)[3] <- "max_invest_obs_failure"

res <- merge(res, res2)
res <- res %>%
  mutate(cost = ifelse(costDev_P1_index==costDev_P1,
                       paste0(costDev_P1_index, ": GBR"),
                       as.character(costDev_P1_index))) %>%
  mutate(cost_fact= factor(costDev_P1_index,
                           levels=c('1e-04', '0.001', '0.01', '0.1'))) %>%
  filter(r_deploy-value_noRD >=-0.1
  )

tmax_vs_rdeploy <- res %>%
  filter(cost_fact == 0.001) %>%
  ggplot(aes(x = (r_deploy-value_noRD+costDev_P1_index)
             # ,color=cost_fact
  )) +
  geom_line(aes(y=max_invest_analytical,
                # linetype="Analytical approx."),
                color="Analytical approx."),
            linewidth=1.2)+
  geom_line(aes(y=max_invest,
                # linetype="Exact"),
                color="Exact solution\nno failure observable"),
            linewidth=1)+
  geom_line(aes(y=max_invest_obs_failure,
                # linetype="Exact\nnon-stationary pdev"),
                color="Exact solution\nfailure observable\npfail=0.05"),
            linewidth=1.2)+
  # geom_line(aes(y=max_invest_non_stat_low,
  #               # linetype="Exact\nnon-stationary pdev\nlow"),
  #               color="Exact non-stationary\npdev (100-year transition)"),
  #           linewidth=1.2)+
  # geom_point(aes(y=max_invest_non_stat))+
  # scale_linetype_manual(values = c(  "dotted","solid", "dashed"))+
  labs(x = TeX("Net benefits $(\\Delta_R)$"),
       y = TeX("Development time limit ($T_{max}$)"),
       color="",
       linetype="")+
  theme_bw()+
  # theme(
  #   title=element_text(size=20),
  #   panel.grid = element_blank(),
  #   axis.text=element_text(size=20),
  #   axis.title = element_text(size=20),
  #   legend.text=element_text(size=20))  +
  lims(x=c(-0.1,0.8))+
  scale_color_manual(values=c("black",
                              "orange",
                              "lightblue"
                              ))
# +
#   # scale_x_continuous(breaks = c(0,0.5,1)) +
#   scale_color_manual(values=c('dodgerblue1',
#                               'red',
#                               "dodgerblue4",
#                               "darkblue"))
tmax_vs_rdeploy


ggsave(results_tmax_vs_rdeploy_obs_failure_figure,
       plot = tmax_vs_rdeploy, width = 8, height =4, units = "in")

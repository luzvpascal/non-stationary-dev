source("0 functions/POMDP/write_hmMDP_non_stationary.R")
source("0 functions/POMDP/solving tech dev non stationary.R")
source("0 functions/simulations/simulations non stationary.R")
## TIME LIMIT TECH DEV ####
res1 <- data.frame()
res2 <- data.frame()

## influence of initial belief ####
for (coeffDev_index in coeffs_costDev_heatmap[2]){
  print(coeffDev_index)
  costDev_P1_index <- coeffDev_index*V_max #might be even smaller
  for (value_noRD in coeffs_r_deploy[coeffs_r_deploy>0.5]){
    for (r_deploy in coeffs_r_deploy){
      ## setting the value of reward_POMDP####
      reward_POMDP <- matrix(c(value_noRD, value_noRD,
                               value_noRD-costDev_P1_index, r_deploy), ncol=2)

      ## Solving POMDP 1 with high transition between models####
      prob_idle_idle_vector <- c(rep(1,10), prob_idle_idle)
      transition_rate <- 1
      initial_belief <- c(0.5, rep(0, length(prob_idle_idle_vector)-1), 0.5)
      models <- solving_POMDP_non_stationary(prob_idle_idle_vector,
                                             transition_rate,
                                             initial_belief,
                                             reward_POMDP,
                                             solve_hmMDP,
                                             file_pomdpx_index="test.pomdpx",
                                             file_outpolicy_index="test.policyx")

      ## figure out Tmax ####
      all_models <-models$all_models
      transition_between_models <- models$transition_between_models

      alphas <- read_policyx2("test.policyx") #alpha vectors

      #real dynamics
      tr_mdp_real <-  transition_function_P1(1)

      tab_ <- trajectory_non_stat(state_prior=1,
                         Tmax = Tmax_SIM,
                         initial_belief_state = initial_belief,
                         transition_ecosystem=all_models,
                         transition_models=transition_between_models,
                         true_transition_ecosystem=tr_mdp_real,
                         reward=reward_POMDP,
                         alpha_momdp=alphas)

      results <- tab_$data_output %>%
        mutate(invest=if_else(action >=2, 1,0)) %>%
        group_by(invest) %>%
        summarize(max_invest=max(max(time+1)*invest)) %>%
        select(-invest) %>%
        filter(max_invest==max(max_invest))


      res1 <- rbind(res1, data.frame(value_noRD=value_noRD,
                                   r_deploy = r_deploy,
                                   max_invest=results$max_invest[1],
                                   costDev_P1_index=costDev_P1_index))


      ## Solving POMDP 2 with high transition between models####
      #linear increase of pdev
      prob_idle_idle_vector <- c(seq(1,prob_idle_idle, -0.01))
      transition_rate <- 1
      initial_belief <- c(0.5, rep(0, length(prob_idle_idle_vector)-1), 0.5)
      models <- solving_POMDP_non_stationary(prob_idle_idle_vector,
                                             transition_rate,
                                             initial_belief,
                                             reward_POMDP,
                                             solve_hmMDP,
                                             file_pomdpx_index="test.pomdpx",
                                             file_outpolicy_index="test.policyx")

      ## figure out Tmax ####
      all_models <-models$all_models
      transition_between_models <- models$transition_between_models

      alphas <- read_policyx2("test.policyx") #alpha vectors

      #real dynamics
      tr_mdp_real <-  transition_function_P1(1)

      tab_ <- trajectory_non_stat(state_prior=1,
                                  Tmax = Tmax_SIM,
                                  initial_belief_state = initial_belief,
                                  transition_ecosystem=all_models,
                                  transition_models=transition_between_models,
                                  true_transition_ecosystem=tr_mdp_real,
                                  reward=reward_POMDP,
                                  alpha_momdp=alphas)

      results <- tab_$data_output %>%
        mutate(invest=if_else(action >=2, 1,0)) %>%
        group_by(invest) %>%
        summarize(max_invest=max(max(time+1)*invest)) %>%
        select(-invest) %>%
        filter(max_invest==max(max_invest))


      res2 <- rbind(res2, data.frame(value_noRD=value_noRD,
                                   r_deploy = r_deploy,
                                   max_invest=results$max_invest[1],
                                   costDev_P1_index=costDev_P1_index))
    }
  }
}

write.csv(res1,
          results_tmax_vs_rdeploy_non_stationary_file, row.names = FALSE)
write.csv(res2,
          results_tmax_vs_rdeploy_non_stationary_linear_file, row.names = FALSE)

############################
## FIGURE ####
##############

## map TMAX#####
res0 <- read.csv(results_tmax_vs_rdeploy_file)
res1 <- read.csv(results_tmax_vs_rdeploy_non_stationary_file)
res2 <- read.csv(results_tmax_vs_rdeploy_non_stationary_linear_file)
names(res1)[3] <- "max_invest_non_stat"
names(res2)[3] <- "max_invest_non_stat_linear"
# names(res2)[4] <- "max_invest_non_stat_low"

res <- left_join(res0, res1,
             by=c("value_noRD", "r_deploy","costDev_P1_index"))
res <- left_join(res, res2,
             by=c("value_noRD", "r_deploy","costDev_P1_index"))

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
                color="Exact solution\nstationary"),
            linewidth=1)+
  geom_line(aes(y=max_invest_non_stat,
                # linetype="Exact\nnon-stationary pdev"),
                color="Exact solution\nnon-stationary\n(10 investments required)"),
            linewidth=1.2)+
  geom_line(aes(y=max_invest_non_stat_linear,
                # linetype="Exact\nnon-stationary pdev"),
                color="Exact solution\nnon-stationary\n(linear increase of pdev)"),
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
                              "red",
                              "lightblue"
                              ))
# +
#   # scale_x_continuous(breaks = c(0,0.5,1)) +
#   scale_color_manual(values=c('dodgerblue1',
#                               'red',
#                               "dodgerblue4",
#                               "darkblue"))
tmax_vs_rdeploy


### pdev plot #####
data <- data.frame(pdev = 1-c(rep(1,11), rep(0.9,5), rep(0.9,15), seq(1,0.9,-0.01)),
                   time=c(seq(0,10),seq(10,14), seq(0,14), seq(0,10)),
                   scenario=c(rep("Non-stationary",16),
                              rep("Stationary",15),
                              rep("Non-stationary (linear increase)",11)))
data$scenario <- factor(data$scenario,
                           level = c("Stationary",
                                      "Non-stationary",
                                     "Non-stationary (linear increase)"))
plot_pdev <- data %>%
  ggplot(aes(x=time, y=pdev, color=scenario))+
  geom_line(linewidth=1.5)+
  geom_point(size=5)+
  theme_bw()+
  labs(x = "Number of R&D investments",
       y = TeX("Development probability ($p_{dev}$)"),
       color="",
       linetype="")+
  scale_color_manual(values=c("lightblue",
                              "orange",
                              "red"))

figure <- ggarrange(plot_pdev,
                    tmax_vs_rdeploy,
                    align = "hv",
                    labels=c("A","B"),
                    nrow=2)

ggsave(results_tmax_vs_rdeploy_non_stationary_figure,
       plot = figure, width = 10, height =6, units = "in")

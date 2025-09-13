#testing write_hmMDP_uncertain_rewards
pdev <- 0.1
p_idle_idle <- 1-pdev
Rbau <- 0.5
Rdep1 <- 1
Rdep2 <- 0
Rdep3 <- 0
Cdev <- 0.1
data_plots <- data.frame()
results <- data.frame()
## with the uncertainty about rewards ####
data_now <- data.frame(Rbau=rep(Rbau, 50),
                       Rdep1=rep(Rdep1, 50),
                       Rdep2=rep(Rdep2, 50),
                       time=seq(50),
                       case_study="Uncertainty in rewards")
data_now <- data_now %>%
  pivot_longer(!c(time, case_study), names_to = "Reward", values_to = "value")
data_plots <- rbind(data_plots, data_now)

TR_FUNCTION <- list(
                    transition_function_P1(p_idle_idle)
                    ,
                    transition_function_P1(1)
                    )
REW <- list(
            reward_instant(Rbau, Rdep1, Cdev)
            ,
            reward_instant(Rbau, Rdep2, Cdev)
            )

res <- transition_reward(REW, perfect_obs = TRUE)

TR_FUNCTION_REW <- res[[1]]
OBS <- res[[2]]

B_FULL <- c(1,0)
B_FULL_REW <- rep(0, length(OBS))
B_FULL_REW[which(OBS==Rbau)] <- 1

B_PAR <- rep(1, length(TR_FUNCTION))/length(TR_FUNCTION)

B_PAR_REW <- rep(1, length(TR_FUNCTION_REW))/length(TR_FUNCTION_REW)

write_hmMDP_uncertain_rewards(TR_FUNCTION,
                              TR_FUNCTION_REW,
                              B_FULL,
                              B_FULL_REW,
                              B_PAR,
                              B_PAR_REW,
                              REW,
                              GAMMA=gamma,
                              FILE="test.pomdpx")

path_to_sarsop <- system.file("bin/x64", "pomdpsol.exe", package="sarsop")

cmd <- paste(path_to_sarsop,
             "--precision", precision,
             "--timeout", timeout ,
             "--output", "test.policyx",
             "test.pomdpx",
             sep=" ")
system(cmd)

alpha_momdp <- read_policyx2("test.policyx")

tab_ <- trajectory_uncertain_rewards(state_prior_tech=1,
                                     state_prior_rew=1,
                                     Tmax=100,
                                     initial_belief_state_tech=B_PAR,
                                     initial_belief_state_rew=B_PAR_REW,
                                     transition_tech = TR_FUNCTION,
                                     transition_rew = TR_FUNCTION_REW,
                                     reward_list = REW,
                                     true_model_tech = TR_FUNCTION[[2]],#failure
                                     true_model_rew = TR_FUNCTION_REW[[2]],
                                     true_rew = REW[[2]],
                                     alpha_momdp,
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
                          case_study = "Uncertainty in rewards")
results <- rbind(results,
                 results_now)

## without uncertainty about rewards ####
data_now <- data.frame(Rbau=rep(Rbau, 50),
                       Rdep1=rep(Rdep1, 50),
                       time=seq(50),
                       case_study="Certainty in rewards")
data_now <- data_now %>%
  pivot_longer(!c(time, case_study), names_to = "Reward", values_to = "value")
data_plots <- rbind(data_plots, data_now)

TR_FUNCTION <- list(
  transition_function_P1(p_idle_idle)
  ,
  transition_function_P1(1)
)
REW <- list(
  reward_instant(Rbau, Rdep1, Cdev)
)

res <- transition_reward(REW, perfect_obs = TRUE)

TR_FUNCTION_REW <- res[[1]]
OBS <- res[[2]]

B_FULL <- c(1,0)
B_FULL_REW <- rep(0, length(OBS))
B_FULL_REW[which(OBS==Rbau)] <- 1

B_PAR <- rep(1, length(TR_FUNCTION))/length(TR_FUNCTION)

B_PAR_REW <- rep(1, length(TR_FUNCTION_REW))/length(TR_FUNCTION_REW)

write_hmMDP_uncertain_rewards(TR_FUNCTION,
                              TR_FUNCTION_REW,
                              B_FULL,
                              B_FULL_REW,
                              B_PAR,
                              B_PAR_REW,
                              REW,
                              GAMMA=gamma,
                              FILE="test.pomdpx")

path_to_sarsop <- system.file("bin/x64", "pomdpsol.exe", package="sarsop")

cmd <- paste(path_to_sarsop,
             "--precision", precision,
             "--timeout", timeout ,
             "--output", "test.policyx",
             "test.pomdpx",
             sep=" ")
system(cmd)

alpha_momdp <- read_policyx2("test.policyx")

tab_ <- trajectory_uncertain_rewards(state_prior_tech=1,
                                     state_prior_rew=1,
                                     Tmax=100,
                                     initial_belief_state_tech=B_PAR,
                                     initial_belief_state_rew=B_PAR_REW,
                                     transition_tech = TR_FUNCTION,
                                     transition_rew = TR_FUNCTION_REW,
                                     reward_list = REW,
                                     true_model_tech = TR_FUNCTION[[2]],#failure
                                     true_model_rew = TR_FUNCTION_REW[[1]],
                                     true_rew = REW[[1]],
                                     alpha_momdp,
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
                          case_study = "Certainty in rewards")
results <- rbind(results,
                 results_now)

################################################################################
## Figures ####

plot <- data_plots%>%
  ggplot(aes(x=time, y=value, group = Reward, linetype=Reward))+
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

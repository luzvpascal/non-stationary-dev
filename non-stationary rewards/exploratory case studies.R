#in this file we run 4 case studies
Rbau_0 = 1
Rdep_0 = 2
Cdev = 0.05
horizon = 20
times = seq(horizon)-1
pdev = 0.1
p_idle_idle = 1-pdev
initial_belief <- c(0.5,0.5)#success or failure

alphas = c(0, 0, 1/horizon, 1/horizon) #slope decline Rbau
betas =  c(0, -1/horizon, -1/horizon, 0) #slope decline Rdep
N_case_studies = length(alphas) #number of case studies

Tmax_solution <- c()
## plot rewards and solve POMDP
data_rewards = data.frame()
for (k in seq(N_case_studies)){
  alpha_k = alphas[k]
  beta_k = betas[k]

  Rbau = Rbau_time(Rbau_0,times,alpha_k)
  Rdep = Rdep_time(Rdep_0, times, beta_k)

  data_rewards_k <- data.frame(Rbau=Rbau,
                             Rdep=Rdep,
                             time=times,
                             case = k)

  data_rewards <- rbind(data_rewards,
                        data_rewards_k)

  # frame and solve the POMDP
  list_outputs <- solving_POMDP_non_stationary_rewards(p_idle_idle,
                                        Rbau_0,
                                        Rdep_0,
                                        alpha_k,
                                        beta_k,
                                        horizon,
                                        Cdev,
                                        initial_belief,
                                        write_hmMDP=FALSE,
                                        solve_hmMDP=FALSE,
                                        paste0("res/pomdpx/", alpha_k, "_",beta_k,".pomdpx"),
                                        paste0("res/policyx/", alpha_k, "_",beta_k,".policyx"))

  alpha_vectors <- read_policyx2(paste0("res/policyx/", alpha_k, "_",beta_k,".policyx")) #alpha vectors
  #transition function momdp
  transition_momdp <- transition_hmMDP(list_outputs$all_models)
  #observation function momdp
  observation_momdp <- obs_hmMDP(list_outputs$all_models)

  #real dynamics
  tr_mdp_real <-  list_outputs$all_models[[2]]

  tab_ <- trajectory(state_prior= 1,
                     Tmax = horizon,
                     initial_belief_state = initial_belief,
                     tr_mdp = tr_mdp_real,
                     rew_mdp= list_outputs$reward_POMDP,
                     tr_momdp = transition_momdp,
                     obs_momdp = observation_momdp,
                     alpha_momdp = alpha_vectors,
                     disc = gamma,
                     optimal_policy = TRUE,
                     naive_policy = NA)

  results_now <- tab_$data_output %>%
    mutate(invest=if_else(action >=2, 1,0)) %>%
    group_by(invest) %>%
    summarize(max_invest=max(max(time+1)*invest)) %>%
    select(-invest) %>%
    filter(max_invest==max(max_invest))

  Tmax_solution <- c(Tmax_solution, c(results_now[1]))
}
Tmax_solution
data_rewards %>%
  pivot_longer(!c(time,case), names_to = "rew", values_to = "val")%>%
  ggplot()+
  geom_line(aes(x=time, y=val, group=rew, col=rew))+#, linewidth = 1)+
  geom_point(aes(x=time, y=val, group=rew, col=rew))+
  facet_wrap(~case, ncol=1)+
  # ylim(c(-0.5,1))+
  theme_classic()+
  labs(y="Value",
       x="Time step",
       col="Reward")+
  theme(legend.position = "bottom")

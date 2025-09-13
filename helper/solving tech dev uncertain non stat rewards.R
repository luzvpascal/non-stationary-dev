solving_uncertain_nonstat_rewards_POMDP <- function(params,
                                            write_hmMDP,
                                            solve_hmMDP,
                                            file_pomdpx_index,
                                            file_outpolicy_index){
  #INPUTS:
  # p_idle_idle: probability that a technology stays idle
  # initial_belief: initial belief state: [probability feasible, probability infeasible]
  # reward_POMDP: reward function POMDP
  # solve_hmMDP: boolean: solve the POMDP
  # file_pomdpx_index: file.pomdpx
  # file_outpolicy_index: file.policyx

  #solves the POMDP for technology development

  #OUTPUTS:
  #list object:
  # all_models: possible models of technology development
  # initial_belief: initial_belief: initial belief state: [probability feasible, probability infeasible]

  ## transitions technology ####
  TR_FUNCTION <- list(transition_function_P1(params$p_idle_idle),
                      transition_function_P1(1))

  ## Reward functions ####
  REW <- reward_non_stationary_wrapper(params)

  # transition between rewards
  res <- transition_reward(REW)
  TR_FUNCTION_REW <- res[[1]]
  OBS <- res[[2]]

  ## transitions times ####
  TR_FUNCTION_TIMES <- transition_function_times(params)
  # priors ####
  B_FULL <- c(1,0) #initial state tech

  B_FULL_REW <- rep(0, length(OBS))
  B_FULL_REW[which(OBS==params$Rbau)] <- 1 #initial state reward (does not really m)

  B_FULL_TIMES <- rep(0, nrow(TR_FUNCTION_TIMES))
  B_FULL_TIMES[1] <- 1 #initial time step

  B_PAR <- rep(1, length(TR_FUNCTION))/length(TR_FUNCTION) #uniform prior on technology success or failure

  B_PAR_REW <- rep(1, length(TR_FUNCTION_REW))/length(TR_FUNCTION_REW) #uniform prior on reward trajectories

  if (write_hmMDP){
    write_hmMDP_uncertain_nonstat_rewards(TR_FUNCTION,
                                          TR_FUNCTION_REW,
                                          TR_FUNCTION_TIMES,
                                          B_FULL,
                                          B_FULL_REW,
                                          B_FULL_TIMES,
                                          B_PAR,
                                          B_PAR_REW,
                                          REW,
                                          GAMMA=gamma,
                                          FILE=file_pomdpx_index)
  }


  if (solve_hmMDP){
    path_to_sarsop <- system.file("bin/x64", "pomdpsol.exe", package="sarsop")

    cmd <- paste(path_to_sarsop,
                 "--precision", precision,
                 "--timeout", timeout ,
                 "--output", file_outpolicy_index,
                 file_pomdpx_index,
                 sep=" ")
    system(cmd)
  }

  return(list(TR_FUNCTION=TR_FUNCTION,
              TR_FUNCTION_REW=TR_FUNCTION_REW,
              TR_FUNCTION_TIMES=TR_FUNCTION_TIMES,
              B_FULL=B_FULL,
              B_FULL_REW=B_FULL_REW,
              B_FULL_TIMES=B_FULL_TIMES,
              B_PARB_PAR=B_PAR,
              B_PAR_REW=B_PAR_REW,
              REW=REW))
}

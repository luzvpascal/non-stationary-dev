solving_POMDP_non_stationary_rewards <- function(p_idle_idle_vector,
                                                  Rbau_0,
                                                  Rdep_0,
                                                  alpha,
                                                  beta,
                                                  horizon,
                                                  Cdev,
                                                  initial_belief,
                                                  write_hmMDP,
                                                  solve_hmMDP,
                                                  file_pomdpx_index,
                                                  file_outpolicy_index){
  #INPUTS:
  # p_idle_idle_vector: vector of probabilities that a technology stays idle e.g. c(0.9) for success and failure
  # Rbau linearly increases with slope alpha
  # Rdep linearly increases with slope beta
  # cost of development constant Cdev
  # horizon
  # initial_belief: initial belief state: [probability feasible, probability infeasible]
  # solve_hmMDP: boolean: solve the POMDP
  # file_pomdpx_index: file.pomdpx
  # file_outpolicy_index: file.policyx

  #solves the POMDP for technology development

  #OUTPUTS:
  #list object:
  # all_models: possible models of technology development
  # initial_belief: initial_belief: initial belief state: [probability feasible, probability infeasible]

  ## transitions ####
  all_models <- list()
  for (index_val in seq_along(p_idle_idle_vector)){
    all_models[[index_val]] <- transition_function_states(horizon,p_idle_idle_vector[index_val])
  }
  all_models[[length(p_idle_idle_vector)+1]] <- transition_function_states(horizon,1)#add failure

  ## rewards ####
  reward_POMDP <- reward_non_stationary(Rbau_0, Rdep_0, alpha, beta, horizon, Cdev)
  B_FULL <- rep(0, nrow(reward_POMDP))
  B_FULL[1] <- 1
  ## solve hidden model MDP####
  if (write_hmMDP){
    print("Writing POMDP")
    write_hmMDP(TR_FUNCTION = all_models,
                B_FULL = B_FULL,
                B_PAR = initial_belief,
                REW = reward_POMDP,
                GAMMA= gamma,
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

  return(list(all_models=all_models,
              initial_belief=initial_belief,
              reward_POMDP=reward_POMDP))
}

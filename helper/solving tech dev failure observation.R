transition_function_failure_observation <- function(p_idle_idle,
                                                    p_idle_idle_failure){

  # p_idle_idle: probability that a technology stays idle if success
  # p_idle_idle_failure: probability that a technology stays idle if failure

  #OUTPUTS:
  #list object:
  # all_models: possible models of technology development

  #state 1: idle, state2 ready, state3: failure observed

  #model do nothing
  do_nothing <- diag(3)
  #model success and invest:
  succes_invest <- matrix(c(p_idle_idle, 1-p_idle_idle, 0,
                           0,1,0,
                           0,0,1),ncol=3, byrow=TRUE)

  success_model <- array(c(do_nothing,succes_invest), dim=c(3,3,2))
  #model failure and invest:
  failure_invest <- matrix(c(p_idle_idle_failure, 0, 1-p_idle_idle_failure,
                           0,1,0,
                           0,0,1),ncol=3, byrow=TRUE)
  failure_model <- array(c(do_nothing,failure_invest), dim=c(3,3,2))

  return(list(success_model,failure_model))
}

solving_POMDP_failure_observation <- function(p_idle_idle,
                                              p_idle_idle_failure,
                                             initial_belief,
                                             reward_POMDP,
                                             solve_hmMDP,
                                             file_pomdpx_index,
                                             file_outpolicy_index){
  #INPUTS:
  # p_idle_idle: probability that a technology stays idle if success
  # p_idle_idle_failure: probability that a technology stays idle if failure
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

  ## transition function ####
  all_models <- transition_function_failure_observation(p_idle_idle,
                                                        p_idle_idle_failure)
  ## solve hidden model MDP####
  write_hmMDP(TR_FUNCTION = all_models,
             B_FULL = c(1,0,0),
             B_PAR = initial_belief,
             REW = reward_POMDP,
             GAMMA= gamma,
             FILE=file_pomdpx_index)
  path_to_sarsop <- system.file("bin/x64", "pomdpsol.exe", package="sarsop")

  cmd <- paste(path_to_sarsop,
               "--precision", precision,
               "--timeout", timeout ,
               "--output", file_outpolicy_index,
               file_pomdpx_index,
               sep=" ")

  if (solve_hmMDP){system(cmd)}

  return(list(all_models=all_models,
              initial_belief=initial_belief))
}

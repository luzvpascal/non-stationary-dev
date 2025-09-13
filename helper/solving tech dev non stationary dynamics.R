solving_POMDP_non_stationary_dynamics <- function(TR_FUNCTION,
                                                  TR_FUNCTION_MODELS,
                                                  TR_FUNCTION_TIMES,
                                                  initial_belief,
                                                  REW,
                                                  write_hmMDP,
                                                  solve_hmMDP,
                                                  file_pomdpx_index,
                                                  file_outpolicy_index){
  # TR_FUNCTION: list of arrays of size [s,s,a]
  # TR_FUNCTION_MODELS: transition function between models
  # TR_FUNCTION_TIMES: transition function between time steps
  # B_FULL: vector, probability distribution over the fully observable state(technologies)
  # B_FULL_TIME: vector, probability distribution over the fully observable states (time steps)
  # initial_belief:vector, probability distribtution over the non observable states (number of models) first for success states, second for failure
  # REW: matrix of size [s*t,a]
  # write_hmMDP: boolean: write the POMDP
  # solve_hmMDP: boolean: solve the POMDP
  # file_pomdpx_index: file.pomdpx
  # file_outpolicy_index: file.policyx

  ##############################################################################
  B_FULL <- c(1,0) #start with idle technology
  B_FULL_TIME <- rep(0, nrow(REW)/2)
  B_FULL_TIME[1] <- 1

  if (length(initial_belief)<length(TR_FUNCTION)){
    B_PAR <- rep(0, length(TR_FUNCTION))

    B_PAR[1] <- initial_belief[1]
    B_PAR[length(TR_FUNCTION)] <- initial_belief[2]
  } else {
    B_PAR <- initial_belief
  }
  ## WRITE hidden model MDP####
  if (write_hmMDP){
    print("Writing POMDP")

    write_hmMDP_non_stationary_dynamics(TR_FUNCTION,
                                  TR_FUNCTION_MODELS,
                                  TR_FUNCTION_TIMES,
                                  B_FULL,
                                  B_FULL_TIME,
                                  B_PAR,
                                  REW,
                                  GAMMA=gamma,
                                  file_pomdpx_index)
  }

  ## solve hidden model MDP####
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

  return(0)
}

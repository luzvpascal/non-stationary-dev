solving_POMDP_non_stationary_rewards <- function(TR_FUNCTION,
                                                  TR_FUNCTION_TIMES,
                                                  initial_belief,
                                                  REW,
                                                  write_hmMDP,
                                                  solve_hmMDP,
                                                  file_pomdpx_index,
                                                  file_outpolicy_index){
  #INPUTS:
  # TR_FUNCTION: vector of probabilities that a technology stays idle e.g. c(0.9) for success and failure
  # TR_FUNCTION_TIMES linearly increases with slope alpha
  # initial_belief: initial belief state: [probability feasible, probability infeasible]
  # write_hmMDP: boolean: write the POMDP
  # solve_hmMDP: boolean: solve the POMDP
  # file_pomdpx_index: file.pomdpx
  # file_outpolicy_index: file.policyx

  #solves the POMDP for technology development

  #OUTPUTS: 0
##############################################################################
  B_FULL_TIME <- rep(0, nrow(REW)/2)
  B_FULL_TIME[1] <- 1
  ## solve hidden model MDP####
  if (write_hmMDP){
    print("Writing POMDP")
    write_hmMDP_non_stationary_rewards(TR_FUNCTION,
                                        TR_FUNCTION_TIMES,
                                        B_FULL = c(1,0),
                                        B_FULL_TIME=B_FULL_TIME,
                                        B_PAR=initial_belief,
                                        REW=REW,
                                        GAMMA= gamma,
                                        FILE=file_pomdpx_index)
  }

  if (solve_hmMDP){
    sys_info <- Sys.info()
    os_name <- sys_info["sysname"]
    if (os_name=="Windows"){
      path_to_sarsop <- system.file("bin/x64", "pomdpsol.exe", package="sarsop")
    } else if (os_name=="Linux"){
      path_to_sarsop <- system.file("bin", "pomdpsol", package="sarsop")
    }
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

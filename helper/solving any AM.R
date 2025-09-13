solving_any_POMDP <- function(TR_FUNCTION,
                         REW,
                         initial_belief,
                         write_hmMDP,
                         solve_hmMDP,
                         file_pomdpx_index,
                         file_outpolicy_index){
  #INPUTS:
  # TR_FUNCTION: list of transition func
  # REW
  # initial_belief: initial belief state: [probability feasible, probability infeasible]
  # solve_hmMDP: boolean: solve the POMDP
  # file_pomdpx_index: file.pomdpx
  # file_outpolicy_index: file.policyx

  #solves the POMDP for technology development

  #OUTPUTS:
  #list object:
  # all_models: possible models of technology development
  # initial_belief: initial_belief: initial belief state: [probability feasible, probability infeasible

  B_FULL <- rep(0, nrow(REW))
  B_FULL[1] <- 1
  ## solve hidden model MDP####
  if (write_hmMDP){
    print("Writing POMDP")
    write_hmMDP(TR_FUNCTION = TR_FUNCTION,
                B_FULL = B_FULL,
                B_PAR = initial_belief,
                REW = REW,
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

  return(0) #if all good return 0
}

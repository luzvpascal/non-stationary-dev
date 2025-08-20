create_matrix_transition_between_models <- function(n, alpha) {
  #n is the number of models (including failure)
  #alpha the transition rate between models
  # e.g with 3 success models and a failure model
  # [1-alpha, alpha, 0, 0]
  # [0, 1-alpha, alpha, 0]
  # [0 , 0, 1, 0]
  # [0,0,0,1]

  mat <- matrix(0, n, n)  # Start with an n x n matrix of zeros
  # Set first and last rows
  mat[n, n] <- 1
  mat[n-1, n-1] <- 1

  # Set the main diagonal and the elements directly above and below it for middle rows
  if (n >=3){
    for (i in 1:(n - 2)) {
      mat[i, i] <- 1 - alpha           # Main diagonal
      mat[i, i + 1] <- alpha    # Above main diagonal
    }
  }
  return(mat)
}

solving_POMDP_non_stationary_dynamics <- function(p_idle_idle_vector,
                                         transition_rate,
                                         initial_belief,
                                        reward_POMDP,
                                        solve_hmMDP,
                                        file_pomdpx_index,
                                        file_outpolicy_index){
  #INPUTS:
  # p_idle_idle_vector: vector of probabilities that a technology stays idle
  # transition_rate: numeric: probability of p_idle_idle_vector[i] transitioning to p_idle_idle_vector[i+1]
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

  ## transitions P1 ####
  all_models <- list()
  for (index_val in seq_along(p_idle_idle_vector)){
    all_models[[index_val]] <- transition_function_P1(p_idle_idle_vector[index_val])
  }
  all_models[[length(p_idle_idle_vector)+1]] <- transition_function_P1(1)

  ## transition between models ####
  #note that the last model is the model "failure"
  transition_between_models_surrender <- create_matrix_transition_between_models(
    length(all_models),
    0
  )
  transition_between_models_invest <- create_matrix_transition_between_models(
    length(all_models),
    transition_rate
  )

  transition_between_models <- array(c(transition_between_models_surrender,
                                       transition_between_models_invest),
                                     dim=c(length(all_models),
                                           length(all_models),
                                           2))
  ## solve hidden model MDP####
  write_hmMDP_non_stationary(TR_FUNCTION = all_models,
                            TR_FUNCTION_MODELS = transition_between_models,
                            B_FULL = c(1,0),
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
              transition_between_models=transition_between_models,
              initial_belief=initial_belief))
}

read_sims <- function(file_path){
  # Read all lines
  lines <- readLines(file_path)

  # Identify simulation start lines
  sim_starts <- which(grepl("^>>> begin", lines))
  sim_starts <- c(sim_starts, length(lines) + 1)  # add end

  # Initialize empty list for data.frames
  all_sim_data <- list()

  for (i in seq_along(sim_starts[-length(sim_starts)])) {
    start_idx <- sim_starts[i]
    end_idx <- sim_starts[i+1] - 1
    sim_lines <- lines[start_idx:end_idx]

    # Stop at the first occurrence of "terminated"
    term_idx <- grep("^terminated", sim_lines)
    if (length(term_idx) > 0) {
      sim_lines <- sim_lines[1:(term_idx[1]-1)]
    }

    # Find indices of X, A, R, Y lines
    x_idx <- grep("^X\\s+:.*", sim_lines)
    a_idx <- grep("^A\\s+:.*", sim_lines)
    r_idx <- grep("^R\\s+:.*", sim_lines)
    y_idx <- grep("^Y\\s+:.*", sim_lines)

    # Extract X info: observed reward, observed state, time
    obs_reward <- unname(sapply(sim_lines[x_idx], function(line) {
      vals <- gsub("^X\\s+:\\s*\\(|\\)$", "", line)
      parts <- strsplit(vals, ",")[[1]]
      as.numeric(gsub("obs_rew", "", parts[1]))
    }))

    obs_state <- unname(sapply(sim_lines[x_idx], function(line) {
      vals <- gsub("^X\\s+:\\s*\\(|\\)$", "", line)
      parts <- strsplit(vals, ",")[[1]]
      as.numeric(gsub("obs_state", "", parts[2]))
    }))

    time_state <- unname(sapply(sim_lines[x_idx], function(line) {
      vals <- gsub("^X\\s+:\\s*\\(|\\)$", "", line)
      parts <- strsplit(vals, ",")[[1]]
      parts[3]  # keep as string
    }))

    # Extract actions
    actions <- unname(sapply(sim_lines[a_idx], function(line) {
      as.numeric(gsub("action", "", gsub("^A\\s+:\\s*\\(|\\)$", "", line)))
    }))

    # Extract rewards
    rewards <- unname(as.numeric(sapply(sim_lines[r_idx], function(line) {
      gsub("^R\\s+:\\s*", "", line)
    })))

    # Extract Y models and model rewards, convert model to numeric
    models <- unname(sapply(sim_lines[y_idx], function(line) {
      vals <- gsub("^Y\\s+:\\s*\\(|\\)$", "", line)
      parts <- strsplit(vals, ",")[[1]]
      as.numeric(gsub("model", "", parts[1]))  # numeric model
    }))

    model_rews <- unname(sapply(sim_lines[y_idx], function(line) {
      vals <- gsub("^Y\\s+:\\s*\\(|\\)$", "", line)
      parts <- strsplit(vals, ",")[[1]]
      as.numeric(gsub("model_rew", "", parts[2]))  # numeric
    }))

    # Combine into a data.frame
    n_steps <- length(obs_reward)
    df <- data.frame(
      sim = rep(i, n_steps),
      time = seq_len(n_steps)-1,
      observed_reward = obs_reward,
      observed_state = obs_state,
      reward = rewards,
      action = actions,
      model = models,
      model_rew = model_rews,
      time_state = time_state,
      stringsAsFactors = FALSE
    )

    all_sim_data[[i]] <- df
  }

  # Combine all simulations into one tidy data.frame and remove row names
  tidy_df <- do.call(rbind, all_sim_data)
  rownames(tidy_df) <- NULL
  return(tidy_df)
}

sims_pomdpsim <- function(simLen, simNum,  file_pomdpx_index, file_outpolicy_index,output_sims){
  path_to_sim <- system.file("bin/x64", "pomdpsim.exe", package="sarsop")

  cmd <- paste(path_to_sim,
               "--simLen", simLen,
               "--simNum", simNum,
               "--policy-file", file_outpolicy_index,
               "--output-file", output_sims,
                file_pomdpx_index,
               sep=" ")
  system(cmd)
}

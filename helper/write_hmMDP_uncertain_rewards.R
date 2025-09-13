write_hmMDP_uncertain_rewards <- function(TR_FUNCTION,
                                          TR_FUNCTION_REW,
                                          # TR_FUNCTION_TIMES,
                                          B_FULL,
                                          B_FULL_REW,
                                          # B_FULL_TIMES,
                                          B_PAR,
                                          B_PAR_REW,
                                          REW,
                                          GAMMA,
                                          FILE){
  # TR_FUNCTION: list of arrays of size [s,s,a] for the states (N_models=length(TR_FUNCTION))
  # TR_FUNCTION_REW: list of arrays of size [s, num_rew, a] (N_models_rew=length(TR_FUNCTION_REW))

  # TR_FUNCTION_TIMES: transition function between time steps
  # B_FULL: vector, probability distribution over the fully observable state(technologies)
  # B_FULL_REW: vector, probability distribution over the fully observable state(technologies)

  # B_PAR:vector probability distribtution over the non observable states (number of models)
  # B_PAR_REW: vector probability distribtution over the non observable states (number of rew models)

  # REW: list of matrices of size [s*t,a]

  # GAMMA: number between 0 and 1, the discount factor
  # FILE: string, path to the pomdpx file

  Num_s <- dim(TR_FUNCTION[[1]])[1]
  Num_a <-dim(TR_FUNCTION[[1]])[3]
  # Num_times <- dim(TR_FUNCTION_TIMES)[1]
  Num_rew <- dim(TR_FUNCTION_REW[[1]])[2]

  Num_mod <- length(TR_FUNCTION)
  Num_mod_rew <- length(TR_FUNCTION_REW)

  STATES <- paste0("obs_state", 1:Num_s)
  # TIMES <- paste0("time_state", 1:Num_times)
  STATES_REW <- paste0("obs_rew", 1:Num_rew)

  ACTIONS <- paste0("action", 1:Num_a)
  MODELS <- paste0("model", 1:Num_mod)
  MODELS_REW <- paste0("model_rew", 1:Num_mod_rew)


  #build header ####
  header <- paste0("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n\n",
                   "<pomdpx version =\"1.0\" id=\"sample\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n      xsi:noNamespaceSchemaLocation=\"pomdpx.xsd\">\n\n",
                   "<Description>hmMDP model</Description>\n\n",
                   paste0("<Discount>",  GAMMA, "</Discount>"), "\n\n",
                   "<Variable>", "\n\n")

  header_state <- paste0("<StateVar vnamePrev=\"species_0\" vnameCurr=\"species_1\" fullyObs=\"true\">",
                         "\n", paste0("<ValueEnum>", paste0(STATES, collapse = " "),
                                      "</ValueEnum>"), "\n", "</StateVar>", "\n\n")

  # header_time <- paste0("<StateVar vnamePrev=\"times_0\" vnameCurr=\"times_1\" fullyObs=\"true\">",
  #                       "\n", paste0("<ValueEnum>", paste0(TIMES, collapse = " "),
  #                                    "</ValueEnum>"), "\n", "</StateVar>", "\n\n")

  header_state_rew <- paste0("<StateVar vnamePrev=\"rew_0\" vnameCurr=\"rew_1\" fullyObs=\"true\">",
                         "\n", paste0("<ValueEnum>", paste0(STATES_REW, collapse = " "),
                                      "</ValueEnum>"), "\n", "</StateVar>", "\n\n")

  header_model <- paste0("<StateVar vnamePrev=\"hidden_0\" vnameCurr=\"hidden_1\" fullyObs=\"false\">",
                         "\r\n", paste0("<ValueEnum>", paste0(MODELS, collapse =" "),"</ValueEnum>",
                                        "\r\n</StateVar>\r\n\n"))

  header_model_rew <- paste0("<StateVar vnamePrev=\"hidden_rew_0\" vnameCurr=\"hidden_rew_1\" fullyObs=\"false\">",
                         "\r\n", paste0("<ValueEnum>", paste0(MODELS_REW, collapse =" "),"</ValueEnum>",
                                        "\r\n</StateVar>\r\n\n"))

  header_action <- paste0("<ActionVar vname=\"action_control\">\r\n",
                          "<ValueEnum>", paste0(ACTIONS, collapse = " "),
                          "</ValueEnum>\r\n</ActionVar>\r\n\n")

  header_obs <- paste0("<ObsVar vname=\"obs\">\r\n
                           <ValueEnum> o </ValueEnum>",
                           "\r\n</ObsVar>\r\n\n",
                           " <RewardVar vname=\"reward_agent\" />\r\n</Variable>\r\n\n")

  header_belief <- paste0("<InitialStateBelief>\r\n\n",

                          "<CondProb>\r\n",
                          "<Var>species_0</Var>\r\n",
                          "<Parent>null</Parent>\r\n",
                          "<Parameter type=\"TBL\">\r\n",
                          "<Entry>\r\n",
                          "<Instance> - </Instance>\r\n",
                          "<ProbTable>", paste0(B_FULL, collapse = " "), "</ProbTable>\n",
                          "</Entry>\r\n",
                          "</Parameter>\r\n",
                          "</CondProb>\r\n\n",

                          # "<CondProb>\r\n",
                          # "<Var>times_0</Var>\r\n",
                          # "<Parent>null</Parent>\r\n",
                          # "<Parameter type=\"TBL\">\r\n",
                          # "<Entry>\r\n",
                          # "<Instance> - </Instance>\r\n",
                          # "<ProbTable>", paste0(B_FULL_TIME, collapse = " "), "</ProbTable>\n",
                          # "</Entry>\r\n",
                          # "</Parameter>\r\n",
                          # "</CondProb>\r\n\n",

                          "<CondProb>\r\n",
                          "<Var>rew_0</Var>\r\n",
                          "<Parent>null</Parent>\r\n",
                          "<Parameter type=\"TBL\">\r\n",
                          "<Entry>\r\n",
                          "<Instance> - </Instance>\r\n",
                          "<ProbTable>", paste0(B_FULL_REW, collapse = " "), "</ProbTable>\n",
                          "</Entry>\r\n",
                          "</Parameter>\r\n",
                          "</CondProb>\r\n\n",

                          "<CondProb>\r\n",
                          "<Var>hidden_0 </Var>\r\n",
                          "<Parent>null</Parent>\r\n",
                          "<Parameter type=\"TBL\">\r\n",
                          "<Entry>\r\n",
                          "<Instance> - </Instance>\r\n",
                          "<ProbTable>", paste0(B_PAR, collapse = " "), "</ProbTable>\r\n",
                          "</Entry>\r\n",
                          "</Parameter>\r\n",
                          "</CondProb>\r\n\n",

                          "<CondProb>\r\n",
                          "<Var>hidden_rew_0</Var>\r\n",
                          "<Parent>null</Parent>\r\n",
                          "<Parameter type=\"TBL\">\r\n",
                          "<Entry>\r\n",
                          "<Instance> - </Instance>\r\n",
                          "<ProbTable>", paste0(B_PAR_REW, collapse = " "), "</ProbTable>\r\n",
                          "</Entry>\r\n",
                          "</Parameter>\r\n",
                          "</CondProb>\r\n\n",

                          "</InitialStateBelief>\r\n\n\n"
                          , sep = "")

  header <- paste0(header,
                   # header_time,
                   header_state_rew,
                   header_state,
                   header_model_rew,
                   header_model,
                   header_action,
                   header_obs,
                   header_belief)

  # build transition matrices for the reefs####
  mod_tr_header = paste(
    "<StateTransitionFunction>\r\n\n",
    "<CondProb>\r\n",
    "<Var>species_1</Var>\r\n",
    "<Parent>action_control hidden_0 species_0</Parent>\r\n",
    "<Parameter type=\"TBL\">\r\n")

  mod_tr_filling <- ""

  for (mod_id in seq(Num_mod)){
    model_text <- MODELS[mod_id]
    model_prob_values <- TR_FUNCTION[[mod_id]]
    mod_tr_filling_mod_id <- ""
    for (act_id in seq(Num_a)){
      action_text <- ACTIONS[act_id]
      action_prob_values <- model_prob_values[,,act_id]
      for (state0_id in seq(Num_s)){
        state0_text <- STATES[state0_id]
        for (state1_id in seq(Num_s)){
          state1_text <- STATES[state1_id]
          mod_tr_filling_mod_id <- paste(mod_tr_filling_mod_id,
                                         "<Entry>\r\n",
                                         "<Instance>",
                                         action_text, model_text, state0_text, state1_text,
                                         " </Instance>\r\n",
                                         "<ProbTable>",
                                         # as.character(round(action_prob_values[state0_id,state1_id], digits = 4)),
                                         as.character(action_prob_values[state0_id,state1_id]),
                                         "</ProbTable>\r\n",
                                         "</Entry>\r\n")
        }
      }

    }
    mod_tr_filling <- paste(mod_tr_filling, "\r\n", mod_tr_filling_mod_id)
  }
  mod_tr_end <- paste(
    "</Parameter>\r\n",
    "</CondProb>\r\n\n")

  mod_tr <- paste(mod_tr_header, mod_tr_filling, mod_tr_end)


  ## transition between time steps ####
  # tr_times <-paste("<CondProb>\r\n",
  #                   "<Var>times_1</Var>\r\n",
  #                   "<Parent>times_0</Parent>\r\n",
  #                   "<Parameter type=\"TBL\">\r\n")
  #
  #   for (time0_id in seq(Num_times)){
  #     time0_text <- TIMES[time0_id]
  #     for (time1_id in seq(Num_times)){
  #       time1_text <- TIMES[time1_id]
  #
  #       tr_times <- paste(tr_times,
  #                          "<Entry>\r\n",
  #                          "<Instance>",
  #                         time0_text, time1_text,
  #                          " </Instance>\r\n",
  #                          "<ProbTable>",
  #                          as.character(TR_FUNCTION_TIMES[time0_id,time1_id]),
  #                          "</ProbTable>\r\n",
  #                          "</Entry>\r\n")
  #     }
  #   }
  # tr_times <- paste(tr_times,
  #                    "</Parameter>\r\n",
  #                    "</CondProb>\r\n\n")
  ## transition between rewards ####
  mod_tr_header_rew <- paste(
    "<CondProb>\r\n",
    "<Var>rew_1</Var>\r\n",
    "<Parent>action_control hidden_rew_0 species_0</Parent>\r\n",
    "<Parameter type=\"TBL\">\r\n")

  mod_tr_filling_rew <- ""
  for (mod_rew_id in seq(Num_mod_rew)){
    model_text <- MODELS_REW[mod_rew_id]
    model_prob_values <- TR_FUNCTION_REW[[mod_rew_id]]
    mod_tr_filling_rew_mod_id <- ""

    for (act_id in seq(Num_a)){
      action_text <- ACTIONS[act_id]
      action_prob_values <- model_prob_values[,,act_id]

      for (state0_id in seq(Num_s)){
        state0_text <- STATES[state0_id]

        for (state_rew1_id in seq(Num_rew)){
          state_rew1_text <- STATES_REW[state_rew1_id]
          mod_tr_filling_rew_mod_id <- paste(mod_tr_filling_rew_mod_id,
                                         "<Entry>\r\n",
                                         "<Instance>",
                                         action_text, model_text, state0_text, state_rew1_text,
                                         " </Instance>\r\n",
                                         "<ProbTable>",
                                         as.character(action_prob_values[state0_id, state_rew1_id]),
                                         "</ProbTable>\r\n",
                                         "</Entry>\r\n")
        }
      }

    }
    mod_tr_filling_rew <- paste(mod_tr_filling_rew, "\r\n", mod_tr_filling_rew_mod_id)
  }
  mod_tr_end_rew  <- paste(
    "</Parameter>\r\n",
    "</CondProb>\r\n\n")

  mod_tr_rew <- paste(mod_tr_header_rew, mod_tr_filling_rew, mod_tr_end_rew)

  ## transition between models ####
  transition_between_models <- paste("<CondProb>\r\n",
                        "<Var>hidden_1</Var>\r\n",
                        "<Parent>hidden_0</Parent>\r\n",
                        "<Parameter type=\"TBL\">\r\n",
                        "<Entry>\r\n<Instance> - - </Instance>\r\n",
                        "<ProbTable>identity</ProbTable>\r\n",
                        "</Entry>\r\n",
                        "</Parameter>\r\n",
                        "</CondProb>\r\n\n")

  ## transition reward models ####
  transition_between_rew_models <- paste("<CondProb>\r\n",
                        "<Var>hidden_rew_1</Var>\r\n",
                        "<Parent>hidden_rew_0</Parent>\r\n",
                        "<Parameter type=\"TBL\">\r\n",
                        "<Entry>\r\n<Instance> - - </Instance>\r\n",
                        "<ProbTable>identity</ProbTable>\r\n",
                        "</Entry>\r\n",
                        "</Parameter>\r\n",
                        "</CondProb>\r\n\n")

  state_tr_end <- "</StateTransitionFunction>\r\n\n\n"

  state_tr <- paste(mod_tr,
                    # tr_times,
                    mod_tr_rew,
                    transition_between_models,
                    transition_between_rew_models,
                    state_tr_end)
  # build div for observations ####
  obs_header <- "<ObsFunction>\r\n\n"
  obs_fill <-paste(
    "<CondProb>\r\n",
    "<Var>obs</Var>\r\n",
    "<Parent>hidden_1</Parent>\r\n",
    "<Parameter type=\"TBL\">\r\n")
  for (mod_id in c(1:Num_mod)){
    model_text <- MODELS[mod_id]
    obs_fill_mod <- paste0(
      "<Entry>\r\n",
      "<Instance> ",
      model_text,
      " o</Instance>\r\n",
      "<ProbTable>1</ProbTable>\r\n",
      "</Entry>\r\n"
    )
    obs_fill <- paste(obs_fill, obs_fill_mod)
  }

  obs_end <-"</Parameter>\r\n</CondProb>\r\n</ObsFunction>\r\n\n\n"

  obs <- paste(obs_header, obs_fill, obs_end)


  # reward ####
  rew_header <- paste0(
    "<RewardFunction>\r\n",
    "<Func>\r\n",
    "<Var>reward_agent</Var>\r\n",
    "<Parent>action_control ",
    #times_0
    "hidden_rew_0 species_0</Parent>\r\n",
    "<Parameter type=\"TBL\">\r\n"
  )
  for (act_id in seq(Num_a)){
    action_text <- ACTIONS[act_id]
    # for (time0_id in seq(Num_times)){
    #   time0_text <- TIMES[time0_id]
      for (mod0_rew_id in seq(Num_mod_rew)){
        mod0_rew_text <- MODELS_REW[mod0_rew_id]
        for (state0_id in seq(Num_s)){
          state0_text <- STATES[state0_id]

          rew_header <- paste(rew_header,
                            "<Entry>\r\n",
                            "<Instance>",
                            action_text,
                            # time0_text,
                            mod0_rew_text,
                            state0_text,
                            " </Instance>\r\n",
                            "<ValueTable>",
                            as.character(REW[[mod0_rew_id]][state0_id,act_id]),
                            # as.character(REW[tuple_to_index(time0_id,state0_id),act_id]),
                            "</ValueTable>\r\n",
                            "</Entry>\r\n")
        }
      }
    }

  rew_end <- "</Parameter>\r\n
    </Func>\r\n
    </RewardFunction>\r\n\n\n"

  rew <- paste(rew_header, rew_end)
  # rew <- paste(rew_header, rew_fill, rew_end)

  end <- "</pomdpx>"

  # paste everything ####
  a = paste(header, state_tr, obs, rew, end, sep = '')
  writeLines(a,FILE)

}

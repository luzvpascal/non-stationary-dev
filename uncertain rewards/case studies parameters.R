horizon <- 50
pdev <- 0.1
p_idle_idle <- 1-pdev
initial_belief <-  c(0.5,0.5)
Cdev <- 0.1
## CASE STUDY A: constant rewards ####
PARAMS_A <- list()

PARAMS_A$Cdev = Cdev
PARAMS_A$horizon = horizon
PARAMS_A$pdev = pdev
PARAMS_A$p_idle_idle = p_idle_idle
PARAMS_A$initial_belief = initial_belief#success or failure
PARAMS_A$Rbau = 0.5
PARAMS_A$alpha = 0
PARAMS_A$beta = 0

PARAMS_A$Rdep_1s = seq(0, 1, 0.1)
PARAMS_A$Rdep_2s = seq(0, 1, 0.1)
PARAMS_A$case_studies = expand.grid(Rdep_1=PARAMS_A$Rdep_1s, Rdep_2=PARAMS_A$Rdep_2s)
# PARAMS_A$case_studies <- PARAMS_A$case_studies[which(PARAMS_A$case_studies$Rdep1>=PARAMS_A$case_studies$Rdep2),]#only if Rdep decline is slower than Rbau
PARAMS_A$N_case_studies = nrow(PARAMS_A$case_studies)

PARAMS_A$Rdep_plot = c(0,1) #constant
PARAMS_A$Rbau_plot = 0.5 #constant
PARAMS_A$log_scale_x = FALSE
PARAMS_A$log_scale_y = FALSE
PARAMS_A$type <- "linear"
## CASE STUDY B: constant rewards ####
PARAMS_B <- PARAMS_A
PARAMS_B$Rbau <- 0.5
## ALL CASE STUDIES ####
CASE_STUDIES <- list(
  "A" = PARAMS_A
  ,
  "B"=PARAMS_B
  ,
  "C"=PARAMS_C,
  "D"=PARAMS_D
  ,
  "E"=PARAMS_E,
  "F"=PARAMS_F
)

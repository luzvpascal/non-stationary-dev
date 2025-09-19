horizon <- 30
pdev <- 0.1
p_idle_idle <- 1-pdev
initial_belief <-  c(0.5,0.5)
Cdev <- 0.1
## CASE STUDY A: constant and known rewards ####
PARAMS_A <- list()

PARAMS_A$Cdev = Cdev
PARAMS_A$horizon = horizon
PARAMS_A$pdev = pdev
PARAMS_A$p_idle_idle = p_idle_idle
PARAMS_A$initial_belief = initial_belief#success or failure
PARAMS_A$alpha = 0
PARAMS_A$beta = 0

PARAMS_A$Rbaus = seq(0, 1, 0.1)
PARAMS_A$Rdeps = seq(0, 1, 0.1)
PARAMS_A$case_studies = expand.grid(Rbau=PARAMS_A$Rbaus, Rdep=PARAMS_A$Rdeps)
PARAMS_A$case_studies <- PARAMS_A$case_studies %>%
  filter(Rbau<=Rdep) #only if Rdep is larger than Rbau
PARAMS_A$N_case_studies = nrow(PARAMS_A$case_studies)

PARAMS_A$log_scale_x = FALSE
PARAMS_A$log_scale_y = FALSE
PARAMS_A$type <- "linear"
PARAMS_A$Rbau_plot <- 0.5
PARAMS_A$Rdep_plot <- 1
PARAMS_A$label <- "A. Baseline"
## CASE STUDY B: decreasing Rbau and Rdep ####
PARAMS_B <- list()

PARAMS_B$Cdev = Cdev
PARAMS_B$horizon = horizon
PARAMS_B$pdev = pdev
PARAMS_B$p_idle_idle = p_idle_idle
PARAMS_B$initial_belief = initial_belief

# parameter grid for alpha and beta
varying_vals <- c(-10^seq(0, -3))

PARAMS_B$case_studies = expand.grid(
  alpha = varying_vals,
  Rbau = seq(0.2, 1, 0.2),
  Rdep = seq(0, 1, 0.2),
  beta = varying_vals
)
PARAMS_B$N_case_studies = nrow(PARAMS_B$case_studies)

PARAMS_B$log_scale_x = FALSE
PARAMS_B$log_scale_y = FALSE
PARAMS_B$type <- "linear"

PARAMS_B$alpha_plot <- -0.025
PARAMS_B$beta_plot <- 0
PARAMS_B$Rbau_plot <- 0.5
PARAMS_B$Rdep_plot <- 1
PARAMS_B$label <- "B. Non-stationary rewards"

## CASE STUDY C: decreasing Rbau, increasing Rdep ####
PARAMS_C <- list()

PARAMS_C$Cdev = Cdev
PARAMS_C$horizon = horizon
PARAMS_C$pdev = pdev
PARAMS_C$p_idle_idle = p_idle_idle
PARAMS_C$initial_belief = initial_belief

# parameter grid for alpha and beta
varying_vals <- c(10^seq(0, -3))

PARAMS_C$case_studies = expand.grid(
  Rbau = seq(0.2, 1, 0.2),
  Rdep = seq(0, 1, 0.2),
  alpha = -varying_vals,
  beta = varying_vals
)
PARAMS_C$N_case_studies = nrow(PARAMS_C$case_studies)

PARAMS_C$log_scale_x = FALSE
PARAMS_C$log_scale_y = FALSE
PARAMS_C$type <- "linear"

PARAMS_C$alpha_plot <- 0
PARAMS_C$beta_plot <- 0.025
PARAMS_C$Rbau_plot <- 0.5
PARAMS_C$Rdep_plot <- 0
PARAMS_C$label <- "C. Non-stationary rewards"

## CASE STUDY D: constant but uncertain rewards ####
PARAMS_D <- list()

PARAMS_D$Cdev = Cdev
PARAMS_D$horizon = horizon
PARAMS_D$pdev = pdev
PARAMS_D$p_idle_idle = p_idle_idle
PARAMS_D$initial_belief = initial_belief#success or failure
PARAMS_D$Rbau = 0.5
PARAMS_D$alpha = 0
PARAMS_D$beta = 0

PARAMS_D$Rdep_1s = seq(0, 1, 0.1)
PARAMS_D$Rdep_2s = seq(0, 1, 0.1)
PARAMS_D$case_studies = expand.grid(Rdep_1=PARAMS_D$Rdep_1s, Rdep_2=PARAMS_D$Rdep_2s)
PARAMS_D$case_studies <- PARAMS_D$case_studies%>%
  filter(Rdep_1<=Rdep_2)
PARAMS_D$N_case_studies = nrow(PARAMS_D$case_studies)

PARAMS_D$log_scale_x = FALSE
PARAMS_D$log_scale_y = FALSE
PARAMS_D$type <- "linear"
PARAMS_D$Rdep_plot <- c(1,0)
PARAMS_D$label <- "D. Uncertain rewards"

## CASE STUDY E: decreasing but uncertain rewards ####
PARAMS_E <- list()

PARAMS_E$Cdev = Cdev
PARAMS_E$horizon = horizon
PARAMS_E$pdev = pdev
PARAMS_E$p_idle_idle = p_idle_idle
PARAMS_E$initial_belief = initial_belief
# PARAMS_E$Rdep = 1
PARAMS_E$Rbau = 0.5

# beta varies, alpha fixed
varying_vals <- seq(0,1,0.1)#for Rdep
# varying_vals <- c(-10^seq(0, -3), -10^seq(-1, -3) * 5) #for betas

# PARAMS_E$alpha = -0.01
PARAMS_E$alpha = -0.025
PARAMS_E$beta = 0
# PARAMS_E$beta_s = varying_vals
PARAMS_E$case_studies = expand.grid(
  Rdep_1 = varying_vals,
  Rdep_2  = varying_vals
)
PARAMS_E$case_studies <- PARAMS_E$case_studies%>%
  filter(Rdep_1>Rdep_2)

PARAMS_E$N_case_studies = nrow(PARAMS_E$case_studies)

PARAMS_E$log_scale_x = FALSE
PARAMS_E$log_scale_y = FALSE
PARAMS_E$type <- "linear"
PARAMS_E$Rdep_plot <- c(1,0)
# PARAMS_E$beta_plot <- c(-0.025,-0.01)
PARAMS_E$label <- "E. Uncertain non-stationary rewards"

## CASE STUDY F: decreasing Rbau, increasing but uncertain Rdep ####
PARAMS_F <- list()

PARAMS_F$Cdev = Cdev
PARAMS_F$horizon = horizon
PARAMS_F$pdev = pdev
PARAMS_F$p_idle_idle = p_idle_idle
PARAMS_F$initial_belief = initial_belief
PARAMS_F$Rdep = 0
PARAMS_F$Rbau = 0.5

# beta varies, alpha fixed
# varying_vals <- c(10^seq(0, -3), 10^seq(-1, -3) * 5)
varying_vals <- seq(0,1,0.1)#for Rdep

# PARAMS_F$alpha = -0.025
PARAMS_F$alpha = 0
# PARAMS_F$beta_s = varying_vals
PARAMS_F$beta = 0.025
# PARAMS_F$case_studies = expand.grid(
#   beta_1 = PARAMS_F$beta_s,
#   beta_2  = PARAMS_F$beta_s
# )
# PARAMS_F$case_studies <- PARAMS_F$case_studies%>%
#   filter(beta_1>beta_2)

PARAMS_F$case_studies = expand.grid(
  Rdep_1 = varying_vals,
  Rdep_2  = varying_vals
)
PARAMS_F$case_studies <- PARAMS_F$case_studies%>%
  filter(Rdep_1>Rdep_2)

PARAMS_F$N_case_studies = nrow(PARAMS_F$case_studies)

PARAMS_F$log_scale_x = FALSE
PARAMS_F$log_scale_y = FALSE
PARAMS_F$type <- "linear"
PARAMS_F$Rdep_plot <-  c(0,-0.5)
# PARAMS_F$beta_plot <-  c(0.025,0.01)
PARAMS_F$label <- "F. Uncertain non-stationary rewards"

## ALL CASE STUDIES ####
CASE_STUDIES <- list(
  "A"=PARAMS_A,
  "B"=PARAMS_B,
  "C"=PARAMS_C,
  "D"=PARAMS_D,
  "E"=PARAMS_E,
  "F"=PARAMS_F)

CASE_STUDIES_VALUE_NON_STAT <- list(
  "B"=PARAMS_B,
  "C"=PARAMS_C)

CASE_STUDIES_VALUE_MODEL_UNCERTAINTY <- list(
  "D"=PARAMS_D,
  "E"=PARAMS_E,
  "F"=PARAMS_F
  )

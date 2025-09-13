horizon <- 50
pdev <- 0.1
p_idle_idle <- 1-pdev
initial_belief <-  c(0.5,0.5)
Cdev <- 0.1
## CASE STUDY A: constant rewards ####
PARAMS_A <- list()

PARAMS_A$alpha = 0
PARAMS_A$beta = 0
PARAMS_A$Cdev = Cdev
PARAMS_A$horizon = horizon
PARAMS_A$pdev = pdev
PARAMS_A$p_idle_idle = p_idle_idle
PARAMS_A$initial_belief = initial_belief#success or failure

PARAMS_A$Rbaus = seq(0, 1, 0.2)
PARAMS_A$Rdeps = seq(0, 1, 0.2)
PARAMS_A$case_studies = expand.grid(Rbau=PARAMS_A$Rbaus, Rdep=PARAMS_A$Rdeps)
PARAMS_A$case_studies <- PARAMS_A$case_studies[which(PARAMS_A$case_studies$Rdep>PARAMS_A$case_studies$Rbau),]#only if Rdep decline is slower than Rbau
PARAMS_A$N_case_studies = nrow(PARAMS_A$case_studies)

PARAMS_A$Rbau_plot = 0.5 #constant
PARAMS_A$Rdep_plot = 1 #constant
PARAMS_A$Rdep_plot = 1 #constant
PARAMS_A$log_scale_x = FALSE
PARAMS_A$log_scale_y = FALSE
PARAMS_A$type <- "linear"

## CASE STUDY B: decline of Rbau, Rdep constant ####
PARAMS_B <- list()

PARAMS_B$Rbau = 0.5
PARAMS_B$Rdep = 1
PARAMS_B$Cdev = Cdev
PARAMS_B$horizon = horizon
PARAMS_B$pdev = pdev
PARAMS_B$p_idle_idle = p_idle_idle
PARAMS_B$initial_belief <- initial_belief#success or failure

PARAMS_B$alphas = c(-10**seq(0, -3), -10**seq(-1, -3)*5) #negative for the decline of Rbau
PARAMS_B$betas = 0 #positive for the increase of Rdep
PARAMS_B$case_studies = expand.grid(alpha=PARAMS_B$alphas, beta=PARAMS_B$betas)
PARAMS_B$N_case_studies = nrow(PARAMS_B$case_studies)

PARAMS_B$alpha_plot = -0.02 #negative for the decline of Rbau
PARAMS_B$beta_plot = 0 #negative for the decline of Rbau
PARAMS_B$log_scale_x = TRUE
PARAMS_B$log_scale_y = FALSE
PARAMS_B$type <- "linear"

## CASE STUDY C: both linearly decline ####
PARAMS_C <- list()

PARAMS_C$Rbau = 0.5
PARAMS_C$Rdep = 1
PARAMS_C$Cdev = Cdev
PARAMS_C$horizon = horizon
PARAMS_C$pdev = pdev
PARAMS_C$p_idle_idle = p_idle_idle
PARAMS_C$initial_belief <- initial_belief#success or failure

PARAMS_C$alphas = c(-10**seq(0, -3), -10**seq(-1, -3)*5) #negative for the decline of Rbau
PARAMS_C$betas = c(-10**seq(0, -3), -10**seq(-1, -3)*5) #positive for the increase of Rdep
PARAMS_C$case_studies = expand.grid(alpha=PARAMS_C$alphas, beta=PARAMS_C$betas)
PARAMS_C$N_case_studies = nrow(PARAMS_C$case_studies)

PARAMS_C$alpha_plot = -0.02 #negative for the decline of Rbau
PARAMS_C$beta_plot = -0.02 #negative for the decline of Rbau
PARAMS_C$log_scale_x = TRUE
PARAMS_C$log_scale_y = TRUE
PARAMS_C$type <- "linear"
## CASE STUDY D: technologies useful in the future but not now ####
PARAMS_D <- list()

PARAMS_D$Rbau = 1
PARAMS_D$Rdep = 0
PARAMS_D$Cdev = Cdev
PARAMS_D$horizon = horizon
PARAMS_D$pdev = pdev
PARAMS_D$p_idle_idle = p_idle_idle
PARAMS_D$initial_belief <- initial_belief#success or failure

PARAMS_D$alphas = c(-10**seq(0, -3), -10**seq(-1, -3)*5) #negative for the decline of Rbau
PARAMS_D$betas = c(10**seq(0, -3), 10**seq(-1, -3)*5) #positive for the increase of Rdep
PARAMS_D$case_studies = expand.grid(alpha=PARAMS_D$alphas, beta=PARAMS_D$betas)
PARAMS_D$N_case_studies = nrow(PARAMS_D$case_studies)

PARAMS_D$alpha_plot = -0.03 #negative for the decline of Rbau
PARAMS_D$beta_plot = 0.03 #negative for the decline of Rbau
PARAMS_D$log_scale_x = TRUE
PARAMS_D$log_scale_y = TRUE
PARAMS_D$type <- "linear"

## CASE STUDY E: random seasonality ####
PARAMS_E <- list()

PARAMS_E$Rbau = c(0, 1) #1 for heatwave, 1 for no-heatwave
PARAMS_E$Rdep = c(0.3, 0.8) #0.3 for heatwave, 0.8 for no-heatwave
PARAMS_E$Cdev = Cdev
PARAMS_E$horizon = 1000
PARAMS_E$pdev = pdev
PARAMS_E$p_idle_idle = p_idle_idle
PARAMS_E$initial_belief <- initial_belief#success or failure

PARAMS_E$p_heats = seq(0,1, length.out=21) #probabilities of staying in heatwave
PARAMS_E$p_cools =  seq(0,1, length.out=21) #probabilities of staying in no-heatwave
PARAMS_E$case_studies = expand.grid(p_heat=PARAMS_E$p_heats, p_cool=PARAMS_E$p_cools)
PARAMS_E$N_case_studies = nrow(PARAMS_E$case_studies)
PARAMS_E$type <- "season"

## CASE STUDY F: logistic decrease of both Rbau and Rdep ####
PARAMS_F <- list()

PARAMS_F$Rbau = 1
PARAMS_F$Rdep = 1
PARAMS_F$Cdev = Cdev
PARAMS_F$horizon = horizon
PARAMS_F$pdev = pdev
PARAMS_F$p_idle_idle = p_idle_idle
PARAMS_F$initial_belief <- initial_belief#success or failure

PARAMS_F$mu_bau <- 0.5#decline of Rbau
PARAMS_F$mu_dep <- 0.5#decline of Rdep

PARAMS_F$phi_baus = seq(0, horizon, 2) #negative for the decline of Rbau
PARAMS_F$phi_deps = seq(0, horizon, 2) #positive for the increase of Rdep

PARAMS_F$case_studies = expand.grid(phi_bau=PARAMS_F$phi_deps, phi_dep=PARAMS_F$phi_deps)
PARAMS_F$N_case_studies = nrow(PARAMS_F$case_studies)

PARAMS_F$phi_bau_plot = 10 #negative for the decline of Rbau
PARAMS_F$phi_dep_plot = 20 #positive for the increase of Rdep
PARAMS_F$type <- "logistic"
## CASE STUDY F: logistic decrease of both Rbau and Rdep ####
PARAMS_G <- PARAMS_F
PARAMS_G$mu_dep <- -PARAMS_F$mu_dep #increase of Rdep
PARAMS_G$phi_bau_plot = 15 #negative for the decline of Rbau
PARAMS_G$phi_dep_plot = 15 #positive for the increase of Rdep

## ALL CASE STUDIES ####
CASE_STUDIES <- list(
                    "A" = PARAMS_A,
                    "B"=PARAMS_B,
                    "C"=PARAMS_C,
                     "D"=PARAMS_D
                     # ,
                     # "E"=PARAMS_E,
                     # "F"=PARAMS_F,
                     # "G"=PARAMS_G
                     )


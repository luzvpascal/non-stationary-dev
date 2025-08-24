#case studies C to E
horizon <- 30
pdev <- 0.1
p_idle_idle <- 1-pdev
initial_belief <-  c(0.5,0.5)
## CASE STUDY C ####
PARAMS_C <- list()

PARAMS_C$Rbau_0 = 1
PARAMS_C$Rdep_0 = 1
PARAMS_C$Cdev = 0.05
PARAMS_C$horizon = horizon
PARAMS_C$pdev = pdev
PARAMS_C$p_idle_idle = p_idle_idle
PARAMS_C$initial_belief <- initial_belief#success or failure

PARAMS_C$alphas = seq(-1, 0, length.out=11) #negative for the decline of Rbau
PARAMS_C$betas =  seq(-1, 0, length.out=11) #negative for the decline of Rdep
PARAMS_C$N_case_studies = length(PARAMS_C$alphas)*length(PARAMS_C$betas) #number of case studies

## CASE STUDY D ####
PARAMS_D <- list()

PARAMS_D$Rbau_0 = 1
PARAMS_D$Rdep_0 = 0
PARAMS_D$Cdev = 0.05
PARAMS_D$horizon = horizon
PARAMS_D$pdev = pdev
PARAMS_D$p_idle_idle = p_idle_idle
PARAMS_D$initial_belief <- initial_belief#success or failure

PARAMS_D$alphas = seq(-1, 0, length.out=11) #negative for the decline of Rbau
PARAMS_D$betas =  seq(0, 1, length.out=11) #positive for the increase of Rdep
PARAMS_D$N_case_studies = length(PARAMS_D$alphas)*length(PARAMS_D$betas) #number of case studies

## CASE STUDY C ####
PARAMS_E <- list()

PARAMS_E$Rbau = c(0, 1) #1 for heatwave, 1 for no-heatwave
PARAMS_E$Rdep_0 = c(0.3, 0.8) #0.3 for heatwave, 0.8 for no-heatwave
PARAMS_E$Cdev = 0.05
PARAMS_E$horizon = horizon
PARAMS_E$pdev = pdev
PARAMS_E$p_idle_idle = p_idle_idle
PARAMS_E$initial_belief <- initial_belief#success or failure

PARAMS_E$p_heats = seq(0,1, length.out=11) #probabilities of staying in heatwave
PARAMS_E$p_cools =  seq(0,1, length.out=11) #probabilities of staying in no-heatwave
PARAMS_E$N_case_studies = length(PARAMS_E$p_heats)*length(PARAMS_E$p_cools) #number of case studies

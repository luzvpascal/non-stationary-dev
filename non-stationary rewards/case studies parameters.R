#case studies C to E
horizon <- 30
pdev <- 0.1
p_idle_idle <- 1-pdev
initial_belief <-  c(0.5,0.5)
Cdev <- 0.05
## CASE STUDY C ####
PARAMS_C <- list()

PARAMS_C$Rbau_0 = 1
PARAMS_C$Rdep_0 = 1
PARAMS_C$Cdev = Cdev
PARAMS_C$horizon = horizon
PARAMS_C$pdev = pdev
PARAMS_C$p_idle_idle = p_idle_idle
PARAMS_C$initial_belief <- initial_belief#success or failure

PARAMS_C$alphas = c(seq(-1, 0, length.out=51)) #negative for the decline of Rbau
# PARAMS_C$alphas = c(seq(-1, 0, length.out=21)) #negative for the decline of Rbau
# PARAMS_C$alphas = c(seq(-1, 0, length.out=11), -10**seq(-2,-5)) #negative for the decline of Rbau
PARAMS_C$betas =  c(seq(-0.5, 0, length.out=26)) #negative for the decline of Rdep
# PARAMS_C$betas =  c(seq(-1, 0, length.out=21)) #negative for the decline of Rdep
# PARAMS_C$betas =  c(seq(-1, 0, length.out=11), -10**seq(-2,-5)) #negative for the decline of Rdep
PARAMS_C$case_studies = expand.grid(alpha=PARAMS_C$alphas, beta=PARAMS_C$betas)
PARAMS_C$case_studies <- PARAMS_C$case_studies[which(PARAMS_C$case_studies$beta>PARAMS_C$case_studies$alpha),]#only if Rdep decline is slower than Rbau
PARAMS_C$N_case_studies = nrow(PARAMS_C$case_studies)

## CASE STUDY D ####
PARAMS_D <- list()

PARAMS_D$Rbau_0 = 1
PARAMS_D$Rdep_0 = 0
PARAMS_D$Cdev = Cdev
PARAMS_D$horizon = horizon
PARAMS_D$pdev = pdev
PARAMS_D$p_idle_idle = p_idle_idle
PARAMS_D$initial_belief <- initial_belief#success or failure

PARAMS_D$alphas = c(seq(-1, 0, length.out=51)) #negative for the decline of Rbau
# PARAMS_D$alphas = c(seq(-1, 0, length.out=11), -10**seq(-2,-5)) #negative for the decline of Rbau
PARAMS_D$betas = c(seq(0,1, length.out=51)) #positive for the increase of Rdep
# PARAMS_D$betas = c(seq(0,1, length.out=11), 10**seq(-2,-5)) #positive for the increase of Rdep
PARAMS_D$case_studies = expand.grid(alpha=PARAMS_D$alphas, beta=PARAMS_D$betas)
PARAMS_D$N_case_studies = nrow(PARAMS_D$case_studies)

## CASE STUDY C ####
PARAMS_E <- list()

PARAMS_E$Rbau = c(0, 1) #1 for heatwave, 1 for no-heatwave
PARAMS_E$Rdep_0 = c(0.3, 0.8) #0.3 for heatwave, 0.8 for no-heatwave
PARAMS_E$Cdev = Cdev
PARAMS_E$horizon = 1000
PARAMS_E$pdev = pdev
PARAMS_E$p_idle_idle = p_idle_idle
PARAMS_E$initial_belief <- initial_belief#success or failure

PARAMS_E$p_heats = seq(0,1, length.out=51) #probabilities of staying in heatwave
PARAMS_E$p_cools =  seq(0,1, length.out=51) #probabilities of staying in no-heatwave
PARAMS_E$case_studies = expand.grid(p_heat=PARAMS_E$p_heats, p_cool=PARAMS_E$p_cools)
PARAMS_E$N_case_studies = nrow(PARAMS_E$case_studies)

## ALL CASE STUDIES ####
CASE_STUDIES <- list(C=PARAMS_C,
                     D=PARAMS_D,
                     E=PARAMS_E)

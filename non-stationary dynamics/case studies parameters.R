#case studies C to E
horizon <- 30
initial_belief <-  c(0.5,0.5)
Cdev <- 0.01
## CASE STUDY C ####
PARAMS_A <- list()

PARAMS_A$Rbau_0 = 0.9
PARAMS_A$Rdep_0 = 1
PARAMS_A$Cdev = Cdev
PARAMS_A$horizon = horizon
PARAMS_A$initial_belief <- initial_belief#success or failure
PARAMS_A$alpha <- 0
PARAMS_A$beta <- 0
PARAMS_A$nus = c(10**seq(0, -2), 10**seq(-1, -2)*5) #positive for the increase of pdev
# PARAMS_A$alphas = c(seq(-1, 0, length.out=21)) #negative for the decline of Rbau
PARAMS_A$kmins = seq(0, horizon/2, 2) #positive for the increase of Rdep

PARAMS_A$case_studies = expand.grid(nu=PARAMS_A$nus, kmin=PARAMS_A$kmins)
PARAMS_A$N_case_studies = nrow(PARAMS_A$case_studies)

PARAMS_A$nu_plot = 0.05 #negative for the decline of Rbau
PARAMS_A$kmin_plot = horizon/2 #negative for the decline of Rbau

### CASE STUDIES ####
CASE_STUDIES <- list("A"=PARAMS_A)

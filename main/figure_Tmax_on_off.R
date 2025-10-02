source("main/case_studies_parameters.R")
results <- read.csv("res/value_non_stat_horizon_on_off_B_bis_50.csv")


results <- results %>%
  mutate(
         # voi_1=pmax(0,(value_non_stat-value_1)/value_non_stat)*100,
         # voi=pmax(0,(value_non_stat-value_stat)/value_non_stat)*100,
         # voi_avg=pmax(0,(value_non_stat-value_avg)/value_non_stat)*100,
         # voi_50=pmax(0,(value_non_stat-value_50)/value_non_stat)*100,
         # case_study_name = ifelse(case_study=="B",
         #                          "Both Rbau and Rdep decreasing",
         #                          "Rbau decreasing, Rdep increasing"),
         initial_slope = beta-alpha)

## RESULTS ####
results %>%
  ggplot(aes(x=as.factor(initial_slope),
             y=Rdep-Rbau,
             fill=Tmax))+
  geom_tile()+
  scale_fill_gradient(low="white",
                      high="purple")+
  geom_text(aes(label=Tmax))

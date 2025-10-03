source("main/case_studies_parameters.R")
# results <- read.csv("res/value_non_stat_horizon_on_off_B_bis_50.csv")
results <- read.csv("res/value_non_stat_horizon_on_off_B_50_0.01_VOI.csv")

results <- results %>%
  mutate(
         voi_1=pmax(0,(value_non_stat-value_1)/(value_non_stat+1))*100,
         # voi_1=pmax(value_non_stat-value_1,0),
         # voi=pmax(0,(value_non_stat-value_stat)),
         # voi=pmax(0,(value_non_stat-value_stat)/value_non_stat)*100,
         # voi_avg=pmax(0,(value_non_stat-value_avg)/value_non_stat)*100,
         # voi_50=pmax(0,(value_non_stat-value_50)/value_non_stat)*100,
         # case_study_name = ifelse(case_study=="B",
         #                          "Both Rbau and Rdep decreasing",
         #                          "Rbau decreasing, Rdep increasing"),
         on_off = Tmax_len >1,
         initial_slope = beta-alpha,
         deltaR=Rdep-Rbau)

## RESULTS ####
results %>%
  ggplot(aes(y=as.factor(initial_slope),
             x=deltaR,
             fill=Tmax))+
  geom_tile()+
  scale_fill_gradient(low="white",
                      high="purple")+
  geom_text(aes(label=Tmax))

results %>%
  ggplot(aes(y=as.factor(initial_slope),
             x=deltaR,
             fill=start))+
  geom_tile()+
  scale_fill_gradient(low="white",
                      high="red")+
  geom_text(aes(label=start))

results %>%
  ggplot(aes(y=as.factor(initial_slope),
             x=deltaR,
             fill=on_off))+
  geom_tile()+
  scale_fill_manual(values=c("blue",
                              "red"))

results %>%
  ggplot(aes(y=as.factor(initial_slope),
             x=deltaR,
             fill=voi_1))+
  geom_tile()+
  scale_fill_gradient2(low="white",
                       mid="red",
                      high="black",
                      midpoint = max(results$voi_1)/2)

results %>%
  # filter(Rdep>Rbau) %>%
  ggplot(aes(y=as.factor(initial_slope),
             x=deltaR,
             fill=Tmax-Tmax_1))+
  geom_tile()+
  scale_fill_gradient2(low="blue",
                       mid="white",
                       high="orange",
                       midpoint = 0)+
  geom_text(aes(label=Tmax-Tmax_1))


## Tmax mean delta R ####
results %>%
  ggplot(aes(x=mean_deltaR+Cdev, y=Tmax))+
  geom_line(aes(y=Tmax_avg,
                linetype="Stationary solution\nwith average\nrewards"),
            col="red",
            linewidth = 2)+
  geom_point()+
  labs(x=TeX("Average net benefit $(R_{dep}-R_{BAU}+C_{dev})$"),
       y=TeX("Maximum number of investments $(T_{max})$"),
       linetype=""
       # linetype="Stationary solution\nwith average\nrewards"
  )+
  theme_bw()

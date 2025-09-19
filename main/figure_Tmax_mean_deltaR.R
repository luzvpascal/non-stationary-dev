results <- read.csv(
  paste0("res/value_non_stat_horizon_",
         "B",
         "_",
         params$horizon,
         ".csv"))
results2 <- read.csv(
  paste0("res/value_non_stat_horizon_",
         "C",
         "_",
         params$horizon,
         ".csv"))

results <- rbind(results,
                 results2)
results <- results %>%
  group_by(Rbau,alpha, Rdep, beta,case_study)%>%
  reframe(Tmax=sum(Tmax),
            start = min(start),
            value_non_stat = min(value_non_stat),
            # value_stat = min(value_stat),
            # best_t_stat = min(best_t_stat ),
            # Tmax_stat = min(Tmax_stat),
            # start_stat = min(start_stat),
          mean_deltaR=min(mean_deltaR))

results %>%
  ggplot(aes(x=mean_deltaR, y=start,col=case_study))+
  # ggplot(aes(x=mean_deltaR, y=Tmax,col=case_study))+
  geom_point()

results %>%
  filter(mean_deltaR>=-0.5) %>%
  ggplot(aes(x=mean_deltaR, y=Tmax))+
  # ggplot(aes(x=mean_deltaR, y=Tmax,col=case_study,group=case_study))+
  # geom_smooth()+
  geom_point(aes(col=case_study))+
  labs(x=TeX("Average difference in rewards $(R_{dep}-R_{BAU})$"),
       y=TeX("Development time limit $(T_{max})$"))+
  theme_bw()


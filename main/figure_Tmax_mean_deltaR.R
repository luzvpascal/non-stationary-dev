results <- read.csv(
  paste0("res/value_non_stat_horizon_",
         "B",
         "_",
         50,
         ".csv"))
results2 <- read.csv(
  paste0("res/value_non_stat_horizon_",
         "C",
         "_",
          50,
         ".csv"))

results <- rbind(results,results2)

results <- results %>%
  group_by(Rbau,alpha, Rdep, beta,case_study)%>%
  reframe(Tmax=sum(Tmax),
            start = min(start),
            value_non_stat = min(value_non_stat),
            value_stat = min(value_stat),
            best_t_stat = min(best_t_stat ),
            Tmax_stat = min(Tmax_stat),
            start_stat = min(start_stat),
          mean_deltaR=min(mean_val),
          integral_gamma=min(integral_gamma),
          diff_max = min(diff_max ),
          end_deltaR=min(end_deltaR),
          Tmax_an=min(Tmax_an)) %>%
  mutate(deltaR0=Rdep-Rbau,
         slope=beta-alpha)

results %>%
  ggplot(aes(x=integral_gamma*(1-gamma)+Cdev, y=Tmax))+
  # ggplot(aes(x=mean_deltaR+Cdev, y=Tmax))+
  geom_point()+
  geom_line(aes(y=Tmax_an))+
  geom_line(aes(y=Tmax_average_rew), linetype="dashed")+
  # geom_line(aes(y=Tmax_stat), linetype="dotted")+
  labs(x=TeX("Average difference in rewards $(R_{dep}-R_{BAU})$"),
       y=TeX("Development time limit $(T_{max})$"))+
  theme_bw()

# value of non-stationarity ####
slopes <- as.character(sort(unique(results$slope)))
results %>%
  ungroup() %>%
  mutate(voi=pmax(0,(value_non_stat-value_stat)/value_non_stat)*100) %>%
  # mutate(voi=(value_non_stat-value_stat)*100/value_stat) %>%
  ggplot(aes(x=voi))+
  geom_histogram()+
  labs(x="Value of modelling non-stationarity (%)",
       y="Number of case studies")+
  theme_bw()

results %>%
  ungroup() %>%
  mutate(voi=pmax(0,(value_non_stat-value_stat)*100/value_non_stat)) %>%
  # mutate(voi=(value_non_stat-value_stat)*100/value_stat) %>%
  mutate(slope_factor=factor(slope, levels=slopes),
  # mutate(slope_factor=factor(average_slope, levels=slopes),
         deltaR0_factor=factor(deltaR0)) %>%
         # deltaR0_factor=factor(deltaR0)) %>%
  group_by(deltaR0_factor,slope_factor) %>%
  reframe(voi_avg=mean(voi),
          voi_sd = sd(voi)) %>%
  ggplot(aes(x=deltaR0_factor, y=slope_factor))+
  geom_tile(aes(fill=voi_avg))+
  geom_text(aes(label=paste(round(voi_avg, digits=1),
                            "±", round(voi_sd, digits=1))))+
  scale_fill_gradient(low="white",
                      high="orange")+
  labs(fill="Value of modelling\nnon-stationarity (%)",
       x=TeX("Initial difference in rewards $\\Delta R_0 = R_{dep,0}-R_{BAU,0}$"),
       y=TeX("Slope: variation of difference in rewards")
       )



results %>%
  ungroup() %>%
  mutate(voi=pmax(0,(value_non_stat-value_stat)*100/value_non_stat)) %>%
  ggplot(aes(x=integral_gamma*(1-gamma), y=average_slope))+
  geom_point(aes(col=voi))+
  scale_colour_gradient(low="green",
                      high="red")+
  labs(fill="Value of modelling\nnon-stationarity (%)",
       x=TeX("Initial difference in rewards $\\Delta R_0 = R_{dep,0}-R_{BAU,0}$"),
       y=TeX("Slope: variation of difference in rewards")
  )

results %>%
  ungroup() %>%
  mutate(voi=pmax(0,(value_non_stat-value_stat)*100/value_non_stat)) %>%
  ggplot(aes(y=Tmax-Tmax_stat, x=voi))+
  geom_point()

# histogram Tmax differences ####
results %>%
  ggplot(aes(x=Tmax-Tmax_stat))+
  geom_histogram()+
  theme_bw()+
  labs(x="Difference in development time (Tmax-Tmax_stat)",
       y="Number of case studies")

# Tmax differences ####
results %>%
  ungroup() %>%
  mutate(slope_factor=factor(slope, levels=slopes),
         deltaR0_factor=factor(deltaR0)) %>%
  group_by(deltaR0_factor,slope_factor) %>%
  # reframe(Tmax_avg=mean(Tmax),
  #         Tmax_sd = sd(Tmax),
  reframe(Tmax_avg=mean(Tmax-Tmax_stat),
          Tmax_sd = sd(Tmax-Tmax_stat),
          Tmax_stat_avg=mean(Tmax_stat),
          Tmax_stat_sd = sd(Tmax_stat)
          ) %>%
  ggplot(aes(x=deltaR0_factor, y=slope_factor))+
  geom_tile(aes(fill=Tmax_avg))+
  geom_text(aes(label=paste(round(Tmax_avg, digits=1),
                "±", round(Tmax_sd, digits=1))))+
  scale_fill_gradient2(low="darkred",
                      mid="white",
                      high="darkblue")+
  labs(x=TeX("Initial difference in rewards $\\Delta R_0 = R_{dep,0}-R_{BAU,0}$"),
       y=TeX("Slope: variation of difference in rewards"),
       fill="Difference in\ndevelopment time\n(Tmax-Tmax_stat)")

## starting investments ####
results %>%
  ungroup() %>%
  mutate(slope_factor=factor(slope, levels=slopes),
         deltaR0_factor=factor(deltaR0)) %>%
  group_by(deltaR0_factor,slope_factor) %>%
  # reframe(start_avg=mean(start),
  #         start_sd = sd(start)
  reframe(start_avg=mean(start),
          start_sd = sd(start)
  ) %>%
  ggplot(aes(x=deltaR0_factor, y=slope_factor))+
  geom_tile(aes(fill=start_avg))+
  geom_text(aes(label=paste(round(start_avg, digits=1),
                            "±", round(start_sd, digits=1))))+
  scale_fill_gradient2(low="darkred",
                       mid="white",
                       high="red")+
  labs(x=TeX("Initial difference in rewards $\\Delta R_0 = R_{dep,0}-R_{BAU,0}$"),
       y=TeX("Slope: variation of difference in rewards"),
       fill="Time when investments start")


results %>%
  ungroup() %>%
  mutate(voi=pmax(0,(value_non_stat-value_stat)/value_non_stat)*100) %>%
  # mutate(voi=(value_non_stat-value_stat)*100/value_stat) %>%
  ggplot(aes(x=start, y=voi))+
  geom_point()+
  labs(x=TeX("Start development"),
       y=TeX("Value of non-stationarity"))+
  theme_bw()

## start and Tmax ####
results %>%
  ungroup() %>%
  mutate(voi=pmax(0,(value_non_stat-value_stat)/value_stat)*100) %>%
  ggplot(aes(x=Tmax-Tmax_stat, y=start))+
  geom_point(aes(col=voi))

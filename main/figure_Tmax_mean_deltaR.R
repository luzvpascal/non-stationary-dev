source("main/case_studies_parameters.R")
results <- read.csv(
  paste0("res/value_non_stat_horizon_",
         "B",
         "_",
         50,
         "_old.csv"))
         # ".csv"))
results2 <- read.csv(
  paste0("res/value_non_stat_horizon_",
         "C",
         "_",
          50,
         "_old.csv"))
         # ".csv"))

results <- rbind(results,results2)

results <- results %>%
  # mutate(voi=pmax(0,(value_non_stat-value_stat)),
  mutate(voi_1=pmax(0,(value_non_stat-value_1)/value_non_stat)*100,
         voi=pmax(0,(value_non_stat-value_stat)/value_non_stat)*100,
         voi_avg=pmax(0,(value_non_stat-value_avg)/value_non_stat)*100,
         voi_50=pmax(0,(value_non_stat-value_50)/value_non_stat)*100,
         case_study_name = ifelse(case_study=="B",
                                  "Both Rbau and Rdep decreasing",
                                  "Rbau decreasing, Rdep increasing"),
         initial_slope = beta-alpha)

## Tmax mean delta R ####
results %>%
  ggplot(aes(x=mean_deltaR+Cdev, y=Tmax))+
  geom_line(aes(y=Tmax_avg, linetype="Stationary solution\nwith average\nrewards"))+
  geom_point()+
  labs(x=TeX("Average net benefit $(R_{dep}-R_{BAU}+C_{dev})$"),
       y=TeX("Maximum number of investments $(T_{max})$"),
       linetype=""
       # linetype="Stationary solution\nwith average\nrewards"
       )+
  theme_bw()

## Tmax integral delta R ####
results %>%
  ggplot(aes(x=integral_gamma+Cdev, y=Tmax))+
  geom_line(aes(y=Tmax_integral_an, linetype = "Analytical approx."))+
  geom_line(aes(y=Tmax_integral, linetype="Optimal"))+
  geom_point()+
  labs(x=TeX("Annualized net benefit $(R_{dep}-R_{BAU}+C_{dev})$"),
       y=TeX("Development time limit $(T_{max})$"),
       linetype="Stationary solution\nwith average\nrewards")+
  theme_bw()

#################################
## compared to start ############
#################################
results %>%
  ggplot(aes(x=Rdep-Rbau+Cdev, y=Tmax))+
  # ggplot(aes(x=mean_deltaR+Cdev, y=Tmax))+
  # geom_line(aes(y=Tmax_mean_an, linetype = "Analytical approx."))+
  # geom_line(aes(y=Tmax_avg, linetype="Stationary solution\nwith average\nrewards"))+
  geom_line(aes(y=Tmax_1, linetype="Stationary solution\nwith initial\nrewards"))+
  geom_point()+
  labs(x=TeX("Initial net benefit $(R_{dep}-R_{BAU}+C_{dev})$"),
       y=TeX("Maximum number of investments $(T_{max})$"),
       linetype=""
       # linetype="Stationary solution\nwith average\nrewards"
  )+
  theme_bw()

results %>%
  ggplot(aes(x=Tmax_1, y=Tmax))+
  geom_point()+
  geom_abline(slope=1, col="red")+
  theme_bw()+
  coord_equal()

results %>%
  ggplot(aes(x=Tmax-Tmax_1))+
  geom_histogram( boundary = 0,      # left edge at 0, 1, 2...
                  closed = "left",   # bins include left edge, exclude right
                  position = "dodge" # case_study groups side by side
                  )+
  theme_bw()

## what makes the Tmax increase or decrease
# slopes <- sort(unique(round(results$initial_slope, digits = 3)))
slopes <- sort(unique(round(results$average_slope, digits = 3)))

results <- results %>%
  ungroup() %>%
  mutate(
         # slope_round=round(initial_slope, digits=3),
         slope_round=round(average_slope, digits=3),
         slope_factor=factor(slope_round, levels=slopes),
         deltaR_round=Rdep-Rbau,
         deltaR_round_factor=factor(deltaR_round))

results %>%
  group_by(deltaR_round,slope_round) %>%
  reframe(Tmax_diff_avg=mean(Tmax-Tmax_1),
          Tmax_diff_sd = sd(Tmax-Tmax_1)) %>%
  ggplot(aes(x=deltaR_round+Cdev, y=slope_round))+
  geom_tile(aes(fill=Tmax_diff_avg))+
  scale_fill_gradient2(low="red",
                       mid="white",
                       high="blue",
                       midpoint = 0)+
  labs(fill="Average difference\n(Tmax-Tmax_init)",
       x=TeX("Initial net benefits $\\Delta R = R_{dep}-R_{BAU}+C_{dev}$"),
       y=TeX("Average slope (average variation of net benefits)")
  )

## value of keeping the same investment strategy #####
results %>%
  group_by(deltaR_round,slope_round) %>%
  reframe(voi_avg=mean(voi_1),
          voi_sd = sd(voi_1)) %>%
  ggplot(aes(x=deltaR_round+Cdev, y=slope_round))+
  geom_tile(aes(fill=voi_avg))+
  scale_fill_gradient(low="white",
                       high="darkred")+
  labs(fill="Average value\nof initial strategy (%)",
       x=TeX("Initial net benefits $\\Delta R = R_{dep}-R_{BAU}+C_{dev}$"),
       y=TeX("Average slope (average variation of net benefits)")
  )

## how often investments delayed ####
results %>%
  ggplot(aes(x=start, y=voi_1))+
  geom_point()+
  # geom_histogram()+
  facet_wrap(~case_study_name)

results %>%
  mutate(start_later=(start>0)) %>%
  group_by(deltaR_round,slope_round,start_later) %>%
  reframe(voi_avg=mean(voi_1),
          voi_sd = sd(voi_1)) %>%
  ggplot(aes(x=deltaR_round+Cdev, y=slope_round))+
  geom_tile(aes(fill=voi_avg))+
  scale_fill_gradient(low="white",
                      high="darkred")+
  labs(fill="Average value\nof initial strategy (%)",
       x=TeX("Initial net benefits $\\Delta R = R_{dep}-R_{BAU}+C_{dev}$"),
       y=TeX("Average slope (average variation of net benefits)")
  )+
  facet_wrap(~start_later)

#influence of Rbau0####
results %>%
  ggplot(aes(x=Rbau, y=voi_1))+
  geom_point()

results %>%
  group_by(Rbau,deltaR_round,slope_round) %>%
  reframe(voi_avg=mean(voi_1),
          voi_sd = sd(voi_1)) %>%
  ggplot(aes(x=Rbau, y=slope_round))+
  # ggplot(aes(x=deltaR_round+Cdev, y=slope_round))+
  geom_tile(aes(fill=voi_avg))+
  scale_fill_gradient(low="white",
                      high="darkred")+
  labs(fill="Average value\nof initial strategy (%)",
       x=TeX("Initial net benefits $\\Delta R = R_{dep}-R_{BAU}+C_{dev}$"),
       y=TeX("Average slope (average variation of net benefits)")
  )+
  facet_wrap(~deltaR_round)


###############################
## compared to end ############
###############################
results %>%
  ggplot(aes(x=end_deltaR+Cdev, y=Tmax))+
  geom_line(aes(y=Tmax_50, linetype="Stationary solution\nwith final\nrewards"))+
  geom_point()+
  labs(x=TeX("Final net benefit $(R_{dep}-R_{BAU}+C_{dev})$"),
       y=TeX("Maximum number of investments $(T_{max})$"),
       linetype=""
       # linetype="Stationary solution\nwith average\nrewards"
  )+
  theme_bw()

## Tmax compared to Tmax_50####
results %>%
  # ggplot(aes(x=Tmax_1, y=Tmax))+
  # ggplot(aes(x=Tmax_avg, y=Tmax))+
  ggplot(aes(x=Tmax_50, y=Tmax))+
  geom_point()+
  geom_abline(slope=1, col="red")+
  theme_bw()+
  coord_equal()

results %>%
  ggplot(aes(x=Tmax-Tmax_50))+
  geom_histogram( boundary = 0,      # left edge at 0, 1, 2...
                  closed = "left",   # bins include left edge, exclude right
                  position = "dodge" # case_study groups side by side
  )+
  theme_bw()

## what makes the Tmax increase or decrease
slopes <- sort(unique(round(results$average_slope, digits = 3)))

results <- results %>%
  ungroup() %>%
  mutate(
    slope_round=round(average_slope, digits=3),
    slope_factor=factor(slope_round, levels=slopes),
    # deltaR_round=Rdep-Rbau,
    deltaR_round=round(end_deltaR,digits=1),
    deltaR_round_factor=factor(deltaR_round))

results %>%
  group_by(deltaR_round,slope_round) %>%
  reframe(Tmax_diff_avg=mean(Tmax-Tmax_50),
          Tmax_diff_sd = sd(Tmax-Tmax_50)) %>%
  ggplot(aes(x=deltaR_round+Cdev, y=slope_round))+
  geom_tile(aes(fill=Tmax_diff_avg))+
  scale_fill_gradient2(low="red",
                       mid="white",
                       high="blue",
                       midpoint = 0)+
  labs(fill="Average difference\n(Tmax-Tmax_50)",
       x=TeX("Final net benefits $\\Delta R = R_{dep}-R_{BAU}+C_{dev}$"),
       y=TeX("Average slope (average variation of net benefits)")
  )

## value of keeping the same investment strategy #####
results %>%
  group_by(deltaR_round,slope_round) %>%
  reframe(voi_avg=mean(voi_50),
          voi_sd = sd(voi_50)) %>%
  ggplot(aes(x=deltaR_round+Cdev, y=slope_round))+
  geom_tile(aes(fill=voi_avg))+
  scale_fill_gradient(low="white",
                      high="darkred")+
  labs(fill="Average value\nof final strategy (%)",
       x=TeX("Final net benefits $\\Delta R = R_{dep}-R_{BAU}+C_{dev}$"),
       y=TeX("Average slope (average variation of net benefits)")
  )


## influence of the delayed investments ####
results %>%
  mutate(start_0=start==0) %>%
  # ggplot(aes(x=voi))+
  ggplot(aes(x=voi_50))+
  geom_histogram()+
  facet_wrap(~start_0)


###############################
# which one is better? average, start or final ?#####
a <- results %>%
  ggplot(aes(x=voi_50))+
  geom_histogram(
    aes(y = after_stat(count)*100 / tapply(after_stat(count), PANEL, sum)[PANEL]),
    binwidth = 0.5,      # size of bins
    boundary = 0,      # left edge at 0, 1, 2...
    closed = "left",   # bins include left edge, exclude right
    position = "dodge" # case_study groups side by side
  )
b <- results %>%
  ggplot(aes(x=voi_1))+
  geom_histogram(
    aes(y = after_stat(count)*100 / tapply(after_stat(count), PANEL, sum)[PANEL]),
    binwidth = 0.5,      # size of bins
    boundary = 0,      # left edge at 0, 1, 2...
    closed = "left",   # bins include left edge, exclude right
    position = "dodge" # case_study groups side by side
  )
c <- results %>%
  select(c("voi_1","voi_50","voi_avg")) %>%
  pivot_longer(c("voi_1","voi_50","voi_avg"),
               names_to = "VoI",
               values_to = "Value") %>%
  ggplot(aes(x=Value))+
  geom_histogram(
    aes(y = after_stat(count)*100 / tapply(after_stat(count), PANEL, sum)[PANEL]),
    binwidth = 5,      # size of bins
    boundary = 0,      # left edge at 0, 1, 2...
    closed = "left",   # bins include left edge, exclude right
    position = "dodge" # case_study groups side by side
  ) +
  facet_wrap(~VoI)
c
ggarrange(a,b,c,
          nrow = 1)

################################
# value of non-stationarity ####
################################
ggplot(results, aes(x = voi,fill="salmon")) +
  geom_histogram(
    aes(y = after_stat(count)*100 / tapply(after_stat(count), PANEL, sum)[PANEL]),
    # binwidth = 5,      # size of bins
    boundary = 0,      # left edge at 0, 1, 2...
    closed = "left",   # bins include left edge, exclude right
    position = "dodge" # case_study groups side by side
  ) +
  # facet_wrap(~case_study) +
  labs(x="Value of modelling non-stationarity (%)",
       y="Proportion of experiments (%)")+
  theme_bw()+
  facet_wrap(~case_study_name)

#grid ####
slopes <- sort(unique(round(results$average_slope, digits = 3)))
deltaR <- sort(unique(round(results$integral_gamma, digits = 1)))
# deltaR <- sort(unique(round(results$mean_deltaR, digits = 1)))

results <- results %>%
  ungroup() %>%
  mutate(slope_round=round(average_slope, digits=3),
         slope_factor=factor(slope_round, levels=slopes),
         # deltaR_round=round(integral_gamma, digits=1),
         deltaR_round=round(mean_deltaR, digits=1),
         deltaR_round_factor=factor(deltaR_round))

results %>%
  group_by(deltaR_round,slope_round,case_study_name) %>%
  reframe(voi_avg=mean(voi),
          voi_sd = sd(voi)) %>%
  ggplot(aes(x=deltaR_round+Cdev, y=slope_round))+
  geom_tile(aes(fill=voi_avg))+
  scale_fill_gradient2(low="green",
                       mid="blue",
                      high="red",
                      midpoint = 50)+
  labs(fill="Average value\nof modelling\nnon-stationarity (%)",
       x=TeX("Average net benefits $\\Delta R = R_{dep}-R_{BAU}+C_{dev}$"),
       y=TeX("Average slope (average variation of net benefits)")
       )+
  facet_wrap(~case_study_name)+
  theme_bw()



results %>%
  ungroup() %>%
  ggplot(aes(x=integral_gamma, y=average_slope))+
  geom_point(aes(col=voi))+
  scale_colour_gradient(low="white",
                      high="red")+
  labs(fill="Value of modelling\nnon-stationarity (%)",
       x=TeX("Initial difference in rewards $\\Delta R_0 = R_{dep,0}-R_{BAU,0}$"),
       y=TeX("Slope: variation of difference in rewards")
  )

results %>%
  ungroup() %>%
  ggplot(aes(y=Tmax-Tmax_1, x=voi))+
  # ggplot(aes(y=Tmax-Tmax_stat, x=voi))+
  geom_point()

# histogram Tmax differences ####
results %>%
  ggplot(aes(x=Tmax-Tmax_1))+
  # ggplot(aes(x=Tmax-Tmax_stat))+
  geom_histogram()+
  theme_bw()+
  labs(x="Difference in development time (Tmax-Tmax_stat)",
       y="Number of case studies")

# Tmax differences ####
results %>%
  ungroup() %>%
  group_by(deltaR_round_factor,slope_factor) %>%
  # reframe(Tmax_avg=mean(Tmax),
  #         Tmax_sd = sd(Tmax),
  reframe(Tmax_avg=mean(Tmax-Tmax_stat),
          Tmax_sd = sd(Tmax-Tmax_stat),
          Tmax_stat_avg=mean(Tmax_stat),
          Tmax_stat_sd = sd(Tmax_stat)
          ) %>%
  ggplot(aes(x=deltaR_round_factor, y=slope_factor))+
  geom_tile(aes(fill=Tmax_avg))+
  # geom_text(aes(label=paste(round(Tmax_avg, digits=1),
  #               "±", round(Tmax_sd, digits=1))))+
  scale_fill_gradient2(low="darkred",
                      mid="white",
                      high="darkblue")+
  labs(x=TeX("Average difference in rewards $\\Delta R = R_{dep}-R_{BAU}$"),
       y=TeX("Average slope"),
       fill="Difference in\ndevelopment time\n(Tmax-Tmax_stat)")

## starting investments ####
results %>%
  ungroup() %>%
  group_by(deltaR_round_factor,slope_factor) %>%
  reframe(start_avg=mean(start-start_stat),
          start_sd = sd(start-start_stat)
  ) %>%
  ggplot(aes(x=deltaR_round_factor, y=slope_factor))+
  geom_tile(aes(fill=start_avg))+
  # geom_text(aes(label=paste(round(start_avg, digits=1),
                            # "±", round(start_sd, digits=1))))+
  scale_fill_gradient2(low="darkred",
                       mid="white",
                       high="red")+
  labs(x=TeX("Initial difference in rewards $\\Delta R_0 = R_{dep,0}-R_{BAU,0}$"),
       y=TeX("Slope: variation of difference in rewards"),
       fill="Time when investments start")


results %>%
  ungroup() %>%
  ggplot(aes(x=start, y=voi))+
  geom_point()+
  labs(x="Start development",
       y="Value of non-stationarity")+
  theme_bw()

## value vs value_end ####
results %>%
  ggplot(aes(x=value_non_stat, y=value_avg))+
  geom_abline(slope=1)+
  geom_point()

results %>%
  ggplot(aes(x=Tmax, y=Tmax_avg))+
  geom_abline(slope=1)+
  geom_point()

results %>%
  # filter(case_study=="C") %>%
  mutate(start_0=start==0) %>%
  ggplot(aes(x=voi))+
  geom_histogram(
    aes(y = after_stat(count)*100 / tapply(after_stat(count), PANEL, sum)[PANEL]),
    binwidth = 0.5,      # size of bins
    boundary = 0,      # left edge at 0, 1, 2...
    closed = "left",   # bins include left edge, exclude right
    position = "dodge" # case_study groups side by side
  ) +
  facet_wrap(~start_0+case_study_name)

results %>%
  arrange(desc(voi)) %>%
  head()


results$Tm_Tstat <- results$Tmax-results$Tmax_1
results_data <- results %>%
  mutate(start_0=start==0) %>%
  select(c(
          "voi",
          # "alpha",
          # "beta",
          # "Rbau",
          # "Rdep",
          # "Tm_Tstat",
          "average_slope",
          "mean_deltaR"
          # ,
          # "start_0"
          ))
mdl <- glm(voi~., data=results_data)

summary(mdl)

library(ggplot2)
library(scales)

# define a "negative log10" transform
neg_log10_trans <- trans_new(
  name      = "neg_log10",
  transform = function(x) -log10(-x),   # map negatives to increasing log space
  inverse   = function(y) -10^(-y),     # invert correctly
  domain    = c(-Inf, 0)                # only works for negative values
)

# data <- read.csv("res/value_non_stat_horizon30.csv")
data <- read.csv("res/value_non_stat_horizon50.csv")

#calculate VOI
data <-data %>%
  mutate(voi=pmax(0,(value_non_stat-value_stat)/value_non_stat))

# case study B ####
# Tmax plots ####
data %>%
  filter(case_study=="B")%>%
  group_by(alpha, beta)%>%
  summarize(Tmax=sum(Tmax)) %>%
  ggplot(aes(x=abs(alpha), y=abs(beta), fill=Tmax))+
  scale_fill_gradient(low="white",high="blue")+
  geom_tile()+
  labs(x = TeX("Slope decline rewards without technology Rbau ($alpha$)"),
       y = TeX("Slope decline rewards with technology Rdep ($beta$)"))+
  # facet_wrap(~case_study)+
  scale_x_log10(breaks=c(0.001,0.01,0.1,1),
                labels=c("-0.001\nslow decline",-0.01,-0.1,"-1\nsharp decline"))+
  scale_y_log10(breaks=c(0.001,0.01,0.1,1),
                labels=c("slow decline -0.001",-0.01,-0.1,"sharp decline -1"))+
  geom_text(aes(label=Tmax))

# voi plots ####
data %>%
  filter(case_study=="B")%>%
  group_by(alpha, beta)%>%
  summarize(voi=mean(voi)) %>%
  ggplot(aes(x=abs(alpha), y=abs(beta), fill=voi*100))+
  scale_fill_gradient(low="white",high="red")+
  geom_tile()+
  labs(x = TeX("Slope decline rewards without technology Rbau ($alpha$)"),
       y = TeX("Slope decline rewards with technology Rdep ($beta$)"),
       fill = "Value of modelling\n non-stationarity (%)")+
  # facet_wrap(~case_study)+
  scale_x_log10(breaks=c(0.001,0.01,0.1,1),
                labels=c("-0.001\nslow decline",-0.01,-0.1,"-1\nsharp decline"))+
  scale_y_log10(breaks=c(0.001,0.01,0.1,1),
                labels=c("slow decline -0.001",-0.01,-0.1,"sharp decline -1"))+
  geom_text(aes(label=round(voi*100)))

# diff Tmax optimal and Tmax stat ####
data %>%
  filter(case_study=="B")%>%
  group_by(alpha, beta)%>%
  summarize(Tmax=sum(Tmax),
            Tmax_stat=mean(Tmax_stat)) %>%
  ggplot(aes(x=abs(alpha), y=abs(beta), fill=Tmax-Tmax_stat))+
  scale_fill_gradient(low="darkgreen",high="white")+
  geom_tile()+
  labs(x = TeX("Slope decline rewards without technology Rbau ($alpha$)"),
       y = TeX("Slope decline rewards with technology Rdep ($beta$)"),
       fill = "Difference between optimal\nand stationary Tmax\n(Tmax-Tmax_stat)")+
  # facet_wrap(~case_study)+
  scale_x_log10(breaks=c(0.001,0.01,0.1,1),
                labels=c("-0.001\nslow decline",-0.01,-0.1,"-1\nsharp decline"))+
  scale_y_log10(breaks=c(0.001,0.01,0.1,1),
                labels=c("slow decline -0.001",-0.01,-0.1,"sharp decline -1"))+
  geom_text(aes(label=Tmax-Tmax_stat))

## C plots ####
data %>%
  filter(case_study=="C")%>%
  group_by(alpha, beta)%>%
  summarize(Tmax=sum(Tmax)) %>%
  ggplot(aes(x=abs(alpha), y=abs(beta), fill=Tmax))+
  scale_fill_gradient(low="white",high="blue")+
  geom_tile()+
  labs(x = TeX("Slope decline rewards without technology ($alpha$)"),
       y = TeX("Slope increase rewards with technology ($beta$)"))+
  scale_x_log10(breaks=c(0.001,0.01,0.1,1),
                labels=c("-0.001\nslow decline",-0.01,-0.1,"-1\nsharp decline"))+
  scale_y_log10(breaks=c(0.001,0.01,0.1,1),
                labels=c("slow increase 0.001",0.01,0.1,"sharp increase 1"))+
  geom_text(aes(label=Tmax))
# voi plots ####
data %>%
  filter(case_study=="C")%>%
  group_by(alpha, beta)%>%
  summarize(voi=mean(voi)) %>%
  ggplot(aes(x=abs(alpha), y=abs(beta), fill=voi*100))+
  scale_fill_gradient(low="white",high="red")+
  geom_tile()+
  labs(x = TeX("Slope decline rewards without technology ($alpha$)"),
       y = TeX("Slope increase rewards with technology ($beta$)"),
       fill = "Value of modelling\n non-stationarity (%)")+
  scale_x_log10(breaks=c(0.001,0.01,0.1,1),
                labels=c("-0.001\nslow decline",-0.01,-0.1,"-1\nsharp decline"))+
  scale_y_log10(breaks=c(0.001,0.01,0.1,1),
                labels=c("slow increase 0.001",0.01,0.1,"sharp increase 1"))+
  geom_text(aes(label=round(voi*100)))
# diff Tmax optimal and Tmax stat ####
data %>%
  filter(case_study=="C")%>%
  group_by(alpha, beta)%>%
  summarize(Tmax=sum(Tmax),
            Tmax_stat=max(Tmax_stat)) %>%
  ggplot(aes(x=abs(alpha), y=abs(beta), fill=Tmax-Tmax_stat))+
  scale_fill_gradient(low="white",high="darkgreen")+
  geom_tile()+
  labs(x = TeX("Slope decline rewards without technology ($alpha$)"),
       y = TeX("Slope increase rewards with technology ($beta$)"))+
  scale_x_log10(breaks=c(0.001,0.01,0.1,1),
                labels=c("-0.001\nslow decline",-0.01,-0.1,"-1\nsharp decline"))+
  scale_y_log10(breaks=c(0.001,0.01,0.1,1),
                labels=c("slow increase 0.001",0.01,0.1,"sharp increase 1"))+
  geom_text(aes(label=Tmax-Tmax_stat))

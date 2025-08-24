# first test
#in this file we run 4 case studies
Rbau_0 = 1
Rdep_0 = 2
Cdev = 0.05
horizon = 20
times = seq(horizon)-1
pdev = 0.1
p_idle_idle = 1-pdev

# alphas = seq(-Rbau_0/horizon, Rbau_0/horizon, length.out=11) #slope decline Rbau
alphas = seq(-Rdep_0/horizon, Rdep_0/horizon, length.out=21) #slope decline Rdep
betas = seq(-Rdep_0/horizon, Rdep_0/horizon, length.out=21) #slope decline Rdep

results <- data.frame()

for (alpha in alphas){
  print(alpha)
  for (beta in betas){
    res_voi <- voi_non_stationary_rewards(p_idle_idle, Rbau_0, Rdep_0, alpha, beta, horizon, Cdev)

    res_now <- data.frame(alpha=alpha, beta=beta, opt=res_voi[1], stat = res_voi[2])
    results <- rbind(results, res_now)
  }
}
results %>%
  # mutate(EVPI = (opt-stat))%>%
  mutate(EVPI = (opt-stat)/(opt))%>%
  ggplot(aes(x=alpha, y=beta, fill=EVPI*100))+
  geom_raster(interpolate = TRUE)+
  scale_fill_gradient(low="white",
                    high="blue")+
  labs(x = TeX("Slope change baseline rewards $\\alpha$"),
       y = TeX("Slope change deployment rewards $\\beta$"),
       fill = TeX("Value of modelling\nnon-stationary rewards")
  )+
  geom_vline(xintercept = 0, col="red")+
  geom_hline(yintercept = 0, col="red")+
  geom_text(aes(label=round(EVPI*100)))+
  coord_equal()

# +
#   geom_contour(aes(z = valueAMinst),
#                binwidth = 0.1,
#                breaks = breaks_countour_AM,
#                colour = "black") +
#   metR::geom_text_contour(aes(z = valueAMinst),
#                           breaks = breaks_countour_AM,
#                           min.size = 0,
#                           skip=0,
#                           stroke = 0.1,
#                           rotate = FALSE,
#                           size = 5,
#                           fontface = "bold",
#                           label.placer = metR::label_placer_fraction(frac = 0.3))+
#   coord_equal()+
#   theme_bw()+
#   theme(
#     legend.position="bottom",
#     legend.title.position="top",
#     legend.key.height= unit(0.75, 'cm'),
#     legend.key.width= unit(1, 'cm'),
#     text=element_text(size=20),
#     title=element_text(size=12),
#     legend.title=element_text(size=18),
#     axis.title=element_text(size=15),
#     legend.title.align=0.5
#   )

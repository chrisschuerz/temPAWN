pwn

fdc_sim <- pwn$simulation$q %>%
  arrange(., desc(sim_max)) %>%
  mutate(p = seq(0,1, length.out = nrow(.)))

gg_fdc_sim <- ggplot(fdc_sim) +
  geom_smooth(aes(x = p, y = sim_min), method = loess) +
  geom_smooth(aes(x = p, y = sim_max), method = loess) +
  labs(y = "FDC") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())


fdc_sens <- fdc_sim %>%
  left_join(., pwn$sensitivity$q) %>%
  select(-date, -sim_min, -sim_max) %>%
  gather(., key = "Parameter", value = "val", -p)


gg_fdc_sens <- ggplot(fdc_sens) +
  geom_smooth(aes(x = p, y = val, col = Parameter), method = loess) +
  # geom_line(aes(x = p, y = val, col = par)) +
  theme(legend.position = "bottom") +
  labs(x = "Exceedance probability p", y = "Sensitivity") +
  theme_bw()


gg_fdc_sim / gg_fdc_sens + plot_layout(heights = c(2,3))

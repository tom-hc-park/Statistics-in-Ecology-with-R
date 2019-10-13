raft <- sal_temp_outplant  %>% 
  filter(b_r == "r")


Dr_plot <- ggplot(aes(x = outplant_time, y = temp), data = Dr) +
  geom_line(colour = "seagreen3", size = 1) +
  ylab("Temperature (˚C)") +
  xlab("Time since experiment start (weeks)") +
  theme_classic()+
  theme(axis.title = element_text(size = "20")) +
  theme(axis.text = element_text(size = "18"))

Dr_plot

r_plot_s <- ggplot(aes(x = outplant_time, y = sal), data = raft) +
  geom_point(colour = "seagreen3", size = 1) +
  ylab("Salinity (psu)") +
  xlab("Time since experiment start (weeks)") +
  theme_classic()+
  theme(axis.title = element_text(size = "20")) +
  theme(axis.text = element_text(size = "18")) +
  facet_wrap(~site)
r_plot_s

r_plot_t <- ggplot(aes(x = outplant_time, y = temp), data = raft) +
  geom_point(colour = "seagreen3", size = 1) +
  ylab("Temperature (˚C)") +
  xlab("Time since experiment start (weeks)") +
  theme_classic()+
  theme(axis.title = element_text(size = "20")) +
  theme(axis.text = element_text(size = "18")) +
  facet_wrap(~site)
r_plot_t

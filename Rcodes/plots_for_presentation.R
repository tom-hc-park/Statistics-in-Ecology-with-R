raft <- sal_temp_outplant  %>% 
  filter(b_r == "r")

Dr <- sal_temp_outplant  %>% 
  filter(site == "D", b_r == "r")

Cr <- sal_temp_outplant  %>% 
  filter(site == "C", b_r == "r")


Dr_plot_temp <- ggplot(aes(x = date, y = temp), data = Dr) +
  geom_line(colour = "seagreen3", size = 1) +
  ylab("Temperature (˚C)") +
  xlab("Time since experiment start (weeks)") +
  theme_classic()+
  theme(axis.title = element_text(size = "20")) +
  theme(axis.text = element_text(size = "18"))

Dr_plot_temp

Dr_plot_sal <- ggplot(aes(x = outplant_time, y = sal), data = Dr) +
  geom_line(colour = "seagreen2", size = 1) +
  ylab("Salinity (psu)") +
  xlab("Time since experiment start (weeks)") +
  theme_classic()+
  theme(axis.title = element_text(size = "20")) +
  theme(axis.text = element_text(size = "18"))

Dr_plot_sal

r_plot_s <- ggplot(aes(x = date, y = sal), data = raft) +
  geom_point(colour = "seagreen3", size = 1) +
  ylab("Salinity (psu)") +
  xlab("Time since experiment start (weeks)") +
  theme_classic()+
  theme(axis.title = element_text(size = "20")) +
  theme(axis.text = element_text(size = "18")) +
  facet_wrap(~site)
r_plot_s

r_plot_t <- ggplot(aes(x = date, y = temp), data = raft) +
  geom_point(colour = "seagreen3", size = 1) +
  ylab("Temperature (˚C)") +
  xlab("Time since experiment start (weeks)") +
  theme_classic()+
  theme(axis.title = element_text(size = "20")) +
  theme(axis.text = element_text(size = "18")) +
  facet_wrap(~site)
r_plot_t

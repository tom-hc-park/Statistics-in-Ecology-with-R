Dr <- sal_temp_outplant  %>% 
  filter(site == "D" & b_r == "r")

Dr_plot <- ggplot(aes(x = outplant_time, y = temp), data = Dr) +
  geom_line(colour = "seagreen3", size = 1) +
  ylab("Temperature (˚C)") +
  xlab("Time since experiment start (weeks)") +
  theme_classic()+
  theme(axis.title = element_text(size = "20")) +
  theme(axis.text = element_text(size = "18"))

Dr_plot

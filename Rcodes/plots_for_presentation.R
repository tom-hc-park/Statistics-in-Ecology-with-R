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

head(just_beach)

cca_r_summ <- just_rafts %>% 
  group_by(site, b_r, outplant_time, temp_av) %>% 
  dplyr::summarise(mean_gr = mean(growth_rate, na.rm = TRUE),
                   se_gr = sd(growth_rate, na.rm = TRUE)/sqrt(length(growth_rate)))

cca_mean_r <- ggplot(aes(x = temp_av, y = mean_gr ), data = cca_r_summ) +
  geom_point(aes(colour = factor(outplant_time)), size = 4) +
  scale_colour_grey(start = 0.8, end = 0.2) +
  geom_smooth(method = "lm") +
  ylab("Growth rate (mm/week)") +
  xlab("Mean temperature (˚C)") +
  theme_classic()+
  theme(axis.title = element_text(size = "20")) +
  theme(axis.text = element_text(size = "18")) +
  geom_errorbar(aes(ymax = mean_gr + se_gr, ymin = mean_gr - se_gr, width = 0.2)) +
  xlim(c(7,18)) +
  ylim(c(0,2.1)) +
  theme(legend.position ="none") 
cca_mean_r

ts_r_summ <- rafts_ts %>% 
  group_by(site, b_r, outplant_time, temp_av) %>% 
  dplyr::summarise(mean_gr = mean(growth_rate, na.rm = TRUE),
                   se_gr = sd(growth_rate, na.rm = TRUE)/sqrt(length(growth_rate)))

ts_mean_r <- ggplot(aes(x = temp_av, y = mean_gr ), data = ts_r_summ) +
  geom_point(aes(colour = factor(outplant_time)), size = 4) +
  scale_colour_grey(start = 0.8, end = 0.2) +
  geom_smooth(method = "lm") +
  ylab("Growth rate (mm/week)") +
  xlab("Mean temperature (˚C)") +
  theme_classic()+
  theme(axis.title = element_text(size = "20")) +
  theme(axis.text = element_text(size = "18")) +
  geom_errorbar(aes(ymax = mean_gr + se_gr, ymin = mean_gr - se_gr), width = 0.2) +
  xlim(c(7,18)) +
  ylim(c(0,2.1)) +
  theme(legend.position ="none") 
ts_mean_r


mi_r_summ <- Xl_r %>% 
  group_by(site, b_r, outplant_time, temp_av) %>% 
  dplyr::summarise(mean_gr = mean(growth_rate, na.rm = TRUE),
                   se_gr = sd(growth_rate, na.rm = TRUE)/sqrt(length(growth_rate)))

mi_mean_r <- ggplot(aes(x = temp_av, y = mean_gr ), data = mi_r_summ) +
  geom_point(aes(colour = factor(outplant_time)), size = 4) +
  scale_colour_grey(start = 0.8, end = 0.2) +
  geom_smooth(method = "lm") +
  ylab("Growth rate (mm/week)") +
  xlab("Mean temperature (˚C)") +
  theme_classic() +
  theme(axis.title = element_text(size = "20")) +
  theme(axis.text = element_text(size = "18")) +
  geom_errorbar(aes(ymax = mean_gr + se_gr, ymin = mean_gr - se_gr), width = 0.2) +
  xlim(c(7,18)) +
  ylim(c(0,2.1)) +
  theme(legend.position ="none") 
mi_mean_r

### beachy

cca_b_summ <- just_beach %>% 
  group_by(site, b_r, outplant_time, tot_dh_s) %>% 
  dplyr::summarise(mean_gr = mean(growth_rate, na.rm = TRUE),
                   se_gr = sd(growth_rate, na.rm = TRUE)/sqrt(length(growth_rate)))

cca_mean_b <- ggplot(aes(x = tot_dh_s, y = mean_gr ), data = cca_b_summ) +
  geom_point(aes(colour = factor(outplant_time)), size = 4) +
  scale_colour_grey(start = 0.8, end = 0.2) +
  geom_smooth(method = "lm") +
  ylab("Growth rate (mm/week)") +
  xlab("PSU Hours (hours)") +
  theme_classic()+
  theme(axis.title = element_text(size = "20")) +
  theme(axis.text = element_text(size = "18")) +
  geom_errorbar(aes(ymax = mean_gr + se_gr, ymin = mean_gr - se_gr, width = 1.9)) +
  theme(legend.position ="none") +
  theme(plot.margin =unit(c(1,1,1,1), "cm"))
cca_mean_b

ts_b_summ <- beach_ts %>% 
  group_by(site, b_r, outplant_time) %>% 
  dplyr::summarise(mean_gr = mean(growth_rate, na.rm = TRUE),
                   se_gr = sd(growth_rate, na.rm = TRUE)/sqrt(length(growth_rate)),
                   mean_dhs = mean(tot_dh_s))

ts_mean_b <- ggplot(aes(x = mean_dhs, y = mean_gr ), data = ts_b_summ) +
  geom_point(aes(colour = factor(outplant_time)), size = 4) +
  scale_colour_grey(start = 0.8, end = 0.2) +
  geom_smooth(method = "lm") +
  ylab("Growth rate (mm/week)") +
  xlab("PSU Hours (hours)") +
  theme_classic()+
  theme(axis.title = element_text(size = "20")) +
  theme(axis.text = element_text(size = "18")) +
  geom_errorbar(aes(ymax = mean_gr + se_gr, ymin = mean_gr - se_gr, width = 1.5)) +
  theme(legend.position ="none") +
  theme(plot.margin =unit(c(1,1,1,1), "cm"))
ts_mean_b

mi_b_summ <- Xl_b %>% 
  group_by(site, b_r, outplant_time, tot_dh_s) %>% 
  dplyr::summarise(mean_gr = mean(growth_rate, na.rm = TRUE),
                   se_gr = sd(growth_rate, na.rm = TRUE)/sqrt(length(growth_rate)),
                   mean_dhs = mean(tot_dh_s))

mi_mean_b <- ggplot(aes(x = tot_dh_s, y = mean_gr ), data = mi_b_summ) +
  geom_point(aes(colour = factor(outplant_time)), size = 4) +
  scale_colour_grey(start = 0.8, end = 0.2) +
  geom_smooth(method = "lm") +
  ylab("Growth rate (mm/week)") +
  xlab("PSU Hours (hours)") +
  theme_classic() +
  theme(axis.title = element_text(size = "20")) +
  theme(axis.text = element_text(size = "18")) +
  geom_errorbar(aes(ymax = mean_gr + se_gr, ymin = mean_gr - se_gr), width = 5) +
  theme(legend.position ="none")+
  theme(plot.margin =unit(c(1,1,1,1), "cm"))
mi_mean_b


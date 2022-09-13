
library(tidyverse)
library(lubridate)
library(scales)
library(usdata)

bkg_data <- readxl::read_xlsx('code/emissions-vs-bkg/data/eto-aqs-data.xlsx')

bkg_data <- bkg_data %>%
  filter(!is.na(sample_measurement)) %>% 
  mutate(date = ymd(as.character(date_local)),
         year = year(date),
         concentration = case_when(sample_measurement == 0 ~ detection_limit/sqrt(2),
                                                          TRUE ~ sample_measurement)) %>% 
  mutate(ID = paste0(state_code, county_code, site_number))
  
eto_daily <- eto_data %>%
  group_by(ID, state_code, state, county_code,county,cbsa_code, site_number,parameter_code,poc,latitude,longitude,datum, parameter,date_local, date,year, units_of_measure, method_type, method) %>%
  summarize(concentration = mean(concentration,na.rm=T),
            N= sum(!is.na(concentration)==T),
            sample_duration = paste0(unique(sample_duration[!is.na(sample_duration)]), collapse = ";"))

eto_daily %>% 
  filter(site_number == "1010", year == 2021) %>% 
  ggplot(aes(x=date, y = concentration)) +
  geom_point()


#summarize by monitoring location and year
eto_sum <- eto_daily %>% 
  group_by(ID, state_code, county_code, site_number,cbsa_code, county, state, year) %>% 
  summarize(
    min = min(concentration,na.rm=T),
    p05 = quantile(concentration, probs = 0.05, na.rm=T),
    p10 = quantile(concentration, probs = 0.10, na.rm=T),
    p25 = quantile(concentration, probs = 0.25, na.rm=T),
    median = median(concentration, na.rm=T),
    mean = mean(concentration, na.rm = T),
    p75 = quantile(concentration, probs = 0.75, na.rm=T),
    p90 = quantile(concentration, probs = 0.90, na.rm=T),
    p95 = quantile(concentration, probs = 0.95, na.rm=T),
    max = max(concentration, na.rm=T))

# 1) 2021 EtO data collected by EPA for each individual monitor -- I think box plot would make sense. x-axis would be the monitor and y-axis would be the measured concentration
# 2) Band from 0.0001 ppb to 0.01 ppb corresponding to EPA's 1e-6 to 1e-4 risk level for EtO
# 3) Same for Texas but their band would be 0.24 to 24 ppb for the same risk level
# 4) Estimated endogenous equivalent EtO of 2.7 ppb. SD is 2.3 ppb

eto21 <- eto_daily %>% 
  mutate(year = year(date)) %>% 
  filter(year == 2021)

eto21sum <- eto_sum %>% 
  filter(year == 2021) %>% 
  mutate(state_abbr = state2abbr(state),
         xlabels = paste0(county,"-", state_abbr)) %>% 
  arrange(ID)

# write_xlsx(eto21sum, path = "P:\\25037\\Analyses\\EtO EPA AQS Data\\Average annual 2021 Concentration of EtO by station.xlsx")


xlabs <- eto21sum$xlabels

#boxplot of daily measurements
ggplot(eto21, aes(x = ID, y = concentration))+
  theme_bw()+
  geom_boxplot()+
  scale_y_log10(breaks = 10^(-4:1), labels = trans_format("log10", math_format(10^.x)))+
  #geom_rect(aes(ymin = 0.0001, ymax= 0.01, xmin = 0, xmax = Inf), fill = "grey80", alpha = 0.9)+
  #geom_rect(aes(ymin = 0.24, ymax= 24, xmin = 0, xmax = Inf), fill = "grey80", alpha = 0.9)+
  geom_hline(aes(yintercept = 2.7), color = "blue")+
  geom_hline(aes(yintercept = 5), color = "blue", linetype="dashed")+
  geom_hline(aes(yintercept = 0.4), color = "blue", linetype="dashed")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  labs(x = "AQS Monitoring Site", y = "Ethylene Oxide Air Concentration (ppb)")

#average for 2021 by site with error bars for min and max

eto21sum %>% 
  ggplot(aes(x = site_number, y= median))+
  geom_point()+
  scale_x_discrete(labels = xlabs)+
  annotate("rect", ymin = 0.0001, ymax= 0.01, xmin = 0, xmax = Inf, fill = "grey80", alpha = 0.3)+
  annotate("rect", ymin = 0.24, ymax= 24, xmin = 0, xmax = Inf, fill = "grey80", alpha = 0.3)+
  geom_errorbar(aes(ymin = min,ymax = max))+
  theme_bw()+
  scale_y_log10(breaks = 10^(-4:1), labels = trans_format("log10", math_format(10^.x)))+
  geom_hline(aes(yintercept = 2.7), color = "blue")+
  geom_hline(aes(yintercept = 5), color = "blue", linetype="dashed")+
  geom_hline(aes(yintercept = 0.4), color = "blue", linetype="dashed")+
  geom_text(label = "",aes(x=53, y = 2.7))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank())+
  labs(x = "AQS Monitoring Site", y = "Ethylene Oxide Air Concentration (ppb)")


# ggsave(plot = last_plot(), file = "P:\\25037\\Analyses\\EtO EPA AQS Data\\Figures\\Average 2021 EtO Levels with risk bands.jpeg", width = 11, height = 8, dpi= 300, device = "jpeg")

#average eto by site with 1x10-5 risk equivalent for texas and epa

eto21sum %>% 
  ggplot(aes(x = ID, y= median))+
  geom_point()+
  scale_x_discrete(labels = xlabs)+
  # annotate("rect", ymin = 0.0001, ymax= 0.01, xmin = 0, xmax = Inf, fill = "grey80", alpha = 0.3)+
  # annotate("rect", ymin = 0.24, ymax= 24, xmin = 0, xmax = Inf, fill = "grey80", alpha = 0.3)+
  geom_errorbar(aes(ymin = min,ymax = max))+
  theme_bw()+
  scale_y_log10(breaks = 10^(-4:1), labels = trans_format("log10", math_format(10^.x)))+
  geom_hline(aes(yintercept = 2.7), color = "blue")+
  #geom_hline(aes(yintercept = 5), color = "blue", linetype="dashed")+
  #geom_hline(aes(yintercept = 0.4), color = "blue", linetype="dashed")+
  geom_hline(aes(yintercept = 2.5), color = "purple")+
  geom_hline(aes(yintercept = 0.001), color = "red")+
  geom_text(label = "",aes(x=53, y = 2.7))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14),
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 20),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank())+
  labs(x = "AQS Monitoring Site", y = "Ethylene Oxide Air Concentration (ppb)")


# ggsave(plot = last_plot(), file = "P:\\25037\\Analyses\\EtO EPA AQS Data\\Figures\\Average 2021 EtO Levels with county label.jpeg", width = 10.5, height = 8, dpi= 300, device = "jpeg")

xlabs <- eto21sum$state_abbr

#same as above, but just labels with state
# 
# eto21sum %>% 
#   ggplot(aes(x = ID, y= median))+
#   geom_point()+
#   scale_x_discrete(labels = xlabs)+
#   # annotate("rect", ymin = 0.0001, ymax= 0.01, xmin = 0, xmax = Inf, fill = "grey80", alpha = 0.3)+
#   # annotate("rect", ymin = 0.24, ymax= 24, xmin = 0, xmax = Inf, fill = "grey80", alpha = 0.3)+
#   geom_errorbar(aes(ymin = min,ymax = max))+
#   theme_bw()+
#   scale_y_log10(breaks = 10^(-4:1), labels = trans_format("log10", math_format(10^.x)))+
#   geom_hline(aes(yintercept = 2.7), color = "blue")+
#   #geom_hline(aes(yintercept = 5), color = "blue", linetype="dashed")+
#   #geom_hline(aes(yintercept = 0.4), color = "blue", linetype="dashed")+
#   geom_hline(aes(yintercept = 2.5), color = "purple")+
#   geom_hline(aes(yintercept = 0.001), color = "red")+
#   geom_text(label = "",aes(x=53, y = 2.7))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
#         axis.line = element_line(colour = "black"),
#         panel.border = element_blank(),
#         panel.grid.major.x = element_blank())+
#   labs(x = "AQS Monitoring Site", y = "Ethylene Oxide Air Concentration (ppb)")
# 
# 
# ggsave(plot = last_plot(), file = "P:\\25037\\Analyses\\EtO EPA AQS Data\\Figures\\Average 2021 EtO Levels with state label.jpeg", width = 10, height = 7, dpi= 300, device = "jpeg")




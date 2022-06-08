library(tidyverse)

# 2014 ETO emissions + cancer data
eto <- read_csv('data/processed/eto-emissions-with-cancer.csv') %>% 
  filter(year %in% c(2014, 2017))

# Baxter EISID = 1073211
# B Braun EISID = 14657611 
p <- ggplot(eto, aes(x = total_emissions_AirToxScreen, y = pt_cancer_risk)) + 
  # facet_wrap(~ year) +
  geom_point(alpha = 0.5, size = 2) +
  geom_point(data = eto %>% filter(eisid == 1073211), 
             aes(x = total_emissions_AirToxScreen, y = pt_cancer_risk), color = 'blue3', size = 4) +
  ggrepel::geom_text_repel(data = eto %>% filter(eisid == 1073211), 
                           aes(x = total_emissions_AirToxScreen, y = pt_cancer_risk, label = 'Baxter'), 
                           nudge_x = -2, nudge_y = 0.5, max.overlaps = Inf, color = 'blue3', fontface = 'bold') +
  geom_point(data = eto %>% filter(eisid == 14657611),
             aes(x = total_emissions_AirToxScreen, y = pt_cancer_risk),
             color = 'green4', size = 4) +
  ggrepel::geom_text_repel(data = eto %>% filter(eisid == 14657611), 
                           aes(x = total_emissions_AirToxScreen, y = pt_cancer_risk, label = 'B Braun'),
                           nudge_x = -2, nudge_y = 0.5, max.overlaps = Inf, color = 'green4', fontface = 'bold') +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  annotation_logticks(sides = 'bl') +
  theme_bw()
p

# Exploratory model fits. 
m1 <- lm(total_cancer_risk ~ poly(total_emissions, 2),  eto2014)
m2 <- lm(log(total_cancer_risk) ~ poly(log(total_emissions), 2),  eto2014)
m3 <- nls(log(total_cancer_risk) ~ a*exp(k*log(total_emissions)), start = list(a = 0.1, k = 0.5), eto2014)

# Only the 2nd degree polynomial fit is acceptable of the three
p +
  stat_smooth(method = 'loess', se = F, col = 'firebrick') + 
  stat_smooth(method = 'lm', se = F, col = 'darkblue')
  geom_line(aes(total_emissions, predict(m1)), size = 1, color = 'darkcyan') +
  geom_line(aes(total_emissions, exp(predict(m2))), size = 1, color = 'magenta') + 
  geom_line(aes(total_emissions, exp(predict(m3))), size = 1, color = 'brown') 


# Base plot
p <- ggplot(comb, aes(x = total_emissions, y = total_cancer_risk)) + 
  geom_point(alpha = 0.5, size = 2) +
  geom_point(data = baxter %>% filter(year == 2014), aes(x = total_emissions, y = cancer_m0), color = 'blue3', size = 4) +
  ggrepel::geom_text_repel(data = baxter %>% filter(year == 2014), aes(x = total_emissions, y = cancer_m0, label = 'Baxter'), 
                           nudge_x = -2, nudge_y = 0.5, max.overlaps = Inf, color = 'blue3', fontface = 'bold') +
  geom_point(data = braun %>% filter(year == 2014), aes(x = total_emissions, y = cancer_m0), color = 'green4', size = 4) +
  ggrepel::geom_text_repel(data = braun %>% filter(year == 2014), aes(x = total_emissions, y = cancer_m0, label = 'B Braun'), 
                           nudge_x = -1, nudge_y = 0.1, max.overlaps = Inf, color = 'green4', fontface = 'bold') +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  annotation_logticks(sides = 'bl') +
  theme_bw()
p

# Exploratory model fits. 
m1 <- lm(total_cancer_risk ~ poly(total_emissions, 2),  comb)
m2 <- lm(log(total_cancer_risk) ~ poly(log(total_emissions), 2),  comb)
m3 <- nls(log(total_cancer_risk) ~ a*exp(k*log(total_emissions)), start = list(a = 0.1, k = 0.5), comb)

# Only the 2nd degree polynomial fit is acceptable of the three
p +
  stat_smooth(method = 'loess', se = F) +
  geom_line(aes(total_emissions, predict(m1)), size = 1, color = 'darkcyan') +
  geom_line(aes(total_emissions, exp(predict(m2))), size = 1, color = 'magenta') + 
  geom_line(aes(total_emissions, exp(predict(m3))), size = 1, color = 'brown')

m2_75 <- quantreg::rq(log(total_cancer_risk) ~ poly(log(total_emissions), 2), eto2014, tau = 0.75)
m2_90 <- quantreg::rq(log(total_cancer_risk) ~ poly(log(total_emissions), 2), eto2014, tau = 0.90)

p +
  geom_line(aes(total_emissions, exp(predict(m2))), size = 1, color = 'magenta') +
  geom_line(aes(total_emissions, exp(predict(m2_75))), size = 1, color = 'magenta2') +
  geom_line(aes(total_emissions, exp(predict(m2_90))), size = 1, color = 'magenta3') 




### Old code
# 
# # Emissions from Baxter specifically.
# eto <- arrow::read_parquet('data/processed/eto-data.parquet')
# comb <- read_rds('data/processed/2014-eto-emissions-and-cancer-risk.rds')
# 
# baxter <- eto %>% 
#   filter(state == 'AR', str_detect(site_name, 'Baxter')) %>% 
#   add_row(year = 2021, total_emissions = 2698)
# mutate(total_emissions = ifelse(year == 2017, 6885, total_emissions), 
#        site_name = 'Baxter', 
#        scaling_factor = comb %>% filter(eisid == "1073211") %>% pull(scaling_factor), 
#        cancer_m0 = total_emissions*scaling_factor) %>%
#   fill(c(state:frsid, site_name, latitude, longitude), .direction = 'down') %>% 
#   arrange(year)
# 
# # Emissions from B Braun 
# braun <- eto %>% filter(state == 'PA', str_detect(site_name, 'Braun')) %>% 
#   mutate(site_name = "B Braun Medical Inc", 
#          scaling_factor = comb %>% filter(eisid == "1073211") %>% pull(scaling_factor), 
#          cancer_m0 = total_emissions*scaling_factor) %>%
#   fill(c(state:frsid, site_name, latitude, longitude), .direction = 'down') %>% 
#   arrange(year)

#   
# # Looks like Baxter emissions are in the 75th percentile. Fit quantile regression. 
# m2a <- quantreg::rq(log(total_cancer_risk) ~ poly(log(total_emissions), 2), comb, tau = 0.75)
# p + 
#   geom_line(aes(total_emissions, exp(predict(m2))), size = 1, color = 'magenta') + 
#   geom_line(aes(total_emissions, exp(predict(m2a))), size = 1, color = 'green') +
#   labs(x = 'Emissions (lbs), log scale', y = 'Cancer risk (per million), log Scale', 
#        title = 'Estimated NATA Cancer Risk from EtO Emissions', subtitle = '2014') 
# ggsave('plots/nata-fitted-models-1.jpeg')
# 
# # A bi-linear model (both axes are log-transformed) seems even better. 
# # Easier to conceptualize
# comb <- comb %>% mutate(split = total_emissions >= 0.01)
# ma <- lm(log(total_cancer_risk) ~ log(total_emissions), comb %>% filter(split))
# mb <- lm(log(total_cancer_risk) ~ log(total_emissions), comb %>% filter(!split))
# mc <- quantreg::rq(log(total_cancer_risk) ~ log(total_emissions), comb %>% filter(split), tau = 0.75)
# md <- quantreg::rq(log(total_cancer_risk) ~ log(total_emissions), comb %>% filter(!split), tau = 0.75)
# me <- quantreg::rq(log(total_cancer_risk) ~ log(total_emissions), comb %>% filter(split), tau = 0.90)
# mf <- quantreg::rq(log(total_cancer_risk) ~ log(total_emissions), comb %>% filter(!split), tau = 0.90)
# 
# ggplot() + 
#   geom_point(data = comb %>% filter(split), aes(x = total_emissions, y = total_cancer_risk), color = "cyan3", alpha = 0.4) + 
#   geom_point(data = comb %>% filter(!split), aes(x = total_emissions, y = total_cancer_risk), color = "darkorange", alpha = 0.4) + 
#   geom_point(data = baxter %>% filter(year == 2014), aes(x = total_emissions, y = cancer_m0), color = 'blue3', size = 4) +
#   ggrepel::geom_text_repel(data = baxter %>% filter(year == 2014), aes(x = total_emissions, y = cancer_m0, label = 'Baxter'), 
#                            nudge_x = -2, nudge_y = 0.5, max.overlaps = Inf, color = 'blue3', fontface = 'bold') +
#   geom_point(data = braun %>% filter(year == 2014), aes(x = total_emissions, y = cancer_m0), color = 'green4', size = 4) +
#   ggrepel::geom_text_repel(data = braun %>% filter(year == 2014), aes(x = total_emissions, y = cancer_m0, label = 'B Braun'), 
#                            nudge_x = -1, nudge_y = 0.1, max.overlaps = Inf, color = 'green4', fontface = 'bold') +
#   geom_line(data = comb %>% filter(split), aes(total_emissions, exp(predict(ma))), size = 1, color = 'cyan3') + 
#   geom_line(data = comb %>% filter(!split), aes(total_emissions, exp(predict(mb))), size = 1, color = 'darkorange') +
#   geom_line(data = comb %>% filter(split), aes(total_emissions, exp(predict(mc))), size = 1, color = 'blue3') +
#   geom_line(data = comb %>% filter(!split), aes(total_emissions, exp(predict(md))), size = 1, color = 'red3') +
#   geom_line(data = comb %>% filter(split), aes(total_emissions, exp(predict(me))), size = 1, color = 'red4') +
#   geom_line(data = comb %>% filter(!split), aes(total_emissions, exp(predict(mf))), size = 1, color = 'red4') +
#   scale_x_log10(labels = scales::comma) + 
#   scale_y_log10(labels = scales::comma) + 
#   annotation_logticks(sides = 'bl') +
#   labs(x = 'Emissions (lbs), log scale', y = 'Cancer risk (per million), log Scale', 
#        title = 'Estimated NATA Cancer Risk from EtO Emissions', subtitle = '2014') +
#   theme_bw()
# ggsave('plots/nata-fitted-models-2.jpeg')
# 
# # Predictions
# baxter$cancer_m2 <- exp(predict(m2, baxter))
# baxter$cancer_m2a <- exp(predict(m2a, baxter))
# baxter$cancer_ma <- exp(predict(ma, baxter))
# baxter$cancer_mc <- exp(predict(mc, baxter))
# baxter %>% select(year, site_name, total_emissions, scaling_factor:cancer_mc)
# 
# 
# # Predictions
# braun$cancer_m2 <- exp(predict(m2, braun))
# braun$cancer_m2a <- exp(predict(m2a, braun))
# braun$cancer_ma <- exp(predict(ma, braun))
# braun$cancer_mc <- exp(predict(mc, braun))
# braun %>% select(year, site_name, total_emissions, scaling_factor:cancer_mc)
# 
# 
# comb <- comb %>% mutate(scaling_factor = total_cancer_risk/total_emissions)
# eto <- eto %>% 
#   left_join(comb %>% select(eisid, tractce, affgeoid, total_cancer_risk, pt_cancer_risk, scaling_factor), 
#             by = 'eisid') %>% 
#   mutate(cancer_m0 = total_emissions*scaling_factor)
# eto$cancer_m2 <- exp(predict(m2, eto))
# eto$cancer_m2a <- exp(predict(m2a, eto))
# eto$cancer_ma <- exp(predict(ma, eto))
# eto$cancer_mc <- exp(predict(mc, eto))
# 
# eto2020 <- eto %>% filter(year == 2020)
# bleto2021 <- eto2020 %>% 
#   mutate(year = 2021) %>% 
#   add_row(baxter %>% filter(year == 2021))

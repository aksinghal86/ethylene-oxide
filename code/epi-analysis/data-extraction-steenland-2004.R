# ---
# title: "Steenland 2004 - Mortality analyses in a cohort of 18,235 ethylene oxide ..."
# author: "FK"
# date: "2022-10-19"
# ---

  
library(tidyverse)
library(here)
library(RColorBrewer)
# 
# ggtheme <- theme_bw() +
#   theme(panel.grid.major = element_blank(), #element_line(color = "gray30", linetype = "solid"),
#         panel.grid.minor = element_blank(),
#         strip.text = element_text(size = 14, face = 'bold'),
#         legend.title = element_blank(),
#         legend.position = "bottom",
#         legend.text = element_text(size = 14),
#         legend.direction = "vertical",
#         axis.text = element_text(size = 12),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.title = element_text(size = 14, face = 'bold'),
#         plot.title = element_text(size = 14, face = 'bold'),
#         plot.subtitle = element_text(size = 12))
# 


plot_steenland <- function(dat, cat = T) { 
  
  ggtheme <- theme_bw() +
    theme(text = element_text(size = 13), 
          panel.grid = element_blank(),
          strip.background = element_rect(fill = '#f2f4f4'), 
          strip.text = element_text(size = 14))
  
  plot.col <- c(brewer.pal(n = 3, "Dark2")[1:3])
  
  ggplot(data = dat, aes(x = if (cat) labs else xaxis, y = OR, color = model)) + 
    geom_point(aes(shape = sig), size = 5, show.legend = F, alpha = 0.95) +
    geom_errorbar(aes(ymin = OR.L, ymax = OR.U), width = 0.5, cex = 1, show.legend = F, alpha = 0.8) +
    facet_wrap(~ fct_relevel(model, 'Females, no lag', 'Males, no lag', 'Males, 15 year lag'), scales = 'free_x') + 
    geom_hline(yintercept = 1, linetype = 2, color = "gray30") +
    scale_shape_identity() + 
    scale_color_manual(values = plot.col) +
    labs(x = "Cumulative exposure dose (ppm-days)",
         y = "Odds ratio")+
    ggtheme
}


# Table 6. ------------------------------------------------------------------
labels <- data.frame(category = c('0 (lagged out)', '0-1199', '1200-3679', '3680-13499', '13500+'), 
                     labs = c('0', 'Low', 'Medium', 'High', 'Very high'),
                     xaxis = c(0, 100, 1200, 3680, 13500))

table6 <- tribble(~No, ~model, ~category, ~OR, ~OR.L, ~OR.U,
                  1, "Males, no lag", "0-1199", 1, 1, 1,
                  2, "Males, no lag", "1200-3679", 2.07, 0.67, 6.41,
                  3, "Males, no lag", "3680-13499", 2.02, 0.68, 5.98,
                  4, "Males, no lag", "13500+", 2.06, 0.72, 5.91,
                  1, "Females, no lag", "0-1199", 1, 1, 1,
                  2, "Females, no lag", "1200-3679", 1.51, 0.69, 3.34,
                  3, "Females, no lag", "3680-13499", 0.93, 0.38, 2.30,
                  4, "Females, no lag", "13500+", 0.52, 0.16, 1.66,
                  1, "Males, 15 year lag", "0 (lagged out)", 1, 1, 1,
                  2, "Males, 15 year lag", "0-1199", 1.23, 0.32, 4.73,
                  3, "Males, 15 year lag", "1200-3679", 2.52, 0.69, 9.22,
                  4, "Males, 15 year lag", "3680-13499", 3.13, 0.95, 10.37,
                  5, "Males, 15 year lag", "13500+", 3.42, 1.09, 10.73) %>% 
  left_join(labels, by = 'category')

table6 <- table6 %>% 
  mutate(model_cat = paste(model, category, sep = "-"),
         sig = ifelse(OR.U < 1 , 15, #15-prtc
                      ifelse((OR.L<=1 & OR.U>=1), 0, 15)), #0-null #15-sigpos
         sig = as.integer(sig))

catz <- unique(table6$model_cat)

table6 <- table6 %>% 
  mutate(model_cat = factor(model_cat, levels = catz), 
         labs = factor(labs, levels = c('0', 'Low', 'Medium', 'High', 'Very high')))


plot_steenland(table6, cat = T) +
  scale_y_continuous(n.breaks = 10) +
  labs(
    title = "Cox regression results for all haematopoietic cancer mortality", 
    subtitle = 'Data from Table 6 of Steenland et al. (2004)'    
  ) 

ggsave(here('output','plot','steenland2004-table6-cat.jpeg'), 
       width = 12, height = 8, dpi = 600)


plot_steenland(table6, cat = F) +
  scale_y_continuous(n.breaks = 10) +
  labs(
    title = "Cox regression results for all haematopoietic cancer mortality", 
    subtitle = 'Data from Table 6 of Steenland et al. (2004)'    
  ) +
  scale_x_continuous(breaks = seq(0, 14000, 3000), labels = scales::comma)

ggsave(here('output','plot','steenland2004-table6-cum.jpeg'), 
       width = 12, height = 8, dpi = 600)




# Table 7. ----------------------------------------------------------------
table7 <- tribble(~model, ~category, ~OR, ~OR.L, ~OR.U,
                  "Males, no lag", "0-1199", 1, 1, 1,
                  "Males, no lag", "1200-3679", 2.45, 0.61, 9.92,
                  "Males, no lag", "3680-13499", 1.85, 0.46, 7.48,
                  "Males, no lag", "13500+", 2.44, 0.67, 8.87,
                  "Females, no lag", "0-1199", 1, 1, 1,
                  "Females, no lag", "1200-3679", 2.05, 0.76, 5.56,
                  "Females, no lag", "3680-13499", 1.25, 0.4, 3.76,
                  "Females, no lag", "13500+", 0.87, 0.24, 3.10,
                  "Males, 15 year lag", "0 (lagged out)", 1, 1, 1,
                  "Males, 15 year lag", "0-1199", 0.90, 0.16, 5.24,
                  "Males, 15 year lag", "1200-3679", 2.89, 0.65, 12.86,
                  "Males, 15 year lag", "3680-13499", 2.74, 0.65, 11.55,
                  "Males, 15 year lag", "13500+", 3.76, 1.03, 13.64) %>% 
  left_join(labels, by = 'category') 

table7 <- table7 %>%
  mutate(model_cat = paste(model, category, sep = "-"),
         sig = ifelse(OR.U < 1 , 15, #15-prtc
                      ifelse((OR.L<=1 & OR.U>=1), 0, 15)), #0-null #15-sigpos
         sig = as.integer(sig))

catz <- unique(table7$model_cat)

table7 <- table7 %>% 
  mutate(model_cat = factor(model_cat, levels = catz), 
         labs = factor(labs, levels = c('0', 'Low', 'Medium', 'High', 'Very high')))


plot_steenland(table7, cat = T) +
  scale_y_continuous(n.breaks = 10) +
  labs(
    title = "Cox regression results for all haematopoietic cancer mortality", 
    subtitle = 'Data from Table 7 of Steenland et al. (2004)'    
  ) 

ggsave(here('output','plot','steenland2004-table7-cat.jpeg'), 
       width = 12, height = 8, dpi = 600)


plot_steenland(table7, cat = F) +
  scale_y_continuous(n.breaks = 10) +
  labs(
    title = "Cox regression results for all haematopoietic cancer mortality", 
    subtitle = 'Data from Table 7 of Steenland et al. (2004)'    
  ) +
  scale_x_continuous(breaks = seq(0, 14000, 3000), labels = scales::comma)

ggsave(here('output','plot','steenland2004-table7-cum.jpeg'), 
       width = 12, height = 8, dpi = 600)

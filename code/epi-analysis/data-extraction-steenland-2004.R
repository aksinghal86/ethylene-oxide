# ---
# title: "Steenland 2004 - Mortality analyses in a cohort of 18,235 ethylene oxide ..."
# author: "FK"
# date: "2022-10-19"
# ---

  
library(tidyverse)
library(here)
library(RColorBrewer)

ggtheme <- theme_bw() + 
  theme(panel.grid.major = element_blank(), #element_line(color = "gray30", linetype = "solid"), 
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 14, face = 'bold'), 
        legend.title = element_blank(), 
        legend.position = "bottom", 
        legend.text = element_text(size = 14),
        legend.direction = "vertical",
        axis.text = element_text(size = 14), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 14, face = 'bold'), 
        plot.subtitle = element_text(size = 12))


# Table 6. ------------------------------------------------------------------
table6 <- tribble(~No, ~model, ~category, ~OR, ~OR.L, ~OR.U,
                  1, "Males", "0-1199", 1, 1, 1,
                  2, "Males", "1200-3679", 2.07, 0.67, 6.41,
                  3, "Males", "3680-13499", 2.02, 0.68, 5.98,
                  4, "Males", "13500+", 2.06, 0.72, 5.91,
                  1, "Females", "0-1199", 1, 1, 1,
                  2, "Females", "1200-3679", 1.51, 0.69, 3.34,
                  3, "Females", "3680-13499", 0.93, 0.38, 2.30,
                  4, "Females", "13500+", 0.52, 0.16, 1.66,
                  1, "Males, 15 year lag", "0 (lagged out)", 1, 1, 1,
                  2, "Males, 15 year lag", ">0-1199", 1.23, 0.32, 4.73,
                  3, "Males, 15 year lag", "1200-3679", 2.52, 0.69, 9.22,
                  4, "Males, 15 year lag", "3680-13499", 3.13, 0.95, 10.37,
                  5, "Males, 15 year lag", "13500+", 3.42, 1.09, 10.73)

table6 <- table6 %>% 
  mutate(model_cat = paste(model, category, sep = "-"),
         sig = ifelse(OR.U < 1 , 15, #15-prtc
                      ifelse((OR.L<=1 & OR.U>=1), 0, 15)), #0-null #15-sigpos
         sig = as.integer(sig))

catz <- unique(table6$model_cat)

table6 <- table6 %>% 
  mutate(model_cat = factor(model_cat, levels = catz))

plot.col <- c(brewer.pal(n = 3, "Set1")[1:3])

ggplot(data = table6, aes(y = OR, ymin = OR.L, ymax = OR.U, x = model_cat)) + 
  geom_hline(yintercept = 1, linetype = 2, color = "gray30") +
  geom_errorbar(aes(ymin = OR.L, ymax = OR.U, col = model),
                width = 0.5, cex = 1, show.legend = F) +
  geom_point(aes(y = OR, x = model_cat, color = model, shape = sig), 
             size = 5, show.legend = F) +
  annotate("text", x = 2.5, y = 10.75, label = "Females", size = 5) +
  geom_vline(aes(xintercept = 4.5)) +
  annotate("text", x = 6.5, y = 10.75, label = "Males", size = 5) +
  geom_vline(aes(xintercept = 8.5)) +
  annotate("text", x = 11, y = 10.75, label = "Males, 15 year lag", size = 5) +
  scale_x_discrete(labels =  table6$category) +
  scale_y_continuous(breaks = seq(0,12,2)) +
  scale_color_manual(values = plot.col) +
  scale_shape_identity() +
  labs(x = "Exposure (ppm-day)",
       y = "Odds ratio",
       title = "Cox regression results for all haematopoietic cancer mortality")+
  ggtheme

ggsave(here('output','plot','steenland2004-table6.jpeg'), 
       width = 12, height = 8, dpi = 600)




# Table 7. ----------------------------------------------------------------
table7 <- tribble(~model, ~category, ~OR, ~OR.L, ~OR.U,
                  "Males", "0-1199", 1, 1, 1,
                  "Males", "1200-3679", 2.45, 0.61, 9.92,
                  "Males", "3680-13499", 1.85, 0.46, 7.48,
                  "Males", "13500+", 2.44, 0.67, 8.87,
                  "Females", "0-1199", 1, 1, 1,
                  "Females", "1200-3679", 2.05, 0.76, 5.56,
                  "Females", "3680-13499", 1.25, 0.4, 3.76,
                  "Females", "13500+", 0.87, 0.24, 3.10,
                  "Males, 15 year lag", "0 (lagged out)", 1, 1, 1,
                  "Males, 15 year lag", ">0-1199", 0.90, 0.16, 5.24,
                  "Males, 15 year lag", "1200-3679", 2.89, 0.65, 12.86,
                  "Males, 15 year lag", "3680-13499", 2.74, 0.65, 11.55,
                  "Males, 15 year lag", "13500+", 3.76, 1.03, 13.64)

table7 <- table7 %>% 
  mutate(model_cat = paste(model, category, sep = "-"),
         sig = ifelse(OR.U < 1 , 15, #15-prtc
                      ifelse((OR.L<=1 & OR.U>=1), 0, 15)), #0-null #15-sigpos
         sig = as.integer(sig))

catz <- unique(table7$model_cat)

table7 <- table7 %>% 
  mutate(model_cat = factor(model_cat, levels = catz))

plot.col <- c(brewer.pal(n = 3, "Set1")[1:3])

ggplot(data = table7, aes(y = OR, ymin = OR.L, ymax = OR.U, x = model_cat)) + 
  geom_hline(yintercept = 1, linetype = 2, color = "gray30") +
  geom_errorbar(aes(ymin = OR.L, ymax = OR.U, col = model),
                width = 0.35, cex = 1, show.legend = F) +
  geom_point(aes(y = OR, x = model_cat, color = model, shape = sig), 
             size = 5, show.legend = F) +
  annotate("text", x = 2.5, y = 13.5, label = "Females", size = 5) +
  geom_vline(aes(xintercept = 4.5)) +
  annotate("text", x = 6.5, y = 13.5, label = "Males", size = 5) +
  geom_vline(aes(xintercept = 8.5)) +
  annotate("text", x = 11, y = 13.5, label = "Males, 15 year lag", size = 5) +
  scale_x_discrete(labels =  table7$category) +
  scale_y_continuous(breaks = seq(0,14,2)) +
  scale_color_manual(values = plot.col) +
  scale_shape_identity() +
  labs(x = "Exposure (ppm-day)",
       y = "Odds ratio",
       title = "Cox regression results for lymphoid cell line tumours")+
  ggtheme

ggsave(here('output','plot','steenland2004-table7.jpeg'), 
       width = 12, height = 8, dpi = 600)

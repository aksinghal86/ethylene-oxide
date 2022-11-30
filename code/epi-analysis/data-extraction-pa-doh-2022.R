# ---
# title: 'PA DOH 2022 - Community Cancer Incidence Data Review ...'
# author: 'FK '
# date: '2022-11-30'
# ---

  
library(tidyverse)
library(here)
library(RColorBrewer)

plot_padoh <- function(dat,time_cat = T) { 
  
  ggtheme <- theme_bw() +
    theme(text = element_text(size = 13), 
          panel.grid = element_blank(),
          strip.background = element_rect(fill = '#f2f4f4'), 
          strip.text = element_text(size = 13))
  
  plot.col <- c(brewer.pal(n = 5, 'Dark2')[1:5])
  
  ggplot(data = dat, aes(x = if (time_cat) labs else xaxis, y = SIR, color = `Cancer Type`)) + 
    geom_point(aes(shape = sig), size = 5, show.legend = F, alpha = 0.95) +
    geom_errorbar(aes(ymin = SIR.L, ymax = SIR.U), width = if (time_cat) 0.25 else 1995, cex = 1, show.legend = F, alpha = 0.8) +
    facet_wrap(~ fct_relevel(`Cancer Type`, c('Female, Adult Lymphohematopoietic Cancer', 'Male, Adult Lymphohematopoietic Cancer', 'Female, Breast Cancer', 'Female, All Childhood Cancer', 'Male, All Childhood Cancer')), scales = 'free_x') + 
    geom_hline(yintercept = 1, linetype = 2, color = 'gray30') +
    scale_shape_identity() + 
    scale_color_manual(values = plot.col) +
    labs(x = 'Time Period',
         y = 'Standardized incidence ratio')+
    ggtheme
}

labels <- data.frame(Time = c('1985 - 1994', '1995 - 2004', '2005 - 2017'), 
                     labs = c('1985 - 1994', '1995 - 2004', '2005 - 2017'),
                     xaxis = c(1985, 1995, 2005))

# Table 1. ----------------------------------------------------------------
table1 <- tribble(~`Cancer Type`, ~Time,	~Observed, ~Expected,	~SIR,	~SIR.L,	~SIR.U,
                  'Male, Adult Lymphohematopoietic Cancer',	'1985 - 1994', 72, 67.2, 1.07, 0.84, 1.35,
                  'Male, Adult Lymphohematopoietic Cancer',	'1995 - 2004', 99, 89.5, 1.11, 0.9, 1.35,
                  'Male, Adult Lymphohematopoietic Cancer',	'2005 - 2017', 121, 145.3, 0.83, 0.69, 0.99,
                  'Female, Adult Lymphohematopoietic Cancer',	'1985 - 1994', 71, 65, 1.09, 0.85, 1.38,
                  'Female, Adult Lymphohematopoietic Cancer',	'1995 - 2004', 94, 85.1, 1.11, 0.89, 1.35,
                  'Female, Adult Lymphohematopoietic Cancer',	'2005 - 2017', 126, 123.9, 1.02, 0.85, 1.21,
                  'Female, Breast Cancer',	'1985 - 1994', 366, 388.3, 0.94, 0.85, 1.04,
                  'Female, Breast Cancer',	'1995 - 2004', 451, 425.5, 1.06, 0.96, 1.16,
                  'Female, Breast Cancer',	'2005 - 2017', 521, 550.4, 0.95, 0.87, 1.03,
                  'Male, All Childhood Cancer',	'1985 - 1994',	9, 10.8, 0.83, 0.38, 1.58,
                  'Male, All Childhood Cancer',	'1995 - 2004',	10,	9.2,	1.09,	0.52,	2.01,
                  'Male, All Childhood Cancer',	'2005 - 2017',	17, 15.3, 1.11, 0.65, 1.78,
                  'Female, All Childhood Cancer',	'1985 - 1994', 13, 6.9, 1.88, 1.01, 3.22,
                  'Female, All Childhood Cancer',	'1995 - 2004', 8, 8.2, 0.98, 0.42, 1.93,
                  'Female, All Childhood Cancer',	'2005 - 2017', 25, 13.6, 1.84, 1.19, 2.71) %>% 
  left_join(labels, by = 'Time')

table1 <- table1 %>% 
  mutate(sig = ifelse(SIR.U < 1 , 15, #15-prtc
                      ifelse((SIR.L<=1 & SIR.U>=1), 0, 15)), #0-null #15-sigpos
         sig = as.integer(sig),
         labs = factor(labs, levels = c('1985 - 1994', '1995 - 2004', '2005 - 2017')))

plot_padoh(table1,time_cat = T) +
  scale_y_continuous(n.breaks = 10) +
  labs(
    title = 'Cancer Incidence Rates of Adults and Children near B. Braun facility (within a 2-mile radius)', 
    subtitle = 'Data from Table 1 of PA DOH (2022)'    
  ) 

ggsave(here('output','plot','pa-doh2022-table1.jpeg'), 
       width = 12, height = 8, dpi = 600)




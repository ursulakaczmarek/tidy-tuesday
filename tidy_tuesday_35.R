library(tidytuesdayR)
library(tidyverse)
library(ggplot2)

list2env(tt_load('2023-08-29'), globalenv())

fair_use_cases <- fair_use_cases %>% 
  filter(year >= 1950)

values <- c("#d00060", "#03a0c5")

ggplot(fair_use_cases, aes(year, case, fill = fair_use_found)) +
  geom_tile() + 
  coord_fixed(0.2) +
  scale_x_continuous(position = "top", n.breaks = 8) +
  scale_fill_manual(values = values) + 
  theme_void(base_family = "mono") +
  labs(title = paste("Fair Use Case Dispositions Since 1950"), fill = "fair use found") +
  theme(plot.title = element_text(size = 14)) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text.x = element_text(size = 10)) +
  theme(panel.grid.major.x = element_line( size = 0.1, color = "gray"))
 

library(tidytuesdayR)
library(tidyverse)
library(ggplot2)

scurvy <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-25/scurvy.csv')

to_plot <- scurvy %>% 
  select(-dosing_regimen_for_scurvy) %>% 
  mutate(across(ends_with("d6"), ~as.integer(str_remove_all(., "\\D+")))) %>% 
  pivot_longer(cols = -c(study_id, treatment, fit_for_duty_d6), names_to = "symptom", values_to = "severity") %>% 
  group_by(study_id) %>% 
  mutate(symptom = gsub("_|d6", " ", symptom),
    severity = ifelse(treatment == "citrus", severity, -severity),
    total_severity = sum(severity))
  

labs <- c("fit", "unfit")

to_plot %>%
  ggplot(aes(x = reorder(treatment, severity), 
             fill = symptom)) +
  geom_bar(aes(y = fit_for_duty_d6), stat = "identity") +
  geom_bar(aes(y = severity), position = "stack", stat = "identity") +
  coord_flip() + # flip x and y labels  
  scale_x_discrete() +
  scale_y_continuous(breaks = 1:length(levels(to_plot$fit_for_duty_d6)), labels = labs, name = "Fit for Duty", 
    sec.axis = sec_axis(~.*-1, breaks = c(25, 10, 0), name = "Total severity")) +
  labs(title = "Severity of scurvy symptoms following experimental treatments",
       subtitle = "Four symptoms seen in 12 subjects, each measured on scale of 0 (none) to 3 (severe)",
       x = "treatment",
       fill = NULL) +
  scale_fill_viridis_d() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank()) +
  geom_hline(yintercept = 0)


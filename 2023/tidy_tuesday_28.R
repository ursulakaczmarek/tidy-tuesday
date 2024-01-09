library(tidytuesdayR)
library(tidyverse)
library(ggplot2)

list2env(tt_load(2023, week = 28), globalenv())

calculate_var <- function(temps){
  temps %>% select(Year:Dec) %>% 
  pivot_longer(!Year, names_to = "month", values_to = "dev_mean_1951_1980") %>% 
  group_by(month) %>% 
  mutate(in_group_variance = var(dev_mean_1951_1980, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(max_in_group_variance = as.factor(+(in_group_variance == max(in_group_variance))),
         label = ifelse(max_in_group_variance == 1, month, NA))
}


to_plot <- bind_rows(map(mget(ls(pattern = "h|l_temps")), calculate_var), .id = "df_id") %>% 
  group_by(df_id) %>% 
  mutate(df_id = recode(df_id, "nh_temps" = "Northern hemisphere", "sh_temps" = "Southern hemisphere", "global_temps" = "global"),
         df_min = min(dev_mean_1951_1980, na.rm = TRUE),
         df_max = max(dev_mean_1951_1980, na.rm = TRUE))
  
values = c("#9ba9c2", "#f84239")
ggplot(to_plot, aes(x = Year, y = dev_mean_1951_1980)) +
  geom_line(aes(color = max_in_group_variance)) +
  geom_ribbon(aes(ymin = df_min, ymax = df_max), alpha = 0.1) +
  geom_label(data = subset(to_plot, !is.na(label) & Year == max(Year)), aes(label = month), nudge_x = 0.35, size = 4) +
  scale_colour_manual(values = values, guide = "none") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  facet_wrap(df_id ~ ., ncol = 1) +
  theme_minimal() +
  labs(title = "Yearly temperature anomalies with highlighted month of greatest variance",
       x = "year",
       y = "deviation from the 1950-1981 means")

  
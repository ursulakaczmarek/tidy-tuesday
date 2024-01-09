library(xml2)
library(tidyverse)
library(lubridate)
library(ggplot2)

# export instructions here: https://support.apple.com/guide/iphone/share-your-health-data-iph5ede58c3d/ios
xml <- read_xml("export.xml")

# filter to distance and date data
workouts <- xml_find_all(xml, ".//WorkoutStatistics[@type = 'HKQuantityTypeIdentifierDistanceWalkingRunning']")

# filtering to sedentary heart rate (@value = '1' or '0', not '2') is not as accurate as the resting heart rate sampling
# see https://developer.apple.com/documentation/healthkit/hkquantitytypeidentifier/2867756-restingheartrate
# bpms <- xml_find_all(xml, ".//Record[MetadataEntry/@key = 'HKMetadataKeyHeartRateMotionContext' and MetadataEntry/@value != '2']")
bpms <- xml_find_all(xml, ".//Record[@type = 'HKQuantityTypeIdentifierRestingHeartRate']")

# convert to tibbles
bpm_df <- tibble(reading_date = xml_attr(bpms, "endDate") |> as.Date(format = "%Y-%m-%d"),  
       bpm = as.integer(xml_attr(bpms, "value")))

combined <- tibble(reading_date = xml_attr(workouts, "endDate") |> as.Date(format = "%Y-%m-%d"),  
  distance = as.numeric(xml_attr(workouts, "sum"))) |> 
  full_join(bpm_df, by = "reading_date") |> 
  replace_na(list(distance = 0)) |> 
  group_by(paste0(week(reading_date), year(reading_date))) |> 
    mutate(weekly_distance = sum(distance)) |> 
  ungroup()

# correlation coefficient
coeff <- cor(combined$bpm, combined$weekly_distance, use = "complete.obs")

# for plotting, secondary axis is estimated relationship between bpm+mileage
plot <- ggplot(data = combined, aes(reading_date, bpm, color = bpm)) + 
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "#102349", linewidth = 0.5) +
  scale_color_gradient(name = "resting heart rate (bpm)", low = "#046b99", high = "#d00060") +
  scale_x_date() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
                     sec.axis = sec_axis(~./2, name = "rolling mean weekly mileage")) +
  theme_minimal(base_family = "mono") +
  labs(title = "resting heart rate plotted against average run miles/week", 
       subtitle = sprintf("correlation coefficient: %f", coeff),
       x = "reading date")

plot +
  geom_line(aes(y = zoo::rollmean(weekly_distance*2, k = 3, fill = mean(distance), align = "right")), color = "#d00060")






    



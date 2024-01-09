library(xml2)
library(methods)
library(tidyverse)
library(lubridate)
library(ggplot2)

xml <- read_xml("export.xml")

# filter to distance and date data
workouts <- xml_find_all(xml, ".//WorkoutStatistics[@type = 'HKQuantityTypeIdentifierDistanceWalkingRunning']")

# filter to resting heart rate (@value = '1')
bpms <- xml_find_all(xml, ".//Record[MetadataEntry/@key = 'HKMetadataKeyHeartRateMotionContext' and MetadataEntry/@value = '1']")

bpm_df <- tibble(bpm_date = xml_attr(bpms, "endDate") |> as.POSIXct(format = "%Y-%m-%d"),  
       year_week = paste(year(bpm_date), week(bpm_date), sep = "-"),
       bpm = xml_attr(bpms, "value")) |> 
  group_by(bpm_date) |> 
  slice_head(n = 2) |> # apple watch logs lots of readings, so limit to first two of the day
  ungroup()

combined <- tibble(end_date = xml_attr(workouts, "endDate") |> as.POSIXct(format = "%Y-%m-%d"),  
  year_week = paste(year(end_date), week(end_date), sep = "-"),
  distance = xml_attr(workouts, "sum") |> as.numeric()) |> 
  group_by(year_week) |> 
  transmute(weekly_distance = sum(distance)) |> 
  ungroup() |> 
  full_join(bpm_df, by = "year_week")
  






    



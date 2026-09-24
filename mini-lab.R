# Lab 2
#Murphy

# We'll start by working with the actual crashes and persons fully 
library(tidyverse)

crashes <- read_csv("data/raw/crashes.csv")
persons <- read_csv("data/raw/person.csv")

# 1: Make a table of person records for female pedestrians 
female_pedestrians <- persons |> 
  filter(PERSON_SEX == "F", PERSON_TYPE == "Pedestrian")
## what does one row represent? 

#one row represents a female pedestrian that was involved in a crash and information about them

# 2: Keep only the crashes that involved at least one female pedestrian. 
# Keep COLLISION_ID, BOROUGH, and the five vehicle type columns. 
# how can we select every variable that starts with "VEHICLE TYPE CODE"?

female_pedestrians_crashes <- crashes |> 
  semi_join(female_pedestrians, join_by(COLLISION_ID)) |> 
  select("COLLISION_ID", "BOROUGH", "VEHICLE TYPE CODE 1", "VEHICLE TYPE CODE 2", "VEHICLE TYPE CODE 3", "VEHICLE TYPE CODE 4", "VEHICLE TYPE CODE 5")
  #starts_with("VEHICLE TYPE CODE")
##  
# does one row still represent one crash? check it. 
##one row represents one crash that involved a female pedestrian.
# why a filtering join instead of a mutating join? 
## so it does not add any more columns into our data
# 3: Right now the vehicle types are columns. We want one row per vehicle. 
# before writing your code, how many rows should we have? 
## 4940*5=24700
female_crash_long <- female_pedestrians_crashes |> 
  pivot_longer(
    cols = starts_with("VEHICLE TYPE CODE"),
    names_to = "vehicle_slot",
    values_to = "vehicle_type"
  )

glimpse(female_crash_long)

female_crash_long |> 
  summarise(
    missing = sum(is.na(vehicle_type)))

female_crash_long |> 
  group_by(vehicle_slot) |> 
  summarise(
    missing = sum(is.na(vehicle_type)),
    rows = n()
  )


vehicle_records <- female_crash_long |> 
  filter(!is.na(vehicle_type))

vehicle_records |> 
  count(vehicle_type, sort = TRUE) |> 
  print(n = 40)

# how many missing values are in the new dataframe? 
##20203
# why do we think that slots 3, 4, and 5 have so many more missing values? 
##Because there are not many accidents that involve more than 2 vehicles
# does every crash have a first vehicle recorded? 
##no
# are we safe to drop missing values?
##Yes we are safe to drop missing values
# 5: what kinds of vehicles are involved in crashes with a female pedestrian? 
vehicle_records |> 
  count(vehicle_type, sort = TRUE) |> 
  print(n = 40)

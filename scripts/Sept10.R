#DATA2004, Murphy, Sept 10
#Lab 1
library(tidyverse)
d

#Grain = what one row represents
#Granularity = the level of detail in observations

#workflow: import, inspect, check documentation, declare grain, calaculate

#Answer three question
  #1.) What is the total population of the US in 2025 according to this file?
  #2.) Which Kentucky counties grew from 2024 to 2025?
  #3.) How many counties or county-equivalent records are in this file

data <- read_csv("data/raw/co-est2025-alldata.csv",
                 locale = locale(encoding = "Latin1"),
                 col_types = cols(.default = col_character()))

glimpse(data)
names(data)

#Use the internet to find background about the info
#state & county, population change, annual residence population and change, census bureau, match documentation

data_trimmed <- data |> 
  select(SUMLEV, REGION, DIVISION, STATE, COUNTY, STNAME, CTYNAME, POPESTIMATE2025, POPESTIMATE2024, NPOPCHG2025)
##dOES every row appear to represent the same kind of geographic observation?
###No. State- county
##Which row(s) look different?
### SUMLEV Column
##Is there a variable that appears to encode that difference?
## SUMLEV Column
glimpse(data_trimmed)
data_trimmed |> slice_head(n = 10)

#declare the grain
##One row represents either a county or a state
##One row represents a mixed state-county grain.
data_trimmed_numeric <- data_trimmed |> 
  mutate(
    pop2025 = as.numeric(POPESTIMATE2025),
    pop2024 = as.numeric(POPESTIMATE2024),
    popchg2025 = as.numeric(NPOPCHG2025)
  )

data_trimmed_numeric |> filter(
  STNAME == "Kentucky") |> 
    slice_head(n = 10)


data_trimmed_numeric |> filter(SUMLEV == "040") |> 
  summarise(total_pop_2025 = sum(pop2025))

#US popoulation = 341784857

data_trimmed_numeric |> 
  filter(STNAME == "Kentucky", SUMLEV == "050") |> 
  filter(popchg2025 > 0) |> 
  select(CTYNAME, popchg2025) |> 
  arrange(desc(popchg2025)) |> 
  print(n = 81) 

data_trimmed_numeric |> 
  filter(SUMLEV == "050", STNAME == "Kentucky", popchg2025 > 0) |> 
  mutate(
    popchgvalid = pop2025 - pop2024
  ) |> 
  select(CTYNAME, popchgvalid) |> 
  arrange(desc(popchgvalid))


data_trimmed_numeric |> filter(SUMLEV == "050") |> 
  nrow()


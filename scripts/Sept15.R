#DATA2004, Murphy, 9/15
library(tidyverse)
library(readxl)
fishing <- read_excel("data/raw/commercial.xlsx", sheet = "Erie")
glimpse(fishing)
#one row represents a region, year, and weight (rounded lbs) of fish caught

nrow(fishing)

fishing_long <- fishing |> 
  pivot_longer(cols =!c(Year, Lake, Species, Comments), 
                                        names_to ="region",
                                        values_to ="values")

nrow(fishing_long)
nrow(fishing_long)/nrow(fishing)

fishing_long |> 
  distinct(region)

fishing_long |> 
  filter(Year == 1885, Species == "Lake Whitefish") |> 
  select(region, values)

fishing_long |> 
  filter(!region %in% c("U.S. Total", "Grand Total")) |> 
  summarise(total = sum(values, na.rm = TRUE))

fishing_long |> 
  filter(!region %in% c("U.S. Total", "Grand Total"),
         !is.na(values)) |> 
  mutate(Species = fct_lump_n(Species, 6)) |> 
  ggplot(aes(x= Year, y = values, color = Species)) +
  scale_color_manual(values = palette.colors(7, "Okabe-Ito"))+
  geom_line()

fishing_long |> 
  select(Year, Species, region, values) |> 
  pivot_wider(names_from = region, values_from = values) |> 
  print(width = Inf)

#lake different than erie
fishing <- read_excel("data/raw/commercial.xlsx", sheet = "Superior")
#What is in the grain?
##one row represents region, year, and weight for fish caught

fishing_long <- fishing |> 
  pivot_longer(cols =!c(Year, Lake, Species, Comments), 
               names_to ="region",
               values_to ="values")

fishing_long |> 
  filter(!region %in% c("U.S. Total", "Grand Total")) |> 
  summarise(total = sum(values, na.rm = TRUE))

#What is the total catch for that lake?
##1903881???

#Distinct regions?
fishing_long |> distinct(region)
##6 distinct regions 

fishing_long |> 
  filter(!region %in% c("U.S. Total", "Grand Total"),
         !is.na(values)) |> 
  mutate(Species = fct_lump_n(Species, 6)) |> 
  ggplot(aes(x= Year, y = values, color = Species)) +
  scale_color_manual(values = palette.colors(7, "Okabe-Ito"))+
  geom_line()

library(tidyverse)
library(here)


plant_trait_raw <- read_csv(file = here("data", "Plant_Longevity_Trydatabase.csv"))


plant_trait_summary <- plant_trait_raw |> 
  filter(TraitID == "59") |> 
  drop_na(OrigValueStr) |> 
  select(AccSpeciesName,OriglName,OrigValueStr) |> 
  mutate(longevity = case_when(OrigValueStr == "annual"|
                                 OrigValueStr == "always annual"|
                                 OrigValueStr == "summer annual"|
                                 OrigValueStr == "winter annual"|
                                 OrigValueStr == "Annual"|
                                 OrigValueStr == "annuals"|
                                 OrigValueStr == "summer annuals"|
                                 OrigValueStr == "winter annuals" ~ 1,
                               OrigValueStr == "1" ~ 1,
                               OrigValueStr == "6"~ 6,
                               OrigValueStr == "7" ~ 7,
                               OrigValueStr == "biennial" ~2,
                               OrigValueStr == "annual/biennial" ~ 1.5,
                               OrigValueStr == "yes" & OriglName == "Plant phenology: Annual" ~1,
                               OrigValueStr == "yes" & OriglName == "Plant phenology: Biennial" ~ 2,
                               .default = NA_real_)) |> 
  group_by(AccSpeciesName) |> 
  summarise(longevity = mean(longevity, na.rm = T))

         
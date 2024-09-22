library(reprex)
library(tidyverse)
library(dplyr)
library(lubridate)
library(scales)
library(patchwork)
library(ggthemes)
library(DT)

energy_data = read_csv("../../../LectureSlides/owid-energy-data.csv")
countries_included = c("USA","DEU","FRA","SWE","CHN")


plot1 = energy_data |> 
  filter(iso_code %in% countries_included, year > 1980) |> 
  group_by(iso_code,country) |> 
  select(iso_code,
         year,
         hydro_share_elec,
         wind_share_elec,
         solar_share_elec,
         coal_share_elec,
         gas_share_elec,
         nuclear_share_elec) |>
  pivot_longer(
    cols = c("hydro_share_elec",
             "wind_share_elec",
             "solar_share_elec",
             "coal_share_elec",
             "gas_share_elec",
             "nuclear_share_elec"),
    names_to = "GeneratorType",
    values_to = "Electricity_Per_Capita"
  ) |> 
  ggplot(aes(x=year,y=Electricity_Per_Capita,color=country)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ GeneratorType, scales = "free") +
  theme_classic() +
  ylab("Share of Electricity from Different Sources") +
  xlab("Year") +
  labs(title = "Change in Electricity Generation Share over time",
       color = "Country")

plot1 







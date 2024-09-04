library(tidyverse)

raw_df <- read_csv("Merged Costs (1.0) - Sheet1.csv") %>% 
  janitor::clean_names() %>% 
  filter(real_cost != "MAX")

raw_df %>% 
  arrange(desc(cost_km_millions))

raw_df %>% 
  write_csv("transit_cost.csv")



transit_costs = read_csv("transit_cost.csv")


glimpse(transit_costs)



transit_costs |>  
  group_by(city) |> 
  summarise(mean_cpm = mean(cost_km_millions)) |>  
  arrange(desc(mean_cpm)) |> 
  mutate(order = rank(-mean_cpm)) |> 
  mutate(is_nyc = (city == "New York")) |> 
  filter(mean_cpm > 100) |> 
  ggplot(aes(x=order,y=mean_cpm,color=is_nyc,size = (mean_cpm))) +
  geom_point()

transit_costs = transit_costs |> mutate(tunnel_per = as.numeric(gsub("%", "", tunnel_per)))


transit_costs = transit_costs |> mutate(start_year = as.numeric(start_year),end_year = as.numeric(end_year))

transit_costs |> 
  filter(length< 500) |> 
  ggplot(aes(x=stations,y=as.numeric(real_cost),color = city == "New York")) +
  geom_point()


countries_list = transit_costs |> count(country) |> filter(n>5)



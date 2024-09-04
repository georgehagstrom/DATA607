library(tidyverse)
library(ggridges)


transit_costs = read_csv("transit_cost.csv")


glimpse(transit_costs)

# making a graph with ggplot

ggplot(data = transit_costs,mapping = aes(x=cost_km_millions))

ggplot(data = transit_costs,mapping = aes(x=length,y=cost_km_millions))

ggplot(data = transit_costs,mapping = aes(x=length,y=cost_km_millions,color=country,))

# Can use the pipe to send data, can leave the mapping = blank:

transit_costs |> ggplot(aes(x=cost_km_millions))

# Making our first visualization, adding a geom

transit_costs |> ggplot(aes(x=cost_km_millions)) +
  geom_histogram(binwidth = 200)



transit_costs |> ggplot(aes(x=cost_km_millions)) +
  geom_density()

# Which are the cities with the highest transit costs?

transit_costs |>
  group_by(city) |> 
  summarise(mean_cpm = mean(cost_km_millions)) |>  
  arrange(desc(mean_cpm)) |> 
  mutate(order = rank(-mean_cpm))

cities_label = c("New York","London","Barcelona","Milan","Paris","Mexico City","Beijing","Singapore","Istanbul")

transit_costs |>  
  group_by(city) |> 
  summarise(mean_cpm = mean(cost_km_millions)) |>  
  arrange(desc(mean_cpm)) |> 
  mutate(order = rank(-mean_cpm)) |> 
  mutate(is_nyc = (city == "New York")) |> 
  filter(mean_cpm > 100) |> 
  ggplot(aes(x=order,y=mean_cpm,color=is_nyc,size = mean_cpm)) +
  geom_point(show.legend = FALSE) +
  scale_size_area() +
  geom_text_repel( data = .%>%  filter(city %in% cities_label),
                   aes(x=order,y=mean_cpm,label = city),
                   color="black",
                   size=8,
                   show.legend = FALSE) +
  theme_bw(base_size = 18) +
  labs(
    x = "Rank",
    y = "Mean Cost in Millions per KM",
    title = "Cities Ranked by Cost of Transit",
    size = "Mean CPM"
  ) 

# Look at Year or project size

transit_costs |> 
  ggplot(aes(x=as.numeric(start_year),y=cost_km_millions)) +
  geom_point()
  

transit_costs |> 
  ggplot(aes(x=length,y=cost_km_millions)) +
  geom_point()

transit_costs |> 
  filter(length<300) |> 
  ggplot(aes(x=length,y=cost_km_millions)) +
  geom_point() +
  theme_bw()

transit_costs |> mutate(years_per_km = (as.numeric(end_year) - as.numeric(start_year))/length) |> 
  ggplot(aes(x=years_per_km,cost_km_millions)) +
  geom_point()  +
  theme_bw()

transit_costs |> arrange(desc(cost_km_millions)) |> 
  select(city,line)














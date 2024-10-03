library(tidyverse)
library(babynames)
library(ggplot2)
library(patchwork)

babynames

# Used this for some inspiration: https://www.washingtonpost.com/business/interactive/2024/baby-names-trendy-suffixes/

# Problem 1: What are the most common starting and ending letters for names, and how have they varied over time

babynames |> 
  mutate(first_letter = str_sub(name,1,1)) |> 
  group_by(year,first_letter) |> 
  summarise(prop = sum(prop)) |> 
  filter(year %in% c(2010,1880)) |> 
  mutate(first_letter = as_factor(first_letter)) |> 
  ggplot(aes(x=prop, y= fct_reorder(first_letter, prop) ,color=as_factor(year))) + 
  geom_point() +
  theme_bw(base_size=16) +
  labs(y = "First Name Letter",
       x = "Proportion",
       title = "Changes in Name first letters over time")



babynames |> 
  mutate(last_letter = str_sub(name,-1,-1)) |> 
  group_by(year,last_letter) |> 
  summarise(prop = sum(prop)) |> 
  filter(year %in% c(2010,1880)) |> 
  mutate(last_letter = as_factor(last_letter)) |> 
  ggplot(aes(x=prop, y= fct_reorder(last_letter, prop) ,color=as_factor(year))) + 
  geom_point() +
  theme_bw(base_size=16) +
  labs(y = "Last Name Letter",
       x = "Proportion",
       title = "Changes in Name final letters over time")



# Problem 2: Have names been getting longer over time?

babynames |> 
  group_by(year) |>
  summarise(mean_name_length = sum(str_length(name) * prop)) |> 
  ggplot(aes(x=year,y=mean_name_length)) +
  geom_point() +
  theme_bw(base_size=16) +
  labs(x="Year",
       y="Mean Name Length",
       title = "Mean Name Length Over Time")



# Problem 3: Which letters are getting more or less popular?

babynames |> 
  group_by(year) |> 
  summarize(prop_x = mean(str_detect(name, "x"))) |> 
  ggplot(aes(x = year, y = prop_x)) + 
  geom_line()


babynames |> 
  group_by(year) |> 
  summarize(prop_q = mean(str_detect(name, "Alex"))) |> 
  ggplot(aes(x = year, y = prop_q)) + 
  geom_line()

babynames |> 
  group_by(year) |> 
  summarize(prop_z = mean(str_detect(name, "z"))) |> 
  ggplot(aes(x = year, y = prop_z)) + 
  geom_line()


# Problem 4: Can we look at all the different versions of Megan?

# Different ways to write 

Meg_match = regex("^(Ma|Me)(g|gh)(yn|in|on|an|en|n)$")

babynames |> filter(str_detect(name,regex_Meg)) |> 
  ggplot(aes(x=year,y=prop,color=as_factor(name))) + geom_point()


babynames |> 
  filter(str_detect(name,Meg_match)) |> 
  group_by(year) |> 
  summarise(Megan_Prop = sum(prop)) |> 
  ggplot(aes(x=year,y=Megan_Prop)) + 
  geom_point()




glimpse(starwars)

starwars |> ggplot(
  aes(x = height, y = mass, color = gender, size = birth_year)) +
  geom_point()


ggplot(starwars, 
       aes(x = height, y = mass, color = gender, size = birth_year)) +
  geom_point(color = "#30509C") +
  labs(
    title = "Properties of Star Wars Characters",
    x = "Height (cm)",
    y = "Mass (kg)",
    size = "Birth Year (BBY)"
  )

starwars |> ggplot(aes(x=mass)) +
  geom_histogram(binwidth = 10) +
  labs(
    title = "Mass of Star Wars Characters",
    y = "Count",
    x = "Mass (kg)"
  )


starwars |> filter(species == "Human") |> 
  ggplot(aes(y = mass, x = height )) +
  geom_point()

starwars |>
  ggplot(aes(x=height,y=eye_color)) +
  geom_boxplot(na.rm=TRUE)
  
  

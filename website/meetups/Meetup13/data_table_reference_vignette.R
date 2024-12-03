library(tidyverse)
library(nycflights13)
library(data.table)

flights

address(flights)

flights = flights |> mutate(speed = distance/air_time*60)

address(flights)

flights = fread("https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv")

address(flights)

flights$year = flights$year -2

address(flights)

flights[ , year := year + 2]
address(flights)

flights

flights[ , `:=`(speed = distance/air_time*60, 
                catch_up = dep_delay - arr_delay)   ]

address(flights)

flights[origin == "JFK", catch_up := 0]

flights

flights[origin == "JFK"][ , catch_up := dep_delay -arr_delay]

flights

address(flights[origin == "JFK"][ , catch_up := dep_delay -arr_delay][])

address(flights)


flights[ , catch_up := NULL]

address(flights)

flights[ , max_speed := max(speed), by = .(origin, dest) ]

address(flights)

my_flights = flights
address(my_flights)

my_flights[origin == "JFK", origin := "jfk"]

address(my_flights)

flights

my_flights = copy(flights)
address(my_flights)
address(flights)

my_flights[origin == "jfk", origin := "JFK"]
my_flights
flights


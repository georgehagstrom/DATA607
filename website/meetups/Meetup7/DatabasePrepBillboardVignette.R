library(tidyverse)
library(DBI)
library(duckdb)
library(dbplyr)

billboard = read_csv("../../meetups/Meetup7/billboard.csv")

billboard

billboard_long = billboard |> pivot_longer(cols = contains("week"),
                                          names_to = "week",
                                          values_to = "rank",
                                          values_drop_na = TRUE) |> 
  mutate(week = parse_number(week))

billboard_long

song = billboard_long |> select(artist.inverted,year,track,time,genre, date.entered,date.peaked)


song = song |> unique()
song
song |> count(track) |> filter(n>1)

song |> filter(track == "Where I Wanna Be")

song = song |> mutate(track_id = row_number())
song = song |> rename(c(artist = "artist.inverted",date_entered = "date.entered",date_peaked = "date.peaked"))


song = song |> mutate(artist = as_factor(artist),
                      genre = as_factor(genre))

billboard_long = billboard_long |> left_join(song)
billboard_long
billboard_long = billboard_long |> select(week,rank,track_id)


con = dbConnect(duckdb(),"/home/georgehagstrom/work/Teaching/DATA607/website/meetups/Meetup7/billboard")
con |> dbWriteTable("rank",billboard_long)
con |> dbWriteTable("song",song)

song = song |> mutate(artist = str_conv(artist,"UTF-8"),
                      track = str_conv(track,"UTF-8"))

con |> dbWriteTable("song",song)

con |> dbReadTable("song")
dbDisconnect(con)

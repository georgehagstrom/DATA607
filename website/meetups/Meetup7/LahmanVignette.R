library(tidyverse)
library(Lahman)

Batting

Batting |> count(playerID,yearID) |> filter(n>1)

Batting |> count(playerID,yearID, teamID) |> filter(n>1)

Batting |> count(playerID,yearID,stint, teamID) |> filter(n>1)


Batting |> count(playerID,yearID,stint) |> filter(n>1)

Salaries

Batting |> inner_join(Salaries)

Batting |> inner_join(Salaries) |> filter(yearID == 2015) |> ggplot(aes(x=salary,y=HR)) + geom_point()

Batting |> inner_join(Salaries) |> anti_join(Pitching, by = c("playerID","yearID","stint")) |> filter(yearID == 2015) |> ggplot(aes(x=salary,y=HR)) + geom_point()


Batting |> inner_join(Salaries) |> anti_join(Pitching, by = c("playerID","yearID","stint")) |> filter(yearID == 2015) |> ggplot(aes(x=log(salary),y=HR)) + geom_point()


Batting |> inner_join(Salaries) |> semi_join(Pitching, by = c("playerID","yearID","stint")) |> filter(yearID == 2015) |> ggplot(aes(x=log(salary),y=HR)) + geom_point()


MVP = AwardsPlayers |> filter(awardID == "Most Valuable Player")
MVP


Batting |> left_join(MVP, by = c("playerID","yearID")) |> ggplot(aes(x=HR,y=awardID)) + geom_boxplot()



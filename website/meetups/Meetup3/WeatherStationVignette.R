library(tidyverse)

# Open a dataset from a weather station MX17004

weather = read_csv("weather.csv")

# Take a look at what is in the dataset

glimpse(weather)

# Things to note, it is very messy, and there are variables spread in both the column names (d1, d2, etc), and across
# rows (tmin, tmax) Need to use both pivot_longer and pivot_wider here

# first thing to do is pivot_longer to convert the days from columns into data. We dropped the "na" values
# because they don't correspond to measurements that weren't take, they correspond to days that don't
# exist because some months have fewer than 31 days

weather = weather |> 
  pivot_longer(d1:d31,
               names_to = "day",
               values_to = "value",
               values_drop_na = TRUE)
  

weather

# This looks good but the day column is a bit ugly. There are several ways to get rid of the "d"
# and turn the day variable into a number, we will use the parse_number function which is introduced 
# in chapter 5

weather = weather |> 
  mutate(
    day = parse_number(day)
  )

weather

# Notice how the type has changed from char to dbl and the "d" is gone. For more complicated cases 
# do ?parse_number, check out the doc page on it, or wait till we cover strings later in the semester

# It makes sense to change the order of columns so day is next to month. We can do this with the select
# function:

weather = weather |> select(id, year, month, day, element, value)

weather

# This dataset is looking a lot nicer, but the element column isn't a variable, it contains variable names,
# so we need to pivot longer

# first we will check on the element data and make sure its just tmin and tmax:

weather |> count(element)

# This shows that for each point in the dataset one measurement of tmin, and one measurement of tmax was made

# Now we pivot wider, taking the "names" from element and the "values from value:

weather = weather |> 
  pivot_wider(
    names_from = element,
    values_from = value
  )

weather

# Now we have a tidy dataset that is ready for analysis
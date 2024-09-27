library(tidyverse)
library(lubridate)
library(TTR)
setwd("/home/georgehagstrom/work/Teaching/DATA607/website/meetups/Meetup5")
electricity = read_csv("household_power_consumption.txt")

electricity;


# Ok not a csv file, so need a different command

electricity = read_delim("household_power_consumption.txt",delim=";")
electricity

# Notice that some variables are characters when they should really be numbers

electricity |> mutate(Sub_metering_1 = parse_number(Sub_metering_1))

# Suggests that "?" is being used for missing values

# Two options here, to show you one way that logical subseting can work, lets replace all ? with
# NA:

electricity[electricity == "?"] = NA

# Now we try to fix each row (note the capital Y):

electricity = electricity |> mutate(Date = parse_date(Date,"%d/%m/%Y"),
                                    Global_active_power = parse_number(Global_active_power),
                                    Global_reactive_power = parse_number(Global_reactive_power),
                                    Voltage = parse_number(Voltage),
                                    Global_intensity = parse_number(Global_intensity),
                                    Sub_metering_1 = parse_number(Sub_metering_1),
                                    Sub_metering_2 = parse_number(Sub_metering_2))



# Now let's look:

electricity |> filter(Date == "2006-12-17") |> ggplot(aes(x=Time,y=Global_active_power)) +
  geom_point()



electricity = electricity |> filter(is.finite(Global_active_power)) |> mutate(GAP_roll = runMean(Global_active_power,n=30))

#electricity = electricity |>  mutate(GAP_roll = roll_meanr(Global_active_power,n=30,na.rm=TRUE,fill=NA))

electricity = electricity |> mutate(GAP_std = runSD(Global_active_power,n=30))


electricity |> filter(Date == "2006-12-17")  |> ggplot(aes(x=Time,y=GAP_roll)) +
  geom_point() + geom_ribbon(aes(ymin=GAP_roll-GAP_std, ymax=GAP_roll+GAP_std),alpha=0.2,fill="red")


electricity = electricity |> mutate(Christmas = (month(Date)==12) & (mday(Date)==25))

electricity |> group_by(Christmas,Time) |> summarise(Sub_metering_3_mean = mean(Sub_metering_3) ) |> 
  ggplot(aes(x=Time,y=Sub_metering_3_mean,color=Christmas)) + geom_point()

electricity |> group_by(Christmas,Time) |> summarise(Sub_metering_3_mean = mean(Sub_metering_3) ) |>
  group_by(Christmas) |> mutate(Sub_metering_3_roll = runMean(Sub_metering_3_mean,30)) |> 
  ggplot(aes(x=Time,y=Sub_metering_3_roll,color=Christmas)) + geom_point()


electricity |> mutate(Day_Of_Week = wday(Date)) |> group_by(Day_Of_Week,Time) |>
  summarise(
    mean_gap = mean(Global_active_power),
    mean_kitchen = mean(Sub_metering_1)
  ) |> 
  ggplot(aes(x=Time,y=mean_gap)) + geom_point() + facet_wrap(~Day_Of_Week)


electricity = electricity |> mutate(Sub3_mean_Norm = runMean(Sub_metering_3,30)/(runMean(Global_active_power,30)*(1000/60)))


electricity |>  group_by(month(Date)) |>
  summarise(
    mean_Sub3_norm = mean(Sub3_mean_Norm,na.rm=TRUE)
  ) |> arrange(desc(mean_Sub3_norm)) |> print()
 



electricity |> mutate(month = month(Date)) |> 
  group_by(month,Time) |> 
  summarise(Sub3_Frac_mean = mean(Sub3_Frac,na.rm=TRUE),
  Sub3Frac_max = max(Sub3_Frac,na.rm=TRUE)) |> 
  ggplot(aes(x=Time,y=Sub3_Frac_mean)) + geom_point() + facet_wrap(~month)


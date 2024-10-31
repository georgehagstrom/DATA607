library(httr2)
library(tidyverse)
library(jsonlite)
library(tibblify)
library(lubridate)

# EPA api description https://aqs.epa.gov/aqsweb/documents/data_api.html


epa_email = "georgehagstrom@gmail.com"

epa_url = "https://aqs.epa.gov/data/api/"
epa_key = read_csv("/home/georgehagstrom/work/Teaching/DATA607/website/meetups/Meetup10/epa_key.csv") |> 
  pull(key)

# This creates a blank request targetted at the epa_url

epa_req = epa_url |> request() |> 
  req_url_query(
    email = epa_email
  )

epa_req

# We are going to need to download some lists to figure out how to find things we are interested in

epa_classes_url = "list/classes"

epa_counties_url = "list/countiesByState"

epa_states_url = "list/states"

epa_pars_url = "list/parametersByClass"

# Which is New York State?

epa_req_states = epa_req |> 
  req_url_path_append(epa_states_url) |> 
  req_url_query(
    key = epa_key
  ) 

states_list = epa_req_states |> req_perform() |> 
  resp_body_json()


states_list

str(states_list)

#

epa_req_counties = epa_req |> 
  req_url_path_append(epa_counties_url) |> 
  req_url_query(
    key = epa_key,
    state = "36"
  )

counties_list = epa_req_counties |> 
  req_perform() |> 
  resp_body_json()


str(counties_list)



epa_classes_list = epa_req |> 
  req_url_path_append(epa_classes_url) |> 
  req_url_query(
    key = epa_key
  ) |> 
  req_perform() |> 
  resp_body_json()

str(epa_classes_list)

pm2.5code = "PM2.5 CONT NONREF" 

epa_pm2.5pars_list = epa_req |> 
  req_url_path_append(epa_pars_url) |> 
  req_url_query(
    key = epa_key,
    pc = pm2.5code
  ) |> 
  req_perform() |> 
  resp_body_json()

str(epa_pm2.5pars_list)

counties_list = epa_req_counties |> req_perform() |> resp_body_json() 

epa_classes_list$Data |> tibblify() |> print(n=27)

pm2.5code = "PM2.5 CONT NONREF" 

epa_pm2.5pars_list = epa_req |> 
  req_url_path_append(epa_pars_url) |> 
  req_url_query(
    email = api_email,
    key = epa_key,
    pc = pm2.5code
  ) |> 
  req_perform() |> 
  resp_body_json()

str(epa_pm2.5pars_list)


county_endpoint = "monitors/byCounty"


epa_req_county_monitors = epa_req |> 
  req_url_path_append(county_endpoint) |> 
  req_url_query(
    key = epa_key,
    county = "061",
    param = "88501",
    bdate = "20150101",
    edate = "20240930",
    state = "36"
  )

county_monitor_list = epa_req_county_monitors |> req_perform() |> resp_body_json()

str(county_monitor_list)


county_monitor_list$Data |> tibblify() 

site = "0135"


data_by_site_url = "sampleData/bySite"

epa_req_sample = epa_req |> 
  req_url_path_append(data_by_site_url) |> 
  req_url_query(
    key = epa_key,
    site = site,
    param = "88501",
    state = "36",
    county = "061",
    bdate = "20070101",
    edate = "20241231"
  )


epa_sample_data = epa_req_sample |> req_perform() |> 
  resp_body_json()

resp <- last_response()
resp |> resp_body_json()
resp |> resp_headers()


epa_req_sample = epa_req |> 
  req_url_path_append(data_by_site_url) |> 
  req_url_query(
    key = epa_key,
    site = site,
    param = "88501",
    state = "36",
    county = "061",
    bdate = "20080101",
    edate = "20081231"
  )

epa_req_sample


epa_sample_data = epa_req_sample |> req_perform() |> 
  resp_body_json()


str(epa_sample_data)



epa_sample_data$Data |> tibblify() |> select(date_gmt,sample_measurement) |> group_by(date_gmt) |> 
  summarise(mean_pm25 = mean(sample_measurement)) |> ggplot(aes(x=as_datetime(date_gmt),y=mean_pm25)) +
  geom_point() +
  scale_x_datetime()


# What if we wanted to get the data for all the years?

# use req_perform_sequential (also see req_perform_iterative, req_perform_parrallel etc)

year_list = as.character(2007:2024)

year_list

year_function = function(year){
  bdate = str_c(year,"0101")
  edate = str_c(year,"1231")
  
  epa_req_sample = epa_req |> 
    req_url_path_append(data_by_site_url) |> 
    req_url_query(
      key = epa_key,
      site = site,
      param = "88501",
      state = "36",
      county = "061",
      bdate = bdate,
      edate = edate,
    ) |> req_throttle(rate = 1 / 10)

}


epa_req_list = year_list |> map(year_function)

epa_sample_data_full = 
  epa_req_list |> 
  req_perform_sequential(
    progress = TRUE
  )

epa_sample_data_full = epa_sample_data_full |> map(resp_body_json)
  

epa_sample_data_full |> map(\(x) x$Data |> tibblify())



site_data = epa_sample_data_full[-18] |> 
  map(\(x) x$Data |> tibblify() ) 


site_data |> list_rbind()

complete_data = site_data |> map(select,!starts_with("date_of_last_change")) |> list_rbind()

complete_data

# How to select multiple things?

sample_data_by_state_url = "sampleData/byState"

state_req_sample = epa_req |> 
  req_url_path_append(sample_data_by_state_url) |> 
  req_url_query(
    key = epa_key,
    param = "88501",
    state = c("36","37"),
    date = "20230101",
  )


state_req_sample = epa_req |> 
  req_url_path_append(sample_data_by_state_url) |> 
  req_url_query(
    key = epa_key,
    param = "88501",
    bdate = "20230101",
    edate = "20230102",
  ) |> 
  req_url_query(state = c("36","37"),.multi="pipe")
state_req_sample |> req_perform()
resp = last_response() |> resp_body_json()
resp$Header[[1]]$error

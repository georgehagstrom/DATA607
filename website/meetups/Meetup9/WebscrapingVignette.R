library(tidyverse)
library(rvest)
library(robotstxt)

# "Easy"

url_nfl_reference = "https://www.pro-football-reference.com/years/2024/passing.htm"

paths_allowed(url_nfl_reference)

passing_data_2024 = read_html(url_nfl_reference) |> 
  html_element("table")

passing_data_2024 |> html_table()




# Easy if you know

cuny_sps_catalog = "https://sps.catalog.cuny.edu/courses?sortBy=code"

paths_allowed(cuny_sps_catalog)

read_html(cuny_sps_catalog) |> html_element("table") # Doesn't work

read_html_live(cuny_sps_catalog) |> html_element("table") # Doesn't work

cuny_catalog = read_html_live(cuny_sps_catalog)

cuny_catalog |> html_element("table") |> html_table()




# Difficult

url_imdb = r"(https://www.imdb.com/chart/top/)" 

paths_allowed(url_imdb)

# Let's try to read the page and find tables

imdb_site = url_imdb |> read_html()

imdb_site |> html_element("table") 

# This produces an error because no table was found. So we have to inspect the page

# After some initial inspection, we find that the table is stored as a list under a number of sub headings. 
# we pulled out the css_selector for the overall list here:

css_main_list = r"(#__next > main > div > div.ipc-page-content-container.ipc-page-content-container--center > section > div > div.ipc-page-grid.ipc-page-grid--bias-left > div > ul)"

imdb_site |>  html_elements(css_main_list)

# Now does this contain a list?

imdb_site |>  html_elements(css_main_list) |> html_elements("li")

# 25 elements!!!

imdb_site = url_imdb |> read_html_live()


top_250 = imdb_site |> html_elements("#__next > main > div > div.ipc-page-content-container.ipc-page-content-container--center > section > div > div.ipc-page-grid.ipc-page-grid--bias-left > div > ul") |> 
   html_elements("li")

top_250

# Good, now we need to do some more inspecting, to find where the data is stored in each list element. 

top_250[[1]] |> html_element("h3.ipc-title__text") |> html_text2() # The title

top_250[[1]] |> html_element("a") |> html_attr("href") # The url link to the imdb page of the movie

top_250[[1]] |> html_element("div.cli-title-metadata")  # A menu showing 3 pieces of data, but in an annoying format

top_250[[1]] |> html_element("div.cli-title-metadata") |> html_elements("span") |> html_text2()  # The data, hope it is the same format for all the other movies

top_250[[1]] |> html_element("span.ipc-rating-star--rating") |> html_text2() # The rating

top_250[[1]] |> html_element("span.ipc-rating-star--voteCount") |> html_text2() # The vote count, also in irritating format


# We want to do this for every single list member, so we will create a function

process_movie_data = function(movie){
  movie_title = movie |> html_element("h3.ipc-title__text") |> html_text2()
  url = movie |> html_element("a") |> html_attr("href")
  info_string = movie |> html_element("div.cli-title-metadata") |> html_elements("span") |> html_text2()
  year = info_string[1]
  run_time = info_string[2]
  rating_scale = info_string[3]
  rating = movie |> html_element("span.ipc-rating-star--rating") |> html_text2()
  vote_count = movie |> html_element("span.ipc-rating-star--voteCount") |> html_text2()
  
  tibble("title" = movie_title,
          url = url,
          year = year,
          run_time = run_time,
          rating_scale = rating_scale,
          rating = rating,
          vote_count = vote_count,
          )
}

imdb_top_250 = top_250 |> 
  map(process_movie_data) |> 
  list_rbind()











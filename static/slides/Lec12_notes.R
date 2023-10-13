library(rvest)
library(tidyverse)

## Example
url = "http://www.rottentomatoes.com/"
(session = polite::bow(url))

page = polite::scrape(session)


movies = tibble::tibble(
  title = page |> 
    html_elements(".dynamic-text-list__streaming-links+ ul .dynamic-text-list__item-title") |>
    html_text2(),
  tomatometer = page |>
    html_elements(".dynamic-text-list__streaming-links+ ul .b--medium") |>
    html_text2() |>
    str_remove("%$") |>
    as.numeric() |>
    (\(x) x/100)(),
  status = page |>
    html_elements(".dynamic-text-list__streaming-links+ ul .icon--tiny") |>
    html_attr("class") |>
    str_remove("icon ") |>
    str_remove("icon--tiny ") |>
    str_remove("icon__"),
  url = page |> 
    html_elements(".dynamic-text-list__streaming-links+ ul li a.dynamic-text-list__tomatometer-group") |>
    html_attr("href") |>
    (\(x) paste0(url, x))()
)


## Exercise 1

#scrape_movie_page = function(url) {
#  page = read_html(url)
#  
#  list(
#    n_reviews = page |> 
#      html_elements(".scoreboard__link--tomatometer") |>
#      html_text2() |>
#      str_remove(" Reviews") |>
#      as.integer(),
#    aud_ratings = page |>
#      html_elements(".scoreboard__link--audience") |>
#      html_text2() |>
#      str_remove(" Ratings"),
#    run_time = page |>
#      html_elements(".scoreboard__info") |>
#      html_text2() |>
#      str_split(", ", simplify = TRUE) |>
#      {.[,3]}
#  )
#}

movies = movies |>
  mutate(
    details = purrr::map(url, scrape_movie_page)
  ) |>
  unnest_wider(details)


## Exercise 1 - Bonus - demo inspector and shutting off javascript



library(tidyverse)
library(httr2)

beef_carc_new <- request("https://www.ec.europa.eu/agrifood/api") %>% 
  req_url_path_append("beef/prices") %>% 
  req_url_query(years = 2025) %>% 
  req_perform() %>% 
  resp_body_json() %>% 
  bind_rows(.id = "name") %>%
  select(begin_date = beginDate, end_date = endDate, state = memberStateName,
         category, product = productCode, price) %>% 
  mutate(price = parse_number(price)) %>%
  mutate(across(begin_date:end_date, dmy))

beef_live_new <- request("https://www.ec.europa.eu/agrifood/api") %>% 
  req_url_path_append("liveAnimal/prices") %>% 
  req_url_query(years = 2025) %>% 
  req_perform() %>% 
  resp_body_json() %>% 
  bind_rows(.id = "name") %>%
  select(begin_date = beginDate, end_date = endDate, state = memberStateName,
         category, unit, price) %>% 
  mutate(price = parse_number(price)) %>%
  mutate(across(begin_date:end_date, dmy))
#-----------------------------------------
req <- request("https://content.guardianapis.com") |>  # start the request with the base URL
  req_url_path("search") |>                            # navigate to the endpoint you want to access
  req_method("GET") |>                                 # specify the method
  req_timeout(seconds = 60) |>                         # how long to wait for a response
  req_headers("User-Agent" = "httr2 guardian test") |> # specify request headers
  # req_body_json() |>                                 # since this is a GET request the body stays empty
  req_url_query(                                       # instead the query is added to the URL
    q = "parliament AND debate",
    "show-blocks" = "all"
  ) |>
  req_url_query(                                       # in this case, the API key is also added to the query
    "api-key" = Sys.getenv("GUARDIAN_KEY")             # but httr2 also has req_auth_* functions for other
  )                                                    # authentication procedures
print(req)

resp <- req_perform(req)
resp

resp_status(resp) < 400

resp_content_type(resp) == "application/json"

returned_body <- resp_body_json(resp)
#lobstr::tree(returned_body, max_length = 25)

pluck(returned_body, "response", "total")

pluck(returned_body, "response", "pageSize")

pluck(returned_body, "response", "pages")

search_res <- pluck(returned_body, "response", "results")

view(search_res)

res <- pluck(search_res, 1)
res

id <- res$id
id

type <- res$type
type

time <- lubridate::ymd_hms(res$webPublicationDate)
time

headline <- res$webTitle
headline

pluck(res, "blocks", "body")

library(rvest)
text <- pluck(res, "blocks", "body", 1, "bodyHtml") |> 
  read_html() |> 
  html_text2()
text

parse_response <- function(res) {
  
  text <- pluck(res, "blocks", "body", 1, "bodyHtml") |> 
    read_html() |> 
    html_text2()
  
  tibble(
    id = res$id,
    type = res$type,
    time = lubridate::ymd_hms(res$webPublicationDate),
    headline = res$webTitle,
    text = text
  )
}
parse_response(res)

map(search_res, parse_response) |> 
  bind_rows()
#---------------------------------
search_members <- function(name) {
  
  # make the initial request
  resp <- request("https://members-api.parliament.uk/api/Members/Search") |>
    req_method("GET") |>
    req_url_query(
      Name = name,
      take = 20,
      skip = 0
    ) |> 
    req_headers(
      accept = "text/plain",
    ) |>
    req_perform() |> 
    resp_body_json()
  
  # checking the total and setting things up for pagination
  total <- resp$totalResults
  message(total, " results found")
  skip <- 0
  page <- 1
  
  # extract initial results
  items <- pluck(resp, "items")
  
  # while loops are repeated until the condition inside is FALSE
  while (total > skip) { 
    
    skip <- skip + 20
    page <- page + 1
    
    # we print a little status message to let the user know work is ongoing
    message("\t...fetching page ", page)
    
    # we retrieve the next page by adding an increasing skip
    resp <- request("https://members-api.parliament.uk/api/Members/Search") |>
      req_method("GET") |>
      req_url_query(
        Name = name,
        skip = skip,
        take = 20
      ) |> 
      req_headers(
        accept = "text/plain",
      ) |>
      req_throttle(rate = 1) |> # do not make more than one request per second
      req_perform() |> 
      resp_body_json()
    
    # we append the original result with the new items
    items <- c(items, pluck(resp, "items"))
    
  }
  
  # wrangle
  return(tibble(
    id                    = map_int(items, function(i) safe_pluck(i, "value", "id")),
    nameListAs            = map_chr(items, function(i) safe_pluck(i, "value", "nameListAs")),
    nameDisplayAs         = map_chr(items, function(i) safe_pluck(i, "value", "nameDisplayAs")),
    nameFullTitle         = map_chr(items, function(i) safe_pluck(i, "value", "nameFullTitle")),
    nameAddressAs         = map_chr(items, function(i) safe_pluck(i, "value", "nameAddressAs")),
    gender                = map_chr(items, function(i) safe_pluck(i, "value", "gender")),
    latestParty           = map(items, function(i) safe_pluck(i, "value", "latestParty")),
    latestHouseMembership = map(items, function(i) safe_pluck(i, "value", "latestHouseMembership"))
  ))
  
}
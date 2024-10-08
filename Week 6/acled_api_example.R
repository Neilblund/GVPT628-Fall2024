library(tidyverse)
library(httr2)


base_url <-'https://api.acleddata.com/acled/read' 
acled_req<-request(base_url)|>
  req_url_query(key = Sys.getenv("ACLED_KEY"),
                email = Sys.getenv("umd_email")
  )

event_query <- acled_req|>
  req_url_query(year = 2024,
                sub_event_type = "Peaceful protest"
                )


allevents<-data.frame()
current_page<-1


# pages remaining starts as true
pages_remaining <- TRUE

while(pages_remaining == TRUE){
  print(sprintf("getting page %s", current_page))
  
  result<-
    event_query|>
    req_url_query(page = current_page)|>
    req_perform()
  
  events<-resp_body_json(result)
  
  events_df<-events$data|>
    bind_rows()
  allevents<-bind_rows(allevents, events_df)
  
  # check to see if there are more results
  pages_remaining <- events$count > 0
  # increment the pagination:
  current_page<-current_page + 1
  
  
}







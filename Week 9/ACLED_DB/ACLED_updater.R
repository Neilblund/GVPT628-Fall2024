library(DBI)
library(httr2)
library(dbplyr)

# connect
acled_data <- dbConnect(RSQLite::SQLite(), "ACLED_data.sqlite")



if(!"acled" %in% dbListTables(acled_data)){
  table_creator <- readr::read_file('ACLED_schema.sql')
  result<-dbSendQuery(acled_data, statement=table_creator)
  dbClearResult(result)
  
}

# create base request
base_request<-request('https://api.acleddata.com/')|>
  req_url_query('key' = Sys.getenv("ACLED_API_KEY"),
                'email' = Sys.getenv("ACLED_EMAIL")
  )

# Updated event data ----
# check on last update
last_update<-dbGetQuery(acled_data, 'SELECT MAX(timestamp) as timestamp from acled')[[1]]

if(is.na(last_update)){
  last_update<-0
}

print(sprintf("updating rows from %s to present", lubridate::as_datetime(last_update)))

i<-1
rows<-5000

# update the acled event data table
while(rows == 5000){
  print(sprintf("updating from page %s", i))
  
  req<-base_request|>
    req_url_path_append('acled/read')|>
    req_url_query(timestamp =last_update,
                  page = i)
  
  result <- req|>
    req_perform()|>
    resp_body_json()
  events<- result[['data']]|>
    dplyr::bind_rows()
  
  if(length(events)==0){
    print("No events found")
    break
  }else{
    print(sprintf("inserting/updating %s rows", nrow(events)))
    dbWriteTable(acled_data, "newrows", events)
    
    upsert_statement<-sql_query_upsert(
      con = acled_data,
      table = ident("acled"),
      from = ident("newrows"),
      by = "event_id_cnty",
      update_cols = colnames(events)[which(colnames(events)!='event_id_cnty')]
    )
    
    result<-dbSendQuery(acled_data, upsert_statement)
    dbClearResult(result)
    dbRemoveTable(acled_data, 'newrows')
    
    rows<-nrow(events)
    i <- i +1
    Sys.sleep(3)
  }
}






# Deletions-----
# Check for deletions
last_deletion_check<-dbGetQuery(acled_data, "SELECT last_deletion_check from checks")[[1]]

print(sprintf("checking for deletions from from %s to present", 
              lubridate::as_datetime(last_deletion_check)))

i<-1
rows <- 5000

while(rows == 5000){
  print(sprintf("checking for deletions from page %s", i))
  
  deletions<-base_request|>
    req_url_path_append('deleted/read')|>
    req_url_query(page=i,
                  timestamp = last_deletion_check
                  )|>
    req_perform()|>
    resp_body_json()
  rows_to_delete<-deletions[['data']]|>
    dplyr::bind_rows()
  
  
  if(length(rows_to_delete)==0){
    print("No events found")
    break
  }else{
    dbWriteTable(acled_data, 'deletions', rows_to_delete)
    
    # update the last deletion check variable
    update<-'UPDATE checks SET last_deletion_check = (WITH lastcheck as (
    SELECT last_deletion_check from checks 
    UNION SELECT max(CAST(deleted_timestamp as integer)) from deletions)
    select MAX(last_deletion_check) as last_deletion_check from lastcheck)'
   
    ud<-dbSendQuery(acled_data, update)
    dbClearResult(ud)
    delete_statement<-sql_query_delete(
      con = acled_data,
      table = ident("acled"),
      from = ident("deletions"),
      by = 'event_id_cnty'
      
    )
    
    
    result<-dbSendQuery(acled_data, delete_statement)
    dbClearResult(result)

    
    dbRemoveTable(acled_data, 'deletions')
    
    rows<-nrow(rows_to_delete)
    i <- i +1
    Sys.sleep(3)
  }
}


dbDisconnect(acled_data)



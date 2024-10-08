library(httr2)
library(tidyverse)

#Functions-----
# the pagination function: 
paginate_congress = function(resp, req, key){
  # take the response, format as json, and extract the next URL 
  nextpage<-resp|>
    resp_body_json()|>
    pluck('pagination', "next")
  # this needs to explicitly return a NULL value if there is no page
  if(is.null(nextpage)){
    return(NULL)
  }
  # if its not null, then add the API key and return the new request object:
  new_request <- nextpage|>
    request()|>
    req_url_query('api_key'= Sys.getenv("CONGRESS_API_KEY"))
  
  return(new_request)
}


# the legislation parser
legFunction<-function(response){
  leg<-response|>
    resp_body_json()
    
  if("sponsoredLegislation" %in% names(leg)){
    leg<-leg|>
      pluck('sponsoredLegislation')
  }
  if("cosponsoredLegislation" %in% names(leg)){
    leg<-leg|>
      pluck('cosponsoredLegislation')
  }
  
  leg_df<-leg|>
    enframe()|>
    unnest_wider(value)|>
    unnest_wider(latestAction)|>
    unnest_wider(matches("policyArea"), names_sep='_')
  return(leg_df)
  
}


# The member parser
memberFunction<-function(response){
  congress_df <- 
    response|>
    resp_body_json()|>
    pluck('members')|>
    enframe(name='index')|>
    unnest_wider(value)
  
  congress_df_wider <-
    congress_df |>
    # hoist lets us use negative indexing to get the last item in list,
    # so this only grabs the most recent term for each member:
    hoist(terms, recent_term = list('item', -1))|>
    unnest_wider(recent_term)
  return(congress_df_wider)
}



#Base information-----
# get the base URL 
request = request('https://api.congress.gov/v3/')|>
  req_url_query('api_key'= Sys.getenv("CONGRESS_API_KEY"))|>
  req_throttle(rate = 5000/3600)


# get the current congress: 
current_congress<-request|>
  req_url_path_append("congress/current")|>
  req_perform()|>
  resp_body_json()



#Member data -----
initial_request<- request |>
  req_url_path_append("member","congress", current_congress$congress$number) |>
  req_url_query(limit = 250, offset = 0) 

all_results<-req_perform_iterative(
  # the initial query: 
    req = initial_request,
    # function to return the the next url 
    next_req = paginate_congress

)

# Apply the memberFunction to all data
all_members<-resps_data(all_results, memberFunction)|>
  # filter retired members
  filter(is.na(endYear))|>
  # filter to only include members of the house
  filter(chamber == 'House of Representatives')


# save the results
saveRDS(all_members, file='current_house_members.rds')


#Sponsors/Cosponsors----



  
##Cosponsors--------------------------------------------------------------------

memberLegislation<-data.frame()

i<-1

for(i in i:length(all_members$bioguideId)){
  id <-all_members$bioguideId[i]
  cosponsor_request <- request|>
    req_url_path_append('member', id, 'cosponsored-legislation')
  
  cosponsored_leg<-req_perform_iterative(
      req =cosponsor_request,
      next_req = paginate_congress
    
  )
  cos_df<-resps_data(cosponsored_leg, legFunction)
  cos_df$legislator<-id
  memberLegislation<-bind_rows(memberLegislation, cos_df)
  print(i)
  
}


# Save the object as cosponsorships
saveRDS(memberLegislation, file='cosponsorships.rds')

##Sponsors----------------------------------------------------------------------




memberLegislation<-data.frame()

i<-1

for(i in i:length(all_members$bioguideId)){
  id <-all_members$bioguideId[i]
  sponsor_request <- request|>
    req_url_path_append('member', id, 'sponsored-legislation')
  
  sponsored_leg<-req_perform_iterative(
    req =sponsor_request,
    next_req = paginate_congress
    
  )
  if(sponsored_leg[[1]]|>resp_body_json()|>pluck('pagination', 'count')>0){
    spon_df<-resps_data(sponsored_leg, legFunction)
    spon_df$legislator<-id
    memberLegislation<-bind_rows(memberLegislation, spon_df)
  }

  print(i)
  
}


# save the list of sponsors
saveRDS(memberLegislation, file='sponsors.rds')



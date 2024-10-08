library(tidyverse)
library(httr2) # the package we'll use to make API requests 

# Storing your API key----------------------------------------------------------
# follow instructions here: https://developer.nytimes.com/get-started to get an 
# API key 
# 
# There's not a ton of incentive for someone to steal your New York Times API key 
# since its free and easy to obtain. But in general you should treat keys like you
# would treat a password. Above all: Never store a key in plain text in a script! 
# especially if you plan to share that script with someone else!
# 
# This is not the most secure way to store this info, but a very easy way to avoid
# this is to do the following: 
# 1. install the usethis package
# 2. run usethis::edit_r_environ()
# 3. Add something the following on a new line in your Renviron file: 
#      NYTIMES_API_KEY = "YOUR API KEY HERE" 
#   to your Renviron file
# 4. Restart R
# 
# Now you should be able to access your API key by running:
#  Sys.getenv("NYTIMES_API_KEY") 
# So now just use that command in place of the key itself whenever you need it. 
#_______________________________________________________________________________




#Making a request---------------------------------------------------------------
# The httr2 package offers a lot of general functions for working with APIs. 
# We'll start with a request, and then add paramters to it until we get what we
# want, then we'll send the request and parse the result. 
#_______________________________________________________________________________

# create the initial request object. Note: nothing is actually sent to the server 
# until we run req_perform() so everything before that is just creating the request

request = request('https://api.nytimes.com/svc/news/v3/content/')|>
# We authenticate the request by just adding our api key as a parameter:
  req_url_query('api-key'= Sys.getenv("NYTIMES_API_KEY"))



# get a list of all the available sections sections 
# (see: https://developer.nytimes.com/docs/timeswire-product/1/routes/content/section-list.json/get)

section_list <- request|> # start with the initial request
  req_url_path_append('section-list.json')|> # append this to the URL
  req_perform()|> # perform the request
  resp_body_json() # parse json formatted result as an R list

# check out the resulting structure: 
str(section_list)


##Pulling wire articles----------------------------------------------------------
# See: https://developer.nytimes.com/docs/timeswire-product/1/overview
#_______________________________________________________________________________
# request an article
us_articles = request|>
  req_url_path_append("all", "u.s..json")|> # append the source and section name
  req_url_query('limit'= 500)|> # max limit of 500 results
  req_perform()|> # run this request
  resp_body_json() # parse the result


# much more complicated structure! Why? 
str(us_articles$results[[1]])


# now convert to a data frame and unnest the results 
cframe <- us_articles$results|> # just take the actual articles part of the response
  enframe()|> # convert to a data frame
  unnest_wider(value) # unnest this result 


# get a count of the people mentioned in each article: 
people <- unnest(cframe, cols=per_facet)|> # the per_facet is still nested, so unnest it
  mutate(person = unlist(per_facet))|> # needs one more unlist-ing
  count(person) |> # count the unique results
  arrange(-n) # sort from highest to lowest

# look at the results
head(people)

##Pulling articles in a loop-----------------------------------------------------
# You can only get 500 results at a time, but there are probably a lot more
# available than that. In order to get more, we need to add an offset parameter 
# to the request. Adding offset=500 along with limit =500 will give you results 
# 501-1000, offset 1000 will give results 1001-1500 and so on. 
# The NYT API sets a limit of 500 requests per day, and 5 requests per minute, 
# so we are somewhat limited in the number of articles we can access quickly, 
# but the example below could be easily scaled up and even run over a period of 
# several days if needed. This scaleability is a major reason we use loops even 
# though R is kinda bad at them
#_______________________________________________________________________________

# start with some initial parameters : 
articles <- data.frame() # empty dataframe to place results

offset <- 0 # an initial offset of zero
limit <- 500 # the limit on the number of results. Here its set to the maximum





# now we're going to do the same thing five times, incrementing the offset
# after each iteration of the loop: 
for(i in 1:5){ 
  # get a single page of 500 results 
  articles_response = request|>
    req_url_path_append("all", "u.s..json")|>
    req_url_query(limit = limit)|>
    req_url_query(offset = offset)|>
    req_perform()|>
    resp_body_json()
  
  # format the result as a dataframe
  articles_frame_i <- articles_response$results|>
    enframe()|>
    unnest_wider(value)

    
  # combine the latest result with the prior result (note, other variables get
  # overwritten after each iteration, but this one grows cumulatively. Why?)
  articles <- bind_rows(articles, articles_frame_i)
  
  
  # increment the offset by the limit amount 
  offset <- offset + limit
  
  # add a pause between each request to avoid sending too many requests
  # the limit is 5 per minute, so we need 60/5 = 12 wait between each iteration
  Sys.sleep(12)
  
  # print the current iteration. This allows us to track our progress in the
  # console
  print(i)
}

# now we have a bunch of articles! 
nrow(articles)

# now get a list of the top people mentioned in each article
people <- articles|>
  unnest(cols= per_facet)|> # unnest the list columns 
  mutate(person = unlist(per_facet))|> # unlist again and assign a new name
  count(person)|> # count the occurrence of each name
  arrange(-n) # sort from highest to lowest

# check out the top results :
head(people)






#Extra notes -------------------------------------------------------------------
# Documentation of other Times APIs here: 
# https://developer.nytimes.com/apis
# 
# There is an R package for the Times API: https://github.com/news-r/nytimes 
# you can use this on future homework, but its really just a wrapper around 
# the kinds of HTTP requests we're doing above . Learning how to create HTTP 
# requests means you aren't reliant on someone else writing a package for the API 
# you want to use. 
#_______________________________________________________________________________













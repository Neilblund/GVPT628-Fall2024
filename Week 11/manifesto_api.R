library(httr2)
library(tidyverse)
# the base URL
base_url <- 'https://manifesto-project.wzb.eu/api/v1/'

# add the api key for all future requests 
base_req<-request(base_url)|>
  req_url_query('api_key'= Sys.getenv("manifesto_key"))


# getting meta data (no API key required for this part)
versions<-base_req|>
  req_url_path_append('list_core_versions')|>
  req_perform()|>
  resp_body_json()

latest_id<-versions$datasets[[length(versions$datasets)]][['id']]


metaversions<-base_req|>
  req_url_path_append('list_metadata_versions')|>
  req_perform()|>
  resp_body_json()

latest_meta<-metaversions$versions[[length(metaversions$versions)]]


codebook_json<-base_req|>
  req_url_path_append('get_core_codebook')|>
  req_url_query(key=latest_id)|>
  req_perform()|>
  resp_body_json()

codebook<-enframe(codebook_json[2:length(codebook_json)])|>
  unnest_wider(value, names_sep ='_')|>
  select(-name)
colnames(codebook) <-  codebook_json[[1]]



# get the core data 
res<-base_req|>
  req_url_path_append('get_core')|>
  req_url_query(`key[]`=latest_id)|>
  req_perform()

# reformatting to a data frame
core<-resp_body_json(res)
mpd<-enframe(core[2:length(core)])|>
  unnest_wider(value, names_sep='_')

colnames(mpd) <- c('id', core[[1]])

# select countries 
countries<-c("United States","United Kingdom","Ireland","Australia","Canada")
# and year after 2000
min_date <- 199000
eng_parties<-mpd|>
  filter(countryname%in%countries)|>
  filter(date>=min_date)|>
  mutate(manifesto_id = paste0(party, "_", date))

# get the meta data 
meta<-base_req|>
  req_url_path_append('metadata')|>
  req_url_query(`keys[]`=eng_parties$manifesto_id,
                version = latest_meta,
                .multi='explode'
  )|>
  req_perform()|>
  resp_body_json()

# extracting just the meta ID element 
metaids<-unlist(map(meta$items, 'manifesto_id'))

# retrieving the sentences 
texts<-base_req|>
  req_url_path_append('texts_and_annotations')|>
  req_url_query(`keys[]` = metaids, 
                version = latest_meta,
                .multi='explode'
  )|>
  req_perform()|>
  resp_body_json()

df<-map(texts$items, 'items')
text_df<-bind_rows(df)
text_df$id<-rep(map_chr(texts$items, 'key'), lengths(df))

text_frame<-text_df|>
  left_join(codebook, by=join_by(cmp_code == code))

set.seed(1000)
immigr_sentences<-text_df|>
  mutate(immigration_related = cmp_code %in% c("602.1", "602.2","607.2", "608.2"))|>
  group_by(immigration_related)|>
  slice_sample(n = 1000)


write_csv(immigr_sentences|>select(-eu_code), file='immigration_sentences.csv')
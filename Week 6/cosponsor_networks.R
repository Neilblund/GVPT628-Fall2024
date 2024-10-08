#Build a network----------------------------------------------------------------
library(tidyverse)
library(igraph)

download.file('https://github.com/Neilblund/APAN/raw/refs/heads/main/cosponsorships.rds', 
              destfile = 'cosponsors.rds',
              mode='wb')

download.file('https://github.com/Neilblund/APAN/raw/refs/heads/main/sponsors.rds', 
              destfile = 'sponsors.rds',
              mode='wb')

download.file('https://github.com/Neilblund/APAN/raw/refs/heads/main/current_house_members.rds', 
              destfile = 'members.rds',
              mode='wb')

cosponsors<-readRDS('cospons.rds')


sponsors<-readRDS('sponsors.rds')

members<-readRDS('members.RDS')


legi_data<-
  cosponsors|>
  bind_rows(sponsors)|>
  select(number, legislator)|>
  distinct()|>
  group_by(number)|>
  filter(n()>2)

# creating a table of similarities
leg_table <- xtabs(~number +legislator, data=legi_data)
idf<-log(ncol(leg_table)/rowSums(leg_table))

tfa<-leg_table * idf

terms <- t(tfa) %*% tfa

# normalize term counts for each member
terms <- terms / (rowSums(terms) - diag(terms))


graph<-graph_from_adjacency_matrix(terms, weighted=TRUE, mode='lower',
                                   diag=FALSE
)
# add party ID
vnames<-names(V(graph))

V(graph)$party<-members$partyName[match(vnames, members$bioguideId)] == "Republican"
V(graph)$color=c("blue","red")[as.numeric(V(graph)$party)+1]

V(graph)$legname<-members$name[match(vnames, members$bioguideId)]

# trim a lot of edges
cut.off <- quantile(E(graph)$weight, .9)
trimmed_net<-delete_edges(graph, E(graph)[weight < cut.off])


# create a big plot: 

pdf(file = 'trimmed_cosponsors.pdf', width=30, height=30)
layout <- layout_with_fr(trimmed_net, weights=E(trimmed_net)$weight)

plot(trimmed_net, 
     vertex.label=V(trimmed_net)$legname,  
     vertex.size=0, 
     layout = layout,
     vertex.label.color=V(trimmed_net)$color)

dev.off()

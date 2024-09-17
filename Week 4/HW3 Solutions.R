library(tidyverse)
pres<-read_csv('https://raw.githubusercontent.com/fivethirtyeight/election-results/main/election_results_presidential.csv')


# filter for:
 # - the 2020 election cycle
 # - the two major party candidates
 # - the general election
 # - removing the Maine/Nebraska splits using a regular expression
two_party_vote<-
  pres|>
  filter(cycle == 2020)|>
  filter(candidate_name == "Donald Trump" | candidate_name == "Joe Biden")|>
  filter(stage == "general")|>
  # str_detect() returns true when it detects a string that meets the regex
  # criteria 
  filter(!str_detect(state, "-[0-9]"))

# We still have extra rows! New York has both candidates listed on more than one ballot
# so we need to group by state/candidate and get the vote total: 


two_party_vote <- two_party_vote |>
  # group by state and candidate
  group_by(state, candidate_name) |>
  # group by vote totals
  summarise(votes = sum(votes))


  pivot_wider(names_from = candidate_name, values_from = votes)




  



races<-read_csv("https://raw.githubusercontent.com/fivethirtyeight/election-results/main/races.csv")|>
  mutate(updated = mdy_hm(updated_at),
         election_date = mdy(paste0(str_extract(date, "(^[0-9]{1,2}/[0-9]{1,2})"), "/", cycle)))|>
  filter(stage == "general")
         


recent<-races|>
  filter(!is.na(`Winning party`))|>
  arrange(election_date)


recent



# now group by candidate name and state and them sum the vote totals (this
# should allow you to calculate vote share in New York even though Biden/Trump
# were on two different ballots)
tp_simple<-two_party_vote|>
  group_by(candidate_name, state)|>
  summarise(votes = sum(votes))


# now pivot to wide format with one column per candidate
tp_wide<-tp_simple|>
  pivot_wider(names_from = candidate_name, values_from = votes)



#Q2 Calculate two party vote shares for each candidate ----
tp_wide<-tp_wide|>
  mutate("demshare" = `Joe Biden`/(`Donald Trump` + `Joe Biden`),
         "repshare" = `Donald Trump`/(`Donald Trump` + `Joe Biden`)
         )


  
#Q3 join with income data----
library(tidycensus)
median_income <- get_acs(geography = "state", variables = c(median_income = "B19013_001"), year = 2020)

tp_wide<-left_join(tp_wide, median_income, by=join_by(state == NAME) )


# A density, histogram, or boxplot
ggplot(tp_wide, aes(x=estimate)) + 
  geom_histogram(bins=30)

ggplot(tp_wide, aes(x=demshare)) + 
  geom_histogram(bins=30) + 
  theme_bw()

# A scatter plot
ggplot(tp_wide, aes(x=estimate, y=demshare)) + 
  geom_point() +
  theme_bw()

# A scatter plot with state names and other stuff
voteplot<-ggplot(tp_wide, aes(x=estimate, y=demshare)) + 
  geom_point()+
  theme_bw() +
  geom_smooth(method='lm') +
  geom_label(aes(label=state)) 


library(ggrepel)
tp_wide<-tp_wide|>
  mutate(`winner` = case_when( demshare > repshare ~ "Joe Biden", 
                               repshare > demshare ~ "Donald Trump"
                               ),
         total_votes = `Joe Biden` + `Donald Trump`
         )
  
voteplot<-ggplot(tp_wide, aes(x=estimate, y=demshare, color=winner)) + 
  geom_smooth(method = 'lm', se=FALSE, aes(group=1), color='black', lwd=.5, lty=2) +
  geom_point(aes(size = log(total_votes) ))+
  theme_bw() +
  geom_label_repel(aes(label=state)) 


voteplot

library(ggExtra)

ggMarginal(voteplot, type='boxplot')


v17 <- load_variables(2020, "acs5", cache = TRUE)

acs<- get_acs(geography = "state", variables = c(median_income = "B19013_001",
                                                 population = "B01003_001")
              , year = 2020)





# example of 
maine_wider<-pres|>
  filter(cycle == 2020)|>
  filter(candidate_name == "Donald Trump" | candidate_name == "Joe Biden")|>
  filter(stage == "general")|>
  filter(str_detect(state, "Maine"))|>
  select(state, candidate_name, votes, stage)|>
  pivot_wider(names_from = state, values_from=votes)

maine_wider



res<-pres|>
  filter(str_detect(state, "-[0-9]"))|>
  filter(cycle == 2020)|>
  filter(candidate_name == "Donald Trump" | candidate_name == "Joe Biden")|>
  filter(stage == "general")|>
  select(state, candidate_name, votes, stage)|>
  pivot_wider(names_from = state, values_from=votes)





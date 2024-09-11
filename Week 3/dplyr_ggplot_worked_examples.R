library(tidyverse) # dplyr and tidyverse are already part of this package

# Mutate and Summarize ----
# data on executive approval around the globe
approval<-read_csv('https://osf.io/download/39pm6/')|>
  filter(!is.na(wb_code))


# How many observations do we have for each country?

nobs <- approval|>
  group_by(country_name)|>
  summarise(n = n())

nobs

## Q1. How many UNIQUE observations do we have for each country? ----
# Note that some countries may have more than one measure of executive approval

approval|>
  group_by(wb_code)|>
  #... do something to count the number of unique YEARs in each group...
  summarise(`number of unique years` = length(unique(year)))


## Q2. What does this code do? ----
selected_countries <- approval |>
  filter(year >= 2010 & year <= 2020) |>
  group_by(wb_code) |>
  filter(length(unique(year)) == 11)



# what about this? 
selected_country_means<-selected_countries|>
  group_by(wb_code, year)|>
  summarise(Approval_Smoothed = mean(Approval_Smoothed),
            Disapproval_Smoothed = mean(Disapproval_Smoothed)
  )|>
  ungroup()


# Mutate: in the simplest case, mutate just adds columns,
# but its more useful when we use it with group-by



selected_country_means<-selected_country_means|>
  ungroup()|>
  mutate(net_approval =   Approval_Smoothed - Disapproval_Smoothed)

## Q3. Calculate the mean approval for each country and the difference between----
# the current value and the country mean (use mutate with group_by)

sel_country<-selected_country_means|>
  group_by(wb_code)|>
  # step 1 get the average approval by country
  
  mutate(mean_approval = mean(Approval_Smoothed))|>
  ungroup()|>
  # step 2 calculate the difference between current approval and overall average
  
  mutate(diff = Approval_Smoothed - mean_approval)



# Pivots ----

# Pivot wider: wide data avoids repeating observations by adding more columns
# (in general its easier for humans to read, but less useful for analysis)

wide_data<-selected_country_means|>
  select(wb_code, year, net_approval)|>
  pivot_wider(names_from = year, values_from = net_approval)


# Pivot longer: long data minimizes columns by repeating info.
# This is less efficient, but more flexible and better for stats and plots

long_data<-wide_data|>
  pivot_longer(cols = `2010`:`2020`, names_to="year", values_to="net_approval")

## Q4 Use pivot longer to place approval and disapproval in separate rows ----

longer<-selected_country_means|>
  pivot_longer(cols = c(Approval_Smoothed, Disapproval_Smoothed) , 
               names_to='measure', values_to ='value')

# ggplot ----


# example of using the data for a plot: 
longer|>
  # show US only: 
  filter(wb_code == "USA")|>
  # now plot disapproval and approval (with color-coding for each)
  ggplot(aes(x=year, y=value, color=measure)) + 
  # add the points
  geom_point() +
  # add a smoothed trend line
  geom_smooth()



# this might not be readable: 
us_uk = long_data|>
  filter(wb_code == "USA" | wb_code =="GBR")

ggplot(us_uk, aes(x=year, y=net_approval)) + 
  geom_point() 

# with color coding: 
ggplot(us_uk, aes(x=year, y=net_approval, color=wb_code)) + 
  geom_point() 

# with faceting: 
ggplot(us_uk, aes(x=year, y=net_approval)) + 
  geom_point() +
  facet_wrap(~wb_code)


# with a line 
ggplot(us_uk, aes(x=year, y=net_approval, group=1)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~wb_code) 



## Q5. Create a plot with approval and disapproval separately for the US only ----



# Joins ----
#install.packages("wbstats")

library(wbstats)

gdp_data<-wb_data(c("gdp" = "NY.GDP.MKTP.CD"), start_date=2010, end_date=2020)


##Q6. How do we add income data to the approval data?----


unique(gdp_data$iso3c) # iso3c in gdp_data is the same as wb_code in the approval data: 
unique(selected_country_means$wb_code) 

# and date in the GDP data is the same as year in the approval data:
unique(gdp_data$date)
unique(selected_country_means$year)

joined_data <- selected_country_means|>
  # so we can just left join by country code and year
  left_join(gdp_data, by = join_by(wb_code == iso3c, year == date))

head(joined_data)

#Note: 
# left_join means we keep all the approval data AND only gdp data with a matching key
# right_join would mean we keep all the gdp_data and only the approval data with a matching key












library(tidyverse) # dplyr and tidyverse are already part of this package

# Mutate and Summarize ----
# data on executive approval around the globe
approval<-read_csv('https://osf.io/download/39pm6/')


# How many observations do we have for each country?

nobs <- approval|>
  group_by(country_name)|>
  summarise(n = n())

nobs

## Q1. How many UNIQUE observations do we have for each country? ----
# Note that some countries may have more than one measure of executive approval


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
  mutate(net_approval =   Approval_Smoothed - Disapproval_Smoothed)

## Q3. Calculate the mean approval for each country and the difference between----
# the current value and the country mean (use mutate with group_by)



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

## Q4 Use pivot longer to place approval and disapproval in separate rows ---


# ggplot ----

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



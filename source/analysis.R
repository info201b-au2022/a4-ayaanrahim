library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")
data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
max_jail_pop_2018 <- filter(data, year == 2018) %>% 
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>% 
  pull(county_name)
max_jail_pop_2018_num <- filter(data, year == 2018) %>% 
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>% 
  pull(total_jail_pop)
max_pop_2018 <- filter(data, year == 2018) %>% 
  filter(total_pop == max(total_pop, na.rm = TRUE)) %>% 
  pull(county_name)
average_jail_pop_2018 <- filter(data, year == 2018)
average_jail_pop_2018 = round(mean(average_jail_pop_2018$total_jail_pop, na.rm = TRUE), 0)
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  new_data <- select(data, year, total_jail_pop) %>% 
    group_by(year) %>% 
    summarize(total = sum(total_jail_pop, na.rm = TRUE))
return(new_data)   
}
library(scales)
get_year_jail_pop()
# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
   pl <- ggplot(get_year_jail_pop(), aes(x=year, y=total)) +
    geom_bar(stat="identity") + 
     scale_y_continuous(labels = comma) + 
     labs(
       title = "Increase of Jail Population in U.S. (1970-2018)",
       x = "Year",
       y = "Total Jail Population",
       caption = "Figure 1. Increase of Jail Population in U.S. (1970-2018)"
     )
  return(pl)
} 
plot_jail_pop_for_us()
## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
get_jail_pop_by_states <- function(states){
  new_data2 <- select(data, state, year, total_jail_pop) %>% 
    filter(state %in% states) %>% 
    group_by(year, state) %>% 
    summarize(total = sum(total_jail_pop, na.rm=TRUE))
  return(new_data2)
}
plot_jail_pop_by_states <- function(states){
  plot <- ggplot(get_jail_pop_by_states(states)) + 
    geom_smooth(aes(x = year, y = total, color = state), se = F) + 
    labs(
      title = "Growth of Prison Population by State",
      x = "Year",
      y = "Total Jail Population",
      caption = "Figure 2. U.S. Prison Population by States"
    )
  return(plot)
}
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
get_latinx_white_jail_pop <- function(){
  new_data <- group_by(data, year) %>% 
    summarize(white_pop_total = max(white_jail_pop, na.rm = TRUE), latinx_pop_total = max(latinx_jail_pop, na.rm = TRUE)) %>% 
    select(year, white_pop_total, latinx_pop_total)
  return(new_data)
}
plot_latinx_white_jail_pop <- function(){
  plot <- ggplot(get_latinx_white_jail_pop()) + 
    geom_smooth(aes(x = year, y = white_pop_total, color = "white"), se = F) + 
    geom_smooth(aes(x = year, y = latinx_pop_total, color = "latinx"), se = F) + 
    labs(
      title = "White Vs. Latinx Prison Populations",
      x = "Year",
      y = "Jail Population",
      caption = "Figure 3. U.S. Prison Population Trends by White and Latinx Ethnicity", 
      color = "Ethnicity"
    )
  return(plot)
}
plot_latinx_white_jail_pop()
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
get_2018_us_data <- function(){
  new_data3 <- filter(data, year == 2018)
  return(new_data3)
}
library(maps)
plot_2018_black_prison_pop <- function(){
  state_shape <- map_data("county") %>%
    unite(polyname, region, subregion, sep = ",") %>% 
    left_join(county.fips, by = "polyname")
  county_shape <- left_join(state_shape, get_2018_us_data(), by = "fips")
  plot <- ggplot(county_shape) + 
    geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop), color = "white", size = .1) + 
    labs(
      title = "Map of Black Prison Population in 2018 by County",
      x = "",
      y = "",
      caption = "Figure 4. Black Prison Population across the U.S.",
      fill = "Black Prison Population")
  return(plot)
}
plot_2018_black_prison_pop()
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 



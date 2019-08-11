library(tidyhydat)
library(dplyr)
library(lubridate)
library(purrr)

## Calculate annual half flow
annual_half_flow <- hy_daily_flows(c("08MF005", "02MB003")) %>% 
  mutate(year = year(Date)) %>% 
  group_by(year, STATION_NUMBER) %>% 
  filter(n() >= 365) %>% ## filter for any year without full number of days
  summarise(half_ann_flow = sum(Value)/2) ##Creare half the annual flow

## Calculate cumulative flow
cumulative_flow <- hy_daily_flows(c("08MF005", "02MB003")) %>% 
  mutate(year = year(Date)) %>% 
  group_by(year, STATION_NUMBER) %>% 
  mutate(cumulative_value = cumsum(Value))


## Join annual half flow to cumulative flow and filter for rows when they equal
joined_table <- cumulative_flow %>% 
  left_join(annual_half_flow, by = c("STATION_NUMBER", "year")) 

## Find first instance where the cumulative exceeds the half annual flow
## Gotta be a better way to do this.
purrr::map_dfr(unique(joined_table$year), ~{
  sub_year <- joined_table[joined_table$year == .x,]
  sub_year[which.max(sub_year$cumulative_value > sub_year$half_ann_flow),]
})
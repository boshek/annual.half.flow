## Calculate annual half flow
annual_half_flow <- function(station_number){
  hy_daily_flows(station_number) %>% 
    mutate(year = year(Date)) %>% 
    group_by(year, STATION_NUMBER) %>% 
    filter(n() >= 365) %>% ## filter for any year without full number of days
    summarise(half_ann_flow = sum(Value)/2) ##Creare half the annual flow
}

## Calculate cumulative flow
cumulative_flow <- function(station_number){
  hy_daily_flows(station_number) %>% 
    mutate(year = year(Date)) %>% 
    group_by(year, STATION_NUMBER) %>% 
    mutate(cumulative_value = cumsum(Value))
}
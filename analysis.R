source("R/load.R")

## This is the part to play with. Subset the stations in any way you desire. 
station_number_obj <- hy_stations() %>% 
  filter(DRAINAGE_AREA_GROSS >= 100) %>% 
  pull_station_number()

annual_half_flow_obj <- annual_half_flow(station_number_obj)
cumulative_flow_obj <- cumulative_flow(station_number_obj)

## Join annual half flow to cumulative flow and filter for rows when they equal
joined_table <- cumulative_flow_obj %>% 
  left_join(annual_half_flow_obj, by = c("STATION_NUMBER", "year")) 

## Find first instance where the cumulative exceeds the half annual flow
## Gotta be a better way to do this.
purrr::map_dfr(unique(joined_table$year), ~{
  sub_year <- joined_table[joined_table$year == .x,]
  sub_year[which.max(sub_year$cumulative_value > sub_year$half_ann_flow),]
})
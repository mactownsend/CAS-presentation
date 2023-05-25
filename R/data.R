regional_councils <- 
  st_read(here("datasets/regional-council-2019-clipped-generalised.csv"),
          crs = 2193) %>%
  st_transform(4326) %>%
  select(-WKT) %>%
  rename(geometry = geom_WKT) %>%
  filter(REGC2019_V1_00_NAME != "Area Outside Region") %>%
  st_simplify(dTolerance = 5000)

regional_councils_true <- 
  st_read(here("datasets/regional-council-2019-generalised.csv"),
          crs = 2193) %>%
  st_transform(4326) %>%
  select(-WKT) %>%
  rename(geometry = geom_WKT)

tms_sites <- read_csv(here("datasets/State_highway_traffic_monitoring_sites.csv")) %>%
  st_as_sf(coords = c("X", "Y"), crs = 2193) %>% st_transform(crs = 4326)

tms_site_table <- tms_sites %>%
  st_drop_geometry() %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  rename(`Counter Type` = type, Count = count)



tms_counts <- read_csv(here("datasets/TMS_daily_traffic_counts_API.csv")) %>%
  mutate(startDate = as.Date(startDate)) %>%
  filter(startDate < "2023-01-01")


crashes <- read_csv(here("datasets/Crash_Analysis_System_(CAS)_data.csv")) %>%
  filter(crashYear > 2017 &
        crashYear < 2023 &
        crashSHDescription == "Yes") %>%
  mutate(motor_vehicle_flag = if_else(motorcycle > 0 | bus > 0 | schoolBus > 0 |
                                      moped > 0 | carStationWagon > 0 | suv > 0 |
                                      taxi > 0 | truck > 0 | vanOrUtility > 0,
                                      1 ,0)) %>%
  filter(motor_vehicle_flag == 1) %>%
  select(crashYear, crashSeverity, X, Y, region, fatalCount, crashSHDescription, motor_vehicle_flag) %>%
  st_as_sf(coords = c("X", "Y"), crs = 2193) %>%
  st_transform(crs = 4326)





example_tms_table <- tms_counts %>% filter(siteID == 3721) %>% arrange(startDate, classWeight, laneNumber) %>% slice(1:10) %>%
  addHtmlTableStyle(align = "llllllllll",
                    align.header = "llllllllll",
                    col.columns = c("none", "#F7F7F7"))


example_crash_table <- crashes %>% slice(1:10) %>%
  addHtmlTableStyle(align = "lllllll",
                    align.header = "lllllll",
                    col.columns = c("none", "#F7F7F7"))

severity_crashes <- crashes %>%
  group_by(crashSeverity) %>%
  st_drop_geometry() %>%
  summarise(N = n()) %>% arrange(-N) %>%
  rename(`Crash Severity` = crashSeverity)


region_crashes <- crashes %>%
  st_join(regional_councils_true %>% select(REGC2019_V1_00_NAME), st_intersects) %>%
  group_by(REGC2019_V1_00_NAME) %>%
  st_drop_geometry() %>%
  summarise(N = n()) %>%
  arrange(-N) %>%
  mutate(Region = str_remove(REGC2019_V1_00_NAME, " Region$")) %>%
  mutate(Region = if_else(Region == "Manawatu-Wanganui",
                          "Manawatū-Whanganui", Region)) %>%
  select(-REGC2019_V1_00_NAME) %>%
  select(Region, N)



good_tms_site_ids <- tms_sites %>%
  st_drop_geometry() %>%
  filter(type %in% c("National", "Continuous")) %>%
  select(siteRef) %>% pull()


earliest_dates <- tms_counts %>%
  filter(SiteRef %in% good_tms_site_ids) %>%
  group_by(SiteRef) %>%
  summarise(start_date = min(startDate))


good_counters <- tms_counts %>%
  filter(SiteRef %in% good_tms_site_ids) %>%
  select(SiteRef, startDate) %>%
  distinct() %>%
  group_by(SiteRef) %>%
  summarise(N = n()) %>%
  filter(N > 0.75 * 1826) %>%
  select(SiteRef) %>% pull()

all_Dates <- seq(as.Date("2018-01-01"), as.Date("2022-12-31"), by = 1)



tms_daily_totals <- tms_counts %>%
  st_drop_geometry() %>%
  filter(SiteRef %in% good_counters) %>%
  group_by(SiteRef, startDate) %>%
  summarise(Daily_Total = sum(trafficCount))


tms_year_average <- tms_daily_totals %>%
  mutate(year = gsub("-.*$","",startDate)) %>%
  group_by(SiteRef, year) %>%
  summarise(yearly_average = mean(Daily_Total))


tms_combos = paste(tms_daily_totals$SiteRef, tms_daily_totals$startDate, sep = "-")

missing_dates_df <-
  tibble(SiteRef = rep(unique(tms_daily_totals$SiteRef), each = length(all_Dates)),
         startDate = rep(all_Dates, length(unique(tms_daily_totals$SiteRef)))) %>%
  filter(!(paste(SiteRef, startDate, sep = "-") %in% tms_combos)) %>%
  mutate(year = gsub("-.*$","",startDate))


combined_df <- missing_dates_df %>%
  inner_join(tms_year_average, by = c("year" = "year", "SiteRef" = "SiteRef")) %>%
  select(-year) %>%
  rename(Daily_Total = yearly_average) %>%
  bind_rows(tms_daily_totals) %>%
  arrange(SiteRef, startDate)


Good_Sites = tms_sites %>%
  filter(siteRef %in% good_counters) %>%
  select(siteRef, geometry)

distance_df <- tibble()
for(i in 1:nrow(crashes)){
  distances <- as.numeric(st_distance(crashes[i,], Good_Sites))
  min_dist = min(distances)
  SiteRef = Good_Sites[which.min(distances),] %>% st_drop_geometry() %>% select(siteRef) %>% pull()
  row_df <- tibble(crash = i, min_dist = min_dist, closest = SiteRef)
  distance_df <- distance_df %>% bind_rows(row_df)
}

crash_distance <- bind_cols(crashes, distance_df)

final_crashes <- crash_distance %>% filter(min_dist < 1666.67)

popular_counter <- final_crashes %>%
  st_drop_geometry() %>%
  group_by(closest) %>%
  summarise(N = n()) %>%
  filter(N == max(N))





  
which(as.numeric(st_distance(Good_Sites %>% filter(siteRef == "01N19424"), Good_Sites))<1667)
close_counters <- Good_Sites[c(81, 91),]



crash_per_counter <- final_crashes %>%
  st_drop_geometry() %>%
  group_by(closest) %>%
  summarise(N = n())


gs <- Good_Sites %>% st_drop_geometry() %>% select(siteRef) %>% pull()
missing_good_sites <- gs[!(gs %in% crash_per_counter$closest)]

good_sites_with_counts <- crash_per_counter %>%
  bind_rows(tibble(closest = missing_good_sites,
                   N = rep(0, length(missing_good_sites))))


counter_totals <-
  combined_df %>% filter(SiteRef %in% Good_Sites$siteRef) %>%
  group_by(SiteRef) %>%
  summarise(Total = sum(Daily_Total))


final_result <- counter_totals %>%
  inner_join(good_sites_with_counts, by = c("SiteRef" = "closest")) %>%
  mutate(crashes_per_million = N/(Total/1000000)) %>%
  arrange(desc(crashes_per_million))


top_10_dangerous <- final_result %>%
  head(n = 10) %>%
  inner_join(tms_sites, by = c("SiteRef" = "siteRef")) %>%
  select(SiteRef, crashes_per_million, description) 

final_with_region <- tms_sites %>%
  st_join(regional_councils_true, st_intersects) %>%
  select(siteRef, REGC2019_V1_00_NAME) %>%
  st_drop_geometry() %>%
  mutate(Region = gsub(" Region$", "", REGC2019_V1_00_NAME)) %>%
  inner_join(final_result, by = c("siteRef" = "SiteRef"))
  
risk_proportions <- final_with_region %>%
  mutate(dangerous_zone = if_else(crashes_per_million > quantile(final_result$crashes_per_million, 0.75),
                                  "Risky", "Safe")) %>%
  group_by(Region, dangerous_zone) %>%
  mutate(Region = if_else(Region == "Manawatu-Wanganui",
                          "Manawatū-Whanganui", Region)) %>%
  summarise(N = n()) %>%
  ungroup() %>%
  group_by(Region) %>%
  mutate(proportion = N/sum(N))


region_levels = risk_proportions  %>% filter(dangerous_zone == "Risky") %>% arrange(desc(proportion)) %>% select(Region) %>% pull()




absolute_risky_safe_by_region <- risk_proportions %>%
  select(-proportion) %>%
  pivot_wider(id_cols = "Region", names_from = "dangerous_zone", values_from = "N") %>%
  mutate(Region = factor(Region, levels = region_levels)) %>%
  arrange(Region)

regional_councils <- 
  st_read(here("datasets/regional-council-2019-clipped-generalised.csv"),
          crs = 2193) %>%
  st_transform(4326) %>%
  select(-WKT) %>%
  rename(geometry = geom_WKT) %>%
  filter(REGC2019_V1_00_NAME != "Area Outside Region") %>%
  st_simplify(dTolerance = 5000)

theme_map <- function(...) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA), 
      panel.background = element_rect(fill = "white", color = NA), 
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      ...
    )
}


labels <- tibble(region = regional_councils %>%
                   st_drop_geometry() %>%
                   mutate(region = str_remove(REGC2019_V1_00_NAME,
                                              " Region$")) %>%
                   mutate(region = if_else(region == "Manawatu-Wanganui",
                          "Manawatū-Whanganui", region)) %>%
                   select(region) %>%
                   pull())

theme_cartoon <- function() {
  theme_minimal() +
    theme(
      panel.background = element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.position = "none",
      plot.margin = margin(0, 0, 0, 0)
    )
}


label_df <- tibble(x = c(173.808635762538, 174.714540519686, 175.507148954217, 176.82960960226,
                      177.917646175535, 176.747614412523, 174.435645987643, 175.543458915085,
                      175.4761147344, 170.935782294664, 171.570702305659, 169.534509115554,
                      167.929613545467, 172.659349955636, 173.386659478586, 173.541118483934),
                y = c(-35.4944567543399, -36.7080835232702, -38.0329280783442, -38.1658102257023,
                      -38.7620778758189, -39.3806040588402, -39.3292332105054, -40.2451774709469,
                      -41.0826431165762, -42.7621351625788, -43.5781078920547, -45.2583365908691,
                      -45.7196008871827, -41.8439390500954, -41.2421310954097, -41.6898100773),
                x2 = c(170.77, 176.39, 173.17, 179.86, 179.97, 178.43, 172.89, 173.43, 177.12,
                       168.58, 174.16, 171.64, 165.87, 169.75, 171.71, 174.70),
                y2 = c(-35.4944567543399, -36.7080835232702, -38.0329280783442, -38.1658102257023,
                       -38.7620778758189, -39.3806040588402, -39.3292332105054, -40.2451774709469,
                       -41.0826431165762, -42.7621351625788, -43.5781078920547, -45.2583365908691,
                       -45.7196008871827, -41.8439390500954, -41.2421310954097, -41.6898100773),
                hjust = c(1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0),
                region_name = regional_councils %>%
                  st_drop_geometry() %>%
                  mutate(region = str_remove(REGC2019_V1_00_NAME,
                                             " Region$")) %>%
                  mutate(region = if_else(region == "Manawatu-Wanganui",
                                          "Manawatū-Whanganui", region)) %>%
                  select(region) %>%
                  pull())

region_colours = c("Northland Region" = "#00FFFF",
                   "Auckland Region" = "#FF6600",
                   "Waikato Region" = "#00FF00",
                   "Bay of Plenty Region" = "#FF00FF",
                   "Gisborne Region" = "#00FF00",
                   "Hawke's Bay Region" = "#FF6600",
                   "Taranaki Region" = "#00FFFF",
                   "Manawatu-Wanganui Region" = "#FF00FF",
                   "Wellington Region" = "#00FFFF",
                   "Tasman Region" = "#00FF00",
                   "Nelson Region" = "#FF6600",
                   "Marlborough Region" = "#FF00FF",
                   "West Coast Region" = "#00FFFF",
                   "Canterbury Region" = "#FF6600",
                   "Otago Region" = "#00FF00",
                   "Southland Region" = "#FF00FF"
)



ggplot(regional_councils) +
  geom_sf(aes(geometry = geometry, fill = REGC2019_V1_00_NAME),
          colour = "black") +
  theme_map() +
  lims(x = c(163, 182)) +
  geom_segment(data = label_df, aes(x = x2, xend = x, y = y2, yend = y), colour = "black") +
  geom_point(data = label_df, aes(x = x, y = y), pch = 21, size = 3, fill = "white") +
  geom_label(data = label_df, aes(x = x2, y = y2, label = region_name ), hjust = label_df$hjust, label.r = unit(0, "pt")) +
  scale_fill_manual(values = region_colours) +
  guides(fill = "none") +
  labs(title = "Regions of New Zealand") +
  theme(plot.title = element_text(hjust = 0.5))



tms_sites <- read_csv(here("datasets/State_highway_traffic_monitoring_sites.csv")) %>%
  st_as_sf(coords = c("X", "Y"), crs = 2193) %>% st_transform(crs = 4326)


ggplot(tms_sites) +
  geom_sf(aes(geometry = geometry, colour = type)) +
  theme_map() +
  labs(colour = "Site Type")

ggplot(tms_sites %>% filter(type %in% c("Continuous", "National"))) +
  geom_sf(aes(geometry = geometry, colour = type)) +
  theme_map() +
  labs(colour = "Site Type")

tms_counts <- read_csv(here("datasets/TMS_daily_traffic_counts_API.csv")) %>%
  mutate(startDate = as.Date(startDate)) %>%
  filter(startDate < "2023-01-01")


crashes <- read_csv(here("datasets/Crash_Analysis_System_(CAS)_data.csv")) %>%
  filter(crashYear > 2017 & crashYear < 2023)


tms_counts %>%
  select(startDate, siteID) %>%
  distinct() %>%
  group_by(siteID) %>%
  summarise(N = n()) %>%
  arrange(-N) %>%
  filter(N > 1800)

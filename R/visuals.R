renv::restore()
source(here::here("R/libraries.R"))
source(here::here("R/data.R"))

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



regional_council_plot <- ggplot(regional_councils) +
  geom_sf(aes(geometry = geometry, fill = REGC2019_V1_00_NAME),
          colour = "black") +
  theme_map() +
  lims(x = c(163, 182)) +
  geom_segment(data = label_df, aes(x = x2, xend = x, y = y2, yend = y), colour = "black") +
  geom_point(data = label_df, aes(x = x, y = y), pch = 21, size = 3, fill = "white") +
  geom_label(data = label_df, aes(x = x2, y = y2, label = region_name ), hjust = label_df$hjust, label.r = unit(0, "pt")) +
  scale_fill_manual(values = region_colours) +
  guides(fill = "none") +
  #labs(title = "Regions of New Zealand") +
  theme(plot.title = element_text(hjust = 0.5))

tms_colours = c("Non-Continuous" = "#AA0000",
                "Continuous" = "#0044AA",
                "National" = "#00AA44",
                "Virtual" = "#6600AA")

all_tms_sites <- ggplot(tms_sites) +
  geom_sf(aes(geometry = geometry, colour = type)) +
  theme_map() +
  labs(colour = "Site Type") +
  scale_colour_manual(values = tms_colours) +
  ggtitle("All TMS Sites") +
  guides(colour = "none")

some_tms_sites <- ggplot(tms_sites %>% filter(type %in% c("Continuous", "National"))) +
  geom_sf(aes(geometry = geometry, colour = type)) +
  theme_map() +
  labs(colour = "Site Type") +
  scale_colour_manual(values = tms_colours) +
  ggtitle("Select TMS Sites")

all_tms_sites + some_tms_sites

example_tms_html <- htmlTable(example_tms_table, rnames = F)
example_crash_html <- htmlTable(example_crash_table, rnames = F)


all_sh_crashes <- ggplot(crashes) +
  geom_sf(aes(geometry = geometry), alpha = 0.2, colour = "lightblue") +
  theme_map() +
  labs(title = "Crashes", subtitle = "Occurring on State Highways\nBetween 2018 and 2022\nInvolving Motor Vehicles")

proportion_risky_safe_plot <- risk_proportions %>%
  mutate(Region = factor(Region, levels = region_levels)) %>%
  ggplot(aes(x = Region, y = proportion, group = Region, fill = dangerous_zone)) +
  geom_col(colour = "black") +
  theme_minimal() +
  labs(y = "Proportion") +
  labs(fill = "Counter Risk Status") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_hline(yintercept = 0.25, colour = "yellow") +
  ggtitle("Proportion of Risky Counters Per Region") +
  labs(subtitle = "Risky counters are the top 25% of crashes\nper traffic count nationwide.")

most_popular_counter <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircles(data = tms_sites %>% filter(siteRef == "01N19424"), color = "blue", label = "TMS Counter", labelOptions = labelOptions(noHide = T), radius = 1667) %>%
  addCircles(data = tms_sites %>% filter(siteRef == "01N10425"), color = "green", label = "Nearby Counter", labelOptions = labelOptions(noHide = T), radius = 20) %>%
  addCircles(data = crash_distance %>% filter(closest == "01N19424"), color = "red") %>%
  addCircles(data = crash_distance %>% filter(closest == "01N10425"), color = "green")

example_association  <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircles(data = crash_distance %>% slice(1), color = "red", label = "Crash", labelOptions = labelOptions(noHide = T)) %>%
  addCircles(data = tms_sites %>% filter(siteRef == "06000091"), color = "blue", label = "TMS Counter", labelOptions = labelOptions(noHide = T))







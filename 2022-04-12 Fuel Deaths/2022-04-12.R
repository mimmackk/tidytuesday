# Setup ------------------------------------------------------------------------

library(tidytuesdayR)
library(janitor)
library(tidyverse)
library(showtext)

font_add_google("IBM Plex Sans", "IBM Plex Sans")
showtext_auto()
showtext_opts(dpi = 300)

# Import -----------------------------------------------------------------------

tt_date <- "2022-04-12"
tt_data <- tt_load(tt_date)

# Tidy -------------------------------------------------------------------------

# Get percent of deaths due to household air pollution
deaths <- tt_data$indoor_pollution %>% 
  clean_names() %>% 
  rename(death_pct = deaths_cause_all_causes_risk_household_air_pollution_from_solid_fuels_sex_both_age_age_standardized_percent)

# Get country info (GDP, clean fuel access, etc)
fuel_access <- tt_data$fuel_gdp %>% 
  clean_names() %>% 
  rename(
    clean_fuel_access = access_to_clean_fuels_and_technologies_for_cooking_percent_of_population,
    gdp = gdp_per_capita_ppp_constant_2017_international,
    population = population_historical_estimates
  )

# Join fuel access and pollution deaths, filtering to entries with complete data
merged <- left_join(
  deaths,
  fuel_access,
  by = c("year", "entity", "code")
) %>% 
  group_by(entity) %>% 
  fill(continent, .direction = "updown") %>% 
  drop_na(death_pct, clean_fuel_access, code, gdp, continent) %>% 
  filter(between(year, 2000, 2016)) %>% 
  mutate(
    first_year = min(year, na.rm = TRUE),
    last_year  = max(year, na.rm = TRUE)
  ) %>% 
  filter(first_year == 2000 & last_year == 2016) %>% 
  ungroup()
  

# Plot -------------------------------------------------------------------------

merged %>% 
  ggplot(aes(
    x = clean_fuel_access,
    y = death_pct,
    group = entity,
    color = gdp,
  )) +
  
  # Line following path of access & deaths over time
  geom_path(
    alpha = 1,
    arrow = arrow(length = unit(0.06, "inches")),
    size = 0.6
  ) +
  
  # Label select countries
  geom_text(
    data = subset(merged, year == 2000 & entity %in% c("Morocco")),
    aes(label = entity),
    nudge_x = 4,
    angle = -45,
    family = "IBM Plex Sans"
  ) +
  geom_text(
    data = subset(merged, year == 2000 & entity %in% c("Bhutan")),
    aes(label = entity),
    nudge_y = -0.25,
    nudge_x = 7.5,
    angle = -45,
    family = "IBM Plex Sans"
  )  +
  geom_text(
    data = subset(merged, year == 2000 & entity %in% c("Palau")),
    aes(label = entity),
    hjust = 1,
    nudge_x = -1,
    family = "IBM Plex Sans"
  ) +
  geom_text(
    data = subset(merged, year == 2000 & entity %in% c("Nepal")),
    aes(label = entity),
    nudge_x = 4,
    angle = -40,
    family = "IBM Plex Sans"
  ) +
  geom_text(
    data = subset(merged, year == 2008 & entity %in% c("Tajikistan")),
    aes(label = entity),
    nudge_y = 1,
    angle = -18,
    family = "IBM Plex Sans"
  ) +
  geom_text(
    data = subset(merged, year == 2016 & entity %in% c("Equatorial Guinea")),
    aes(label = entity),
    hjust = 1,
    nudge_x = -2,
    nudge_y = 0,
    family = "IBM Plex Sans"
  ) +
  geom_text(
    data = subset(merged, year == 2000 & entity %in% c("Solomon Islands")),
    aes(label = entity),
    hjust = 0,
    nudge_x = 2,
    nudge_y = -0.5,
    family = "IBM Plex Sans"
  ) +
  
  # Color by GDP & add theme styling
  scale_color_viridis_c(
    "GDP (PPP)\nper capita\n",
    trans = "log10",
    breaks = c(10^3, 10^4, 10^5),
    labels = c("$1K", "$10K", "$100K"),
    option = "inferno"
  ) +
  labs(
    x = "% Population with Clean Fuel / Cooking Technology Access",
    y = "% Deaths Due to Household Air Pollution from  Solid Fuel",
    title = "Greater clean fuel access reduces household air pollution deaths",
    subtitle = "2000-2016",
    caption = "Data Source: Our World In Data\nGraphic: @kjmimmack 2022 | Made with R"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "IBM Plex Sans"),
    plot.caption = element_text(
      color = "grey40",
      margin = margin(0.2, 0, 0, 0, "in")
    )
  )
  
# Export to file
ggsave(
  "2022-04-12.png",
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  type = "cairo"
)


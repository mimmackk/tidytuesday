library(tidytuesdayR)
library(janitor)
library(tidyverse)
library(showtext)

font_add_google("IBM Plex Sans", "IBM Plex Sans")
showtext_auto()
showtext_opts(dpi = 300)


# Import -----------------------------------------------------------------------

tt_date <- "2022-05-24"
tt_data <- tt_load(tt_date)

# Tidy -------------------------------------------------------------------------

wins_by_year <- tt_data$fifteens |> 
  
  # Select only the games ending in a win/loss (dropping ties, etc)
  filter(home_away_win %in% c("H", "A")) |> 
  
  # Summarize total wins / losses by year
  mutate(year = lubridate::year(date)) |> 
  group_by(year) |> 
  count(home_away_win) |> 
  ungroup() |> 
  pivot_wider(names_from = home_away_win, values_from = n) |> 
  
  # Compute proportion of home wins and sample proportion standard error
  mutate(
    across(c(H, A), replace_na, 0),
    total_games = H + A,
    p_home = H / total_games,
    p_away = A / total_games,
    margin_of_err = 2 * sqrt(p_home * p_away / total_games)
  ) |> 
  
  # Drop early years with few games
  filter(year >= 1992)

# Plot -------------------------------------------------------------------------


wins_by_year |> 
  ggplot(aes(x = year, y = p_home)) +
  
  # Dashed reference line at 50% even split
  geom_hline(yintercept = 0.5, linetype = 2) +
  
  # Line graph of proportion of home team wins by year
  geom_line() +
  geom_point() +
  
  # Add background standard error bars for each year
  geom_linerange(
    aes(ymin = p_home - margin_of_err, ymax = p_home + margin_of_err),
    alpha = 0.2
  ) +
  
  # Style plot
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Year",
    y = "% Games Won by Home Teams",
    title = "Home Team Advantage in Women's Rugby",
    subtitle = "Int'l fifteens tournaments ending in a win, 1992–2022",
    caption = "Data Source: ScrumQueens\nGraphic: @kjmimmack 2022 | Made with R"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "IBM Plex Sans"),
    plot.caption = element_text(
      color = "grey40",
      margin = margin(0.2, 0, 0, 0, "in")
    )
  )

# Export to file
ggsave(
  paste0(tt_date, ".png"),
  device = "png",
  width = 5,
  height = 4,
  units = "in",
  type = "cairo"
)

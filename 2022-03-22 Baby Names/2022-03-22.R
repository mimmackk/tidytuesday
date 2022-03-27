# Setup ------------------------------------------------------------------------

library(knitr)
library(tidytuesdayR)
library(tidyverse)
library(showtext)

theme_set(theme_minimal())
font_add_google("IBM Plex Sans", "IBM Plex Sans")
showtext_auto()
showtext_opts(dpi = 300)


# Import -----------------------------------------------------------------------

tt_date   <- "2022-03-22"
tt_data   <- tt_load(tt_date)
babynames <- tt_data$babynames

# Tidy -------------------------------------------------------------------------

og_names        <- c("Luke", "Leia", "Han", "Lando")
prequel_names   <- c("Anakin", "Obi / Obie", "Padme", "Ahsoka")
sequel_names    <- c("Finn", "Rey", "Kylo", "Poe")

# Order character names for faceting by column
character_names <- map(
  1:4, 
  ~ c(og_names[[.x]], prequel_names[[.x]], sequel_names[[.x]])
) |> 
  unlist()

# Dates of trilogy releases
film_dates <- tibble(
  trilogy  = c("OG Trilogy", "Prequels", "Sequels"),
  year_min = c(1977, 1999, 2015),
  year_max = c(1983, 2005, 2019),
  ymin     = -Inf,
  ymax     =  Inf
)

# Display release dates for all trilogies after character's first appearance 
character_dates <- expand_grid(film_dates, name = character_names) |> 
  filter(
    (name %in% og_names      & trilogy %in% c("Sequels", "Prequels", "OG Trilogy")) |
    (name %in% prequel_names & trilogy %in% c("Sequels", "Prequels")) |
    (name %in% sequel_names  & trilogy %in% c("Sequels"))
  ) |> 
  mutate(name = factor(name, levels = character_names))

# Format dataset for plotting
starwars <- babynames |> 
  
  # Combine Obi/Obie and M/F and filter to Star Wars names after 1970
  mutate(name = recode(name, "Obi" = "Obi / Obie", "Obie" = "Obi / Obie")) |> 
  filter(name %in% character_names) |>
  group_by(name, year) |> 
  summarize(n = sum(n)) |> 
  ungroup() |> 
  filter(year >= 1970) |>
  
  # Convert years with no entries to explicit zeroes
  select(name, year, n) |> 
  pivot_wider(names_from = year, values_from = n, values_fill = 0) |> 
  pivot_longer(!name, names_to = "year", values_to = "n") |> 
  mutate(
    year = as.numeric(year),
    name = factor(name, levels = character_names)
  )

# Plot -------------------------------------------------------------------------

theme_colors <- list(
  "text"       = "#403a32",
  "caption"    = "grey55",
  "bg"         = "#efe7dd",
  "sw_og"      = "#2a5a97",
  "sw_prequel" = "#548c9c",
  "sw_sequel"  = "#f49446"
)

# Graph name trends, highlighting film release dates
starwars |> 
  ggplot() +
  geom_rect(
    data = character_dates,
    aes(xmin = year_min, xmax = year_max, ymin = ymin, ymax = ymax, fill = trilogy),
    inherit.aes = FALSE,
    alpha = 0.6
  ) +
  geom_line(
    aes(x = year, y = n, group = name),
    size = 0.75,
    color = theme_colors$text
  ) +
  facet_wrap(~ name, scales = "free_y", ncol = 3) +
  labs(
    x = "Year", 
    y = "# Newborns", 
    title = "THE STAR WARS EFFECT",
    subtitle = "Baby Name Trends 1970 - 2017",
    caption = 'Data source: "babynames" R package | Graphic: @kjmimmack 2022 | Made with R'
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 2)) +
  ylim(min = 0, max = NA) +
  
  # Custom theme options
  scale_fill_manual(
    "Release Dates", 
    values = c(
      "OG Trilogy" = theme_colors$sw_og,
      "Prequels"   = theme_colors$sw_prequel,
      "Sequels"    = theme_colors$sw_sequel
    )
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line        = element_blank(),
    axis.ticks       = element_line(color = theme_colors$text),
    
    plot.background  = element_rect(fill = theme_colors$bg, color = theme_colors$bg),
    panel.background = element_rect(fill = theme_colors$bg, color = theme_colors$bg),
    
    text       = element_text(family = "IBM Plex Sans", color = theme_colors$text),
    axis.title = element_text(family = "IBM Plex Sans", color = theme_colors$text),
    
    plot.title    = element_text(hjust = 0.5, size = 20, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
    strip.text    = element_text(face = "bold"),
    plot.caption  = element_text(size = 8, color = theme_colors$caption),
    
    legend.position = "bottom",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  ) +
  annotate("segment", x = -Inf, xend =  Inf, y = -Inf, yend = -Inf, size = 1, color = theme_colors$text) +
  annotate("segment", x = -Inf, xend = -Inf, y = -Inf, yend =  Inf, size = 1, color = theme_colors$text)


# Save plot
ggsave(
  paste0(tt_date, ".png"), 
  device = "png", 
  height = 7, 
  width = 7, 
  units = "in"
)


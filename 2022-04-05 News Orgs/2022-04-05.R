# Setup ------------------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(MetBrewer)
library(showtext)

font_add_google("IBM Plex Sans", "IBM Plex Sans")
showtext_auto()
showtext_opts(dpi = 300)

# Import -----------------------------------------------------------------------

tt_date   <- "2022-04-05"
tt_data   <- tt_load(tt_date)
news_orgs <- tt_data$news_orgs

# Tidy -------------------------------------------------------------------------

# Create a list of most common non-location-specific words in outlet names
wordlist <- news_orgs %>%
  
  # Filter to United States
  filter(country == "United States" & state %in% state.abb) %>% 
  
  # Create variables for text variations on state name
  select(publication_name, city, state) %>% 
  mutate(
    state_full = state.name[match(state, state.abb)],
    state_slim = str_remove(state_full, "North |South |West |New "),
  ) %>% 
  
  # Strip out irrelevant text from publication names
  mutate(
    
    # Remove ".com" and "LLC", as usage is not consistent
    name_mod = str_remove_all(publication_name, "\\.com|LLC"),
    
    # Remove references to local city, if given
    name_mod = case_when(
      !is.na(city) ~ str_remove(name_mod, city),
      TRUE ~ name_mod
    ),
    
    # Remove references to local state
    name_mod = str_remove_all(
      name_mod, 
      paste(state_full, state_slim, state, sep = "|")
    ),
    
    # Remove digits
    name_mod = str_replace_all(name_mod, "\\d+", " "),
    
    # Remove franchise names
    name_mod = str_remove_all(name_mod, "TAP into|Tap Into|TAPinto"),
    name_mod = str_remove_all(name_mod, "Technical.ly"),
    name_mod = str_remove_all(name_mod, "Go Local|GoLocal")
    
  ) %>% 
  
  # Separate remaining words into one row per word and count occurrences
  unnest_tokens(word, name_mod, drop = FALSE) %>% 
  group_by(name_mod, word) %>% 
  distinct() %>% 
  ungroup() %>% 
  count(word, sort = TRUE) %>% 
  
  # Examine only relatively common words
  filter(n >= 6) %>% 
  
  # Remove words like the/of/etc and other generic location references
  filter(!word %in% c(
    "the",
    "of",
    "for",
    "and",
    "la",
    "county",
    "city",
    "valley",
    "coast",
    "bay",
    "north",
    "south",
    "east",
    "west"
  ))


# Plot -------------------------------------------------------------------------

# Add some extra width to each bar graph so that words fit on each bar
bar_nudge <- 6

wordlist %>% 
  mutate(word = fct_rev(fct_inorder(str_to_title(word)))) %>% 
  ggplot(aes(y = word, x = n + bar_nudge, fill = log(n))) +
  geom_col() +
  
  # Add counts on right side of bars
  geom_text(
    aes(x = n + 0.5 + bar_nudge, y = word, label = n),
    hjust = 0,
    color = "#3c0d03",
    family = "IBM Plex Sans",
    size = 6,
    fontface = "bold"
  ) +
  
  # Add word labels on top of bars
  geom_text(
    aes(x = 0, y = word, label = word),
    hjust = 0,
    nudge_x = 0.5,
    color = "white",
    fontface = "bold.italic",
    family = "IBM Plex Sans",
    size = 6
  ) +
  
  # Separate title line by line to simulate justified-width with variable font size
  geom_text(
    aes(x = 60, y = "Daily", label = "Most Common"),
    hjust = 1,
    vjust = 1,
    nudge_y = 1.5,
    size = 15.2,
    fontface = "bold",
    family = "IBM Plex Sans",
    color = "#3c0d03"
  ) + 
  geom_text(
    aes(x = 60, y = "Press", label = "Words"),
    hjust = 1,
    vjust = 1,
    size = 35,
    nudge_y = 0.65,
    fontface = "bold.italic",
    family = "IBM Plex Sans",
    color = "#3c0d03"
  ) +
  geom_text(
    aes(x = 60, y = "Beat", label = "in Local Online Media Outlets'"),
    hjust = 1,
    vjust = 1,
    nudge_y = 0.25,
    size = 7,
    fontface = "bold",
    family = "IBM Plex Sans",
    color = "#3c0d03"
  ) + 
  geom_text(
    aes( x = 60, y = "Free", label = "Names"),
    hjust = 1,
    vjust = 1,
    size = 31.75,
    fontface = "bold.italic",
    family = "IBM Plex Sans",
    color = "#3c0d03"
  ) +
  geom_text(
    aes(
      x = 60,
      y = "Independent",
      label = "*Local geographic terms and media franchise names excluded"
    ),
    hjust = 1,
    vjust = 1,
    size = 3,
    family = "IBM Plex Sans",
    fontface = "bold",
    color = "#3c0d03"
  ) +

  # Theme Options
  scale_fill_gradientn(colors = met.brewer("Greek")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    
    # Hide y-axis elements and gridlines
    axis.title.y     = element_blank(),
    axis.text.y      = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Include x-axis and tickmarks
    axis.line.x.bottom = element_line(size = 0.4),
    axis.ticks.x       = element_line(size = 0.4),
    
    plot.background  = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    
    # Set font and text options
    text         = element_text(family = "IBM Plex Sans"),
    axis.title   = element_text(family = "IBM Plex Sans", size = 15),
    axis.text.x  = element_text(family = "IBM Plex Sans", size = 12),
    plot.caption = element_text(
      color = "grey40",
      margin = margin(0.2, 0, 0, 0, "in")
    )
  ) + 
  # Adjust scale to account for the extra padded-width bars
  scale_x_continuous(
    breaks = seq(bar_nudge, 60 + bar_nudge, by = 5),
    expand = c(0, 0),
    limits = c(0, 65),
    labels = ~.x - bar_nudge
  ) +
  labs(
    x = "Number of Occurrences", 
    y = "",
    caption = "Data source: Project Oasis | Graphic: @kjmimmack 2022 | Made with R"
  )

# Save -------------------------------------------------------------------------

ggsave(
  paste0(tt_date, ".png"),
  device = "png",  
  height = 7, 
  width = 9, 
  units = "in",
  type = "cairo"
)

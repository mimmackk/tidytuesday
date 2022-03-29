# Setup ------------------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(showtext)

font_add_google("IBM Plex Sans", "IBM Plex Sans")
showtext_auto()
showtext_opts(dpi = 300)

# Import -----------------------------------------------------------------------

tt_date   <- "2022-03-29"
tt_data   <- tt_load(tt_date)
sports  <- tt_data$sports

# Plot -------------------------------------------------------------------------

# Green for reference line, orange for fit line
color_ref <- "#799d2d"
color_fit <- "#f28200"

# Plot 2019 Basketball Men vs Women expenses on Log Scale
sports %>%
  filter(year == 2019, sports == "Basketball") %>%
  drop_na(exp_women, exp_men) %>% 
  ggplot(aes(x = log10(exp_men), y = log10(exp_women))) +
  geom_point(alpha = 0.5, size = 2.5) +
  
  # Add 1:1 ratio reference line
  geom_abline(slope = 1, intercept = 0, color = color_ref, size = 1.5) +
  
  # Add quadratic fit
  geom_smooth(
    method = "lm",
    formula = "y ~ x + I(x^2)", 
    color = color_fit,
    size = 1.5,
    se = FALSE
  ) +
  
  # Arrows & Labels for 1:1 line and quadratic line
  geom_curve(
    mapping = aes(x = 5, y = 6.5, xend = 6.6, yend = 6.7),
    arrow = arrow(length = unit(0.2, "inch")), 
    size = 1.5,
    color = color_ref, 
    curvature = -0.2
  ) +
  geom_curve(
    mapping = aes(x = 6.15, y = 4.5, xend = 6.3, yend = 5.8),
    arrow = arrow(length = unit(0.2, "inch")),
    size = 1.5,
    color = color_fit, 
    curvature = 0.3
  ) +
  annotate(
    "text", 
    x = 4.85,
    y = 6.35, 
    label = "1:1 Reference",
    size = 5, 
    fontface = "bold",
    color = color_ref
  ) +
  annotate(
    "text",
    x = 6.15, 
    y = 4.35,
    label = "Quadratic Fit",
    size = 5, 
    fontface = "bold",
    color = color_fit
  ) +
  
  # Label a couple of schools
  geom_text(
    aes(label = if_else(
      unitid == 168430,
      str_replace(institution_name, "State University", "State\nUniversity"),
      ""
    )),
    hjust = "right",
    vjust = "bottom",
    nudge_y = 0.05,
    lineheight = 0.85,
    family = "IBM Plex Sans",
  ) +
  geom_text(
    aes(label = if_else(
      log10(exp_men) > 7 & log10(exp_women) < 6,
      str_replace(institution_name, " ", "\n"),
      ""
    )),
    hjust = "left",
    vjust = "top",
    nudge_y = -0.05,
    lineheight = 0.85,
    family = "IBM Plex Sans",
  ) +
  
  # Axis labels & theme options
  labs(
    x = "Expenditures for Men (USD)",
    y = "Expenditures for Women (USD)",
    title ="Gender Inequality in Collegiate Basketball Spending",
    subtitle = "Academic Year 2019-2020",
    caption = "Data source: Equity in Athletics Data Analysis | Graphic: @kjmimmack 2022 | Made with R"
  ) +
  scale_x_continuous(
    breaks = 4:7,
    labels = c("$10K", "$100K", "$1M", "$10M")
  ) +
  scale_y_continuous(
    breaks = 4:7,
    labels = c("$10K", "$100K", "$1M", "$10M")
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "IBM Plex Sans"),
    axis.title = element_text(family = "IBM Plex Sans"),
    plot.caption = element_text(
      color = "grey40",
      margin = margin(0.2, 0, 0, 0, "in")
    )
  )

# Save -------------------------------------------------------------------------

ggsave(
  paste0(tt_date, ".png"),
  device = "png",  
  height = 7, 
  width = 7, 
  units = "in",
  type = "cairo"
)

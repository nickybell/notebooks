# Recreate Our World in Data Cherry Blossom Graph
# Data: Kyoto Cherry Blossom Peak Bloom Dates (812-2025)

library(ggplot2)
library(showtext)
library(sysfonts)
library(ggtext)
library(hrbrthemes)

# Load Roboto Condensed from Google Fonts
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

# Load the data
data <- read.csv(
  "/Users/nickybell/.mcp-servers/kaggle-mcp/datasets/japans-cherry/date-of-the-peak-cherry-tree-blossom-in-kyoto.csv"
)

# Rename columns for easier handling
colnames(data) <- c("Entity", "Code", "Year", "Twenty_year_avg", "Day_of_year")

# Convert day of year to Date objects (using 2023 as reference year - non-leap year)
# Day 1 = Jan 1, so we use origin of Dec 31, 2022
data$date_display <- as.Date(data$Day_of_year - 1, origin = "2022-12-31")

# Define y-axis limits (Mar 21 = day 80, Apr 30 = day 120)
y_min <- as.Date("2023-03-21")
y_max <- as.Date("2023-04-30")

# Create the plot
(p <- ggplot(data, aes(x = Year, y = date_display)) +
  # Layer 1: Individual observations as scatter points
  geom_point(
    color = "#D4A5A5", # Muted pink
    alpha = 0.6,
    size = 1.5
  ) +

  # Layer 2: LOESS smoothed line for individual observations
  geom_smooth(
    color = "#A41E22", # Dark red
    linewidth = 1.2,
    se = FALSE,
    span = 0.3
  ) +

  # X-axis configuration
  scale_x_continuous(
    breaks = c(812, seq(1000, 1800, 200), 2025),
    limits = c(800, 2050),
    expand = c(0.01, 0)
  ) +

  # Y-axis configuration with date labels
  scale_y_date(
    date_labels = "%b %d",
    breaks = as.Date(c(
      "2023-03-21",
      "2023-03-31",
      "2023-04-10",
      "2023-04-20",
      "2023-04-30"
    )),
    limits = c(y_min, y_max)
  ) +

  # Labels
  labs(
    title = "Cherry trees have been blossoming earlier\ndue to warmer spring temperatures",
    subtitle = expression(paste(
      "Date of peak cherry tree (",
      italic("Prunus jamasakura"),
      ") blossom in Kyoto, Japan"
    )),
    x = NULL,
    y = NULL,
    caption = "Data: Yasuyuki Aono; smoothed using cubic splines."
  ) +

  # Theme customization
  theme_minimal(base_size = 12) +
  theme(
    # Background
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),

    # Grid lines
    panel.grid.major.y = element_line(color = "#E5E5E5", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),

    # Explicit font family override
    text = element_text(family = "Roboto Condensed"),

    # Typography
    plot.title = element_text(
      family = "Roboto Condensed",
      face = "bold",
      size = 22,
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      family = "Roboto Condensed",
      size = 14,
      lineheight = 1.2,
      margin = margin(b = 15)
    ),
    plot.caption = element_text(
      family = "Roboto Condensed",
      color = "gray50",
      size = 10,
      margin = margin(t = 15)
    ),

    # Axis styling
    axis.text.x = element_text(
      family = "Roboto Condensed",
      size = 11
    ),
    axis.text.y = element_text(family = "Roboto Condensed", size = 11),

    # Plot margins
    plot.margin = margin(20, 20, 20, 20)
  ))

# Save the plot - use absolute path to avoid directory issues
output_path <- "/Users/nickybell/notebooks/cherry_blossoms/graphs/cherry_blossom_kyoto.png"

ggsave(
  filename = output_path,
  plot = p,
  width = 8,
  height = 8 / 3 * 2,
  dpi = 300,
  bg = "white"
)

cat("Graph saved to", output_path, "\n")

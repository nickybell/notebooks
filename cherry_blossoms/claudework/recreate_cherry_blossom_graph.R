# Recreate Our World in Data Cherry Blossom Graph
# Data: Kyoto Cherry Blossom Peak Bloom Dates (812-2025)

library(ggplot2)

# Load the data
data <- read.csv("/Users/nickybell/.mcp-servers/kaggle-mcp/datasets/japans-cherry/date-of-the-peak-cherry-tree-blossom-in-kyoto.csv")

# Rename columns for easier handling
colnames(data) <- c("Entity", "Code", "Year", "Twenty_year_avg", "Day_of_year")

# Convert day of year to Date objects (using 2023 as reference year - non-leap year)
# Day 1 = Jan 1, so we use origin of Dec 31, 2022
data$date_display <- as.Date(data$Day_of_year - 1, origin = "2022-12-31")
data$avg_date_display <- as.Date(data$Twenty_year_avg - 1, origin = "2022-12-31")

# Filter data for the 20-year average line (non-NA values)
avg_data <- data[!is.na(data$Twenty_year_avg), ]

# Define y-axis limits (Mar 21 = day 80, Apr 30 = day 120)
y_min <- as.Date("2023-03-21")
y_max <- as.Date("2023-04-30")

# Create the plot
p <- ggplot() +
  # Layer 1: Individual observations as scatter points
  geom_point(data = data,
             aes(x = Year, y = date_display),
             color = "#D4A5A5",  # Muted pink
             alpha = 0.6,
             size = 1.5) +

  # Layer 2: 20-year rolling average line
  geom_line(data = avg_data,
            aes(x = Year, y = avg_date_display),
            color = "#A41E22",  # Dark red
            linewidth = 1.2) +

  # X-axis configuration
  scale_x_continuous(
    breaks = seq(800, 2000, 200),
    limits = c(800, 2050),
    expand = c(0.01, 0)
  ) +

  # Y-axis configuration with date labels
  scale_y_date(
    date_labels = "%b %d",
    breaks = as.Date(c("2023-03-21", "2023-03-31", "2023-04-10", "2023-04-20", "2023-04-30")),
    limits = c(y_min, y_max)
  ) +

  # Labels
  labs(
    title = "Cherry trees have been blossoming earlier\ndue to warmer spring temperatures",
    subtitle = expression(paste("Date of peak cherry tree (", italic("Prunus jamasakura"), ") blossom in Kyoto, Japan.")),
    x = NULL,
    y = NULL,
    caption = "Note: The 20-year average is calculated when there are at least five years with\ndata in the 20-year window. The dates are off by one day on leap years.\nData source: Yasuyuki Aono (2021; 2025)"
  ) +

  # Add annotation for "20 year average" label
  annotate("text", x = 1920, y = as.Date("2023-04-08"),
           label = "20 year\naverage",
           color = "#A41E22",
           size = 3.5,
           hjust = 0,
           lineheight = 0.9) +

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

    # Title and subtitle
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0,
      color = "#333333",
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      size = 11,
      hjust = 0,
      color = "#666666",
      margin = margin(b = 15)
    ),

    # Caption (footnote)
    plot.caption = element_text(
      size = 8,
      hjust = 0,
      color = "#888888",
      margin = margin(t = 15),
      lineheight = 1.2
    ),

    # Axis text
    axis.text.x = element_text(size = 10, color = "#666666"),
    axis.text.y = element_text(size = 10, color = "#666666"),

    # Plot margins
    plot.margin = margin(20, 20, 20, 20)
  )

# Save the plot - use absolute path to avoid directory issues
output_path <- "/Users/nickybell/notebooks/cherry_blossoms/graphs/cherry_blossom_kyoto.png"

ggsave(
  filename = output_path,
  plot = p,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

cat("Graph saved to", output_path, "\n")

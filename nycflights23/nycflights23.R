library(dplyr)
library(ggplot2)
library(nycflights23)
library(ggimage)
library(magick)

data(flights)
data(airlines)

jfk <- filter(flights, origin == "JFK") |>
  left_join(airlines, by = join_by(carrier)) |>
  mutate(name = forcats::fct_inorder(name))

# Create rotated logos directory if it doesn't exist
dir.create("airlines_logos/rotated", showWarnings = FALSE)

# Get list of all logo files and rotate them 270 degrees at high resolution
logo_files <- list.files(
  "airlines_logos",
  full.names = TRUE,
  pattern = "\\.svg$"
)

for (logo in logo_files) {
  # Read SVG at very high resolution (4000 pixels wide)
  img <- image_read_svg(logo, width = 4000)
  img_rotated <- image_rotate(img, 270)

  # Save rotated version as PNG
  filename <- sub("\\.svg$", ".png", basename(logo))
  output_path <- file.path("airlines_logos/rotated", filename)
  image_write(img_rotated, output_path, format = "png")
}

# Create summary data with counts
jfk_summary <- jfk |>
  count(name)

# Create mapping to rotated logos
jfk_summary <- jfk_summary |>
  mutate(
    logo = case_when(
      name == "JetBlue Airways" ~ "airlines_logos/rotated/jetblue.png",
      name == "Delta Air Lines Inc." ~ "airlines_logos/rotated/delta.png",
      name == "American Airlines Inc." ~ "airlines_logos/rotated/american.png",
      name == "Endeavor Air Inc." ~ "airlines_logos/rotated/endeavor.png",
      name == "Republic Airline" ~ "airlines_logos/rotated/republic.png",
      name == "Alaska Airlines Inc." ~ "airlines_logos/rotated/alaska.png",
      name == "Hawaiian Airlines Inc." ~ "airlines_logos/rotated/hawaiian.png",
      name == "SkyWest Airlines Inc." ~ "airlines_logos/rotated/skywest.png"
    )
  )

# Create plot with custom aspect ratios for each airline
dir.create("graphs", showWarnings = FALSE)

p <- ggplot(jfk_summary, aes(x = reorder(name, n), y = n)) +
  geom_col(fill = "white", color = "gray80") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Number of Flights from JFK by Airline",
    x = "Airline",
    y = "Number of Flights"
  )

# Add each logo with custom aspect ratio based on bar height
for (i in 1:nrow(jfk_summary)) {
  row <- jfk_summary[i, ]
  p <- p +
    geom_image(
      data = row,
      aes(x = name, y = n / 2, image = logo),
      size = 0.95,
      by = "width",
      asp = row$n / max(jfk_summary$n) * 0.14
    )
}

p

ggsave(
  "graphs/jfk_flights_filled_logos.png",
  width = 12,
  height = 8,
  dpi = 600
)

library(ggplot2)
library(dplyr)
library(showtext)
library(sysfonts)
library(ggtext)
library(hrbrthemes)

# Load Roboto Condensed from Google Fonts
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

marathon_df <- readr::read_csv(here::here("boston_marathon_mens_open.csv")) |>
  mutate(year = lubridate::make_date(year))

format_marathon_time <- function(seconds) {
  hours <- seconds %/% 3600
  minutes <- (seconds %% 3600) %/% 60
  secs <- seconds %% 60
  sprintf("%d:%02d:%02d", hours, minutes, secs)
}

p_base <- ggplot(
  marathon_df,
  aes(x = year, y = time_seconds)
) +
  scale_y_continuous(
    labels = format_marathon_time,
    limits = c(7200, 10800),
    breaks = seq(7200, 10800, by = 1200)
  ) +
  scale_x_date(
    date_labels = "%Y",
    breaks = c(
      lubridate::make_date(1897),
      seq(
        lubridate::make_date(1912),
        lubridate::make_date(2024),
        by = "16 years"
      )
    )
  ) +
  labs(
    title = "Winning Times in the Boston Marathon Men's Division",
    subtitle = "How much lower can they go?",
    x = NULL,
    y = NULL
  ) +
  theme_ipsum_rc(grid = "Y") +
  theme(
    # Explicit font family override
    text = element_text(family = "Roboto Condensed"),

    # Typography
    plot.title = element_text(
      family = "Roboto Condensed",
      face = "bold",
      size = 22,
      color = "#22367B",
      margin = margin(b = 5)
    ),
    plot.subtitle = element_markdown(
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
      angle = 45,
      hjust = 1,
      size = 11
    ),
    axis.text.y = element_text(family = "Roboto Condensed", size = 11),

    # Remove unnecessary gridlines
    panel.grid.major.y = element_line(
      color = "#F6D440",
      linewidth = 0.3
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),

    # Legend inside plot
    legend.position = c(0.85, 0.85),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),

    # Breathing room
    plot.margin = margin(20, 20, 10, 10)
  )

(p1 <- p_base +
  geom_line(color = "#22367B", linewidth = 1.2) +
  labs(caption = "Data: Boston Athletic Association"))

(p2 <- p_base +
  geom_smooth(color = "#22367B", linewidth = 1.2, span = .3, se = F) +
  labs(
    caption = "Data: Boston Athletic Association; LOESS smoothing with span = 0.3"
  ))

ggsave(
  filename = "graphs/boston_marathon_winning_times_jagged.png",
  plot = p1,
  width = 8,
  height = 7 / 3 * 2,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "graphs/boston_marathon_winning_times_smooth.png",
  plot = p2,
  width = 8,
  height = 7 / 3 * 2,
  dpi = 300,
  bg = "white"
)

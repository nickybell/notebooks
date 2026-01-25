library(ggplot2)
library(dplyr)
library(showtext)
library(sysfonts)
library(ggtext)
library(hrbrthemes)

# Load Roboto Condensed from Google Fonts
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

salaries_df <- readr::read_csv(here::here("data/nba_salaries_2024_25.csv"))

# Prepare summary data
team_summary <- salaries_df |>
  group_by(team) |>
  summarise(
    avg_salary = mean(x2024_25, na.rm = TRUE),
    win_pct = mean(win_pct, na.rm = TRUE)
  )

# Create polished visualization - BAR CHART
p1 <- ggplot(
  team_summary,
  aes(x = reorder(team, -avg_salary), y = avg_salary, fill = win_pct)
) +
  geom_col(width = 0.8) +
  scale_fill_gradient2(
    low = "#24292E",
    mid = "#8E9AAF",
    high = "#FFD700",
    midpoint = 0.5,
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(scale = 1e-6, suffix = "M"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Does Spending Big Mean Winning Big?",
    subtitle = "Average player salary by NBA team for the 2024–25 season, colored by <span style='color:#FFD700;'>**winning**</span> and <span style='color:#24292E;'>**losing**</span> records",
    x = NULL,
    y = NULL,
    fill = "Win %",
    caption = "Data: HoopsHype & ESPN"
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 8,
      barheight = 0.5,
      title.position = "left",
      title.vjust = 0.9,
      reverse = TRUE
    )
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

# Create polished visualization - VIOLIN PLOT
# Order teams by average salary for violin plot
salaries_violin <- salaries_df |>
  mutate(
    team = factor(
      team,
      levels = team_summary |>
        arrange(desc(avg_salary)) |>
        pull(team)
    ),
    highlight = case_match(
      team,
      c("Thunder", "Cavaliers", "Celtics") ~ "Winner",
      c("Jazz", "76ers", "Suns") ~ "Loser",
      .default = "Other"
    )
  )

salaries_positioned <- salaries_violin |>
  filter(x2024_25 > 40000000) |>
  group_by(team, x2024_25) |>
  mutate(
    n_at_value = n(),
    x_offset = if_else(
      n_at_value > 1,
      (row_number() - (n_at_value + 1) / 2) * 0.25,
      0
    )
  ) |>
  ungroup()

p2 <- ggplot(
  salaries_violin,
  aes(x = team, y = x2024_25, fill = win_pct, alpha = highlight)
) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(
    data = salaries_positioned,
    aes(
      x = as.numeric(team) + x_offset,
      y = x2024_25,
      color = highlight,
      alpha = highlight
    ),
    size = 1.5,
    inherit.aes = FALSE
  ) +
  scale_fill_gradient2(
    low = "#24292E",
    mid = "#8E9AAF",
    high = "#FFD700",
    midpoint = 0.5,
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(scale = 1e-6, suffix = "M"),
    expand = expansion(mult = c(0.02, 0.05)),
    limits = c(0, 60000000)
  ) +
  labs(
    title = "Does Spending Big Mean Winning Big?",
    subtitle = "Distribution of player salaries by NBA team for the 2024–25 season, colored by <span style='color:#FFD700;'>**winning**</span> and <span style='color:#24292E;'>**losing**</span> records",
    x = NULL,
    y = NULL,
    fill = "Win %",
    caption = "Data: HoopsHype & ESPN | Points show players earning $40M+"
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 8,
      barheight = 0.5,
      title.position = "left",
      title.vjust = 0.9,
      reverse = TRUE
    ),
    alpha = "none",
    color = "none"
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

# Save both plots
ggsave(
  filename = "graphs/nba_salaries_bar.png",
  plot = p1,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "graphs/nba_salaries_violin_all.png",
  plot = p2 +
    scale_alpha_manual(
      values = c("Winner" = 1, "Loser" = 1, "Other" = 1)
    ) +
    scale_color_manual(
      values = c("Winner" = "#000000", "Loser" = "#000000", "Other" = "#000000")
    ),
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "graphs/nba_salaries_violin_winners.png",
  plot = p2 +
    scale_alpha_manual(
      values = c("Winner" = 1, "Loser" = 0.1, "Other" = 0.1)
    ) +
    scale_color_manual(
      values = c("Winner" = "#FFD700", "Loser" = "#ffffff", "Other" = "#ffffff")
    ),
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "graphs/nba_salaries_violin_losers.png",
  plot = p2 +
    scale_alpha_manual(
      values = c("Winner" = 0.1, "Loser" = 1, "Other" = 0.1)
    ) +
    scale_color_manual(
      values = c("Winner" = "#ffffff", "Loser" = "#24292E", "Other" = "#ffffff")
    ),
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

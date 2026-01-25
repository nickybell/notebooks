# NBA Salaries Scraper using chromote

library(chromote)
library(rvest)
library(jsonlite)
library(dplyr)
library(stringr)

# Start Chrome session
b <- ChromoteSession$new()

# First, get team ID to team name mapping from team salaries page
b$Page$navigate("https://hoopshype.com/salaries/teams/")
b$Page$loadEventFired()
Sys.sleep(2)

teams_json <- b$Runtime$evaluate("
  JSON.stringify(
    Array.from(document.querySelectorAll('table tbody tr')).map(row => {
      const img = row.querySelector('img');
      const teamLink = row.querySelector('a');
      const logoUrl = img ? img.src : '';
      const match = logoUrl.match(/logos\\/(\\d+)\\.png/);
      return {
        team: teamLink ? teamLink.textContent.trim() : '',
        teamId: match ? match[1] : ''
      };
    })
  );
")$result$value

team_lookup <- fromJSON(teams_json)
team_map <- setNames(team_lookup$team, team_lookup$teamId)

# Now navigate to player salaries page
b$Page$navigate("https://www.hoopshype.com/salaries/players/?season=2024")
b$Page$loadEventFired()

all_salaries <- list()
last_first_player <- ""

for (page_num in 1:31) {
  cat("Scraping page", page_num, "\n")

  # Wait until we get a valid table with a different first player

  for (i in 1:20) {
    Sys.sleep(0.5)
    page_html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    page <- read_html(page_html)
    salary_table <- html_table(html_node(page, "table"))
    
    # Check that table has "Player" column and valid data
    if (!"Player" %in% names(salary_table)) next
    if (nrow(salary_table) == 0) next
    
    current_first_player <- salary_table$Player[1]
    if (!is.null(current_first_player) && 
        !is.na(current_first_player) && 
        current_first_player != last_first_player) {
      last_first_player <- current_first_player
      break
    }
  }
  
  # Extract team IDs from player logo URLs
  player_teams_json <- b$Runtime$evaluate("
    JSON.stringify(
      Array.from(document.querySelectorAll('table tbody tr')).map(row => {
        const img = row.querySelector('img');
        const logoUrl = img ? img.src : '';
        const match = logoUrl.match(/logos\\/(\\d+)\\.png/);
        return match ? match[1] : '';
      })
    );
  ")$result$value
  
  team_ids <- fromJSON(player_teams_json)
  salary_table$teamId <- team_ids
  salary_table$team <- team_map[salary_table$teamId]
  
  all_salaries[[page_num]] <- salary_table

  # Click next button (if not last page)
  if (page_num < 30) {
    b$Runtime$evaluate("document.querySelectorAll('button[class*=\"hd3Vfp\"]')[1].click()")
  }
}

# Clean up
b$close()

# Combine all pages into single data frame
salaries_df <- do.call(rbind, all_salaries)

# --- Scrape ESPN standings for win percentage ---
b <- ChromoteSession$new()

b$Page$navigate("https://www.espn.com/nba/standings/_/season/2025/group/league")
b$Page$loadEventFired()
Sys.sleep(3)

# Get team names
teams_json <- b$Runtime$evaluate("
  JSON.stringify(
    Array.from(document.querySelectorAll('.Table--fixed-left .hide-mobile a')).map(a => {
      return a.textContent.trim();
    })
  );
")$result$value

# Get PCT values (3rd column in stats table)
pct_json <- b$Runtime$evaluate("
  JSON.stringify(
    Array.from(document.querySelectorAll('table:not(.Table--fixed-left) tbody tr')).map(row => {
      const cells = row.querySelectorAll('td');
      return cells[2] ? cells[2].textContent.trim() : '';
    })
  );
")$result$value

b$close()

# Create standings dataframe
standings_df <- tibble(
  espn_team = fromJSON(teams_json),
  win_pct = as.numeric(fromJSON(pct_json))
)

# Map ESPN team names to HoopsHype short names
team_name_map <- c(
  "Oklahoma City Thunder" = "Thunder",
  "Cleveland Cavaliers" = "Cavaliers",
  "Boston Celtics" = "Celtics",
  "Houston Rockets" = "Rockets",
  "New York Knicks" = "Knicks",
  "Los Angeles Lakers" = "Lakers",
  "Denver Nuggets" = "Nuggets",
  "Indiana Pacers" = "Pacers",
  "LA Clippers" = "Clippers",
  "Minnesota Timberwolves" = "Timberwolves",
  "Golden State Warriors" = "Warriors",
  "Memphis Grizzlies" = "Grizzlies",
  "Milwaukee Bucks" = "Bucks",
  "Detroit Pistons" = "Pistons",
  "Orlando Magic" = "Magic",
  "Atlanta Hawks" = "Hawks",
  "Sacramento Kings" = "Kings",
  "Chicago Bulls" = "Bulls",
  "Dallas Mavericks" = "Mavericks",
  "Miami Heat" = "Heat",
  "Phoenix Suns" = "Suns",
  "Portland Trail Blazers" = "Trail Blazers",
  "San Antonio Spurs" = "Spurs",
  "Toronto Raptors" = "Raptors",
  "Brooklyn Nets" = "Nets",
  "Philadelphia 76ers" = "76ers",
  "New Orleans Pelicans" = "Pelicans",
  "Charlotte Hornets" = "Hornets",
  "Washington Wizards" = "Wizards",
  "Utah Jazz" = "Jazz"
)

standings_df <- standings_df |>
  mutate(team = team_name_map[espn_team]) |>
  select(team, win_pct)

# Join win percentage with salaries
salaries_df <- salaries_df |>
  left_join(standings_df, by = "team")

# Remove non-numeric characters from salary columns
salaries_lim <- salaries_df |>
  janitor::clean_names() |>
  select(player, team, x2024_25, win_pct) |>
  mutate(
    x2024_25 = str_remove_all(x2024_25, "[^\\d]"),
    x2024_25 = as.numeric(x2024_25)
  ) |>
  filter(x2024_25 >= 1157153) # Minimum NBA salary 2024-25

readr::write_csv(salaries_lim, here::here("data/nba_salaries_2024_25.csv"), na = "")

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
b$Page$navigate("https://hoopshype.com/salaries/players/")
b$Page$loadEventFired()

all_salaries <- list()
last_first_player <- ""

for (page_num in 1:30) {
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

# Remove non-numeric characters from salary columns
salaries_lim <- salaries_df |>
  janitor::clean_names() |>
  select(player, team, x2025_26) |>
  mutate(
    x2025_26 = str_remove_all(x2025_26, "[^\\d]"),
    x2025_26 = as.numeric(x2025_26)
  ) |>
  filter(x2025_26 >= 1272870) # Minimum NBA salary 2025-26

readr::write_csv(salaries_lim, here::here("data/nba_salaries_2025_26.csv"), na = "")

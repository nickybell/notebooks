#!/usr/bin/env Rscript
# Boston Marathon Men's Open Division Web Scraper
# Scrapes winning times and years from the BAA champions webpage

# ==============================================================================
# 1. LOAD REQUIRED PACKAGES
# ==============================================================================

# Note: This script uses renv for package management.
# To install required packages, run: renv::install(c("rvest", "dplyr"))

library(rvest)
library(dplyr)

# ==============================================================================
# 2. FETCH AND PARSE HTML
# ==============================================================================

url <- "https://www.baa.org/races/boston-marathon/results/champions/"

message("Fetching data from: ", url)

# Fetch the webpage
tryCatch({
  webpage <- read_html(url)
  message("Successfully fetched webpage")
}, error = function(e) {
  stop("Failed to fetch webpage: ", e$message)
})

# ==============================================================================
# 3. EXTRACT MEN'S OPEN DIVISION DATA
# ==============================================================================

# Extract all text content from the page
page_text <- webpage %>% html_text2()

# Find the "Men's Open" section and extract text until next section
mens_open_pattern <- "Men's Open"
womens_open_pattern <- "Women's Open"

# Find start and end positions
mens_start <- regexpr(mens_open_pattern, page_text, fixed = TRUE)[1]
womens_start <- regexpr(womens_open_pattern, page_text, fixed = TRUE)[1]

if (mens_start == -1) {
  stop("Could not find 'Men's Open' section in the webpage")
}

message("Found Men's Open section")

# Extract just the Men's Open section text
if (womens_start > mens_start) {
  mens_section <- substr(page_text, mens_start + nchar(mens_open_pattern), womens_start - 1)
} else {
  # If Women's section isn't found, take a large chunk
  mens_section <- substr(page_text, mens_start + nchar(mens_open_pattern), mens_start + 50000)
}

# List of countries that appear in Boston Marathon results
countries <- c("Kenya", "Ethiopia", "United States", "Japan", "Italy", "England",
               "Australia", "Canada", "South Korea", "Finland", "Belgium",
               "New Zealand", "Yugoslavia", "Colombia", "Ireland", "Guatemala",
               "Korea", "Sweden", "Greece", "Germany")

# Sort countries by length (longest first) to ensure "United States" matches before "States"
countries <- countries[order(nchar(countries), decreasing = TRUE)]

# Create a pattern that matches any of these countries
country_pattern <- paste(countries, collapse = "|")

# Pattern to match each entry: Year Name(s) Country Time
# Allow for flexible whitespace (including newlines) between components
# More precise: year, then text until we hit a known country, then country, then time
# Note: Includes both straight quotes (\") and curly quotes (\u201C\u201D) for names like "Meb"
entry_pattern <- paste0("(\\d{4})\\s+([A-Za-z\\s\\.\\(\\)\"\u201C\u201D'-]+?)\\s+(",
                       country_pattern,
                       ")\\s+(\\d{1,2}:\\d{2}:\\d{2}[*^]?)")

# Note: \\s matches any whitespace including newlines in R regex
# \u201C = left double quotation mark, \u201D = right double quotation mark

# Find all matches
matches <- gregexpr(entry_pattern, mens_section, perl = TRUE)
match_data <- regmatches(mens_section, matches)[[1]]

message("Found ", length(match_data), " Men's Open championship records")

# ==============================================================================
# 4. PARSE DATA INTO STRUCTURED FORMAT
# ==============================================================================

# Function to convert time string to seconds
time_to_seconds <- function(time_str) {
  # Remove asterisks and carets
  time_clean <- gsub("[*^]", "", time_str)

  # Split by colon
  parts <- as.numeric(strsplit(time_clean, ":")[[1]])

  if (length(parts) == 3) {
    # H:MM:SS format
    return(parts[1] * 3600 + parts[2] * 60 + parts[3])
  } else if (length(parts) == 2) {
    # MM:SS format
    return(parts[1] * 60 + parts[2])
  } else {
    return(NA)
  }
}

# Parse each match
parsed_data <- lapply(match_data, function(entry) {
  # Use regex to extract components
  components <- regmatches(entry, regexec(entry_pattern, entry, perl = TRUE))[[1]]

  if (length(components) == 5) {
    year <- as.integer(components[2])
    name <- trimws(components[3])
    country <- trimws(components[4])
    time <- trimws(components[5])
    time_seconds <- time_to_seconds(time)

    return(data.frame(
      year = year,
      name = name,
      country = country,
      time = time,
      time_seconds = time_seconds,
      stringsAsFactors = FALSE
    ))
  } else {
    return(NULL)
  }
})

# Combine into a single data frame
boston_marathon_data <- bind_rows(parsed_data)

# Sort by year (descending)
boston_marathon_data <- boston_marathon_data %>%
  arrange(desc(year))

# ==============================================================================
# 5. SAVE RESULTS
# ==============================================================================

output_file <- "~/notebooks/boston_marathon/boston_marathon_mens_open.csv"
output_file <- path.expand(output_file)

write.csv(boston_marathon_data, output_file, row.names = FALSE)
message("\nData saved to: ", output_file)

# ==============================================================================
# 6. DISPLAY SUMMARY STATISTICS
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("BOSTON MARATHON MEN'S OPEN DIVISION - SUMMARY\n")
cat(strrep("=", 70), "\n\n")

cat("Total races scraped:", nrow(boston_marathon_data), "\n")
cat("Year range:", min(boston_marathon_data$year), "-", max(boston_marathon_data$year), "\n\n")

# Fastest time
fastest <- boston_marathon_data %>%
  filter(time_seconds == min(time_seconds, na.rm = TRUE))
cat("Fastest time:", fastest$time, "by", fastest$name, "(", fastest$year, ")\n")

# Slowest time
slowest <- boston_marathon_data %>%
  filter(time_seconds == max(time_seconds, na.rm = TRUE))
cat("Slowest time:", slowest$time, "by", slowest$name, "(", slowest$year, ")\n\n")

# Recent winners (last 5 years)
cat("Most recent champions:\n")
print(head(boston_marathon_data %>% select(year, name, country, time), 5))

cat("\n")

# Earliest winners (first 5 years)
cat("Earliest champions:\n")
print(tail(boston_marathon_data %>% select(year, name, country, time), 5))

cat("\n", strrep("=", 70), "\n")
cat("Scraping completed successfully!\n")
cat(strrep("=", 70), "\n")

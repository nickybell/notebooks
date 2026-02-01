library(rvest)

url <- "https://www.baa.org/races/boston-marathon/results/champions/"
webpage <- read_html(url)
page_text <- webpage %>% html_text2()

# Find sections
mens_open_pattern <- "Men's Open"
womens_open_pattern <- "Women's Open"

mens_start <- regexpr(mens_open_pattern, page_text, fixed = TRUE)[1]
womens_start <- regexpr(womens_open_pattern, page_text, fixed = TRUE)[1]

cat("Men's Open starts at:", mens_start, "\n")
cat("Women's Open starts at:", womens_start, "\n\n")

# Extract Men's section
if (womens_start > mens_start) {
  mens_section <- substr(page_text, mens_start + nchar(mens_open_pattern), womens_start - 1)
} else {
  mens_section <- substr(page_text, mens_start + nchar(mens_open_pattern), mens_start + 50000)
}

cat("Men's section length:", nchar(mens_section), "\n\n")

# Check if 2014 is in the section
if (grepl("2014", mens_section)) {
  cat("✓ 2014 found in Men's section\n\n")

  # Show context around 2014
  pos_2014 <- regexpr("2014", mens_section)
  start_pos <- max(1, pos_2014 - 100)
  end_pos <- min(nchar(mens_section), pos_2014 + 200)
  context <- substr(mens_section, start_pos, end_pos)

  cat("Context around 2014 in extracted section:\n")
  cat("==========================================\n")
  cat(context)
  cat("\n==========================================\n")
} else {
  cat("✗ 2014 NOT found in Men's section\n")
  cat("This is the problem!\n")
}

# Test the regex
countries <- c("Kenya", "Ethiopia", "United States", "Japan", "Italy", "England",
               "Australia", "Canada", "South Korea", "Finland", "Belgium",
               "New Zealand", "Yugoslavia", "Colombia", "Ireland", "Guatemala",
               "Korea", "Sweden", "Greece", "Germany")
countries <- countries[order(nchar(countries), decreasing = TRUE)]
country_pattern <- paste(countries, collapse = "|")
entry_pattern <- paste0("(\\d{4})\\s+([A-Za-z\\s\\.\\(\\)\"'-]+?)\\s+(",
                       country_pattern,
                       ")\\s+(\\d{1,2}:\\d{2}:\\d{2}[*^]?)")

matches <- gregexpr(entry_pattern, mens_section, perl = TRUE)
match_data <- regmatches(mens_section, matches)[[1]]

cat("\nTotal matches found:", length(match_data), "\n")

# Check for 2014 specifically
has_2014 <- any(grepl("^2014", match_data))
cat("2014 in matches:", has_2014, "\n")

library(rvest)

# Test string with 2014 entry
test_string <- "2015 Lelisa Desisa Ethiopia 2:09:17 2014 Mebrahtom \"Meb\" Keflezighi United States 2:08:37 2013 Lelisa Desisa Ethiopia 2:10:22"

# List of countries
countries <- c("Kenya", "Ethiopia", "United States", "Japan", "Italy", "England",
               "Australia", "Canada", "South Korea", "Finland", "Belgium",
               "New Zealand", "Yugoslavia", "Colombia", "Ireland", "Guatemala",
               "Korea", "Sweden", "Greece", "Germany")

# Sort by length
countries <- countries[order(nchar(countries), decreasing = TRUE)]

# Create pattern
country_pattern <- paste(countries, collapse = "|")
entry_pattern <- paste0("(\\d{4})\\s+([A-Za-z\\s\\.\\(\\)\"'-]+?)\\s+(",
                       country_pattern,
                       ")\\s+(\\d{1,2}:\\d{2}:\\d{2}[*^]?)")

cat("Country pattern:\n", country_pattern, "\n\n")
cat("Entry pattern:\n", entry_pattern, "\n\n")

# Find matches
matches <- gregexpr(entry_pattern, test_string, perl = TRUE)
match_data <- regmatches(test_string, matches)[[1]]

cat("Number of matches:", length(match_data), "\n\n")

for (i in seq_along(match_data)) {
  cat("Match", i, ":\n")
  cat(match_data[i], "\n")

  # Parse it
  components <- regmatches(match_data[i], regexec(entry_pattern, match_data[i], perl = TRUE))[[1]]
  if (length(components) == 5) {
    cat("  Year:", components[2], "\n")
    cat("  Name:", components[3], "\n")
    cat("  Country:", components[4], "\n")
    cat("  Time:", components[5], "\n")
  }
  cat("\n")
}

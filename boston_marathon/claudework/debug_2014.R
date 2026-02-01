library(rvest)

url <- "https://www.baa.org/races/boston-marathon/results/champions/"
webpage <- read_html(url)
page_text <- webpage %>% html_text2()

# Find the position of "2014" in the text
pos_2014 <- regexpr("2014", page_text)
if (pos_2014 > 0) {
  # Extract 200 characters around 2014
  start_pos <- max(1, pos_2014 - 100)
  end_pos <- min(nchar(page_text), pos_2014 + 200)
  context <- substr(page_text, start_pos, end_pos)

  cat("Context around 2014:\n")
  cat("====================\n")
  cat(context)
  cat("\n====================\n")

  # Show it with visible whitespace
  cat("\nWith visible whitespace:\n")
  cat("====================\n")
  cat(gsub("\n", "\\\\n\n", context))
  cat("\n====================\n")
}

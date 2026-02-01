library(rvest)

url <- "https://www.baa.org/races/boston-marathon/results/champions/"
webpage <- read_html(url)
page_text <- webpage %>% html_text2()

# Find the 2014 entry
pos_2014 <- regexpr("2014", page_text)
start_pos <- pos_2014
end_pos <- pos_2014 + 60

# Extract the 2014 entry
entry_2014 <- substr(page_text, start_pos, end_pos)

cat("2014 entry text:\n")
cat(entry_2014, "\n\n")

cat("Character codes:\n")
chars <- strsplit(entry_2014, "")[[1]]
for (i in seq_along(chars)) {
  char <- chars[i]
  code <- utf8ToInt(char)
  if (char == " ") {
    cat(sprintf("%2d: [SPACE] (code %d)\n", i, code))
  } else if (char == "\n") {
    cat(sprintf("%2d: [NEWLINE] (code %d)\n", i, code))
  } else if (char == "\t") {
    cat(sprintf("%2d: [TAB] (code %d)\n", i, code))
  } else {
    cat(sprintf("%2d: '%s' (code %d)\n", i, char, code))
  }
}

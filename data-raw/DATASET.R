## code to prepare scrabble list
wordlist_path <- tempfile(fileext = "txt")

download.file(
  url = "http://norvig.com/ngrams/word.list",
  destfile = wordlist_path,
  mode = "wb"
)

wordlist <- readLines(wordlist_path)

usethis::use_data(wordlist, overwrite = TRUE)

## code to prepare color palette
color_pal <- sample(colorRampPalette(colors = c("purple", "violet", "gold"))(
  15
))
scales::show_col(color_pal)

usethis::use_data(color_pal, overwrite = TRUE)

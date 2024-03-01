## code to prepare `DATASET` dataset goes here
wordlist_path <- tempfile(fileext = "txt")

download.file(
	url = "http://norvig.com/ngrams/word.list",
	destfile = wordlist_path,
	mode = "wb"
)

wordlist <- readLines(wordlist_path)

usethis::use_data(wordlist, overwrite = TRUE)

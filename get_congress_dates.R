library(tidyverse)
library(pdftools)  # pdf_text()

url <- "~/Downloads/U.S. Senate Dates of Sessions of the Congress.pdf"

output <- pdf_text(url)

temp <-
  output |>
  read_lines()
head(temp)

col_names <-  c("congress", "session", "begin_date", "adjourn_date")
skip_rows <- 2

congress_sessions_raw <-
  output |>
  read_lines(skip = skip_rows) |>
  tibble(temp = _)

regex <- str_c("^\\s*",          # Start string (perhaps followed by spaces)
               "([^\\s]+)\\s+",  # Non-space characters (followed by spaces)
               "([^\\s]+)\\s+",
               "(.*)(?=\\s[A-Z])",  # Non-space characters (followed by spaces)
               "(.*)$")

extract_dates <- function(temp) {
  str_extract_all(temp, "[A-Z][a-z]{2}\\s\\d+, \\d{4,}")
}


congress_sessions <-
  congress_sessions_raw |>
  mutate(temp = str_replace_all(temp, "(\\d{4})\\d{1,}(\\s|$)", "\\1")) |>
  # Here we use the regular expression to split the data into columns
  extract(temp, col_names, regex) |>
  filter(congress != "Congress") |>
  mutate(session = str_extract_all(session, "."),
         begin_date = extract_dates(begin_date),
         adjourn_date = extract_dates(adjourn_date)) |>
  unnest(c(session, begin_date, adjourn_date)) |>
  mutate(across(c(begin_date, adjourn_date), mdy),
         congress = as.integer(congress))

congress_terms <-
  congress_sessions |>
  filter(session != "S") |>
  group_by(congress) |>
  summarize(term_start = min(begin_date),
            .groups = "drop") |>
  mutate(term_end_year = 1789 + congress * 2,
         term_end_month = if_else(term_end_year <= 1933, "03", "01")) |>
  mutate(term_end = ymd(str_c(term_end_year, "-",
                              term_end_month, "-03"))) |>
  mutate(term_end = case_when(term_end == "1927-03-03" ~ as.Date("1927-03-04"),
                              .default = term_end)) |>
  arrange(congress)

congress_terms |>
  arrow::write_parquet("data/congress_terms.parquet")

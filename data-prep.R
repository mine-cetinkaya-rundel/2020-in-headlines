# load packages ----------------------------------------------------------------

library(tidyverse)
library(jsonlite)
library(tidytext)
library(fs)
library(lubridate)
library(glue)

# read json files --------------------------------------------------------------

json_files <- dir_ls("raw-data")
nyt_json <- map(json_files, read_json)

# convert json files to tibbles ------------------------------------------------

nyt_jan <- as_tibble(nyt_json$`raw-data/1.json`$response)
nyt_feb <- as_tibble(nyt_json$`raw-data/2.json`$response)
nyt_mar <- as_tibble(nyt_json$`raw-data/3.json`$response)
nyt_apr <- as_tibble(nyt_json$`raw-data/4.json`$response)
nyt_may <- as_tibble(nyt_json$`raw-data/5.json`$response)
nyt_jun <- as_tibble(nyt_json$`raw-data/6.json`$response)
nyt_jul <- as_tibble(nyt_json$`raw-data/7.json`$response)
nyt_aug <- as_tibble(nyt_json$`raw-data/8.json`$response)
nyt_sep <- as_tibble(nyt_json$`raw-data/9.json`$response)
nyt_oct <- as_tibble(nyt_json$`raw-data/10.json`$response)
nyt_nov <- as_tibble(nyt_json$`raw-data/11.json`$response)
nyt_dec <- as_tibble(nyt_json$`raw-data/12.json`$response)

# bind rows for monthly tibbles ------------------------------------------------

nyt_nested <- bind_rows(
  nyt_jan, nyt_feb, nyt_mar, nyt_apr, nyt_may, nyt_jun,
  nyt_jul, nyt_aug, nyt_sep, nyt_oct, nyt_nov, nyt_dec
)

# unnest columns ---------------------------------------------------------------

nyt_unnested <- nyt_nested %>%
  unnest_wider(docs) %>%
  unnest_wider(headline)

# data fixes / wrangling -------------------------------------------------------

nyt <- nyt_unnested %>%
  select(main, abstract, lead_paragraph, pub_date, web_url) %>%
  rename(headline = main) %>%
  mutate(
    text  = paste(headline, abstract),
    month = month(pub_date, label = TRUE, abbr = FALSE),
    month = fct_relevel(month, month.name),
    # streamline various spelling/representation of common words
    # Covid-19
    text = str_replace_all(text, "[Cc]OVID-19", "Covid_19"),
    text = str_replace_all(text, "COVID 19", "Covid_19"),
    text = str_replace_all(text, "[Cc]ovid-19", "Covid_19"),
    text = str_replace_all(text, "Covid 19", "Covid_19"),
    text = str_replace_all(text, "[Cc]ovid ", "Covid_19 "),
    # RBG
    text = str_replace_all(text, "Ruth Bader Ginsburgh", "Ruth_Bader_Ginsburg"),
    text = str_replace_all(text, "Justice Ginsburg", "Ruth_Bader_Ginsburg"),
    # George Floyd
    text = str_replace_all(text, "George Floyd", "George_Floyd"),
    # NYT
    text = str_replace_all(text, "New York Times", "New_York_Times"),
    # Barrett
    text = str_replace_all(text, "Amy Coney Barrett", "Amy_Coney_Barrett"),
    text = str_replace_all(text, "Judge Barrett", "Amy_Coney_Barrett"),
    # Biden
    text = str_replace_all(text, "Joe Biden", "Joe_Biden"),
    text = str_replace_all(text, "Joseph R. Biden Jr.", "Joe_Biden"),
    # trump
    text = str_replace_all(text, "President Trump", "Trump"),
    # vote (make voting into vote to combine with vote)
    text = str_replace_all(text, "voting", "vote"),
    # protesters (make protesters into protests to combine with protests)
    text = str_replace_all(text, "protesters", "protests"),
    # Fix apostrophe
    text = str_replace_all(text, "’", "'"),
    text = str_remove_all(text, "'s")
  ) %>%
  filter(!str_detect(text, "Quotation of the Day"))

# unnest tokens: words ---------------------------------------------------------

nyt_words <- nyt %>%
  unnest_tokens(word, text, drop = FALSE) %>%
  anti_join(stop_words)

# top 10 words in headlines, first appearance ----------------------------------

nyt_top100 <- nyt_words %>%
  group_by(month) %>%
  count(word, sort = TRUE) %>%
  mutate(rank = row_number()) %>%
  slice_head(n = 100) %>%
  ungroup()

jan_top10 <- nyt_top100 %>%
  filter(
    month == "January",
    rank <= 10
  )

feb_top10 <- nyt_top100 %>%
  filter(month == "February") %>%
  anti_join(jan_top10, by = "word") %>%
  slice_head(n = 10)

mar_top10 <- nyt_top100 %>%
  filter(month == "March") %>%
  anti_join(jan_top10, by = "word") %>%
  anti_join(feb_top10, by = "word") %>%
  slice_head(n = 10)

apr_top10 <- nyt_top100 %>%
  filter(month == "April") %>%
  anti_join(jan_top10, by = "word") %>%
  anti_join(feb_top10, by = "word") %>%
  anti_join(mar_top10, by = "word") %>%
  slice_head(n = 10)

may_top10 <- nyt_top100 %>%
  filter(month == "May") %>%
  anti_join(jan_top10, by = "word") %>%
  anti_join(feb_top10, by = "word") %>%
  anti_join(mar_top10, by = "word") %>%
  anti_join(apr_top10, by = "word") %>%
  slice_head(n = 10)

jun_top10 <- nyt_top100 %>%
  filter(month == "June") %>%
  anti_join(jan_top10, by = "word") %>%
  anti_join(feb_top10, by = "word") %>%
  anti_join(mar_top10, by = "word") %>%
  anti_join(apr_top10, by = "word") %>%
  anti_join(may_top10, by = "word") %>%
  slice_head(n = 10)

jul_top10 <- nyt_top100 %>%
  filter(month == "July") %>%
  anti_join(jan_top10, by = "word") %>%
  anti_join(feb_top10, by = "word") %>%
  anti_join(mar_top10, by = "word") %>%
  anti_join(apr_top10, by = "word") %>%
  anti_join(may_top10, by = "word") %>%
  anti_join(jun_top10, by = "word") %>%
  slice_head(n = 10)

aug_top10 <- nyt_top100 %>%
  filter(month == "August") %>%
  anti_join(jan_top10, by = "word") %>%
  anti_join(feb_top10, by = "word") %>%
  anti_join(mar_top10, by = "word") %>%
  anti_join(apr_top10, by = "word") %>%
  anti_join(may_top10, by = "word") %>%
  anti_join(jun_top10, by = "word") %>%
  anti_join(jul_top10, by = "word") %>%
  slice_head(n = 10)

sep_top10 <- nyt_top100 %>%
  filter(month == "September") %>%
  anti_join(jan_top10, by = "word") %>%
  anti_join(feb_top10, by = "word") %>%
  anti_join(mar_top10, by = "word") %>%
  anti_join(apr_top10, by = "word") %>%
  anti_join(may_top10, by = "word") %>%
  anti_join(jun_top10, by = "word") %>%
  anti_join(jul_top10, by = "word") %>%
  anti_join(aug_top10, by = "word") %>%
  slice_head(n = 10)

oct_top10 <- nyt_top100 %>%
  filter(month == "October") %>%
  anti_join(jan_top10, by = "word") %>%
  anti_join(feb_top10, by = "word") %>%
  anti_join(mar_top10, by = "word") %>%
  anti_join(apr_top10, by = "word") %>%
  anti_join(may_top10, by = "word") %>%
  anti_join(jun_top10, by = "word") %>%
  anti_join(jul_top10, by = "word") %>%
  anti_join(aug_top10, by = "word") %>%
  anti_join(sep_top10, by = "word") %>%
  slice_head(n = 10)

nov_top10 <- nyt_top100 %>%
  filter(month == "November") %>%
  anti_join(jan_top10, by = "word") %>%
  anti_join(feb_top10, by = "word") %>%
  anti_join(mar_top10, by = "word") %>%
  anti_join(apr_top10, by = "word") %>%
  anti_join(may_top10, by = "word") %>%
  anti_join(jun_top10, by = "word") %>%
  anti_join(jul_top10, by = "word") %>%
  anti_join(aug_top10, by = "word") %>%
  anti_join(sep_top10, by = "word") %>%
  anti_join(oct_top10, by = "word") %>%
  slice_head(n = 10)

dec_top10 <- nyt_top100 %>%
  filter(month == "December") %>%
  anti_join(jan_top10, by = "word") %>%
  anti_join(feb_top10, by = "word") %>%
  anti_join(mar_top10, by = "word") %>%
  anti_join(apr_top10, by = "word") %>%
  anti_join(may_top10, by = "word") %>%
  anti_join(jun_top10, by = "word") %>%
  anti_join(jul_top10, by = "word") %>%
  anti_join(aug_top10, by = "word") %>%
  anti_join(sep_top10, by = "word") %>%
  anti_join(oct_top10, by = "word") %>%
  anti_join(nov_top10, by = "word") %>%
  slice_head(n = 10)

top_words <- bind_rows(
  jan_top10,
  feb_top10,
  mar_top10,
  apr_top10,
  may_top10,
  jun_top10,
  jul_top10,
  aug_top10,
  sep_top10,
  oct_top10,
  nov_top10,
  dec_top10
)

top_words %>%
  select(month, word) %>%
  group_by(month) %>%
  mutate(words = glue_collapse(word, sep = " . ")) %>%
  select(-word) %>%
  distinct(month, .keep_all = TRUE)

top_words_long <- top_words %>%
  group_by(month) %>%
  mutate(id = 1:10) %>%
  select(month, word, id)

top_words_wide <- top_words %>%
  group_by(month) %>%
  mutate(id = 1:10) %>%
  select(month, word, id) %>%
  pivot_wider(
    names_from = id,
    values_from = word,
    names_prefix = "word_"
  )

# save processed data ----------------------------------------------------------

top_words_nested <- top_words_long %>%
  mutate(month = fct_relevel(month, month.name)) %>%
  group_by(month) %>%
  summarise(words = list(word), .groups = "drop")

write_rds(top_words_nested, file = "data/top-words-nested.rds", compress = "bz2")

top_articles <- nyt_words %>%
  mutate(date = paste(day(pub_date), month(pub_date, label = TRUE, abbr = FALSE))) %>%
select(-lead_paragraph, -pub_date, -text) %>%
  filter(
    (month == "January"   & word %in% jan_top10$word) |
    (month == "February"  & word %in% feb_top10$word) |
    (month == "March"     & word %in% mar_top10$word) |
    (month == "April"     & word %in% apr_top10$word) |
    (month == "May"       & word %in% may_top10$word) |
    (month == "June"      & word %in% jun_top10$word) |
    (month == "July"      & word %in% jul_top10$word) |
    (month == "August"    & word %in% aug_top10$word) |
    (month == "September" & word %in% sep_top10$word) |
    (month == "October"   & word %in% oct_top10$word) |
    (month == "November"  & word %in% nov_top10$word) |
    (month == "December"  & word %in% dec_top10$word)
  )

write_rds(top_articles, "data/top-articles.rds", compress = "bz2")

# monthly top 10 words ---------------------------------------------------------

nyt_top10 <- nyt_words %>%
  group_by(month) %>%
  count(word, sort = TRUE) %>%
  mutate(prop = n / sum(n)) %>%
  slice_head(n = 10) %>%
  ungroup()

ggplot(nyt_top10, aes(y = fct_reorder(word, n), x = n, fill = month)) +
  geom_col() +
  facet_wrap(~month, scales = "free") +
  guides(fill = FALSE)

ggplot(nyt_top10, aes(x = month, y = word, size = prop)) +
  geom_point()

# monthly top 5 words ----------------------------------------------------------

nyt_top5 <- nyt_words %>%
  group_by(month) %>%
  count(word, sort = TRUE) %>%
  mutate(prop = n / sum(n)) %>%
  slice_head(n = 5) %>%
  ungroup()

nyt_top5 %>%
  select(month, word) %>%
  group_by(month) %>%
  mutate(words = glue_collapse(word, sep = " . ")) %>%
  select(-word) %>%
  distinct(month, .keep_all = TRUE)

ggplot(nyt_top5, aes(x = month, y = word, size = prop)) +
  geom_point()

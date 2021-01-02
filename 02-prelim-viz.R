# load packages ----------------------------------------------------------------

library(tidyverse)
library(glue)

# read data --------------------------------------------------------------

nyt_words <- read_rds("data/nyt-words.rds")

# monthly top 10 words ---------------------------------------------------------

nyt_top10 <- nyt_words %>%
  group_by(month) %>%
  count(word, sort = TRUE) %>%
  mutate(prop = n / sum(n)) %>%
  slice_head(n = 10) %>%
  arrange(month, n) %>%
  mutate(order = row_number())

nyt_top10 %>%
  select(month, word) %>%
  group_by(month) %>%
  mutate(words = glue_collapse(word, sep = " . ")) %>%
  select(-word) %>%
  distinct(month, .keep_all = TRUE)

# barplot

ggplot(nyt_top10, aes(y = as.factor(order), x = n, fill = month)) +
  geom_col() +
  facet_wrap(~month, scales = "free") +
  guides(fill = FALSE) +
  labs(
    x = "Count",
    y = "Word",
    title = "Words that topped The New York Times headlines in 2020",
    caption = "Data: The New York Times Archive API. Plot: @iowio + @minebocek"
  ) +
  scale_y_discrete(
    breaks = nyt_top10$order,
    labels = nyt_top10$word,
    expand = c(0,0)
  )

ggsave("plots/nyt-top10-bar.png", width = 9, height = 6)

# dot plot

ggplot(nyt_top10, aes(x = month, y = fct_rev(word), size = prop)) +
  geom_point() +
  theme(legend.position = "bottom") +
  labs(
    x = NULL,
    y = NULL,
    size = "Proportion",
    title = "Words that topped The New York Times headlines in 2020",
    subtitle = "Size of the points indicate the proportion of headlines with that word in that month",
    caption = "Data: The New York Times Archive API. Plot: @iowio + @minebocek"
  )

ggsave("plots/nyt-top10-dot.png", width = 9, height = 6)

# monthly top 5 words ----------------------------------------------------------

nyt_top5 <- nyt_words %>%
  group_by(month) %>%
  count(word, sort = TRUE) %>%
  mutate(prop = n / sum(n)) %>%
  slice_head(n = 5) %>%
  arrange(month, n) %>%
  mutate(order = row_number())

nyt_top5 %>%
  select(month, word) %>%
  group_by(month) %>%
  mutate(words = glue_collapse(word, sep = " . ")) %>%
  select(-word) %>%
  distinct(month, .keep_all = TRUE)

# barplot

ggplot(nyt_top5, aes(y = as.factor(order), x = n, fill = month)) +
  geom_col() +
  facet_wrap(~month, scales = "free") +
  guides(fill = FALSE) +
  labs(
    x = "Count",
    y = "Word",
    title = "Words that topped The New York Times headlines in 2020",
    caption = "Data: The New York Times Archive API. Plot: @iowio + @minebocek"
  ) +
  scale_y_discrete(
    breaks = nyt_top10$order,
    labels = nyt_top10$word,
    expand = c(0,0)
  )

ggsave("plots/nyt-top5-bar.png", width = 9, height = 6)

# dot plot

ggplot(nyt_top5, aes(x = month, y = fct_rev(word), size = prop)) +
  geom_point() +
  theme(legend.position = "bottom") +
  labs(
    x = NULL,
    y = NULL,
    size = "Proportion",
    title = "Words that topped The New York Times headlines in 2020",
    subtitle = "Size of the points indicate the proportion of headlines with that word in that month",
    caption = "Data: The New York Times Archive API. Plot: @iowio + @minebocek"
  )

ggsave("plots/nyt-top5-dot.png", width = 9, height = 6)

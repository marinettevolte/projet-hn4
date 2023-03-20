# ----------------------------+
# Frequencies and Correlations
# in the “tidyverse”
# Marinette Volte
# ----------------------------+

# instead of install.packages() + library()
# you can use p_load from the pacman package
pacman::p_load(here, ggraph, igraph, tidytext, tidyverse, widyr, wordcloud)
source("code/corpus_functions.R")

# TERM FREQUENCIES --------------------------------------------------------

# Iliad + Iliad lemmatised ------------------------------------------------

iliad <-
  read_lines(here("data", "homer", "homer_tlg_text/iliad_tlg.txt")) |>
  str_split(regex("\\{.*?\\}", ignore_case = TRUE)) |>
  unlist() |>
  str_replace_all("\\s+", " ")
iliad_clean_df <- iliad |>
  tibble(book = 0:(length(iliad) - 1), text = iliad) |>
  filter(book > 0)
iliad_df_full <- iliad_clean_df |>
  unnest_tokens(word, text)

iliad_count <- iliad_df_full |> count(word, sort = TRUE)
iliad_count$word |> n_distinct()
iliad_count$n |> sum()

iliad_lem <-
  read_lines(here(
    "data",
    "homer",
    "homer_perseus_lem",
    "iliad_perseus_lem_books.txt"
  )) |>
  str_split(regex("Book ..?", ignore_case = TRUE)) |>
  unlist()
iliad_lem_clean_df <- iliad_lem |>
  tibble(book = 0:(length(iliad_lem) - 1), text = iliad_lem) |>
  filter(book > 0)
iliad_lem_df_full <- iliad_lem_clean_df |>
  unnest_tokens(word, text)

iliad_lem_count <- iliad_lem_df_full |> count(word, sort = TRUE)
iliad_lem_count$word |> n_distinct()
iliad_lem_count$n |> sum()

stopwords <-
  read_lines(here("data", "stopwords", "stopwords_greek_v2_8.txt"))
stopwords_df <- tibble(word = stopwords)

length(iliad_df_full$word)
iliad_df <- iliad_df_full |> anti_join(stopwords_df)
length(iliad_df$word)
iliad_df |> count(word, sort = TRUE)

iliad_df |>
  count(word, sort = TRUE) |>
  slice(1:10) |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# save last plot
ggsave(here("output", "iliad_df.png"))
ggsave(here("output", "iliad_df_2.png"),
       width = 5,
       height = 5)

mycol <- c("#0072B2", "#D55E00", "#CC79A7", "#2B9F78", "#E69F00")
# Color-blind friendly values
# #0072B2 ± blue, bleu
# #D55E00 ± dark orange/brown, orange sombre/brun
# #CC79A7 ± pink, rose
# #2B9F78 ± green, vert
# #E69F00 ± gold, or

iliad_df |>
  count(word) |>
  with(
    wordcloud(
      word,
      n,
      scale = c(5, .5),
      max.words = 50,
      rot.per = 0.2,
      random.order = F,
      colors = mycol
    )
  )

length(iliad_lem_df_full$word)
iliad_lem_df <- iliad_lem_df_full |> anti_join(stopwords_df)
length(iliad_lem_df$word)
iliad_lem_df |> count(word, sort = TRUE)

iliad_lem_df |>
  count(word, sort = TRUE) |>
  slice(1:10) |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

iliad_lem_df |>
  count(word) |>
  with(
    wordcloud(
      word,
      n,
      scale = c(5, .5),
      max.words = 50,
      rot.per = 0.2,
      random.order = F,
      ordered.colors = F,
      colors = mycol
      # try below code for sequence of colours not based on frequency
      # ordered.colors = T,
      # colors = rep_len(brewer.pal(8, "Dark2"), n_distinct(iliad_lem_df$word))
    )
  )
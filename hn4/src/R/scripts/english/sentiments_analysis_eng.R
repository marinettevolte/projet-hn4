# -------------------------------+
# Sentiment Analysis
# Marinette Volte
# -------------------------------+

pacman::p_load(here)
library("dplyr")
library(here)
library(ggplot2)

eng_negative_words <-
  scan(file=here("src", "R", "data", "english", "sentiments_lexicon", "negative_words_eng.txt"), what = "word")

eng_positive_words <-
  scan(file=here("src", "R", "data", "english", "sentiments_lexicon", "positive_words_eng.txt"), what = "word")

to_hermes <- scan(here("src", "R", "data", "english", "to_hermes_eng.txt"), what="word")
to_hermes <- gsub(',', '', to_hermes)
to_hermes <- gsub(':', '', to_hermes)
to_hermes <- gsub('\\.', '', to_hermes)
to_hermes <- tolower(to_hermes)
to_hermes

intersect_negative_w <- list()
intersect_positive_w <- list()

for (word in intersect(eng_negative_words, to_hermes)) {
  intersect_negative_w[[ word ]] <- 0
}

for (word in intersect(eng_positive_words, to_hermes)) {
  intersect_positive_w[[ word ]] <- 0
}

for (negative_word in names(intersect_negative_w)) {
  for (word_txt in to_hermes) {
    if (negative_word == word_txt) {
      intersect_negative_w[[ negative_word ]] <- intersect_negative_w[[ negative_word ]] + 1
    }
  }
}

for (positive_word in names(intersect_positive_w)) {
  for (word_txt in to_hermes) {
    if (positive_word == word_txt) {
      intersect_positive_w[[ positive_word ]] <- intersect_positive_w[[ positive_word ]] + 1
    }
  }
}

intersect_negative_w
intersect_positive_w

intersect_negative_w <- unlist(intersect_negative_w)
intersect_positive_w <- unlist(intersect_positive_w)

# Trier la liste en ordre décroissant des valeurs
intersect_negative_w_sort <- sort(intersect_negative_w, decreasing = TRUE, index.return = TRUE)
intersect_positive_w_sort <- sort(intersect_positive_w, decreasing = TRUE, index.return = TRUE)

# Convertir la liste en un dataframe
df_negative <- data.frame(noms=names(intersect_negative_w_sort$x), valeurs=intersect_negative_w_sort$x)
df_positive <- data.frame(noms=names(intersect_positive_w_sort$x), valeurs=intersect_positive_w_sort$x)

# Ggplot : top 10 positive words
ggplot(head(df_positive, 10), aes(x = reorder(noms, -valeurs), y = valeurs)) +
  geom_bar(stat="identity") +
  labs(x="Noms", y="Valeurs")
#

# Ggplot : top 10 negative words
ggplot(head(df_negative, 10), aes(x = reorder(noms, -valeurs), y = valeurs)) +
  geom_bar(stat="identity") +
  labs(x="Noms", y="Valeurs")
# sombre grotte rusé hâte a volé tromper voler courbé traîné peur faible abattre coupable dur préjudice

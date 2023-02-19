# -------------------------------+
# Sentiment Analysis
# Marinette Volte
# Version 1.1 – 2023-19-02
# -------------------------------+

pacman::p_load(here)
library(here)

install.packages("ggplot2")
library(ggplot2)

greek_negative_words <-
  scan(file=here("src", "data", "greek", "sentiments_lexicon", "negative_words.txt"), what = "word")

greek_positive_words <-
  scan(file=here("src", "data", "greek", "sentiments_lexicon", "positive_words.txt"), what = "word")

to_hermes <-
  scan(here("src", "data", "greek", "to_hermes_gr.txt"), what="word")
to_hermes <- gsub(',', '', to_hermes)
to_hermes <- gsub(':', '', to_hermes)
to_hermes <- gsub('\\.', '', to_hermes)
to_hermes <- tolower(to_hermes)
to_hermes

intersect_negative_w <- list()
intersect_positive_w <- list()

for (word in intersect(greek_negative_words, to_hermes)) {
  intersect_negative_w[[ word ]] <- 0
}
for (word in intersect(greek_positive_words, to_hermes)) {
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

# Convertir la liste en un data frame
df <- data.frame(noms=names(intersect_negative_w), valeurs=unlist(intersect_negative_w))

# Tracer un graphique à barres horizontales
ggplot(df, aes(x=valeurs, y=noms)) +
  geom_bar(stat="identity") +
  labs(x="Valeurs", y="Noms")

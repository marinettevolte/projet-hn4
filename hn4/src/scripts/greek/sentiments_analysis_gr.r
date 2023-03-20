# -------------------------------+
# Sentiment Analysis
# Marinette Volte
# -------------------------------+

# pacman::p_load(here)
library(here)

# install.packages("ggplot2")
library(ggplot2)

greek_negative_words <-
  fread(here("src", "data", "greek", "sentiments_lexicon", "negative_words_deucalion.txt"), select = c("lemma"))
greek_negative_words <- greek_negative_words$lemma
greek_negative_words <- greek_negative_words[greek_negative_words != "μοῖρα"]
greek_negative_words <- greek_negative_words[greek_negative_words != "φάος"]

greek_positive_words <-
  fread(here("src", "data", "greek", "sentiments_lexicon", "positive_words_deucalion.txt"), select = c("lemma"))
greek_positive_words <- greek_positive_words$lemma
greek_positive_words <- greek_positive_words[greek_positive_words != "αὐτός"]
greek_positive_words <- greek_positive_words[greek_positive_words != "ἔχω"]
greek_positive_words <- greek_positive_words[greek_positive_words != "ἀπό"]
                       
to_hermes <- 
  fread(here("src", "data", "greek", "to_hermes_gr_deucalion.txt"), select = c("lemma"))
to_hermes <- to_hermes$lemma
to_hermes <- gsub(',', '', to_hermes)
to_hermes <- gsub(':', '', to_hermes)
to_hermes <- gsub('\\.', '', to_hermes)
to_hermes <- gsub('\\[', "", to_hermes)
to_hermes <- gsub(']', '', to_hermes)
to_hermes <- tolower(to_hermes)
to_hermes <- to_hermes[to_hermes != ""]

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

# Convertir la liste en un data frame
df_negative <- data.frame(noms=names(intersect_negative_w), valeurs=unlist(intersect_negative_w))
df_negative_sort <- df_negative[order(-df_negative$valeurs),]
df_negative_sort
df_positive <- data.frame(noms=names(intersect_positive_w), valeurs=unlist(intersect_positive_w))
df_positive_sort <- df_positive[order(-df_positive$valeurs),]

print(paste("Nombre de mots positifs : ", length(intersect_positive_w)))
print(paste("Nombre de mots négatifs : ", length(intersect_negative_w)))

# Ggplot : top 10 positive words
ggplot(head(df_positive_sort, 10), aes(x = reorder(noms, -valeurs), y = valeurs)) +
  geom_bar(stat="identity") +
  labs(x="Noms", y="Valeurs") +
  ggtitle("Top 10 positive words")
# beaucoup/richesse exploits

# Ggplot : top 10 negative words
ggplot(head(df_negative_sort, 10), aes(x = reorder(noms, -valeurs), y = valeurs)) +
  geom_bar(stat="identity") +
  labs(x="Noms", y="Valeurs") +
  ggtitle("Top 10 negative words")
# rage grotte obéir (δαίμων le 4 on sait pas) dénoncer capturer  race / part


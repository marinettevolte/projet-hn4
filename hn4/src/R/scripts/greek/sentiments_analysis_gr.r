# -------------------------------+
# Sentiment Analysis with positive and negative words
# Marinette Volte
# -------------------------------+

# je n'ai pas pu utiliser la librairie syuzhet car celle-ci ne supporte pas le grecque
# import packages
library(ggplot2)
library(here)
library(data.table)

# read greek positive words
greek_negative_words <-
  fread(here("src", "R", "data", "greek", "sentiments_lexicon", "negative_words_deucalion.txt"),
  select = c("lemma"))
greek_negative_words <- greek_negative_words$lemma
greek_negative_words <- greek_negative_words[greek_negative_words != "μοῖρα"]
greek_negative_words <- greek_negative_words[greek_negative_words != "φάος"]

# read greek positive words
greek_positive_words <-
  fread(here("src", "R", "data", "greek", "sentiments_lexicon", "positive_words_deucalion.txt"),
  select = c("lemma"))
greek_positive_words <- greek_positive_words$lemma
greek_positive_words <- greek_positive_words[greek_positive_words != "αὐτός"]
greek_positive_words <- greek_positive_words[greek_positive_words != "ἔχω"]
greek_positive_words <- greek_positive_words[greek_positive_words != "ἀπό"]

# read to_hermes_gr_deucalion
to_hermes <-
  fread(here("src", "R", "data", "greek", "to_hermes_gr_deucalion.txt"), select = c("lemma"))
to_hermes_lemma <- to_hermes$lemma

#
to_hermes_lemma <- gsub(',', '', to_hermes_lemma)
to_hermes_lemma <- gsub(':', '', to_hermes_lemma)
to_hermes_lemma <- gsub('\\.', '', to_hermes_lemma)
to_hermes_lemma <- gsub('\\[', "", to_hermes_lemma)
to_hermes_lemma <- gsub(']', '', to_hermes_lemma)
to_hermes_lemma <- tolower(to_hermes_lemma)
to_hermes_lemma <- to_hermes_lemma[to_hermes_lemma != ""]

intersect_negative_w <- list()
intersect_positive_w <- list()

for (word in intersect(greek_negative_words, to_hermes_lemma)) {
  intersect_negative_w[[ word ]] <- 0
}

for (word in intersect(greek_positive_words, to_hermes_lemma)) {
  intersect_positive_w[[ word ]] <- 0
}

# boucle qui parcourt l'ensembe des mots négatifs/positifs qui sont
# présents dans to_hermes_lemma et dans intersect_negative_w
for (negative_word in names(intersect_negative_w)) {
  for (word_txt in to_hermes_lemma) {
    if (negative_word == word_txt) {
      intersect_negative_w[[ negative_word ]] <- intersect_negative_w[[ negative_word ]] + 1
    }
  }
}

for (positive_word in names(intersect_positive_w)) {
  for (word_txt in to_hermes_lemma) {
    if (positive_word == word_txt) {
      intersect_positive_w[[ positive_word ]] <- intersect_positive_w[[ positive_word ]] + 1
    }
  }
}

# convertir la liste en un data frame
df_negative <- data.frame(noms=names(intersect_negative_w), valeurs=unlist(intersect_negative_w))
df_negative_sort <- df_negative[order(-df_negative$valeurs),]
df_negative_sort
df_positive <- data.frame(noms=names(intersect_positive_w), valeurs=unlist(intersect_positive_w))
df_positive_sort <- df_positive[order(-df_positive$valeurs),]

print(paste("Nombre de mots positifs : ", length(intersect_positive_w)))
print(paste("Nombre de mots négatifs : ", length(intersect_negative_w)))

# ggplot: top 10 positive words
ggplot(head(df_positive_sort, 10), aes(x = reorder(noms, -valeurs), y = valeurs)) +
  geom_bar(stat="identity") +
  labs(x="Mots", y="Nombre d'occurences") +
  ggtitle("Top 10 mots positifs")
# beaucoup/richesse exploits

# ggplot: top 10 negative words
ggplot(head(df_negative_sort, 10), aes(x = reorder(noms, -valeurs), y = valeurs)) +
  geom_bar(stat="identity") +
  labs(x="Mots", y="Nombre d'doccurences") +
  ggtitle("Top 10 mots négatifs")
# rage grotte obéir (δαίμων le 4 on sait pas) dénoncer capturer  race / part


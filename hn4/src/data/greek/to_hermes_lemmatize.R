library(here)
library(data.table)

greek_lemma <- fread(here("src", "data", "greek", "to_hermes_gr_deucalion.txt"), select = c("lemma"))
print(greek_lemma$lemma)

greek_lemma <-
  scan(file=here("src", "data", "greek", "to_hermes_gr_deucalion.txt"), what = "word")
greek_lemma

# to_hermes <-
#  scan(here("src", "data", "greek", "to_hermes_gr.txt"), what="word")

# greek_negative_words <-
#  scan(file=here("src", "data", "greek", "sentiments_lexicon", "negative_words_gr.txt"), what = "word")
library(rvest); library(robotstxt); library(dplyr); library(tidyr); library(stringr)
library(tidytext); library(textstem); library(ggplot2); library(igraph); library(ggraph)

urls <- c(
  "https://en.wikipedia.org/wiki/Natural_language_processing",
  "https://en.wikipedia.org/wiki/Large_language_model",
  "https://en.wikipedia.org/wiki/Text_mining",
  "https://en.wikipedia.org/wiki/Automatic_summarization"
)

my_agent <- "Student text mining project (R rvest) - contact: andrew.ler23@gmail.com"
dir.create("plots", showWarnings = FALSE)

#  3.1  
allowed <- paths_allowed(urls)
print(data.frame(url = urls, allowed_by_robots_txt = allowed))
if (!all(allowed)) stop("robots.txt does not allow one of these pages - choose another page.")

skip_sections <- c("See also", "References", "Notes", "Further reading", "External links",
                   "Sources", "Citations", "Bibliography")

scrape_page <- function(url) {
  Sys.sleep(2)
  page  <- session(url, httr::user_agent(my_agent))
  title <- html_text2(html_element(page, "h1"))
  nodes <- html_elements(page, xpath = paste0(
    "//div[contains(@class,'mw-parser-output')]//*[self::p or self::h2 or self::h3]",
    "[not(ancestor::table)][not(ancestor::*[contains(@class,'navbox') or contains(@class,'reflist')])]"))
  
  section <- "Introduction"; skipping <- FALSE; rows <- list()
  for (node in nodes) {
    tag  <- html_name(node)
    text <- str_squish(html_text2(node))
    if (tag == "h2") {
      section  <- text
      skipping <- section %in% skip_sections
    } else if (tag == "h3") {
      if (!skipping) section <- text
    } else if (!skipping) {
      rows[[length(rows) + 1]] <- data.frame(section = section, raw_text = html_text2(node))
    }
  }
  data.frame(article = title, url = url, bind_rows(rows))
}

web_df <- bind_rows(lapply(urls, scrape_page)) %>%
  filter(str_squish(raw_text) != "") %>%
  mutate(paragraph_id = row_number(), .after = section)

str(web_df)
write.csv(web_df, "scraped_web_text.csv", row.names = FALSE)

#  3.2  TEXT PREPROCESSING
web_df <- web_df %>%
  mutate(clean_text = raw_text %>%
           str_replace_all("\\[[^\\]]*\\]", " ") %>%
           str_replace_all("\\S*[\\\\{}]\\S*", " ") %>%
           str_replace_all("\\(\\s*\\)", " ") %>%
           iconv(from = "UTF-8", to = "ASCII//TRANSLIT", sub = " ") %>%
           str_squish()) %>%
  filter(str_length(clean_text) >= 60) %>%
  distinct(clean_text, .keep_all = TRUE)

custom_stop <- c("also", "use", "used", "using", "can", "may", "one", "two", "first",
                 "however", "e.g", "i.e", "etc", "include", "including", "example")
lemma_fix <- c(datum = "data", mine = "mining", llms = "llm")
words_df <- web_df %>%
  unnest_tokens(word, clean_text) %>%
  filter(!str_detect(word, "[^a-z]")) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = lemmatize_words(word),
         word = ifelse(word %in% names(lemma_fix), lemma_fix[word], word)) %>%
  filter(!word %in% custom_stop, str_length(word) >= 3)

cat("\nAfter preprocessing:", nrow(web_df), "paragraphs,", nrow(words_df), "words,",
    n_distinct(words_df$word), "unique words\n")



#  3.3  TEXT SUMMARISATION
#Technique 1: keyword summary
top_words <- words_df %>% count(word, sort = TRUE) %>% slice_head(n = 15)

tfidf <- words_df %>% count(article, word) %>%
  bind_tf_idf(word, article, n) %>%
  group_by(article) %>% slice_max(tf_idf, n = 8, with_ties = FALSE) %>% ungroup()

bigrams <- web_df %>%
  unnest_tokens(bigram, clean_text, token = "ngrams", n = 2) %>%
  separate(bigram, c("w1", "w2"), sep = " ") %>%
  filter(!w1 %in% stop_words$word, !w2 %in% stop_words$word,
         !str_detect(w1, "[^a-z]"), !str_detect(w2, "[^a-z]")) %>%
  count(w1, w2, sort = TRUE)

sentences <- web_df %>%
  unnest_tokens(sentence, clean_text, token = "sentences", to_lower = FALSE) %>%
  filter(str_count(sentence, "\\S+") >= 8) %>%                   
  group_by(article) %>% mutate(sentence_id = row_number()) %>% ungroup()

sentence_words <- sentences %>%
  unnest_tokens(word, sentence, drop = FALSE) %>%
  filter(!str_detect(word, "[^a-z]")) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = lemmatize_words(word),
         word = ifelse(word %in% names(lemma_fix), lemma_fix[word], word)) %>%
  filter(!word %in% custom_stop, str_length(word) >= 3)

n_summary <- 4

#Technique 2: frequency-based extractive summary
word_freq <- sentence_words %>% count(article, word, name = "freq")
freq_scores <- sentence_words %>%
  left_join(word_freq, by = c("article", "word")) %>%
  group_by(article, sentence_id) %>%
  summarise(freq_score = mean(freq), .groups = "drop")

freq_summary <- sentences %>%
  inner_join(freq_scores, by = c("article", "sentence_id")) %>%
  group_by(article) %>% slice_max(freq_score, n = n_summary, with_ties = FALSE) %>%
  arrange(article, sentence_id) %>% ungroup()

#Technique 3: Text rank extractive summary
textrank_article <- function(a) {
  sw   <- sentence_words %>% filter(article == a) %>% distinct(sentence_id, word)
  ids  <- sort(unique(sw$sentence_id))
  sets <- split(sw$word, sw$sentence_id)
  pairs <- t(combn(ids, 2))
  w <- apply(pairs, 1, function(p) {
    s1 <- sets[[as.character(p[1])]]; s2 <- sets[[as.character(p[2])]]
    length(intersect(s1, s2)) / (log(length(s1) + 1) + log(length(s2) + 1))
  })
  edges <- data.frame(from = pairs[, 1], to = pairs[, 2], weight = w) %>% filter(weight > 0)
  g <- graph_from_data_frame(edges, directed = FALSE, vertices = data.frame(name = ids))
  data.frame(article = a, sentence_id = ids,
             textrank = as.numeric(page_rank(g, weights = E(g)$weight)$vector))
}
tr_scores <- bind_rows(lapply(unique(sentences$article), textrank_article))

textrank_summary <- sentences %>%
  inner_join(tr_scores, by = c("article", "sentence_id")) %>%
  group_by(article) %>% slice_max(textrank, n = n_summary, with_ties = FALSE) %>%
  arrange(article, sentence_id) %>% ungroup()

for (a in unique(sentences$article)) {
  cat("Frequency-based summary:\n")
  cat(paste0(" - ", freq_summary$sentence[freq_summary$article == a]), sep = "\n")
  cat("TextRank summary:\n")
  cat(paste0(" - ", textrank_summary$sentence[textrank_summary$article == a]), sep = "\n")
}

comparison <- sentences %>% group_by(article) %>%
  summarise(sentences_in_article = n(),
            words_in_article = sum(str_count(sentence, "\\S+"))) %>%
  left_join(textrank_summary %>% group_by(article) %>%
              summarise(words_in_summary = sum(str_count(sentence, "\\S+"))), by = "article") %>%
  left_join(inner_join(freq_summary, textrank_summary, by = c("article", "sentence_id")) %>%
              count(article, name = "sentences_chosen_by_both"), by = "article") %>%
  mutate(compression = sprintf("%.0f%%", 100 * words_in_summary / words_in_article),
         sentences_chosen_by_both = coalesce(sentences_chosen_by_both, 0L))
print(as.data.frame(comparison))


#  3.3
# Figure 1: most frequent words across all articles
f1 <- ggplot(top_words, aes(n, reorder(word, n))) +
  geom_col(fill = "#2f5d86") +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  labs(title = "Figure 1: Top 15 words across the four scraped articles",
       subtitle = "After stop-word removal and lemmatisation",
       x = "Number of occurrences", y = "Word", caption = "Source: Wikipedia (CC BY-SA)") +
  theme_minimal(base_size = 12)
print(f1); ggsave("plots/1_top_words.png", f1, width = 8, height = 6, dpi = 150)

# Figure 2: words that characterise each article (TF-IDF)
f2 <- ggplot(tfidf, aes(tf_idf, reorder_within(word, tf_idf, article), fill = article)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ article, scales = "free") +
  scale_y_reordered() +
  scale_x_continuous(n.breaks = 4, expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Figure 2: Most distinctive words per article (TF-IDF)",
       subtitle = "Higher TF-IDF = frequent in this article but rare in the others",
       x = "TF-IDF score", y = "Word", caption = "Source: Wikipedia (CC BY-SA)") +
  theme_minimal(base_size = 12)
print(f2); ggsave("plots/2_tfidf_per_article.png", f2, width = 11, height = 7, dpi = 150)

# Figure 3: network of the most common word pairs
set.seed(2026)
bigram_graph <- graph_from_data_frame(head(bigrams, 40))
f3 <- ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_width = n), edge_colour = "grey60", alpha = 0.7) +
  geom_node_point(colour = "#e07a2e", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3.5) +
  scale_edge_width(range = c(0.4, 2.5), name = "Times the pair occurs") +
  labs(title = "Figure 3: Network of the 40 most common word pairs (bigrams)",
       subtitle = "Linked words appear next to each other in the text",
       caption = "Source: Wikipedia (CC BY-SA)") +
  theme_void(base_size = 12)
print(f3); ggsave("plots/3_bigram_network.png", f3, width = 10, height = 7, dpi = 150)

# Figure 4: TextRank score of every sentence, with the selected summary sentences marked
tr_plot <- tr_scores %>%
  left_join(textrank_summary %>% transmute(article, sentence_id, selected = "In summary"),
            by = c("article", "sentence_id")) %>%
  mutate(selected = coalesce(selected, "Not selected"))
f4 <- ggplot(tr_plot, aes(sentence_id, textrank, colour = selected)) +
  geom_point(size = 2) +
  facet_wrap(~ article, scales = "free") +
  scale_colour_manual(values = c("In summary" = "#e07a2e", "Not selected" = "grey65")) +
  labs(title = "Figure 4: TextRank importance of each sentence",
       subtitle = paste("The", n_summary, "highest-scoring sentences per article form the summary"),
       x = "Sentence position in article", y = "TextRank score", colour = NULL,
       caption = "Source: Wikipedia (CC BY-SA)") +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
print(f4); ggsave("plots/4_textrank_scores.png", f4, width = 11, height = 7, dpi = 150)
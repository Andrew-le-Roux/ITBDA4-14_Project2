library(tm); library(topicmodels); library(tidytext); library(dplyr); library(tidyr)
library(ggplot2); library(wordcloud); library(RColorBrewer); library(textstem); library(stringr)

news_dir <- "news/sa_news"   
n_topics <- 6                
top_k    <- 10               
set.seed(1234)
dir.create("plots", showWarnings = FALSE)


#  2.1  DATA FRAME AND PREPROCESSING 

files <- list.files(news_dir, pattern = "\\.txt$", full.names = TRUE)
files <- files[order(as.numeric(str_extract(basename(files), "\\d+")))]

news_df <- data.frame(
  file_name = basename(files),
  content   = sapply(files, function(f) paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = " ")),
  row.names = NULL
)

noise_words <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday",
                 "sunday", "january", "february", "march", "april", "may", "june",
                 "july", "august", "september", "october", "november", "december",
                 "sanews", "gov", "say", "said", "also", "will", "can", "one", "two",
                 "per", "new", "must", "well", "year", "mr", "ms", "minister", "make",
                 "need", "ensure", "include", "take", "however", "many", "much",
                 "first", "last", "time", "like", "get", "just", "come", "part")

denoise <- function(text) {
  text <- iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT", sub = " ")  
  text <- tolower(text)                                                   
  text <- str_replace_all(text, "[^a-z ]", " ")                          
  text <- removeWords(text, stopwords("english"))                       
  text <- lemmatize_strings(text)                                        
  text <- removeWords(text, c(stopwords("english"), noise_words))        
  text <- str_replace_all(text, "\\b[a-z]{1,2}\\b", " ")                  
  str_squish(text)                                                       
}
news_df$content <- sapply(news_df$content, denoise, USE.NAMES = FALSE)
print(data.frame(file_name = news_df$file_name, content = substr(news_df$content, 1, 60)))


#  2.2  LDA

dtm <- DocumentTermMatrix(VCorpus(VectorSource(news_df$content)), control = list(wordLengths = c(3, Inf)))
m   <- as.matrix(dtm)
dtm <- dtm[, colSums(m) >= 2 & colSums(m > 0) < 0.6 * nrow(dtm)]  
rownames(dtm) <- news_df$file_name

lda_control <- list(seed = 1234, burnin = 1000, iter = 2000, thin = 100, alpha = 0.1)

k_check <- data.frame(topics = 2:8)
k_check$log_likelihood <- sapply(k_check$topics, function(k)
  as.numeric(logLik(LDA(dtm, k = k, method = "Gibbs", control = lda_control))))
print(k_check)

lda_model   <- LDA(dtm, k = n_topics, method = "Gibbs", control = lda_control)
topic_words <- tidy(lda_model, matrix = "beta")   
doc_topics  <- tidy(lda_model, matrix = "gamma")   

top_terms <- topic_words %>% group_by(topic) %>%
  slice_max(beta, n = top_k, with_ties = FALSE) %>%
  mutate(rank = row_number()) %>% ungroup()

print(as.data.frame(top_terms %>%
                      mutate(cell = sprintf("%s (%.3f)", term, beta)) %>%
                      select(rank, topic, cell) %>%
                      pivot_wider(names_from = topic, values_from = cell, names_prefix = "Topic ")), row.names = FALSE)

print(top_terms %>% group_by(topic) %>% summarise(top_k_share = sum(beta)))  

print(as.data.frame(doc_topics %>% mutate(gamma = round(gamma, 3)) %>%
                      pivot_wider(names_from = topic, values_from = gamma, names_prefix = "Topic ")), row.names = FALSE)


#  2.3  VISUALISATION

top_terms <- top_terms %>% group_by(topic) %>%
  mutate(label = paste0("Topic ", topic, ": ", paste(head(term, 3), collapse = ", "))) %>% ungroup()

# Method 1: bar chart
p1 <- ggplot(top_terms, aes(beta, reorder_within(term, beta, topic), fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ label, scales = "free_y") +
  scale_y_reordered() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = paste("Top", top_k, "words per topic (LDA)"), x = "Word probability in topic (beta)", y = NULL) +
  theme_minimal(base_size = 12)
print(p1)
ggsave("plots/1_top_words_bar_chart.png", p1, width = 11, height = 7, dpi = 150)

# Method 2: word clouds
png("plots/2_word_clouds.png", width = 1500, height = 1050, res = 130)
par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
for (t in 1:n_topics) {
  tw <- topic_words %>% filter(topic == t) %>% slice_max(beta, n = 20, with_ties = FALSE)
  wordcloud(tw$term, tw$beta, scale = c(3, 0.8), random.order = FALSE, rot.per = 0,
            colors = brewer.pal(8, "Dark2"))
  title(unique(top_terms$label[top_terms$topic == t]))
}
dev.off()

# Method 3: heatmap of the top 5 words of each topic
heat_order <- top_terms %>% filter(rank <= 5) %>% distinct(term, .keep_all = TRUE)
heat_data  <- topic_words %>% filter(term %in% heat_order$term) %>%
  mutate(term = factor(term, levels = rev(heat_order$term)))
p3 <- ggplot(heat_data, aes(factor(topic), term, fill = beta)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = ifelse(beta >= 0.01, sprintf("%.3f", beta), "")), size = 3) +
  scale_fill_gradient(low = "#f7fbff", high = "#2171b5") +
  labs(title = "Top words and their probability in every topic", x = "Topic", y = NULL) +
  theme_minimal(base_size = 11)
print(p3)
ggsave("plots/3_top_words_heatmap.png", p3, width = 7, height = 8, dpi = 150)
# 0) Setup ----
library(tidyverse)
library(rvest)
library(janitor)
library(stringr)
library(readr)
library(lubridate)
library(forcats)
library(scales)

Sys.setenv(lang = "en_US")
options(scipen = 999)
theme_set(new = theme_light())

# 0) Load data ----
readRDS(file = "E:/The Data Digest/GitHub/Skool/input/skool1000.RDS")
skool1000_BU <- skool1000
View(skool1000 %>% select(ranking, community, meta, description, background_image_url))
skool1000 <- skool1000 %>% 
  select(-c(members_cleaned, language_textcat, language_cld2, language_cld3))

View(skool1000 %>% select(ranking, community, meta, privacy, access, access2))

# 1) Access ----
access_res <- skool1000 %>% count(privacy, access2) %>% 
  pivot_wider(names_from = privacy, values_from = n) %>% 
  mutate(Total = Private + Public)

col_total <- colSums(access_res[-1])
access_res <- bind_rows(access_res, col_total)
access_res[4,1] <- "Total"

access_res

# 2) Member analysis ----

skool1000 %>% 
  arrange(desc(members_n)) %>% 
  select(ranking, community, members_n, privacy, access, description) %>% 
  View()

skool1000 %>% group_by(access2) %>% 
  summarize(min = min(members_n),
            q25_Q1 = quantile(members_n, probs = 0.25),
            median = median(members_n),
            mean = mean(members_n),
            q75_Q3 = quantile(members_n, probs = 0.75), 
            max = max(members_n),
            N = n())

skool1000 %>% ggplot(aes(x = members_n, y = access2, color = access2, fill = access2)) +
  geom_boxplot(color = "black", alpha = 0.3, outlier.shape = NA) +
  geom_jitter(alpha = 0.8) +
  scale_x_log10() + 
  theme(legend.position = "none")


# 3) Price analysis ----

skool1000 %>% arrange(desc(price)) %>% 
  filter(!is.na(price)) %>% 
  select(ranking, community, price, privacy, description) %>% View()

skool1000 %>% 
  filter(!is.na(price)) %>% 
  summarize(min = min(price),
            q25_Q1 = quantile(price, probs = 0.25),
            median = median(price),
            mean = round(mean(price),1),
            q75_Q3 = quantile(price, probs = 0.75), 
            max = max(price),
            N = n())


skool1000 %>% 
  filter(!is.na(price)) %>% 
  summarize('below $10' = sum(price < 10),
            '$10-$29' = sum(price >= 10 & price < 30),
            '$30-$49' = sum(price >= 30 & price < 50),
            '$50-$79' = sum(price >= 50 & price < 80),
            '$80-$99' = sum(price >= 80 & price < 100),
            '$100-$499' = sum(price >= 100 & price < 500),
            '$500-$999' = sum(price >= 500 & price < 1000))

skool1000_price$price_category <- factor(skool1000_price$price_category, ordered = TRUE, 
                                         levels = c('below $10', '$10-$29', '$30-$49', '$50-$79', '$80-$99','$100-$499' ,'$500-$999'))
View(skool1000_price %>% arrange(price))

skool1000_price %>% 
  count(price_category) %>% 
  mutate(percent = n/sum(n)) %>% 
  ggplot(aes(x = price_category, y = percent, fill = price_category)) +
  geom_col(color = "black") +
  scale_y_continuous(name = "Percent",
                     breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3), 
                     labels = scales::percent(c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3)))  +
  theme(legend.position = "none") +
  labs(title = "Membership price in USD", subtitle = "of the Top 1000 skool commmunities (8.8.2024)", x = "", y = "",
       caption = "Source: https://www.skool.com/discovery")



# 4) Scatterplot ----
library(plotly)

skool1000_price$hover_label <- paste0(skool1000_price$community,"\nMembers: ",skool1000_price$members_n, "\nPrice: $", skool1000_price$price, "\nRank: ", skool1000_price$ranking)

g1 <- skool1000_price %>% 
  filter(!is.na(price)) %>% 
  ggplot(aes(x = members_n, y = price, text = hover_label, color = privacy)) +
  geom_point(size = 1.5, alpha = 0.8) +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  labs(x = "# of members (log10)", y="Price in USD (log10)", subtitle = "Top 1000 communities (8.8.2024)") +
  annotation_logticks()  +
  theme_bw()

# Customize the hover label appearance
p_interactive <- ggplotly(g1, tooltip = "text") %>%
  layout(
    hoverlabel = list(
      bgcolor = "lightyellow",  
      font = list(family = "Arial", size = 16, color = "black")  
    ) )

# Display the interactive plot
p_interactive


# 5) Languages ----
skool1000 %>% count(language_long, 
                    sort = TRUE)

library(forcats)
skool1000 %>% filter(language_long != "English") %>% 
  count(language_long, sort = TRUE) %>% 
  mutate(language_long = fct_reorder(language_long, n)) %>% 
  ggplot(aes(x = n, y = language_long, fill = language_long, label = n)) +
  geom_col(show.legend = F) +
  scale_x_continuous(limits = c(0, 65), expand = c(0, 0)) +
  geom_text(nudge_x = 1.5, size = 3.5) +
  labs(x = "", y = "", title = "Description language of the top 1000 skool communities",
       subtitle = "824 communities were in English (not shown in the chart)") +
  theme(panel.grid = element_blank())


# 6) Wordcloud -----
# Creating a word cloud in R is a great way to visually represent the most common words in a text column, such as the "Community" column in your data frame. You can use the `wordcloud` package along with `tm` (text mining) and `dplyr` for preprocessing the text data. Here’s how you can do it:

### Step-by-Step Guide to Create a Word Cloud

## 1. **Install and Load Required Packages**: If you haven’t installed these packages yet, do so, and then load them.

# Install necessary packages if not already installed
install.packages(c("wordcloud", "tm", "RColorBrewer"))

# Load the packages
library(wordcloud)
library(tm)
library(RColorBrewer)

# 3. **Text Preprocessing**: Clean and prepare the text data for the word cloud.

# Combine all community names into a single text
english_text <- skool1000 %>% filter(language_long == "English") %>% pull(community)
# non_english_text <- skool_df %>% filter(language_comm != "english") %>% pull(community)
# 
# skool_df %>% filter(language_comm == "english") %>% pull(description)
# skool_df %>% filter(language_comm != "english") %>% pull(description)
# 
# 
text <- paste(skool1000$community, collapse = " ")
text <- paste(english_text, collapse = " ")

# Create a text corpus
corpus <- Corpus(VectorSource(text))

# Preprocess the text: convert to lower case, remove punctuation, numbers, and stopwords
corpus_clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace)

# 4. **Create a Term Document Matrix**: Convert the corpus into a matrix format suitable for the word cloud.

# Create a Term Document Matrix
tdm <- TermDocumentMatrix(corpus_clean)

# Convert to a matrix
m <- as.matrix(tdm)

# Sum the term frequencies
word_freqs <- sort(rowSums(m), decreasing = TRUE)

# Create a data frame with words and their frequencies
word_freqs_df <- data.frame(word = names(word_freqs), freq = word_freqs)
View(head(word_freqs_df, n = 100))

## 5. **Generate the Word Cloud**:

# Set the color palette
palette <- brewer.pal(8, "Dark2")

# Plot the word cloud
wordcloud(
  words = word_freqs_df$word,
  freq = word_freqs_df$freq,
  min.freq = 4,
  scale = c(3, 0.5),
  colors = palette,
  random.order = FALSE
)


# 7) Emojies ----
skool1000 %>% filter(comm_emoji) %>% 
  select(id, community, meta) %>% View()

nchar(skool1000$description) %>% hist()
nchar(skool1000$description) %>% median()
nchar(skool1000$description) %>% mean()
nchar(skool1000$description) %>% summary()






## Thumbnail
p <- skool1000_price %>% 
  filter(!is.na(price)) %>% 
  ggplot(aes(x = members_n, y = price, text = hover_label)) +
  geom_point(size = 1.2, alpha = 0.7, color = "black") +
  scale_x_log10() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x="", y="")



#The fastest way to modify the plot above to have a completely transparent background is to set theme()'s rect argument, as all the rectangle elements inherit from rect:

p <- p + theme(panel.background = element_rect(fill = "transparent",
                                               colour = NA_character_),
               plot.background = element_rect(fill = "transparent",
                                              colour = NA_character_),
               legend.box.background = element_rect(fill = "transparent"),
               axis.text.x = element_text(colour = "white"),
               axis.text.y = element_text(colour = "white"))

ggsave(plot = p,filename = "test.png",  bg = "transparent", width = 10, height = 6, units = "cm", dpi = 300)



#A more controlled way is to set theme()'s more specific arguments individually:

p <- p + theme(
  panel.background = element_rect(fill = "transparent",
                                  colour = NA_character_), # necessary to avoid drawing panel outline
  panel.grid.major = element_blank(), # get rid of major grid
  panel.grid.minor = element_blank(), # get rid of minor grid
  plot.background = element_rect(fill = "transparent",
                                 colour = NA_character_), # necessary to avoid drawing plot outline
  legend.background = element_rect(fill = "transparent"),
  legend.box.background = element_rect(fill = "transparent"),
  legend.key = element_rect(fill = "transparent")
)

p
# ggsave() offers a dedicated argument bg to set the
# 
# Background colour. If NULL, uses the plot.background fill value from the plot theme.
# 
# To write a ggplot object p to filename on disk using a transparent background:
#   #
  
  ggsave(plot = p,filename = "test.png",  bg = "transparent")



























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

# 1) Load data (web scraping) ----

# Define the base URL
base_url <- "https://www.skool.com/discovery?p="
search_term <- "Academy" # ACADEMY, Academia, etc.
search_term <- "Community"
search_term <- "Club"
search_term <- "Tribe"
search_term <- "Gang"
search_term <- "School"
search_term <- "Group"
search_term <- "Circle" # or inner circle

# Initialize empty lists to store the data
rankings <- list()
community_names <- list()
descriptions <- list()
meta_info <- list()
background_image_urls <- list()

# Loop through the pages 
for (page_num in 1:34) {
  # Construct the URL for the current page
  url <- paste0(base_url, page_num)
  # url <- paste0(base_url, page_num, paste0("&q=", search_term))
  
  # Read the HTML content of the webpage
  temp_page <- read_html(url)
  
  # Extract the community name
  community_names[[page_num]] <- temp_page %>%
    html_nodes('.styled__TypographyWrapper-sc-m28jfn-0.eoHmvk') %>%
    html_text()
  
  # Extract the ranking
  rankings[[page_num]] <- temp_page %>%
    html_nodes('.styled__DiscoveryCardRanking-sc-13ysp3k-6.egeJZg') %>%
    html_text()
  
  # Extract the description
  descriptions[[page_num]] <- temp_page %>%
    html_nodes('.styled__DiscoveryCardDescription-sc-13ysp3k-5.dCJqtG') %>%
    html_text()
  
  # Extract the meta information (Private/Public, Members, Paid/Free)
  meta_info[[page_num]] <- temp_page %>%
    html_nodes('.styled__DiscoveryCardMeta-sc-13ysp3k-7.jjNZwk') %>%
    html_text()
  
  # Extract the image URLs and handle missing values
  image_elements <- temp_page %>%
    html_nodes('.styled__AvatarWrapper-sc-140v536-0.hDDweh img, .styled__AvatarWrapper-sc-140v536-0.hDDweh')
  
  # Extract the background image URLs and handle missing values
  bg_image_elements <- temp_page %>%
    html_nodes('.styled__DiscoveryCardCover-sc-13ysp3k-2.hSDuvz, .styled__DiscoveryCardCoverPlaceholder-sc-13ysp3k-3.kXNGWM')
  
  background_image_urls[[page_num]] <- sapply(bg_image_elements, function(node) {
    style_attr <- html_attr(node, 'style')
    if (!is.null(style_attr)) {
      url <- str_extract(style_attr, 'url\\((.*?)\\)')
      if (!is.na(url)) {
        str_replace_all(url, 'url\\(|\\)|"|&quot;', '')
      } else {
        NA
      }
    } else {
      NA
    }
  })
}



# Combine the lists into a single data frame
data <- data.frame(
  Ranking = unlist(rankings),
  Community = unlist(community_names),
  Description = unlist(descriptions),
  Meta = unlist(meta_info),
  Background_Image_URL = unlist(background_image_urls),
  stringsAsFactors = FALSE
)

# Display the extracted data
#View(data)
#str(data)


# 2) Data cleaning and manipulation ----
skool_df <- clean_names(data)

skool_df <- skool_df %>% mutate(
  id = parse_number(ranking),
  comm_emoji = str_detect(community, "[^\x20-\x7E]"),
  desc_emoji = str_detect(description, "[^\x20-\x7E]"),
)

# Split the "meta" column into "privacy", "members", and "access"
skool_df <- skool_df %>%
  separate(meta, into = c("privacy", "members", "access"), sep = "  •  ", remove = FALSE)


# Create a new column "Numeric_Members" from "Members"
skool_df <- skool_df %>%
  mutate(
    members_cleaned = str_remove(members, " Members"),
    members_n = case_when(
      str_detect(members_cleaned, "k") ~ as.numeric(str_remove(members_cleaned, "k")) * 1000,
      TRUE ~ as.numeric(members_cleaned)),
    price = parse_number(access))

skool_df$access2 <- skool_df$access
skool_df$access2[!is.na(skool_df$price)] <- "Paid_USD"

View(skool_df)

## 2b) language detection ----

# Install textcat if you haven't already
# Load the package
library(textcat)
library(cld2)
library(cld3)

skool_df$language_textcat = textcat(skool_df$description)
skool_df$language_cld2 = cld2::detect_language(skool_df$description)
skool_df$language_cld3 = cld3::detect_language(skool_df$description)

# compare results (cld2 and cld3 differences)
skool_df %>% filter(language_cld2 != language_cld3) %>% 
  select(id, community, contains("language"), description) %>% View()

# if one is NA then use the info from the other
skool_df$language[is.na(skool_df$language_cld2) & !is.na(skool_df$language_cld3)] <- skool_df$language_cld3[is.na(skool_df$language_cld2) & !is.na(skool_df$language_cld3)]   # 19 cases

skool_df$language[is.na(skool_df$language_cld3) & !is.na(skool_df$language_cld2)] <- skool_df$language_cld2[is.na(skool_df$language_cld3) & !is.na(skool_df$language_cld2)]   # 46 cases

skool_df %>% 
  select(id, community, contains("language"), description) %>% arrange(language) %>% View()

# manual checks
# manual language corrections
skool_df$language <- skool_df$language_cld3
skool_df$language[skool_df$id %in% c(293,785,834,960)] <- "zh"
skool_df$language[skool_df$id %in% c(13,250,254,401,431,518,638,701,890,925,315,852,342)] <- "en"
skool_df$language[skool_df$id %in% c(11,518)] <- "es"
skool_df$language[skool_df$id %in% c(787,39)] <- "de"
skool_df$language[skool_df$language_cld3 == "ja" & !is.na(skool_df$language_cld3 == "ja")] <- "en"
skool_df$language[skool_df$id %in% c(209,956)] <- "zh"

View(skool_df)

skool_df <- skool_df %>% mutate(
  language_long = case_when(
      language == "en" ~ "English",
      language == "ar" ~ "Arabic",
      language == "de" ~ "German",
      language == "es" ~ "Spanish",
      language == "fr" ~ "French",
      language == "bg" ~ "Bulgarian",
      language == "cs" ~ "Czech",
      language == "fa" ~ "Farsi",
      language == "mn" ~ "Mongolian",
      language == "pl" ~ "Polish",
      language == "pt" ~ "Portuguese",
      language == "lt" ~ "Lithuanian",
      language == "it" ~ "Italian",
      language == "iw" ~ "Hebrew",
      language == "hr" ~ "Croatian",
      language == "ht" ~ "Haitian creole",
      language == "hu" ~ "Hungarian",
      language == "sk" ~ "Slovak",
      language == "sv" ~ "Swedish",
      language == "th" ~ "Thai",
      language == "tr" ~ "Turkish",
      language == "vi" ~ "Vietnamese",
      language == "zh" ~ "Chinese",
      .default = as.character(language)
    )
  )

# 3) Save objects ----

# save.image(file = "euro2024.RData")
# Save an object to a file
skool1000 <- skool_df
saveRDS(object = skool1000, file = "E:/The Data Digest/GitHub/Skool/input/skool1000.RDS")




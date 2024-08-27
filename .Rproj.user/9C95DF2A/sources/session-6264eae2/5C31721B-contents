
# Helper functions ----
# Function to find the max page number
get_max_page <- function(url) {
  page <- read_html(url)
  
  # Extract pagination numbers
  pagination_numbers <- page %>%
    html_nodes(".styled__DesktopPaginationControls-sc-4zz1jl-1 .styled__ButtonWrapper-sc-dscagy-1 span") %>%
    html_text(trim = TRUE) %>%
    as.numeric()
  
  # Find the maximum number, ignoring any non-numeric entries (e.g., '...') and NA values
  max_page <- max(pagination_numbers, na.rm = TRUE)
  
  return(max_page)
}


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
# Initialize empty df to bind_rows() the different search results
search_df <- data.frame(
  Ranking = NA,
  Community = NA,
  Meta = NA,
  Search = NA,
  stringsAsFactors = FALSE
) 

#xx <- readRDS(file = "E:/The Data Digest/GitHub/Skool/input/old/search_community_9_2024-08-14.RDS")

# Define the base URL
base_url <- "https://www.skool.com/discovery"
search_term <- "academy" # ACADEMY, Academia, etc.

# search_terms <- c("academy", "community")

#search_term <- c("academy")

# saveRDS(object = temp_data, 
#         file = paste0("E:/The Data Digest/GitHub/Skool/input/search_community_1_", search_term, today(), ".RDS"))
#academy <- readRDS("E:/The Data Digest/GitHub/Skool/input/search_community_1_academy2024-08-14.RDS")

search_df <- bind_rows(search_df, temp_data)

dim(search_df)
saveRDS(object = search_df, 
        file = paste0("E:/The Data Digest/GitHub/Skool/input/search_community_44_", today(), ".RDS"))
#search_df <- readRDS(file = "E:/The Data Digest/GitHub/Skool/input/search_community_10_2024-08-14.RDS")

(search_term <- search_terms_community[7])
# # Start search term loop
# for(term in search_terms){
#     
#     temp_search_term <- term
#     
    # Initialize empty lists to store the data
    rankings <- list()
    community_names <- list()
    meta_info <- list()

    # build url
    search_url <- paste0("https://www.skool.com/discovery?q=", search_term)
      
    # extract last page number
    max_page <- get_max_page(url = search_url)
      
    # Get max page number
    print(search_term)
    print(max_page)
    
    # Loop through pages up to the max page
    for(page_num in 1:max_page){
      
  #     # time control, simulate manual clicking
  # time <- round(sample(x = 2:10, size = 1) + rnorm(1), 2)
  # date_time <- Sys.time()
  # while((as.numeric(Sys.time()) - as.numeric(date_time)) < time) {
  
      
      # page_url
      page_url <- paste0(search_url, "&p=", page_num) 
  
      # Read the HTML content of the webpage
      temp_page <- read_html(page_url)
      
      # Extract the community name
      community_names[[page_num]] <- temp_page %>%
        html_nodes('.styled__TypographyWrapper-sc-m28jfn-0.eoHmvk') %>%
        html_text()
      
      # Extract the ranking
      rankings[[page_num]] <- temp_page %>%
        html_nodes('.styled__DiscoveryCardRanking-sc-13ysp3k-6.egeJZg') %>%
        html_text()
      
      # Extract the meta information (Private/Public, Members, Paid/Free)
      meta_info[[page_num]] <- temp_page %>%
        html_nodes('.styled__DiscoveryCardMeta-sc-13ysp3k-7.jjNZwk') %>%
        html_text()
  
 }
  
  # Combine the lists into a single data frame
  temp_data <- data.frame(
    Ranking = unlist(rankings),
    Community = unlist(community_names),
    Meta = unlist(meta_info),
    stringsAsFactors = FALSE
  ) %>% mutate(Search = search_term)

search_df <- bind_rows(search_df, temp_data)

# Display the extracted data
dim(search_df)

#str(search_df)

# analyze the % name hit in title. Guild almost 100%, group almost 0%
# important to being search in discovery

# table, name, pages/#communities (competition), % title match, real competition

# make scatterplot (ggtext) and line, high vs low competition terms
# has to feel authentic

# talk about search terms, sometimes appearing in description ("minds", "movement")
# but not really clear why certain communities appear for certain words
# Maybe Sam Ovens can tell us

# Analzye search terms by free/paid/paid_USD % stacked bar chart (horizontal)
# same for public vs private (ordered by category free/private etc.)

# log transformed histogram of member numbers? many low categories, 
# name and price (log10 box plot) is going to be most interesting

# reactable html widget
# ! red bar based on members x price estimation of group value

# check if color code for price makes sense high mid low, is it gradient or only 5 values? Yes looks like it!


# 2) Data cleaning and manipulation ----
skool_df <- readRDS(file = "E:/The Data Digest/GitHub/Skool/input/search_community_all_2024-08-21.RDS")
dim(skool_df); View(skool_df)

skool_df <- temp_data # to add the missing "circle" info
skool_df <- clean_names(skool_df)

skool_df <- skool_df %>% mutate(
  id = parse_number(ranking),
  comm_emoji = str_detect(community, "[^\x20-\x7E]"))

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

# check search term being part of title
skool_df$name <- tolower(skool_df$community)

skool_df$term_in_title <- str_detect(string = skool_df$name, pattern = skool_df$search)
View(search_df)
skool_df$term_in_title %>% table() # only 3679

skool_df %>% group_by(search) %>% summarize(mean(term_in_title), n())
# already a fun and interesting result
# SEO use business?

circle_df <- skool_df
skool_search_BU <- skool_df

# filter for unique
skool_search_BU %>% group_by(community, search) %>% count() %>% filter(n >1)
# 65 cases
skool_search_BU %>% group_by(community, search, meta) %>% count() %>% filter(n >1)
# 6 cases

skool_search_BU %>% group_by(community, meta) %>% count() %>% filter(n >1)
# 3972 cases
skool_search_unique <- skool_search_BU %>% distinct(community, meta, .keep_all = TRUE)
View(skool_search_unique)

# filter for unique and search hit
skool_search_unique_hit <- skool_df %>% filter(term_in_title) # seems to be enough
xx <- skool_search_unique_hit %>% count(community, sort = T) %>% filter(n > 1) %>% pull(community)
skool_search_unique_hit %>% filter(community %in% xx) %>% arrange(community, ranking) %>% View()

# almost done, split in 2 tables, 1 with unique titles those are safe
# one with duplicates/triplicates, often just remove one (higher members or randomly
# some have to be kept with identical name but different members
# this will then be the basis for initial tables (communities scraped regardless of search term and title match)

skool_search_unique_hit1 <- skool_search_unique_hit %>% group_by(community) %>% add_count() %>% filter(n == 1)
skool_search_unique_hit2 <- skool_search_unique_hit %>% group_by(community) %>% add_count() %>% filter(n == 2)
skool_search_unique_hit3 <- skool_search_unique_hit %>% group_by(community) %>% add_count() %>% filter(n == 3)
skool_search_unique_hit4 <- skool_search_unique_hit %>% group_by(community) %>% add_count() %>% filter(n == 4)

#2
View(skool_search_unique_hit2)
skool_search_unique_hit2_BU <- skool_search_unique_hit2
#"delete the 6518 The Social School"
skool_search_unique_hit2 <- skool_search_unique_hit2 %>% filter(id != 6518)
#"delete the 1093 free Community Empire
skool_search_unique_hit2 <- skool_search_unique_hit2 %>% filter(id != 1093)


# filter and keep the doubles that are different communities
hit2_keep <- c("Achieve Academy", "Agent School", "AI PIRATE SKOOL", "Arbitrage Academy", "Coaching Empire",   "Content Academy", "Creator Collective", "Creator Skool", "Creators Club", "Crypto Academy", "Digital Freedom Academy", "Growth Academy", "High Performance Academy", "Kingdom Builders", "Remote Cleaning Academy", "Sales Mastermind", "Scaling School", "Social Skool", "The Bulking Brotherhood", "The Elite", "The Family")

hit2_doubles <- skool_search_unique_hit2 %>% filter(community %in% hit2_keep)
hit2_uniques <- skool_search_unique_hit2 %>% filter(!community %in% hit2_keep)
hit2_uniques <- hit2_uniques %>% group_by(community) %>% arrange(desc(members_n)) %>% slice_head(n = 1) %>% ungroup()

skool_search_unique_hit2_cleaned <- bind_rows(hit2_uniques, hit2_doubles)

#3
View(skool_search_unique_hit3)
hit3_keep <- c("Elevate Academy",  "The Academy", "The Brotherhood", "The Tribe")
hit3_triplets <- skool_search_unique_hit3 %>% filter(community %in% hit3_keep)
hit3_uniques <- skool_search_unique_hit3 %>% filter(!community %in% hit3_keep)
hit3_uniques <- hit3_uniques %>% group_by(community) %>% arrange(desc(members_n)) %>% slice_head(n = 1) %>% ungroup()
skool_search_unique_hit3_cleaned <- bind_rows(hit3_uniques, hit3_triplets)

#4
View(skool_search_unique_hit4)
skool_search_unique_hit4_cleaned <- skool_search_unique_hit4 %>% filter(!id %in% c(3707, 3929, 6896,8545))

# combine #1, #2, #3, #4
skool_search_unique_hit <- bind_rows(skool_search_unique_hit1, 
                                     skool_search_unique_hit2_cleaned, 
                                     skool_search_unique_hit3_cleaned, 
                                     skool_search_unique_hit4_cleaned)

# 3) Save objects ----

# save.image(file = "euro2024.RData")
# Save an object to a file
# skool1000 <- skool_df
# saveRDS(object = skool1000, 
#         file = paste0("E:/The Data Digest/GitHub/Skool/input/skool1000_", today(), ".RDS"))

# missing "circle" fix
skool_search_BU <- bind_rows(skool_search_BU, circle_df)
saveRDS(object = skool_search_BU, 
        file = paste0("E:/The Data Digest/GitHub/Skool/input/skool_search_BU_", today(), ".RDS"))

# re-run 209 before saving (skool_search_unique <- skool_search_BU %>% distinct(community, meta, .keep_all = TRUE))
saveRDS(object = skool_search_unique, 
        file = paste0("E:/The Data Digest/GitHub/Skool/input/skool_search_unique_", today(), ".RDS"))

# also fine (circle added to skool_serach_BU)
saveRDS(object = skool_search_BU %>% filter(term_in_title), 
        file = paste0("E:/The Data Digest/GitHub/Skool/input/skool_multi_search_hit_", today(), ".RDS"))

# add circle, sort again by duplicated names
skool_search_unique_hit_circle <- bind_rows(skool_search_unique_hit %>% select(-n), circle_df)
skool_search_unique_hit_circle <- skool_search_unique_hit_circle %>% filter(term_in_title)

skool_search_unique_hit <- skool_search_unique_hit_circle %>% filter(!(id %in% c(2287, 13435, 96, 10785,4208,12071,6913,5290,5450,14693) & search == "circle"))

saveRDS(object = skool_search_unique_hit, 
        file = paste0("E:/The Data Digest/GitHub/Skool/input/skool_search_unique_hit_", today(), ".RDS"))



# 4) Search terms backup ----

search_terms_community <- c("academy", "agency", "bootcamp", "brand", "brotherhood", "business", "circle", "club", "collective", "community", "crew", "dojo", "elite","empire", "family", "forum", "gang", "generation", "group", "guild", "herd", "home","kingdom", "klub", "lab", "league", "legion", "masterclass", "mastermind", "meets", "movement","nation", "preschool", "school", "skool", "society", "squad","startup", "studio", "system", "team", "tribe", "university", "workshop")

search_terms_individual_title <- c("achiever", "anonymous", "apprentice", "architects", "artist", "builder", "coach", "creatives", "creator", "developer", "editor", "engineer", "entrepreneurs", "expert", "founder", "friends", "girls", "humans", "husband", "immigrant", "influencer", "insider", "kevin", "king", "kyle", "leader",  "lions", "lords", "major", "marketer", "master", "member", "mentor",  "men", "millionaire", "mogul", "muslim",  "nerd", "ninjas", "nomad", "noob", "one", "operator", "outlier", "owners", "parents", "partners", "pathfinders",  "photographers", "pilots", "player", "practitioner", "producer", "prodigies", "professionals", "pursuers", "queen", "raiser", "rebels", "resellpreneur", "rick", "royalty", "sage", "saint",   "self", "sellers", "servant", "servants", "setters", "shepherds", "shesells", "sharks", "shredders", "singers", "sisyphus","sisterhood",  "sisterheard", "sniper", "socializer", "solo", "star", "starter", "students", "superhuman", "superior", "supermums", "surfer", "synthesizer", "tailors", "talent", "tarzans", "taylor", "teacher", "tester", "tom", "trader", "trainer", "tycoon", "unicorn", "users", "vegans", "vip", "warrior", "winner", "woman", "women", "worker")

search_terms_others <- c("accelerator", "access", "acquisition", "action", "basecamp", "best", "challenge", "channels", "coaching", "day", "digital", "education", "exclusive", "fit", "free", "game", "grow", "habit", "healing", "health", "help", "hustle", "impact", "income", "instagram", "international", "internet", "invest", "land", "launch", "learn", "legacy", "life", "magnet", "market", "media", "mission", "money", "motivation", "official", "onlyfans", "passion", "power", "premium", "product", "profit", "project", "public", "purpose", "radikal", "reiki", "remote", "renaissance", "result", "retire", "revolution", "rich", "sales", "secret", "service", "shopify", "skills", "social", "solution", "spiritually", "sports", "strategy", "strength", "study", "success", "support", "tarot", "teach", "thailand", "therapy", "thrive", "ticket", "time", "tools", "top", "track", "trading", "training", "ultimate", "universe", "unlimited", "value", "wealth", "watercolor", "weekend", "website", "white", "world", "yacht", "youtube")


search_terms_community_BU <- c("academy", "agencies", "agency", "alliance", "army", "beehive", "bootcamp", "brand", "brotherhood", "business","camp", "circle", "club", "collective", "college", "community", "corp", "crew", "dojo", "elite","empire", "exclusive", "family", "forum", "gang", "generation", "group", "guild", "herd", "home","kingdom", "klub", "lab", "league", "legion", "masterclass", "mastermind", "meets", "minds",  "movement","nation", "ninjas", "preschool", "school", "sisterhood", "skool", "sobriety", "society", "space", "squad","startup", "studio", "system", "team", "together", "tribe", "university", "workshop")

# Handle non-existing pages
tryCatch({
  page_data <- read_html(page_url)
  # Do your scraping here
}, error = function(e) {
  message(paste("Page", i, "does not exist. Stopping loop."))
  break
})



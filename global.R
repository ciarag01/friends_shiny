##############################################.
# GLOBAL ----
##############################################.

##############################################.
# LOAD PACKAGES ----
##############################################.
library(dplyr) #data manipulation
library(plotly) #charts
library(shiny)
library(shinyWidgets)
library(tidyr)
library(magrittr)
library(readr)
library(janitor)
library(circlize)
library(tm)
library(wordcloud)
library(memoise)
library(plotly)

##############################################.
# LOAD DATA ----
##############################################.
friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')
friends_emotions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')
friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')

##############################################.
# TIDY DATA ----
##############################################.
friends %<>%
  mutate(speaker_grpd = case_when(speaker %in% c("Monica Geller", "Courtney Cox") ~ 
                                    "Monica",
                                  speaker %in% c("Rachel Green", "Jennifer Aniston") ~ 
                                    "Rachel",
                                  speaker %in% c("Joey Tribbiani") ~ "Joey",
                                  speaker %in% c("Chandler Bing", "Matthew Perry") ~
                                    "Chandler",
                                  speaker %in% c("Phoebe Buffay", "Lisa Kudrow") ~
                                    "Phoebe",
                                   speaker %in% c("Ross Geller") ~ "Ross",
                                  TRUE ~ "Other")) 

friends_emotions %<>%
  arrange(season, episode, scene, utterance)

char_names <- c("chandler", "rachel", "monica", "ross", "joey", "phoebe")


##############################################.
# CREATE OBJECTS USED IN OUTPUTS ----
##############################################.
speakers <- friends %>% 
  group_by(speaker_grpd, season) %>%
  summarise(tot = n()) %>%
  ungroup() %>%
  filter(speaker_grpd != "Other") %>% 
  mutate(season = as.character(season)) %>% 
  group_by(season) %>% 
  mutate(avg = mean(tot)) %>% 
  ungroup()
  

emotions <- friends %>% 
  select(season, episode, scene, utterance, speaker_grpd) %>% 
  left_join(friends_emotions) %>% 
  arrange(season, episode, scene, utterance) %>% 
  mutate(emotion = if_else(is.na(emotion), "Unknown", emotion)) %>% 
  group_by(season, speaker_grpd, emotion) %>% 
  summarise(tot = n()) %>% 
  ungroup() %>% 
  filter(emotion != "Unknown") %>% 
  group_by(season, speaker_grpd) %>% 
  mutate(n = sum(tot)) %>% 
  ungroup() %>% 
  mutate(perc = round_half_up((tot/n)*100, 2))
  
ratings <- friends %>% 
  group_by(speaker_grpd, season, episode) %>%
  summarise(tot = n()) %>%
  ungroup() %>%
  filter(speaker_grpd != "Other") %>% 
  mutate(season = as.character(season)) %>% 
  arrange(season, episode, desc(tot)) %>% 
  group_by(season, episode) %>% 
  filter(tot == max(tot)) %>% 
  ungroup() %>% 
  rename(main_char = speaker_grpd) %>% 
  left_join(friends_info %>% 
              select(season, episode, us_views_millions, imdb_rating) %>% 
              mutate(season = as.character(season)), 
            by = c("season", "episode")) %>% 
  pivot_longer(cols = us_views_millions:imdb_rating, names_to = "rating_measure", 
               values_to = "n") %>% 
  mutate(rating_measure = if_else(rating_measure == "imdb_rating", "IMDB Rating", 
                                  "US Views (Millions)")) %>% 
  select(-tot)


relationships <- friends %>% 
  group_by(season, episode, scene, speaker_grpd) %>% 
  summarise(tot = n()) %>% 
  ungroup() %>% 
  mutate(chandler = if_else(speaker_grpd == "Chandler", 1, 0), 
         rachel = if_else(speaker_grpd == "Rachel", 1, 0), 
         monica = if_else(speaker_grpd == "Monica", 1, 0), 
         ross = if_else(speaker_grpd == "Ross", 1, 0), 
         joey = if_else(speaker_grpd == "Joey", 1, 0), 
         phoebe = if_else(speaker_grpd == "Phoebe", 1, 0), 
         other = if_else(speaker_grpd == "Other", 1, 0)) %>% 
  group_by(season, episode, scene) %>% 
  summarise(chandler = sum(chandler),
            rachel = sum(rachel),
            monica = sum(monica),
            ross = sum(ross), 
            joey = sum(joey), 
            phoebe = sum(phoebe), 
            other = sum(other)) %>% 
  ungroup() %>% 
  select(-other, -scene, -episode, -season) 

interactions <- relationships %>% 
  mutate(chandler_1 = if_else(chandler == 1, "chandler", "NA"),
         rachel_1 = if_else(rachel == 1, "rachel", "NA"),
         monica_1 = if_else(monica == 1, "monica", "NA"),
         ross_1 = if_else(ross == 1, "ross", "NA"),
         joey_1 = if_else(joey == 1, "joey", "NA"),
         phoebe_1 = if_else(phoebe == 1, "phoebe", "NA")) %>% 
  pivot_longer(cols = chandler_1:phoebe_1, names_to = "name", 
               values_to = "character") %>% 
  select(-name) %>% 
  filter(character != "NA") %>% 
  group_by(character) %>% 
  summarise(chandler = sum(chandler),
            rachel = sum(rachel),
            monica = sum(monica),
            ross = sum(ross), 
            joey = sum(joey), 
            phoebe = sum(phoebe)) %>% 
  ungroup() %>% 
  slice(match(char_names, character))

solo_scenes <- relationships %>% 
  mutate(tot = chandler + rachel + ross + monica + phoebe + joey) %>% 
  filter(tot == 1) %>% 
  mutate(solo_chandler = if_else(chandler == 1 & tot == 1, 1, 0), 
         solo_rachel = if_else(rachel == 1 & tot == 1, 1, 0), 
         solo_monica = if_else(monica == 1 & tot == 1, 1, 0), 
         solo_ross = if_else(ross == 1 & tot == 1, 1, 0), 
         solo_joey = if_else(joey == 1 & tot == 1, 1, 0), 
         solo_phoebe = if_else(phoebe == 1 & tot == 1, 1, 0)) %>% 
  select(-(chandler:tot)) %>% 
  mutate(character = case_when(solo_chandler == 1 ~ "chandler", 
                               solo_rachel == 1 ~ "rachel",
                               solo_monica == 1 ~ "monica",
                               solo_ross == 1 ~ "ross",
                               solo_joey == 1 ~ "joey",
                               solo_phoebe == 1 ~ "phoebe",
                               TRUE ~ "NA")) %>% 
  filter(character != "NA") %>% 
  group_by(character) %>% 
  summarise(chandler = sum(solo_chandler),
            rachel = sum(solo_rachel), 
            monica = sum(solo_monica), 
            ross = sum(solo_ross), 
            joey = sum(solo_joey), 
            phoebe = sum(solo_phoebe)) %>% 
    ungroup()

char_names <- c(interactions$character)

interactions %<>%
  select(-character) %>% 
  as.matrix()

row.names(interactions) <- char_names
diag(interactions) <- 0

chordDiagram(interactions, directional = 1, transparency = 0)
circos.clear()

words <- friends %>% 
  filter(speaker_grpd != "Other") %>% 
  filter(episode == 1) %>% 
  filter(season %in% c("1", "5", "10")) %>% 
  select(season, speaker_grpd, text) %>% 
  mutate(words = stringr::str_split(text, pattern = " "))
# 
# library(ggwordcloud)
# #> Loading required package: ggplot2
# data("love_words_small")
# data("love_words")
# set.seed(42)
# ggplot(words[1:20,], aes(label = text)) +
#   geom_text_wordcloud() +
#   theme_minimal()

new <- words

for (i in 1:length(new$words)){
  for (j in 1:length(new$words[[i]])){

    new[i, ncol(new) + 1] <- new$words[[i]][j]
    names(new)[ncol(new)] <- paste0("word", j)

  }
}
colnames(new)[5] <- c("first_word")
colnames(new)[ncol(new)] <- "last_word"

new %<>%
  pivot_longer(cols = first_word:last_word, names_to = "pos", values_to = "words2") %>% 
  select(-words, -text) %>% 
  filter(!is.na(words2)) %>% 
  mutate(words2 = stringr::str_to_lower(words2)) %>% 
  mutate(words2 =  gsub("[^[:alnum:][:space:]']", "", words2)) %>% 
  filter(!(words2 %in% c("i", "you", "he", "she", "it", "at", "in", "is", "i'm",
                         "my", "we", "of", "this", "that's", "is", "if", "you're",
                         "on", "was", "be", "had", "have", "had", "there", "their", 
                         "they're", "to", "the", "and", "that", "a", "your", "but", 
                         "he's", "she's", "it's", "hi", "us", "so", "or"))) %>% 
  select(-pos)
  

check <- new %>% 
  group_by(words2) %>% 
  summarise(tot = n()) %>% 
  ungroup() %>% 
  arrange(desc(tot))

library(ggwordcloud)
ggplot(new, aes(label = words2)) +
  geom_text_wordcloud() +
  theme_minimal()

fn2 <- function(season){
  
  list_plots <- lapply(season, function(x) ggplot(speakers %>% filter(season == x)) +
                         geom_col(aes(x = speaker_grpd, y = tot, fill = speaker_grpd),
                                  show.legend = FALSE) +
                         ggtitle(paste0("season ", x)))
  
  for(i in c(1:length(list_plots))) {
    y <- season
    names(list_plots)[[i]] <- paste0("season_", y[i])
  }
  
  return(list_plots)
  
}


library(spotifyr)
library(highcharter)
library(ggbeeswarm)
library(plotly)
library(sentimentr)
library(magrittr)
library(dplyr)
library(tidyverse)

Sys.setenv(SPOTIFY_CLIENT_ID = '040a9199706d4ee5a1729414f7fa4a3e')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'bc369271b5164ff3a601b923e2e22e25')

access_token <- get_spotify_access_token()

kanye <- get_artist_audio_features('kanye west')

song_data <- kanye %>% 
  select(album_release_year, danceability:tempo, duration_ms, explicit, track_name, album_images, external_urls.spotify, album_name:key_mode)

song_data <- song_data %>% 
  filter(track_name != str_detect(track_name, "Skit *"),
         album_name != "Graduation (Alternative Business Partners)",
         album_name != "Kanye West Presents Good Music Cruel Summer",
         album_name != "Late Orchestration") %>% 
  distinct(track_name, .keep_all = TRUE)

for (row in 1:nrow(song_data)) {
  song_data$album_images[[row]] <- song_data$album_images[[row]][['url']][[2]]
}

song_data$album_images <- unlist(song_data$album_images)

## a couple of duplicated album images, unsure why
song_data <- song_data %>%
  mutate(album_images = ifelse(album_name == "My Beautiful Dark Twisted Fantasy",
                               "https://i.scdn.co/image/ab67616d00001e025bd363295a677dacd0b4187b",
                               album_images),
         album_images = ifelse(album_name == "Late Registration",
                               "https://i.scdn.co/image/ab67616d00001e02de720f487ec5a6a3cfad9ebb",
                               album_images),
         album_images = ifelse(album_name == "808s & Heartbreak",
                               "https://i.scdn.co/image/ab67616d00001e0265fbf56c2837be4879b5020a",
                               album_images),
         album_images = ifelse(album_name == "The College Dropout",
                               "https://i.scdn.co/image/ab67616d00001e02ba869545176c153a9542db2d",
                               album_images),
         album_images = ifelse(album_name == "Watch The Throne",
                               "https://i.scdn.co/image/ab67616d00001e02bf2d62374f877d312b34e67a",
                               album_images))

# write.csv(song_data, "data/song.csv", row.names = FALSE)

grouped <- song_data %>% 
  group_by(album_name) %>% 
  summarise_at(vars(danceability:tempo), mean) 

grouped <- grouped %>% 
  filter(album_name != "Graduation (Alternative Business Partners)",
         album_name != "Late Orchestration",
         album_name != "Kanye West Presents Good Music Cruel Summer") 

# write.csv(grouped, "data/album.csv", row.names = FALSE)

#### LYRICS

library(genius)

all_kanye_albums <- unique(grouped$album_name)

all_album_lyrics <- data.frame()

for(album in all_kanye_albums) {
  these_album_lyrics <- genius::possible_album('Kanye West', album, info = 'simple')
  all_album_lyrics <- rbind(all_album_lyrics, these_album_lyrics)
}

# Unclear why these didn't work, but will add manually

setdiff(unique(song_data$track_name), unique(all_album_lyrics$track_title))

heartbreak <- genius::possible_album('Kanye West', '808s heartbreak', info = 'simple')
tlop <- genius::possible_album('Kanye West', 'The Life of Pablo', info = 'simple')
ksg <- genius::possible_album('KIDS SEE GHOSTS', 'KIDS SEE GHOSTS', info = 'simple')
wtt <- genius::possible_album('Jay-Z & Kanye West', 'Watch the Throne', info = 'simple')

all_album_lyrics <- do.call("rbind", list(all_album_lyrics, heartbreak, tlop, ksg, wtt))

all_album_lyrics <- all_album_lyrics %>% 
  mutate(clean_track_title = str_replace_all(track_title, " \\(.+?\\)", ""),
         clean_track_title = str_to_lower(clean_track_title),
         clean_track_title = str_replace_all(clean_track_title, " - album version", ""))

song_data <- song_data %>% 
  mutate(clean_track_title = str_replace_all(track_name, " \\(.+?\\)", ""),
         clean_track_title = str_to_lower(clean_track_title),
         clean_track_title = str_replace_all(clean_track_title, " - album version", ""))

setdiff(unique(song_data$clean_track_title), unique(all_album_lyrics$clean_track_title))

all_album_lyrics <- all_album_lyrics %>% filter(!is.na(lyric))

write.csv(all_album_lyrics, "data/lyrics.csv", row.names = FALSE)

sentiment <- all_album_lyrics %>% 
  # get_sentences() %$%
  mutate(lyric_sentiment = sentiment_by(lyric))

sentiment_score <- sentiment$lyric_sentiment$ave_sentiment

sentiment_no_nums <- sentiment %>% 
  select(track_n:clean_track_title)

sentiment <- cbind(sentiment_no_nums, sentiment_score)

sentiment <- left_join(sentiment, song_data, by = 'clean_track_title')
write.csv(sentiment, "data/sentiment.csv", row.names = FALSE)

## SONG SENTIMENT

song_sentiments <- sentiment %>% 
  group_by(clean_track_title) %>% 
  summarise(sentiment_score = mean(sentiment_score, na.rm = TRUE))

song_data_w_sentiments <- left_join(song_data, song_sentiments, by = "clean_track_title")
# song_data_w_sentiments %>% filter(is.na(sentiment_score))

write.csv(song_data_w_sentiments, "data/song.csv", row.names = FALSE)

## JOIN FOR BIG ALBUM DATAFRAME

sentiment_by_album <- song_data_w_sentiments %>% 
  group_by(album_name) %>% 
  summarise(sentiment_score = mean(sentiment_score, na.rm = TRUE))

all_info_albums <- merge(grouped, sentiment_by_album, by = "album_name")

album_images <- song_data %>% 
  select(album_name, album_images, album_release_year) %>% 
  distinct()

all_info_albums <- merge(all_info_albums, album_images, by = "album_name")

write.csv(all_info_albums, "data/album.csv", row.names = FALSE)


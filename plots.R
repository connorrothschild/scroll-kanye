library(spotifyr)
library(tidyverse)
library(highcharter)
library(ggbeeswarm)
library(plotly)
library(sentimentr)
library(magrittr)
library(dplyr)
library(RColorBrewer)
library(htmltools)

album_data <- readr::read_csv("data/album.csv")
song_data <- readr::read_csv("data/song.csv")
lyrics <- readr::read_csv("data/lyrics.csv")
sentiment <- readr::read_csv("data/sentiment.csv")

plot <- sentiment %>%
  # filter(element_artist == "Kanye West") %>% 
  distinct(lyric, .keep_all = TRUE) %>% 
  filter(sentiment_score != 0) %>% 
  ggplot() +
  geom_beeswarm(aes(x = sentiment_score, 
                    y = album_name,
                    color = album_name, 
                    tooltip = lyric), 
                groupOnX = FALSE,
                alpha = .3)

ggplotly(plot, tooltip = "tooltip")

plot_grouped <- sentiment %>% 
  group_by(album_name, track_title) %>% 
  summarise(sentiment = mean(sentiment_score)) %>% 
  filter(sentiment != 0) %>% 
  ggplot() +
  geom_beeswarm(aes(x = sentiment, 
                    y = album_name,
                    color = album_name, 
                    tooltip = paste(track_title, sentiment)), 
                groupOnX = FALSE,
                alpha = .3)

ggplotly(plot_grouped, tooltip = "tooltip")

plot_df <- album_data %>%
  arrange(desc(sentiment_score)) %>% 
  ungroup() %>%
  mutate(
    tooltip = paste0(
      '<a style = "margin-right:',
      max(nchar(album_name)) + 150,
      'px">',
      '<img src=',
      album_images,
      ' height="50" style="float:left;margin-right:5px">',
      '<b>Album:</b> ',
      album_name,
      '<br><b>Average Sentiment</b>: ',
      round(sentiment_score, 3),
      '<br><b>Year Released</b>: ',
      album_release_year,
      '</a>'
    )
  )

hchart(plot_df, type = "bar", hcaes(x = album_name, y = sentiment_score)) %>%
  hc_tooltip(formatter = JS(paste0(
    "function() {return this.point.tooltip;}"
  )),
  useHTML = T) %>%
  hc_add_theme(hc_theme_smpl()) %>%
  hc_title(text = paste0("Kanye's Mood")) %>%
  hc_subtitle(text = paste0("Kanye West's sentiment by album")) %>%
  hc_xAxis(title = list(text = '')) %>%
  hc_yAxis(title = list(text = "Sentiment"))

plot_df <- song_data %>%
  mutate(album_release_year =
           ifelse(album_name == "KIDS SEE GHOSTS", 2018.5, album_release_year)) %>%
  mutate(
    tooltip = paste0(
      '<a style = "margin-right:',
      max(nchar(track_name), nchar(album_name)) + 150,
      'px">',
      '<img src=',
      album_images,
      ' height="50" style="float:left;margin-right:5px">',
      '<b>Album:</b> ',
      album_name,
      '<br><b>Track:</b> ',
      track_name
    )
  ) %>%
  ungroup

avg_line <- plot_df %>%
  mutate(album_release_year =
           ifelse(album_name == "KIDS SEE GHOSTS", 2018.5, album_release_year)) %>%
  group_by(album_release_year, album_name, album_images) %>%
  summarise(avg = mean(sentiment_score, na.rm = TRUE)) %>%
  ungroup %>%
  transmute(
    x = as.numeric(as.factor(album_release_year)),
    y = avg,
    tooltip = paste0(
      '<a style = "margin-right:',
      max(nchar(album_name)) + 100,
      'px">',
      '<img src=',
      album_images,
      ' height="50" style="float:left;margin-right:5px">',
      '<b>Album:</b> ',
      album_name,
      '<br><b>Average Sentiment</b> ',
      round(avg, 2),
      '</a>'
    )
  )

plot_track_df <- plot_df %>%
  mutate(
    tooltip = paste0(tooltip, '<br><b>Sentiment:</b> ', round(sentiment_score, 3), '</a>'),
    album_number = as.numeric(as.factor(album_release_year))
  ) %>%
  ungroup

album_chart <-
  hchart(plot_track_df,
         'scatter',
         hcaes(
           x = as.numeric(as.factor(album_release_year)),
           y = sentiment_score,
           group = album_name
         )) %>%
  hc_add_series(data = avg_line, type = 'line') %>%
  hc_tooltip(formatter = JS(paste0(
    "function() {return this.point.tooltip;}"
  )),
  useHTML = T) %>%
  hc_colors(c(sample(brewer.pal(
    n_distinct(song_data$album_name), 'Paired'
  )), 'black')) %>%
  hc_xAxis(title = list(text = element_blank()),
           labels = list(enabled = F)) %>%
  hc_yAxis(title = list(text = 'Sentiment')) %>%
  hc_title(text = "Kanye's Mood") %>%
  hc_subtitle(text = "Kanye West's Sentiment by Song") %>%
  hc_legend(enabled = FALSE) %>% 
  hc_add_theme(hc_theme_smpl())
album_chart$x$hc_opts$series[[12]]$name <- 'Album Averages'
album_chart

library(ggridges)
library(cr)

ridges <- song_data %>% 
  mutate(album_name = reorder(album_name, -album_release_year)) %>% 
  ggplot(aes(x = valence, y = album_name, fill = stat(x))) +
  geom_density_ridges_gradient(jittered_points = TRUE, show.legend = FALSE,
                               # position = position_points_jitter(width = 0.05, height = 0),
                               point_size = 1, point_alpha = 0.3, alpha = 0.3) +
  scale_fill_viridis_c() +
  # theme_ridges(grid = FALSE, font_size = 12) +
  labs(x = 'Valence', 
       y = element_blank()) +
  scale_x_continuous(breaks = c(0, 1), labels = c("← More negative", "More positive →")) +
  theme_cr() +
  drop_axis() +
  theme(panel.grid.major = element_blank())

ridges

ridges <- song_data %>% 
  mutate(album_name = reorder(album_name, -album_release_year)) %>% 
  ggplot(aes(x = sentiment_score, y = album_name, fill = stat(x))) +
  geom_density_ridges_gradient(jittered_points = TRUE, show.legend = FALSE,
                               # position = position_points_jitter(width = 0.05, height = 0),
                               point_size = 1, point_alpha = 0.3, alpha = 0.3) +
  scale_fill_viridis_c() +
  # theme_ridges(grid = FALSE, font_size = 12) +
  labs(x = 'Sentiment', 
       y = element_blank()) +
  scale_x_continuous(breaks = c(-.5, .5), labels = c("← More negative", "More positive →")) +
  theme_cr() +
  drop_axis() +
  theme(panel.grid.major = element_blank())

ridges

# song_data %>% 
#   filter(sentiment_score > -.3, 
#          sentiment_score < .3) %>% 
#   ggplot(aes(x = sentiment_score, y = valence)) +
#   geom_point() +
#   geom_smooth()

setwd("~/Documents/University/Geography Degree/02 Year 2/DDC/R/Practical-Week6")
source("install_packages.R")
library(tidyverse)
library(sf)
library(photosearcher)
library(textclean)
library(tidytext)
library(igraph, warn.conflicts = FALSE)

boundary <- st_read("Sutton Park Boundary_region.shp")
ggplot(boundary) +
  geom_sf()

sutton_photos <- photo_search(mindate_taken = "2020-01-01", maxdate_taken = "2026-01-01", sf_layer = boundary, has_geo = TRUE)
str(sutton_photos)

sutton_text <- sutton_photos %>%
  mutate(
    text = paste(title, description, tags),
    text = replace_url(text),
    text = str_squish(text)
  ) %>%
  filter(!is.na(text), text != "")

data("stop_words")

tidy_tokens <- sutton_text %>%
  select(url_l, longitude, latitude, title, description, tags, text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "^[a-z][a-z'-]*$")) %>%
  count(url_l, word, sort = FALSE)

photo_word <- tidy_tokens %>%
  mutate(n = 1) %>%
  pivot_wider(names_from = word, values_from = n, values_fill = 0)

word_counts <- tidy_tokens %>% count(word, name = "photo_count")

keep_words <- word_counts %>% filter(photo_count >= 5) %>% pull(word)

photo_word_mat <- tidy_tokens %>%
  filter(word %in% keep_words) %>%
  mutate(n = 1) %>%
  pivot_wider(names_from = word, values_from = n, values_fill = 0) %>%
  select(-url_l) %>%
  as.matrix()

keyword_cooccurrance <- t(photo_word_mat) %*% photo_word_mat

graph <- graph_from_adjacency_matrix(keyword_cooccurrance,
                         weighted = TRUE,
                         mode="undirected",
                         diag=FALSE)

clusters <- cluster_walktrap(graph)

keyword_importance <- map_df(1:max(clusters$membership), function(i) {
  graph_subset <- keyword_cooccurrance[clusters$membership == i,clusters$membership == i]
  graph_subset <- graph.adjacency(graph_subset,
                                  weighted = TRUE,
                                  mode="undirected",
                                  diag=FALSE)
  eigens <- eigen_centrality(graph)$vector
  eigens_df <- tibble(eigenvalue = eigens, keyword = names(eigens), cluster = i) %>%
    arrange(-eigenvalue)
})

keyword_importance %>% group_by(cluster) %>%
  slice(1:10)

word_to_cluster <- tibble(keyword = V(graph)$name, cluster = clusters$membership)

assign_cluster <- tidy_tokens %>%
  filter(word %in% word_to_cluster$keyword) %>%
  inner_join(word_to_cluster, by = c("word" = "keyword")) %>%
  count(url_l, cluster, name = "votes") %>%
  group_by(url_l) %>%
  slice_max(votes, n = 1, with_ties = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(cluster = factor(cluster, labels = paste("Cluster", sort(unique(cluster)))))

assign_cluster$cluster <- factor(assign_cluster$cluster,
                                 labels = c("Cluster 1",
                                            "Cluster 2",
                                            "Cluster 3",
                                            "Cluster 4",
                                            "Cluster 5",
                                            "Cluster 6",
                                            "Cluster 7",
                                            "Cluster 8",
                                            "Cluster 9"))

sutton_photo_clusters <- sutton_photos %>%
  inner_join(assign_cluster) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326))

ggplot() +
  geom_sf(data = boundary) +
  geom_sf(data = sutton_photo_clusters, aes(colour = cluster))

sutton_photo_clusters$text <- paste(sutton_photo_clusters$title,
                                    sutton_photo_clusters$description,
                                    sutton_photo_clusters$tags)

sutton_photo_text <- sutton_photo_clusters %>%
  unnest_tokens(word, text)

sentiment <- sutton_photo_text %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(url_l) %>%
  summarise(sentiment = sum(value)) %>%
  st_set_geometry(NULL) %>%
  inner_join(sutton_photo_clusters) %>%
  arrange(-sentiment) %>%
  mutate(id = 1:n())

ggplot(sentiment, aes(x = id, y = sentiment,
                      fill = cluster)) +
  geom_bar(stat = "identity")

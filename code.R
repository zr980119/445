library(tidyverse)
library(ggridges)
library(fmsb)
library(GGally)
library(igraph)
library(ggraph)
library(viridis)
library(scales)
theme_set(theme_minimal(base_size = 11))

df <- read_csv("billboard_24years_lyrics_spotify.csv", show_col_types = FALSE)

# Clean data
df_clean <- df %>%
  filter(!is.na(danceability), !is.na(energy)) %>%
  distinct(song, band_singer, year, .keep_all = TRUE)

# Create ranking categories
df_clean <- df_clean %>%
  mutate(
    rank_category = case_when(
      ranking <= 10 ~ "Top 10",
      ranking <= 25 ~ "11-25",
      ranking <= 50 ~ "26-50",
      ranking <= 100 ~ "51-100"
    ),
    rank_category = factor(rank_category, 
                           levels = c("Top 10", "11-25", "26-50", "51-100")),
    year_label = as.character(year)
  )



# Total songs with complete features
print(nrow(df_clean))

# Year range
print(min(df_clean$year))
print(max(df_clean$year))

# Songs per year
print(table(df_clean$year))

# Songs per ranking category
print(table(df_clean$rank_category))



# FIGURE 1
# Prepare data for ridgeline plot
ridgeline_data <- df_clean %>%
  filter(year %in% c(2000, 2001, 2002, 2003, 2004)) %>%
  select(year_label, danceability, energy, valence) %>%
  pivot_longer(cols = c(danceability, energy, valence), 
               names_to = "feature", 
               values_to = "value") %>%
  mutate(
    feature = case_when(
      feature == "danceability" ~ "Danceability",
      feature == "energy" ~ "Energy",
      feature == "valence" ~ "Valence (Positivity)"
    )
  )

# Create ridgeline plot
figure1 <- ggplot(ridgeline_data, 
                  aes(x = value, y = year_label, fill = after_stat(x))) +
  geom_density_ridges_gradient(
    scale = 2.5, 
    rel_min_height = 0.01,
    gradient_lwd = 0.5,
    alpha = 0.8
  ) +
  facet_wrap(~ feature, ncol = 3, scales = "free_x") +
  scale_fill_viridis_c(option = "plasma", name = "Value") +
  scale_x_continuous(labels = percent_format()) +
  labs(
    title = "Evolution of Music Feature Distributions (2000-2004)",
    subtitle = "Ridgeline plots showing density distributions across years",
    x = "Feature Value (0-1 scale)",
    y = "Year",
    caption = "Higher peaks indicate more songs with those values"
  ) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray95", color = NA),
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 0)
  )

ggsave("figure1.png", figure1, 
       width = 14, height = 8, dpi = 300, bg = "white")


# FIGURE 2
# Prepare data for radar chart
radar_data <- df_clean %>%
  group_by(rank_category) %>%
  summarise(
    Danceability = mean(danceability, na.rm = TRUE),
    Energy = mean(energy, na.rm = TRUE),
    Valence = mean(valence, na.rm = TRUE),
    Acousticness = mean(acousticness, na.rm = TRUE),
    Speechiness = mean(speechiness, na.rm = TRUE),
    Liveness = mean(liveness, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(across(where(is.numeric), ~ . * 100))  # Convert to 0-100 scale

# Print radar data statistics
# Radar Chart Data (Mean values by ranking)
print(radar_data)

png("figure2.png", width = 12, height = 10, 
    units = "in", res = 300, bg = "white")

par(mfrow = c(2, 2), mar = c(2, 2, 3, 2))

# Set up colors
colors <- viridis(4, option = "mako", begin = 0.2, end = 0.8)

# Create radar for each category
categories <- c("Top 10", "11-25", "26-50", "51-100")

for (i in 1:4) {
  cat_data <- radar_data[radar_data$rank_category == categories[i], ]
  
  # Prepare data for radarchart (needs max and min rows) - MUST BE DATA.FRAME
  radar_df <- data.frame(
    Danceability = c(100, 0, as.numeric(cat_data[, 2])),
    Energy = c(100, 0, as.numeric(cat_data[, 3])),
    Valence = c(100, 0, as.numeric(cat_data[, 4])),
    Acousticness = c(100, 0, as.numeric(cat_data[, 5])),
    Speechiness = c(100, 0, as.numeric(cat_data[, 6])),
    Liveness = c(100, 0, as.numeric(cat_data[, 7]))
  )
  
  # Create radar
  radarchart(
    radar_df,
    axistype = 1,
    pcol = colors[i],
    pfcol = adjustcolor(colors[i], alpha.f = 0.3),
    plwd = 3,
    cglcol = "grey",
    cglty = 1,
    axislabcol = "black",
    caxislabels = seq(0, 100, 25),
    cglwd = 0.8,
    vlcex = 1.0,
    title = categories[i]
  )
}

# Add main title
mtext("Multi-dimensional Audio Feature Comparison by Chart Ranking", 
      side = 3, line = -2, outer = TRUE, cex = 1.5, font = 2)

dev.off()


# FIGURE 3
# Prepare data for parallel coordinates
parallel_data <- df_clean %>%
  select(ranking, rank_category, danceability, energy, valence, 
         acousticness, loudness, tempo) %>%
  mutate(
    loudness_scaled = rescale(loudness, to = c(0, 1)),
    tempo_scaled = rescale(tempo, to = c(0, 1))
  ) %>%
  select(rank_category, danceability, energy, valence, 
         acousticness, loudness_scaled, tempo_scaled)

# Create parallel coordinates plot
figure3 <- ggparcoord(
  parallel_data,
  columns = 2:7,
  groupColumn = "rank_category",
  scale = "globalminmax",
  alphaLines = 0.3,
  showPoints = FALSE
) +
  scale_color_viridis_d(option = "mako", begin = 0.2, end = 0.8) +
  labs(
    title = "Parallel Coordinates Analysis of Audio Features",
    subtitle = "Each line represents a song, colored by chart ranking category",
    x = "Audio Feature",
    y = "Normalized Value (0-1)",
    color = "Chart Ranking",
    caption = "Features are normalized to 0-1 scale for comparison"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  )

# Customize axis labels
figure3 <- figure3 +
  scale_x_discrete(labels = c(
    "danceability" = "Danceability",
    "energy" = "Energy",
    "valence" = "Valence",
    "acousticness" = "Acousticness",
    "loudness_scaled" = "Loudness",
    "tempo_scaled" = "Tempo"
  ))

ggsave("figure3.png", figure3, 
       width = 13, height = 9, dpi = 300, bg = "white")


# FIGURE 4
# Calculate correlation matrix
features_for_network <- df_clean %>%
  select(danceability, energy, loudness, speechiness, 
         acousticness, liveness, valence, tempo)

cor_matrix <- cor(features_for_network, use = "complete.obs")

# Create edge list from correlation matrix
edges <- cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column("from") %>%
  pivot_longer(-from, names_to = "to", values_to = "correlation") %>%
  filter(from != to) %>%
  filter(abs(correlation) > 0.1) %>%  # Only show correlations > 0.1
  mutate(
    weight = abs(correlation),
    type = ifelse(correlation > 0, "positive", "negative")
  )

# Remove duplicate edges (keep only upper triangle)
edges <- edges %>%
  rowwise() %>%
  mutate(pair = paste(sort(c(from, to)), collapse = "-")) %>%
  ungroup() %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(-pair)

# Create graph object
g <- graph_from_data_frame(edges, directed = FALSE)

# Create network visualization - FIXED VERSION
figure4 <- ggraph(g, layout = 'circle') +
  geom_edge_link(
    aes(edge_width = weight, edge_color = correlation, edge_alpha = weight),
    show.legend = TRUE
  ) +
  geom_node_point(size = 15, color = "#440154", alpha = 0.8) +
  geom_node_text(
    aes(label = str_to_title(name)), 
    color = "white", 
    size = 3.5, 
    fontface = "bold"
  ) +
  scale_edge_width_continuous(range = c(0.5, 3), name = "Correlation\nStrength") +
  scale_edge_color_gradient2(
    low = "#2166ac", 
    mid = "gray80", 
    high = "#b2182b",
    midpoint = 0,
    name = "Correlation"
  ) +
  scale_edge_alpha_continuous(range = c(0.3, 0.9), guide = "none") +
  labs(
    title = "Network Analysis of Audio Feature Correlations",
    subtitle = "Circular network showing relationships between Spotify audio features",
    caption = "Edge thickness represents correlation strength; color represents direction (blue=negative, red=positive)"
  ) +
  theme_graph(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 0),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("figure4.png", figure4, 
       width = 12, height = 10, dpi = 300, bg = "white")



# Overall statistics
overall_stats <- df_clean %>%
  summarise(
    total_songs = n(),
    avg_danceability = mean(danceability, na.rm = TRUE),
    avg_energy = mean(energy, na.rm = TRUE),
    avg_valence = mean(valence, na.rm = TRUE),
    avg_tempo = mean(tempo, na.rm = TRUE),
    sd_danceability = sd(danceability, na.rm = TRUE),
    sd_energy = sd(energy, na.rm = TRUE)
  )

# Overall Statistics
print(overall_stats)


# Year statistics
year_stats <- df_clean %>%
  group_by(year) %>%
  summarise(
    n = n(),
    danceability = mean(danceability),
    energy = mean(energy),
    valence = mean(valence),
    .groups = 'drop'
  )

# Year-wise Statistics
print(year_stats)


# Ranking statistics
ranking_stats <- df_clean %>%
  group_by(rank_category) %>%
  summarise(
    n = n(),
    danceability = mean(danceability),
    energy = mean(energy),
    valence = mean(valence),
    .groups = 'drop'
  )

# Ranking Category Statistics
print(ranking_stats)


# Key Correlations
print(cor(df_clean$danceability, df_clean$energy))
print(cor(df_clean$energy, df_clean$acousticness))
print(cor(df_clean$energy, df_clean$loudness))
print(cor(df_clean$valence, df_clean$energy))
print(cor(df_clean$danceability, df_clean$valence))


# Network Graph Statistics
print(vcount(g))
print(ecount(g))
print(round(mean(abs(E(g)$weight)), 3))



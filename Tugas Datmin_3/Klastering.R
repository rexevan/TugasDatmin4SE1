# install paket
# install.packages("tidverse", "devtools")
# devtools::install_github('jalapic/engsoccerdata', username = "jalapic")

library(tidyverse)
library(engsoccerdata)

# check data
data(package= "engsoccerdata")

# setting sebagai tibble
# Batasi hanya selama musim 2007/2008 - 2016/2017
pl <- as.tibble(england) %>% 
  filter(division == 1, 
         Season >= 2007 & Season <= 2016)

# Hitung Total selisih gol dan Total poin setiap tim 
pl_total <- pl %>% 
  #filter(Season == 2016) %>% 
  gather(status, team, home, visitor) %>% 
  mutate(points = 
    case_when(
      status == "home" & result == "H" ~ 3, 
      status == "home" & result == "D" ~ 1, 
      status == "home" & result == "A" ~ 0,
      
      status == "visitor" & result == "H" ~ 0, 
      status == "visitor" & result == "D" ~ 1, 
      status == "visitor" & result == "A" ~ 3)
  ) %>% 
  mutate(goaldif = as.double(goaldif), 
         goaldif = if_else(status == "visitor", -1*goaldif, goaldif)) %>% 
  arrange(Date, status) %>% 
  group_by(Date, Season, team) %>% 
  summarise(GD = sum(goaldif), 
            P  = sum(points)) %>%
  group_by(Season, team) %>%
  summarise(GD = sum(GD), 
            P  = sum(P)) %>% 
  group_by(team) %>%
  summarise(GD = mean(GD), 
            P  = mean(P)) %>% 
  arrange(desc(P), desc(GD))


# Gambar pertama 
# Dengan menggunakan mata, perkirakan klaster yang terbentuk
theme_set(theme_light())
library(ggrepel)

ggplot(pl_total, aes(x = GD, y = P)) + 
  geom_point(size = 3) + 
  labs(
    x = "Goal Difference", 
    y = "Points", 
    title = "Premier League '07/08 - '16/17",
    subtitle = "Skala Rata-rata"
  ) + 
  theme(text = element_text(size = 12))

# Building Cluster -----------

# K-means ---------
pl_kmeans <- tibble(
  num_klaster = 2:10, 
  data = list(pl_total[,-1]), 
  k_means = map2(.x = data, .y = num_klaster , 
                 ~ kmeans(x = .x, center = .y)), 
  ss_within_var = map_dbl(k_means, 'tot.withinss')
)

# Choose 5 !! 
pl_kmeans %>% 
  ggplot(aes(x = factor(num_klaster), y = ss_within_var)) + 
  geom_col(aes( fill = num_klaster == 5), show.legend = FALSE) + 
  labs(
    x = "# Cluster", 
    y = "Sum Square Within Varians"
  )

pl_kmeans_gg <- pl_kmeans %>% 
  filter(num_klaster <= 5) %>% 
  mutate(kmeans_graph = map2(k_means, data,  ~ broom::augment(.x, .y))) %>% 
  select(num_klaster, kmeans_graph) %>% 
  unnest()

pl_kmeans_gg %>% 
  ggplot(aes(x = GD, y = P)) + 
  geom_point(aes(col = .cluster), size = 3) + 
  facet_wrap(~ num_klaster) + 
  theme(legend.position = "top") + 
  labs(
    x = "Goal Difference", 
    y = "Points", 
    color = "Cluster", 
    title = "Premier League Clustering"
  )


## Hirarki -------------------
pl_total_df <- pl_total %>% 
  as.data.frame(row.names = team)

hc <- dist(pl_total_df[,-1]) %>% 
  hclust(method = "complete")

plot(hc)
cutree(hc, k = 4)

# DBSCAN -----------------------
library(dbscan)
pl_db <- dbscan(pl_total[,-1], eps = 10, minPts = 3)

pl_total$db = pl_db$cluster

ggplot(pl_total, aes(GD, P)) + 
  geom_point(size = 3, aes(color = factor(db)))

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

# let's talk about Last Season 
# Hitung Total selisih gol dan Total poin setiap tim 
by_date <- pl %>% 
  filter(Season == 2016) %>% 
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
            P  = sum(points)) 

# end of the season table
pl2016 <- by_date %>%
  group_by(Season, team) %>%
  summarise(GD = sum(GD), 
            P  = sum(P)) %>% 
  arrange(desc(P), desc(GD))

library(ggrepel)

pl2016 <- pl2016 %>% 
  mutate(Pos = 1:20) %>% 
  mutate(
    Got = case_when(
      Pos <= 4 ~ "Champions League (CL)",
      Pos %in% c(5:7) ~ "Europe League (EL)",
      Pos %in% (8:17) ~ "Domestic Only",
      Pos >= 18 ~ "Relegated"
    ), 
    Got2016 = if_else(team == "Manchester United", 
                      "Champions League (CL)", 
                      Got)
  )

theme_set(theme_minimal())

clr.black <- "#5E5A59"

clr.season <- c(
  "#D283B2",
  "#5EC6A3",
  "#F4C283",
  #"#EF6653",
  #"#009EA7", 
  "#00635F"  
)

pl2016gg <- ggplot(pl2016, aes(x = GD, y = P, color = Got2016)) + 
  geom_point(size = 4) +
  scale_x_continuous(
    limits = c(-60, 60), 
    breaks = seq(-60, 60, 15)
  ) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) + 
  geom_text_repel(
    aes(label = team), 
    fontface = "bold", 
    size = 4.5, 
    force = 2, 
    color = clr.black, 
    point.padding = unit(0.2, "cm")
  ) + 
  geom_vline(xintercept = -60, size = 0.6) + 
  geom_hline(yintercept = 0, size = 0.6) + 
  labs(
    x = "# of Goal Differences",
    y = "# of Points", 
    title = "Premier League Season 2016/17 Last Standing"
  ) + 
  theme(
    text = element_text(family = "Roboto", color = clr.black), 
    axis.text = element_text(size  = 12), 
    panel.grid.minor = element_blank(), 
    legend.position = c(0.1,0.9),
    legend.justification = c(0,1), 
    legend.direction = "vertical",  
    legend.title = element_text(size = 11, face = "bold"), 
    legend.text = element_text(size = 10, face = "bold"),
    legend.background = element_rect(size = 0.7, fill = NULL),
    plot.title = element_text(size = 16, face = "bold"), 
    axis.title = element_text(size = 14)
  ) + 
  scale_color_manual(values = clr.season) + 
  guides(
    color = guide_legend(
      keywidth = 1.5, 
      ncol = 2, 
      nrow = 2, 
      title = "What do they got for next season?"
    )
  )

ggsave(
  plot  = pl2016gg, 
  filename = "EPL_1617_38.png", 
  device = "png", dpi = 216,
  units = "cm", 
  width = 24, 
  height = 15
)



# Lihat bagaimana Tim Top 7 bergerak pada Season '16/17 
top_7 <- c("Chelsea", "Tottenham Hotspur", "Arsenal", 
           "Liverpool", "Everton", 
           "Manchester United", "Manchester City")

top_7_color 
c(
  "" # Chelsea Blue 
  "" # Everton Blue 
  "" # Man City Light Blue 
  "" # Spurs White Gray 
  "" # Arsenal  Red 
  "" # Liverpool Red 
  "" # Man United Red 
)

by_date %>% 
  filter(team %in% top_7) %>% 
  group_by(team) %>% 
  mutate(P = cumsum(P)) %>% 
  ggplot(aes(x = Date, y = P)) + 
  geom_line(aes(color = team), show.legend = FALSE, size = 1) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) 
  geom_text_repel(
    aes(label = team), 
    fontface = "bold", 
    size = 4.5, 
    force = 2, 
    color = clr.black, 
    point.padding = unit(0.2, "cm")
  )
  
  
  
result_color <-  c(
  "#29ABA4", # Green Win
  "#A8A8A8", # Gray Draw
  "#EC1B4B" # Red Lose
)

result_label <-  c(
  "W", # Green 
  "D", # Gray
  "L" # Red
)

theme_set(theme_light())

pl2016_timeline <- by_date %>% 
  mutate(
    result = case_when(
      P == 3 ~ "W",
      P == 1 ~ "D",
      P == 0 ~ "L"
    ),
    result = ordered(result, levels = c("W", "D", "L"))
  ) %>% 
  filter(team %in% top_7) %>% 
  ggplot(aes(x = Date, y = team)) + 
  geom_point(size = 5, aes(color = result), alpha = 0.8) +
  scale_color_manual(values = result_color) + 
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b '%y") + 
  labs(
    title = "Premier League '16/17 Top Teams Result Timeline", 
    color = "Result"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 12),
    legend.position = "top"
  )

ggsave(
  plot = pl2016_timeline, 
  filename = "pl2016_timeline.png", 
  units = "cm", 
  dpi = 200, 
  width = 24, 
  height = 10
)

# Import data

library(tidyverse)
grand_slams <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")

# Fix grand_slam strings, needed for facet strip text
# (didn't pipe nicely, so needs to be put up front)
grand_slams$grand_slam <- str_replace(grand_slams$grand_slam, '_', ' ') %>% 
    str_to_title() %>%
    str_replace('Us', 'US') 

# Prep data for plot
grand_slams <- grand_slams %>% 
    group_by(name, gender) %>% 
    mutate(wins_all = n()) %>%
    group_by(name, gender, grand_slam, wins_all) %>% 
    summarize(wins_per_gs = n()) %>% 
    group_by(grand_slam) %>% 
    top_n(10, wt = wins_all)

# Plot
library(wesanderson)
library(extrafont)
grand_slams %>% 
    ggplot(aes(x = reorder(name, wins_all), y = wins_per_gs, fill = grand_slam)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values = wes_palette("Chevalier1")) +
    coord_flip() +
    labs(y = 'Championship Titles',
         title = 'Grand Slam Singles Championships',
         subtitle = 'Top 10 players, by total Grand Slam Wins (1968 - Present)',
         caption = 'Chart: @SamProhaska\nData Source: Wikipedia')
    theme(
        plot.background = element_rect(fill = '#fbf8f4'),
        text = element_text(family = 'Raleway', color = '#34495e'),
        axis.title.x = element_text(color = '#34495e', size = 12),
        axis.ticks.y = element_line(color = '#34495e', size = 0.1),
        axis.line.y = element_line(color = '#34495e', size = 0.25),
        axis.text = element_text(color = '#34495e', size = 10),
        axis.ticks.x = element_line(color = '#34495e', size = 0.25),
        axis.line.x = element_line(color = '#34495e', size = 0.25),
        legend.position = "none",
        panel.grid.major.y = element_line(color = '#34495e', size = 0.1),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0, color = '#34495e'),
        plot.subtitle = element_text(hjust = 0, color = '#34495e', size = 10),
        plot.caption = element_text(color = '#34495e', size = 8, face = 'italic')
    ) +
    facet_grid(cols = vars(grand_slam),
               labeller = labeller(gs_names)) +
    theme(strip.text.x = element_text(color = "#34495e"),
          strip.background = element_rect(fill="#fbf8f4"))
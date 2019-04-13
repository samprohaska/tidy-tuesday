# Import data

library(tidyverse)
bike_traffic <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv",
                         col_types = cols(ped_count = col_double())
)

# A quick glance at the format: 

bike_traffic %>% 
    filter(crossing == 'MTS Trail') %>%
    head(5)

## frequency of `0` values per crossing for pedestrian sensors
df_nil <- bike_traffic %>%     # pipe data
    filter(ped_count == 0) %>%   # filter for `0` values
    group_by(crossing) %>%       # run function per crossing
    count() %>%                  # total the number of rows 
    rename(ped_nil = n)          # rename column name

## frequency of `0` values/crossing for bike sensors, join with pedestrian table
bike_traffic %>%
    filter(bike_count == 0) %>%
    group_by(crossing) %>%
    count() %>%
    rename(bike_nil = n) %>%
    full_join(df_nil, by = "crossing")

# Data wrangling & analysis

## count NA in bike counter, by crossing
bike_na <- bike_traffic %>%
    filter(is.na(bike_count) == TRUE) %>%
    group_by(crossing) %>%
    count() %>%
    rename(bike_na = n)

## count NA in ped counter, by crossing, then join with bike table
df_na <- bike_traffic %>%
    filter(is.na(ped_count) == TRUE) %>%
    group_by(crossing) %>%
    count() %>%
    rename(ped_na = n) %>%
    full_join(bike_na, by = "crossing") # join w/ bike data

## join with count 'n' of all observations
df_na <- bike_traffic %>%
    group_by(crossing) %>%
    count() %>%
    full_join(df_na, by = "crossing")

## calc NA frequency by counter and type (ped, bike)
df_na <- df_na %>% 
    mutate(
        ped_na_perc = ped_na / n,
        bike_na_perc = bike_na / n
    )
df_na

# Plotting
library(wesanderson)
library(ggthemes)
library(extrafont)

df_na %>%
    select(crossing, ped_na_perc, bike_na_perc) %>%
    gather(counter_type, perc, -crossing) %>% #
    filter(perc < 1) %>% # Drops non-existent ped counters with 100% NA rate
    ggplot(aes(
        x = str_wrap(reorder(crossing, perc), 20), # Wrap axis text to save space
        y = 100 * perc,
        group = counter_type,
        fill = counter_type)
    ) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
        title = "Seattle Bike & Pedestrian Traffic Counters",
        subtitle = "Signal error rate, by counter location",
        caption = "Data Source: Seattle DOT",
        x = NULL,
        y = "Non-response rate (%)"
    ) +
    scale_fill_manual(
        values = wes_palette("Royal1", n = 2),
        labels = c("Bike", "Pedestrian"),
        name = "Counter Type"
    ) +
    theme_tufte() +
    geom_rangeframe() +
    scale_color_manual() +
    coord_flip() +
    theme(
        text = element_text(family = 'Raleway'),
        plot.caption = element_text(face = "italic")
    )

ggsave('seattle_na.png', width = 9, height = 6)
# Import data

library(tidyverse)

bike_traffic <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv",
                         col_types = cols(ped_count = col_double())
)

# Data wrangling & analysis

## count NA in bike counter, by crossing
bike_na <- bike_traffic %>%
    filter(is.na(bike_count) == TRUE) %>%
    group_by(crossing) %>%
    count() %>%
    rename(bike_na = n)

## count NA in ped counter, by crossing
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
df_na <- df_na %>% mutate(
    ped_na.perc = ped_na / n,
    bike_na.perc = bike_na / n
)

# Ploting
library(wesanderson)
library(ggthemes)

df_na %>%
    select(crossing, ped_na.perc, bike_na.perc) %>%
    gather(counter_type, perc, -crossing) %>% #
    filter(perc < 1) %>% # Drops non-existent ped counters with 100% NA rate
    ggplot(aes(x = reorder(crossing, perc), y = 100 * perc, group = counter_type, fill = counter_type)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_tufte() +
    geom_rangeframe() +
    scale_color_manual() +
    coord_flip() +
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
    theme(plot.caption = element_text(face = "italic"))
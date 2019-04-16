# Import data

library(tidyverse)
women_research <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv")

library(stringr)
women_research$field <-
  str_replace_all(women_research$field, 'Women inventores', 'Patent applications') %>%
  str_wrap(20)

# Plot 1: Grouped by field

library(plotly)

women_research %>%
  plot_ly(x = ~percent_women * 100, y = ~field, color = ~country, type = 'bar') %>%
  layout(title = 'Share of published women researchers, by field (2011-2015)',
         barmode = 'group',
         xaxis = list(title = "Women's representation in academic publishing (2011-2015)",
                      ticksuffix = "%",
                      range = c(0,100)),
         yaxis = list(title = ''),
         font = list(family = 'Raleway',
                     color = '#34495e'),
         annotations = list(
           text = 'Data: Elsevier, via The Economist',
           x = 80,
           y = -0.4,
           showarrow = FALSE
         ))

# Plot 2: Grouped by country

women_research %>%
  plot_ly(x = ~percent_women * 100, y = ~country, color = ~field, type = 'bar') %>%
  layout(title = "Women's representation in academic publishing (2011-2015)",
         barmode = 'group',
         xaxis = list(title = 'Women, as % of total published authors',
                      ticksuffix = "%",
                      range = c(0,100)),
         yaxis = list(title = ''),
         font = list(family = 'Raleway',
                     color = '#34495e'),
         annotations = list(
           text = 'Data: Elsevier, via The Economist',
           x = 80,
           y = -0.4,
           showarrow = FALSE
         ))

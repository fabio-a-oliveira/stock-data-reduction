# Housekeeping -----------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(gganimate)

source("declaration_of_functions.R")

options(digits = 6)

# Create mock stock data -------------------------------------------------------

stock <- rstock(duration = 5 * 240,
                exp_yield = 1 ^ (1/240),
                se_yield = 1.5 / 100)
  
# Tests ------------------------------------------------------------------------

pivot_points_linear <- find.pivot(stock$day,
                                  stock$value,
                                  tolerance = .1,
                                  mode = "Linear")

pivot_points_recursive <- find.pivot(stock$day,
                                     stock$value,
                                     tolerance = .1,
                                     max.depth = 10,
                                     mode = "Recursive")

# Plot - algo comparison -------------------------------------------------------

stock_line1 <- pivot_points_linear
stock_line2 <- pivot_points_recursive

stock %>% ggplot(aes(x = day, y = value)) +
  geom_ribbon(data = stock_line1,
              mapping = aes(x = pivot,
                            ymin = value*0.9,
                            ymax = value*1.1),
              fill = 'blue', alpha = .05) +
  geom_ribbon(data = stock_line2,
              mapping = aes(x = pivot,
                            ymin = value*0.9,
                            ymax = value*1.1),
              fill = 'red', alpha = .05) +
  geom_line(alpha=.2) + 
  geom_line(data = stock_line1,
            mapping = aes(x = pivot, y = value),
            color = 'blue',size=1,alpha=.5) +
  geom_line(data = stock_line2,
             mapping = aes(x = pivot, y = value),
             color = 'red',size=1,alpha=.5) +
  geom_point(data = stock_line1,
             mapping = aes(x = pivot, y = value),
             color = 'blue',size=3,alpha=.5) +
  geom_point(data = stock_line2,
             mapping = aes(x = pivot, y = value),
             color = 'red',size=3,alpha=.5) +
  scale_x_continuous(breaks = seq(0,duration,240),
                     minor_breaks = seq(0,duration,60)) +
  scale_y_continuous(labels = scales::percent)

pivot_points1
pivot_points2
cat(length(pivot_points1),length(pivot_points2))
cat(pivot_points1 %in% pivot_points2 %>% mean(),pivot_points2 %in% pivot_points1 %>% mean())
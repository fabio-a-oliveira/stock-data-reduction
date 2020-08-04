# Housekeeping -----------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)

source("declaration_of_functions.R")

options(digits = 6)

# Create mock stock data -------------------------------------------------------

stock <- rstock(duration = 5 * 240,
                exp_yield = 1 ^ (1/240),
                se_yield = 1.5 / 100)
  
# Tests ------------------------------------------------------------------------

pivot_points_sequential <- find.pivot(stock$day,
                                  stock$value,
                                  tolerance = .1,
                                  mode = "Sequential")

pivot_points_recursive <- find.pivot(stock$day,
                                     stock$value,
                                     tolerance = .1,
                                     max.depth = 10,
                                     mode = "Recursive")

# Plot - algo comparison -------------------------------------------------------

stock_line1 <- pivot_points_sequential
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
  scale_x_continuous(breaks = seq(0,max(stock$day),240),
                     minor_breaks = seq(0,max(stock$day),60)) +
  scale_y_continuous(labels = scales::percent)


# Animation - sequential algo --------------------------------------------------

stock <- 
  mutate(stock,
         is.pivot = FALSE)

pivot_points_tidy <- 
  pivot_points_sequential %>%
  mutate(day = pivot,
         is.pivot = TRUE) %>%
  select(-inclination,-pivot)
  


tidy_stock <- full_join(stock,pivot_points_tidy,c("day","value"))

tidy_stock <- 
  bind_rows(stock,pivot_points_tidy) %>%
  arrange(day) %>%
  group_by(is.pivot)




p<- 
  
  tidy_stock %>% 
  ggplot(aes(x = day,
             y = value,
             color = is.pivot),) +
  geom_line(show.legend = FALSE) +
  scale_color_manual(values = c('blue','black'))

  transition_reveal(day)


p

summary(p)


# p


# anim <- animate(p,
#                 nframes = 120,
#                 duration = 10,
#                 end_pause = 20,
#                 renderer = gifski_renderer())
# 
# anim_save(filename = "animation.gif")


# Animation using renderer -----------------------------------------------------

tidy_stock %>% 
  ggplot(aes(x = day,
             y = value,
             color = is.pivot,
             alpha = is.pivot,
             size = is.pivot)) +
  geom_line(show.legend = FALSE) +
  scale_color_manual(values = c('blue','red')) +
  scale_alpha_manual(values = c(.2,1)) +
  scale_size_manual(values = c(.5,1))

ggsave(filename = "figure001.png",
       path = "images",
       dpi = "screen")

tidy_stock %>% 
  ggplot(aes(x = day,
             y = value,
             color = is.pivot,
             alpha = is.pivot,
             size = is.pivot)) +
  geom_line(show.legend = FALSE) +
  scale_color_manual(values = c('blue','red')) +
  scale_alpha_manual(values = c(.5,1)) +
  scale_size_manual(values = c(1,.5))

ggsave(filename = "figure002.png",
       path = "images",
       dpi = "screen")


gifski(png_files = c("images/figure001.png",
                     "images/figure002.png"),
       gif_file = "images/animation.gif",
       delay = 1,
       loop = TRUE,
       progress = TRUE)



for (i in 1:nrow(stock)) {
  
}

# Quick test - max depth -------------------------------------------------------

# stock <- rstock(240)

pp <- find.pivot(stock$day,stock$value,mode="Recursive",max.depth = 3)

pp


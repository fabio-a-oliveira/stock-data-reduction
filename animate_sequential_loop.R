# Housekeeping -----------------------------------------------------------------

# load libraries
library(tidyverse)
library(ggplot2)
library(gifski)

# runs script with the definition of the pivot points functions and the function
# that creates random stock data
source("declaration_of_functions.R")

# Create mock stock data -------------------------------------------------------

# create stock data
stock <- rstock(duration = 20 * 240,
                exp_yield = 1 ^ (1/240),
                se_yield = 1.5 / 100)

# scale data to begin and end at same value
stock$value <- 
  stock$value / 
  seq(1,stock$value[nrow(stock)],length.out = nrow(stock))

# Create individual images -----------------------------------------------------

# parameters for the animation
days_per_frame <- 6
num_frames <- nrow(stock)/days_per_frame %>% ceiling()
filenames <- rep("",num_frames)
days_per_figure <- 240

# repeat beginning of data at the end (makes loop connect)
stock_first_figure <- stock %>% 
  filter(day <= 2*days_per_figure) %>%
  mutate(day = day + nrow(stock))
stock_loop <- bind_rows(stock,stock_first_figure) %>% arrange(day)

# creation of individual picture frames
for (i in 1:num_frames) {
  
  # print progress status
  print(paste("Creating frame #",
              i," out of ", num_frames,
              " (",floor(100*i/num_frames),"%)",
              sep=""))
  
  # day for this particular frame (calculates 2 full screens before beginning)
  active_day <- 2*days_per_figure + (i-1)*days_per_frame
  
  # each frame uses data from the beginning up to the frame day
  stock_frame <-
    stock_loop %>%
    mutate(is.pivot = FALSE) %>%
    filter(day <= active_day)
  
  # calculates the pivot points for the frame day
  pivot_points <- 
    find.pivot(stock_frame$day,
               stock_frame$value,
               tolerance = .1,
               mode = "Sequential") %>%
    mutate(day = pivot,
           is.pivot = TRUE) %>%
    select(-inclination, - pivot)
  
  # joins frame data and pivot points in tidy format
  tidy_stock <- 
    bind_rows(stock_frame,pivot_points) %>%
    arrange(day)
  
  # gets last 2 pivot points (will use to calculate the current pivot candidate)
  current_segment_pivots <- 
    pivot_points %>%
    mutate(rank = rank(-day)) %>%
    filter(rank %in% 1:2) %>%
    select(day)
  
  # gets the stock data in the interval corresponding to the last pivot points
  current_segment_stock <- 
    stock_loop %>%
    filter(day >= current_segment_pivots[1,] & day <= current_segment_pivots[2,])
  
  # identify which point in the segment is the candidate for new pivot point
  # (point that further deviates from the straight line segment)
  pivot_candidate <- 
    current_segment_stock %>%
    mutate(line_segment = seq(current_segment_stock$value[1],
                              current_segment_stock$value[n()],
                              length.out = n()),
           deviation = abs((value-line_segment)/line_segment)) %>%
    filter(deviation == max(deviation))
  
  # create image for this particular frame
  p <- 
  tidy_stock %>% 
    ggplot(aes(x = day,
               y = value,
               color = is.pivot,
               alpha = is.pivot,
               size = is.pivot)) +
    # creates and formats stock data and line connecting pivot points
    geom_line(show.legend = FALSE) +
    scale_color_manual(values = c('black','red')) +
    scale_alpha_manual(values = c(1,.7)) +
    scale_size_manual(values = c(1,1)) +
    # draws points at each pivot point
    geom_point(data = filter(tidy_stock,is.pivot==TRUE),
               inherit.aes = FALSE,
               mapping = aes(x = day,
                             y = value),
               show.legend = FALSE,
               size = 4,
               color = 'red') +
    # inserts blue vertical bar showing the current pivot candidate
    geom_errorbar(data = pivot_candidate,
               inherit.aes = FALSE,
               mapping = aes(x = day,
                             ymin = min(c(value,line_segment)),
                             ymax = max(c(value,line_segment))),
               color = 'blue',
               size = 2,
               width = 1) +
    # draws green band with fixed width around line connecting pivot points
    geom_ribbon(data = filter(tidy_stock,is.pivot==TRUE),
                inherit.aes = FALSE,
                mapping = aes(x = day,
                              ymin = value * (1-.1),
                              ymax = value * (1+.1)),
                alpha = .05,
                fill = 'green',
                outline.type = 'both',
                color = 'green',
                linetype = 3) + 
    # removes grid
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    # defines scale limits
    coord_cartesian(xlim = c(active_day-days_per_figure,
                             active_day),
                    ylim = c(min(stock$value)-.12, # for fixed y scale: min(stock$value)-.12, for moving y scale: stock_frame$value[active_day]-.3, 
                             max(stock$value)+.12)) + # for fixed y scale: max(stock$value)+.12), for moving y scale: stock_frame$value[active_day]+.3)
    # adds labels
    labs(title = "Sequential algorithm",
         subtitle = "at each new value, decides whether or not to set new pivot point to keep all values inside tolerance",
         x = "timestamp \n should be any unit of time that captures high frequency behavior",
         y = "value \n stock price or any continuous variable")

  # add current frame to list of file names
  filenames[i] <-
    paste("figure",
          formatC(i,width=4,format="d",flag="0"),
          ".png",
          sep="")
  
  # saves file with current frame
  ggsave(plot = p,
         filename = filenames[i],
         path = "images",
         dpi = "screen",
         width = 20,
         height = 20,
         unit = "cm")
}

# Create gif from images -------------------------------------------------------

# renders gif from list of created files
gifski(png_files = file.path("images",filenames),
       gif_file = tempfile(pattern = "animation_sequential_loop_", 
                           fileext = ".gif", 
                           tmpdir = "images"),
       delay = .01,
       loop = TRUE,
       progress = TRUE)

# removes files with individual images
file.remove(file.path("images",filenames))
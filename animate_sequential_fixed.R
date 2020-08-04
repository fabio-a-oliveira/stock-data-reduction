# Housekeeping -----------------------------------------------------------------

# load libraries
library(tidyverse)
library(ggplot2)
library(gifski)

# runs script with the definition of the pivot points functions and the function
# that creates random stock data
source("declaration_of_functions.R")

# Image parameters -------------------------------------------------------------

num_stocks <- 20
num_days <- 2 * 240
days_per_frame <- 6 # needs to be a divisor of num_days
frames_per_stock <- num_days / days_per_frame %>% ceiling() # not including repetition of first and last frames
num_frames_start <- 1
num_frames_end <- 10
filenames <- 
  rep("",(frames_per_stock+1 + num_frames_start-1 + num_frames_end-1)*num_stocks)

# Create individual images -----------------------------------------------------

# for each stock to be animated
for (stock_num in 1:num_stocks) {

  stock <- rstock(duration = num_days,
                  exp_yield = 1 ^ (1/240),
                  se_yield = 1.5 / 100)
  
  # for each frame
  for (frame_num in 1:(frames_per_stock+1 + num_frames_start-1 + num_frames_end-1)) {
    
    active_day <- (frame_num-1) * days_per_frame
    if (active_day > num_days) {active_day <- num_days}
    
    # calculate tidy data frame (stock + pivot points) to plot
    # 1st frame (no pivot points)
    if (active_day == 0) {
      tidy_stock <- stock
      
      # create plot
      p <-
        tidy_stock %>%
        ggplot(aes(x = day,
                   y = value)) +
        geom_line(show.legend = FALSE,
                  color = 'black',
                  size = 1) +
        scale_x_continuous(breaks = seq(0,num_days,60),
                           minor_breaks = seq(0,num_days,20),
                           limits = c(0,num_days)) +
        scale_y_continuous(breaks = seq(0,3,.25),
                           minor_breaks = seq(0,3,.05),
                           limits = c(min(stock$value) * .85,
                                      max(stock$value) * 1.15)) +
        # adds labels
        labs(title = "Sequential algorithm",
             subtitle = "at each new value, decides whether or not to set new pivot point to keep all values inside tolerance",
             x = "timestamp \n should be any unit of time that captures high frequency behavior",
             y = "value \n stock price or any continuous variable")
    } 
    # 2nd frame onward
    else {
      # include is.pivot column
      stock <-
        stock %>%
        mutate(is.pivot = FALSE)
      
      # create stock info up to current day
      stock_active_day <- 
        stock %>%
        filter(day <= active_day)
      
      # calculates the pivot points for the frame day
      pivot_points <- 
        find.pivot(stock_active_day$day,
                   stock_active_day$value,
                   tolerance = .1,
                   mode = "Sequential") %>%
        mutate(day = pivot,
               is.pivot = TRUE) %>%
        select(-inclination, - pivot) %>%
        filter(day <= active_day)
      
      # gets last 2 pivot points (will use to calculate the current pivot candidate)
      current_segment_pivots <- 
        pivot_points %>%
        mutate(rank = rank(-day)) %>%
        filter(rank %in% 1:2) %>%
        select(day)
      
      # gets the stock data in the interval corresponding to the last pivot points
      current_segment_stock <- 
        stock_active_day %>%
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
      
      # joins frame data and pivot points in tidy format
      tidy_stock <- 
        bind_rows(stock,pivot_points) %>%
        arrange(day)
      
      # create plot
      p <-
        tidy_stock %>%
        ggplot(aes(x = day,
                   y = value,
                   color = is.pivot,
                   alpha = is.pivot,
                   size = is.pivot)) +
        geom_line(show.legend = FALSE) +
        scale_color_manual(values = c('black','red')) +
        scale_alpha_manual(values = c(1,.7)) +
        scale_size_manual(values = c(1,1)) +
        scale_x_continuous(breaks = seq(0,num_days,60),
                           minor_breaks = seq(0,num_days,20),
                           limits = c(0,num_days)) +
        scale_y_continuous(breaks = seq(0,3,.25),
                           minor_breaks = seq(0,3,.05),
                           limits = c(min(stock$value) * .85,
                                      max(stock$value) * 1.15)) +
        # draws points at each pivot point
        geom_point(data = filter(tidy_stock,is.pivot==TRUE),
                   inherit.aes = FALSE,
                   mapping = aes(x = day,
                                 y = value),
                   show.legend = FALSE,
                   size = 4,
                   color = 'red') +
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
        # inserts blue vertical bar showing the current pivot candidate
        geom_errorbar(data = pivot_candidate,
                      inherit.aes = FALSE,
                      mapping = aes(x = day,
                                    ymin = min(c(value,line_segment)),
                                    ymax = max(c(value,line_segment))),
                      color = 'blue',
                      size = 2,
                      width = 1) +
        # adds labels
        labs(title = "Sequential algorithm",
             subtitle = "at each new value, decides whether or not to set new pivot point to keep all values inside tolerance",
             x = "timestamp \n should be any unit of time that captures high frequency behavior",
             y = "value \n stock price or any continuous variable")
    }
    
    # add current frame to list of file names
    filenames[(stock_num-1)*(frames_per_stock+1 + num_frames_start-1 + num_frames_end-1) + frame_num] <-
      paste("stock",
            formatC(stock_num,width=3,format="d",flag="0"),
            "frame",
            formatC(frame_num,width=4,format="d",flag="0"),
            ".png",
            sep="")
    
    # print progress status
    print(paste("Creating frame #",
                frame_num," of ", frames_per_stock+1 + num_frames_start-1 + num_frames_end-1,
                " for stock #", stock_num,
                " of ", num_stocks,
                sep=""))
    
    # saves file with current frame
    ggsave(plot = p,
           filename =
             filenames[(stock_num-1)*(frames_per_stock+1 + num_frames_start-1 + num_frames_end-1) + frame_num],
           device = "png",
           path = "images",
           dpi = "screen",
           width = 20,
           height = 20,
           unit = "cm")
  }
}

# Create gif from images -------------------------------------------------------

# renders gif from list of created files
gifski(png_files = file.path("images",filenames),
       gif_file = tempfile(pattern = "animation_sequential_fixed_", 
                           fileext = ".gif", 
                           tmpdir = "images"),
       delay = .01,
       loop = TRUE,
       progress = TRUE)

# removes files with individual images
file.remove(file.path("images",filenames))



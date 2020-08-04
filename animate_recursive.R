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
max_depth <- 10
num_days <- 4 * 240
frames_per_stock <- max_depth + 2

filenames <- rep("" , frames_per_stock * num_stocks)

plot_title <- "Recursive algorithm"
plot_subtitle <- "For each segment, define pivot at point further from reference line, then repeat \nrecursively for each new segment, until all points are within tolerance"
plot_x_label <- "timestamp \n should be any unit of time that captures high frequency behavior"
plot_y_label <- "value \n stock price or any continuous variable"

# Create individual frames -----------------------------------------------------

for (current_stock in 1:num_stocks){
  
  # Create mock stock data
  stock <- rstock(duration = num_days,
                  exp_yield = 1 ^ (1/240),
                  se_yield = 1.5 / 100)
  
  for (current_frame in 1:frames_per_stock){
    
    if (current_frame == 1) {
      
      # define object to be plotted
      tidy_stock <- stock
      
      # create plot
      p <-
        tidy_stock %>%
        ggplot(aes(x = day, y = value),
               color = 'black',
               size = 1) +
        geom_line(size = 1) +
        scale_x_continuous(breaks = seq(-240,num_days+240,60),
                           minor_breaks = seq(-240,num_days+240,20),
                           limits = c(0,num_days)) +
        scale_y_continuous(breaks = seq(0,10,.25),
                           minor_breaks = seq(0,10,.05),
                           limits = c(min(stock$value) * .85,
                                      max(stock$value) * 1.15)) +
        labs(title = plot_title,
             subtitle = plot_subtitle,
             x = plot_x_label,
             y = plot_y_label)
      
    } else {
      
      # include is.pivot column
      stock <-
        stock %>%
        mutate(is.pivot = FALSE)
      
      # calculates the pivot points for the current depth level
      pivot_points <- 
        find.pivot(stock$day,
                   stock$value,
                   tolerance = .1,
                   mode = "Recursive",
                   max.depth = current_frame-2) %>%
        mutate(day = pivot,
               is.pivot = TRUE) %>%
        select(-inclination, - pivot)
      
      # defines object to be plotted (joins stock data and pivot points in tidy format)
      tidy_stock <- 
        bind_rows(stock,pivot_points) %>%
        arrange(day)
      
      # create plot
      p <-
        tidy_stock %>%
        ggplot(aes(x = day, y = value,
                   color = is.pivot,
                   size = is.pivot,
                   alpha = is.pivot)) +
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
        # defines limits and breaks to scales
        scale_x_continuous(breaks = seq(-240,num_days+240,60),
                           minor_breaks = seq(-240,num_days+240,20),
                           limits = c(0,num_days)) +
        scale_y_continuous(breaks = seq(0,10,.25),
                           minor_breaks = seq(0,10,.05),
                           limits = c(min(stock$value) * .85,
                                      max(stock$value) * 1.15)) +
        # defines labels
        labs(title = plot_title,
             subtitle = plot_subtitle,
             x = plot_x_label,
             y = plot_y_label)
      
      
    } # if (current_depth == 0)
    
    # add current frame to list of file names
    filenames[(current_stock-1)*frames_per_stock + current_frame] <-
      paste("stock",
            formatC(current_stock,width=3,format="d",flag="0"),
            "frame",
            formatC(current_frame,width=4,format="d",flag="0"),
            ".png",
            sep="")
    
    # print progress status
    print(paste("Creating frame #",
                current_frame," of ", frames_per_stock,
                " for stock #", current_stock,
                " of ", num_stocks,
                sep=""))
    
    # saves file with current frame
    ggsave(plot = p,
           filename =
             filenames[(current_stock-1)*frames_per_stock + current_frame],
           device = "png",
           path = "images",
           dpi = "screen",
           width = 20,
           height = 20,
           unit = "cm")
    
  } # for (current_depth in 0:max_depth)
  
} # for (current_stock in 1:num_stocks)


# Render gif animation ---------------------------------------------------------

# renders gif from list of created files
gifski(png_files = file.path("images",filenames),
       gif_file = tempfile(pattern = "animation_recursive_", 
                           fileext = ".gif", 
                           tmpdir = "images"),
       delay = 1,
       loop = TRUE,
       progress = TRUE)

# removes files with individual images
file.remove(file.path("images",filenames))
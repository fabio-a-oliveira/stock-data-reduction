# Housekeeping -----------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)

source("declaration_of_functions.R")

options(digits = 6)

# Create mock stock data -------------------------------------------------------

stock <- rstock(duration = 20 * 240,
                exp_yield = 1 ^ (1/240),
                se_yield = 1.5 / 100)

stock$value <- 
  stock$value / 
  seq(1,stock$value[nrow(stock)],length.out = nrow(stock))

# Create individual images -----------------------------------------------------

days_per_frame <- 6
num_frames <- nrow(stock)/days_per_frame %>% ceiling()
filenames <- rep("",num_frames)

days_per_figure <- 240
stock_first_figure <- stock %>% 
  filter(day <= days_per_figure) %>%
  mutate(day = day + nrow(stock))
stock_loop <- bind_rows(stock,stock_first_figure) %>% arrange(day)

for (i in 1:num_frames) {
  
  print(paste("Creating frame #",
              i," out of ", num_frames,
              " (",floor(100*i/num_frames),"%)",
              sep=""))
  
  active_day <- days_per_figure + (i-1)*days_per_frame
  
  stock_frame <-
    stock_loop %>%
    mutate(is.pivot = FALSE) %>%
    filter(day <= active_day)
  
  pivot_points <- 
    find.pivot(stock_frame$day,
               stock_frame$value,
               tolerance = .1,
               mode = "Sequential") %>%
    mutate(day = pivot,
           is.pivot = TRUE) %>%
    select(-inclination, - pivot)
  
  tidy_stock <- 
    bind_rows(stock_frame,pivot_points) %>%
    arrange(day)
  
  current_segment_pivots <- 
    pivot_points %>%
    mutate(rank = rank(-day)) %>%
    filter(rank %in% 1:2) %>%
    select(day)
  
  current_segment_stock <- 
    stock_loop %>%
    filter(day >= current_segment_pivots[1,] & day <= current_segment_pivots[2,])
    
  pivot_candidate <- 
    current_segment_stock %>%
    mutate(line_segment = seq(current_segment_stock$value[1],
                              current_segment_stock$value[n()],
                              length.out = n()),
           deviation = abs((value-line_segment)/line_segment)) %>%
    filter(deviation == max(deviation))
  
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
    geom_point(data = filter(tidy_stock,is.pivot==TRUE),
               inherit.aes = FALSE,
               mapping = aes(x = day,
                             y = value),
               show.legend = FALSE,
               size = 4,
               color = 'red') +
    geom_errorbar(data = pivot_candidate,
               inherit.aes = FALSE,
               mapping = aes(x = day,
                             ymin = min(c(value,line_segment)),
                             ymax = max(c(value,line_segment))),
               color = 'blue',
               size = 1) +
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
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    coord_cartesian(xlim = c(active_day-days_per_figure,
                             active_day),
                    ylim = c(min(stock$value)-.12,
                             max(stock$value)+.12)) +
    labs(title = "Sequential algorithm",
         subtitle = "at each new value, decides whether or not to set new pivot point to keep all values inside tolerance",
         x = "timestamp \n should be any unit of time that captures high frequency behavior",
         y = "value \n stock price or any continuous variable")

  filenames[i] <-
    paste("figure",
          formatC(i,width=4,format="d",flag="0"),
          ".png",
          sep="")
  
  ggsave(plot = p,
         filename = filenames[i],
         path = "temp",
         dpi = "screen",
         width = 20,
         height = 10,
         unit = "cm")
}

# Create gif from images -------------------------------------------------------

# gifski(png_files = file.path("temp",filenames),
#        gif_file = "images/animation.gif",
#        delay = .01,
#        loop = TRUE,
#        progress = TRUE)

gifski(png_files = file.path("temp",filenames),
       gif_file = tempfile(pattern = "animation_", 
                           fileext = ".gif", 
                           tmpdir = "images"),
       delay = .01,
       loop = TRUE,
       progress = TRUE)

# RESOLVER ESSE NEGÓCIO DE DIRETÓRIO TEMPORÁRIO, USAR O IMAGES E FILE.REMOVE APÓS TERMINAR

# INCLUIR BARRA DE ERRO ENTRE LINHA VERMELHA E O CANDIDATO NO ÚLTIMO SEGMENTO

# GERAR PRIMEIRO E ÚLTIMO FRAME E CONFIRMAR QUE ESTÃO IGUAIS (COMPATÍVEIS, NA VERDADE)

# Trash ------------------------------------------------------------------------

# prettynum()


# 
# filename <-
#   paste("figure",
#         formatC(i,width=4,format="d",flag="0"),
#         ".png",
#         sep="")

# ggsave(plot = p,
#        filename = "figure%04d.png",
#        path = "images",
#        dpi = "screen")



# geom_label(data = filter(tidy_stock,is.pivot==TRUE),
#            inherit.aes = FALSE,
#            mapping = aes(x = day,
#                          y = value,
#                          label= paste("day: ",
#                                       day,
#                                       "\n price: ",
#                                       round(value,2),
#                                       sep = "")),
# show.legend = FALSE,
# size = 3,
# nudge_x = -60)
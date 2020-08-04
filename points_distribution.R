# Housekeeping -----------------------------------------------------------------

# load libraries
library(tidyverse)
library(ggplot2)

# runs script with the definition of the pivot points functions and the function
# that creates random stock data
source("declaration_of_functions.R")

# Create mock stock data -------------------------------------------------------

number_days <- 1000 * 240
yearly_yield <- 1
se_yield <- 1.5 / 100

stock <- rstock(duration = number_days,
                exp_yield = yearly_yield ^ (1/240),
                se_yield = se_yield)

# Calculate pivot points -------------------------------------------------------

pivot_points <-
  find.pivot(stock$day,
             stock$value,
             mode = "Sequential",
             tolerance = .10)

# Calculate segment length -----------------------------------------------------

pivot_points_end <-
  pivot_points %>%
  slice(2:n())

pivot_points_beginning <- 
  pivot_points %>%
  slice(1:n()-1)

pivot_points <-
  pivot_points %>%
  mutate(length = 0,
         variation = 0)

pivot_delta <-
  data.frame(length = pivot_points_end$pivot - pivot_points_beginning$pivot,
             variation = pivot_points_end$value - pivot_points_beginning$value,
             inclination = pivot_points_end$inclination)

# Plot distribution of length --------------------------------------------------

pivot_delta %>%
  ggplot(aes(x=length)) +
  geom_histogram(binwidth = 1) +
  ggtitle("Length")
  

# Plot distribution of variation -----------------------------------------------

pivot_delta %>%
  ggplot(aes(x=variation)) +
  geom_histogram(binwidth = .001) +
  ggtitle("Variation")

# Plot distribution of inclination ---------------------------------------------

pivot_delta %>%
  ggplot(aes(x=inclination)) +
  geom_histogram(binwidth = .0001) +
  ggtitle("Inclination")

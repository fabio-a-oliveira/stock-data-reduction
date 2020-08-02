# Create mock stock data -------------------------------------------------------

rstock <- function(duration = 1*240, # number of data points
                   exp_yield = 1, # average yield per data point
                   se_yield = 1.5){ # standard error per data point
  
  stock <- data.frame(day = 1:duration,
                      value = c(1,rep(0,duration-1)))
  
  for (day in 2:duration) {
    stock$value[day] = 
      rnorm(1,exp_yield,se_yield) * stock$value[day-1]
  }
  
  stock
}

# Recursive algo ---------------------------------------------------------------

find.pivot.recursive <- function(x, y,
                                 tolerance = .15, 
                                 depth = 1, 
                                 max.depth = Inf) {
  n <- length(x)
  start <- x[1]
  end <- x[n]
  
  segment <- seq(y[1],y[n],length.out = n)
  deviation <- abs((y-segment)/segment)
  pivot_candidate <- which.max(deviation)
  candidate_day <- x[pivot_candidate]
  candidate_deviation <- deviation[pivot_candidate]
  
  if (candidate_deviation >= tolerance) {
    if (depth < max.depth) {
      first_seg <- 
        find.pivot.recursive(x[1:pivot_candidate],
                             y[1:pivot_candidate],
                             tolerance = tolerance,
                             depth = depth+1,
                             max.depth = max.depth) %>%
        .$pivot
      second_seg <- 
        find.pivot.recursive(x[pivot_candidate:n],
                             y[pivot_candidate:n],
                             tolerance = tolerance,
                             depth = depth+1,
                             max.depth = max.depth) %>%
        .$pivot
      if (length(first_seg) > 2) {
        first_seg <- first_seg[2:(length(first_seg)-1)]
      } else {first_seg <- NULL}
      if (length(second_seg) > 2) {
        second_seg <- second_seg[2:(length(second_seg)-1)]
      } else {second_seg <- NULL}
      
      pivot_list <- c(start,first_seg,candidate_day,second_seg,end)
      
    } else {pivot_list <- c(start,candidate_day,end)}
    
  } else {pivot_list <- c(start,end)}
  
  size <- length(pivot_list)
  value_list <- y[x %in% pivot_list]
  inclination_list <- 
    c(value_list[2:size] - value_list[1:(size-1)], value_list[size]-value_list[size-1]) / 
    c(pivot_list[2:size] - pivot_list[1:(size-1)], pivot_list[size]-pivot_list[size-1])
  
  result <- data.frame(pivot = pivot_list,
                       value = value_list,
                       inclination = inclination_list)
  
}

# Sequential algo --------------------------------------------------------------

find.pivot.sequential <- function(x, y, 
                                  tolerance = .15) {
  n <- length(x)
  last.pivot <- 1
  pivot_list <- x[1]
  
  for (day in 3:n) {
    segment <- seq(y[last.pivot],y[day],
                   length.out = day - last.pivot+1)
    values <- y[last.pivot:day]
    timestamp <- x[last.pivot:day]
    deviation <- abs((values-segment)/segment)
    pivot_candidate <- which.max(deviation)
    candidate_day <- timestamp[pivot_candidate]
    candidate_deviation <- deviation[pivot_candidate]
    
    if (candidate_deviation >= tolerance){
      last.pivot <- candidate_day
      pivot_list <- c(pivot_list,candidate_day)
    }
  }
  
  pivot_list <- c(pivot_list,day)
  
  size <- length(pivot_list)
  value_list <- y[x %in% pivot_list]
  inclination_list <- 
    c(value_list[2:size] - value_list[1:(size-1)], value_list[size]-value_list[size-1]) / 
    c(pivot_list[2:size] - pivot_list[1:(size-1)], pivot_list[size]-pivot_list[size-1])
  
  result <- data.frame(pivot = pivot_list,
                       value = value_list,
                       inclination = inclination_list)
}

# Top-level find.pivot() function ----------------------------------------------

find.pivot <- function(x,y,
                       mode = "Sequential",
                       tolerance = .15,
                       depth = 1,
                       max.depth = Inf) {
  if (length(x) != length(y)) {
    warning("'x' and 'y' must be vectors of same length")
    NULL
  } else if (length(x) == 1) {
    result <- data.frame(pivot = x,
                         value = y,
                         inclination = NA)
    result
  } else if (length(x) == 2) {
    result <- data.frame(pivot = x,
                         value = y,
                         inclination = (y[2]-y[1])/(x[2]-x[1]))
  } else if (mode == "Sequential") {
    find.pivot.sequential(x,y,
                      tolerance = tolerance)
  } else if (mode == "Recursive") {
    find.pivot.recursive(x,y,
                         tolerance = tolerance,
                         depth = depth,
                         max.depth = max.depth)
  } else {
    warning("Non recognized 'mode' argument, reverting to 'Sequential'")
    find.pivot.sequential(x,y,
                      tolerance = tolerance)
  }
}


Stock data reduction via identification of “pivot points”
================
Fabio A Oliveira

# TLDR

#### (*Too Long, Didn’t Read or the contents of your typical README.md file*)

This project implements a method for simplifying and reducing time
series data via the identification of “pivot points” (defined here as a
series of points selected from the data which determine a set of line
segments from which the actual data does not deviate more than a given
tolerance).

Two different algorithms are presented for calculating these points,
depending on whether the full time series is available or if the points
are being calculated in real time.

It is assumed that the representation of the actual data via these
“pivot points” is more advantageous for certain applications than the
full data (particularly for stock market price prediction, trend
identification and recognition of patterns).

The following files are contained in the repository:

  - A .R script implementing functions for finding the pivot points and
    for generating randomized stock data for testing.
  - .R scripts for creating animations to illustrate features of each of
    the algorithms used for calculation of the pivot points.
  - An images folder, where the scripts that create animations save the
    temporary .png and the resulting .gif files and from which this
    report uses the images.

If you want to replicate this analysis, you can clone this repository
and either 1) run the declaration\_of\_functions.R script to be able to
use the find.pivot() and rstock() functions or 2) run the animate
scripts to create animations based on randomly generated stock data.

The remainder of this file contains a detailed discussion of the
implementation and characteristics.

# Introduction

## Motivation

Motivation:  
Identify prominent features in a time series  
Motivated by analysis of stock market  
Difficulty in defining explicit buy/sell strategies from raw data, too
noisy and patterns are difficult to identify  
Best characterization of a time series with minimal amount of data
points

3 hypothesis (yet to be confirmed): \* **Easier to identify
relationships:** stocks from the same sector etc \* **More efficient
trend identification:** less noise, bare essentials and prominent
features, algo needs to predict next point, good enough info for
buy/sell decisions, closer to features humans identify and more related
to fundamentalist analysis \* **Evidences market volatility:** histogram
with point density shows periods of intense volatility

# The algos

## Recursive algo

  - 1st version: recursive - features:, limitations:, gif ..

  - Recursive algo:

<!-- end list -->

1.  Define interval under analysis as the full interval between first
    and last available points
2.  Draw reference line segment between the two points
3.  Evaluate deviation between all the values in the segment and the
    reference line
4.  If any point deviates more than the tolerance, the point where the
    highest deviation is found will be defined as a pivot point
5.  For each new pivot point added, run steps 2-4 to recursively
    evaluate the two segments defined by cutting the interval defined in
    step 1 at the new pivot point

## Sequential algo

  - 2nd version: sequential - features:, limitations, gif:..

  - Sequential algo:  

<!-- end list -->

1.  Define interval under analysis (will begin at last pivot point and
    end at current value, if there is no last point, will use the first
    point in the time series)
2.  Draw reference line segment between the two points
3.  Evaluate deviation between all the values in the segment and the
    reference line
4.  If any point deviates more than the tolerance, the point where the
    highest deviation is found will be defined as a pivot point
5.  For each new value, repeat steps 1-4

ilustrar com 2 gifs:  
\* escala estática, ação inteira já mostrada, pontos calculados valor a
valor (como se estivesse analisando uma ação já disponível) \* escala
móvel, gif infinito, ilustrando o funcionamento com novos pontos
chegando

## Choosing the tolerance

Criar visualização com diferentes tolerâncias e mostrar a lista e
quantidade de pontos identificados. Usar faceting para colocar, nas
linhas, diferentes valores de tolerância, e nas colunas, diferentes
ações (opcional, pode ficar muito grande).

Tolerância pode ser adaptada à riqueza de detalhes e à sensibilidade que
se quer ter a features da ação.

# Next steps

  - Run on full or significant portion of stock market history
  - Attempt to identify patterns in volatility and link to significant
    events
  - Attempt to cluster different stocks into sectors by finding
    correlations
  - Attempt to predict next pivot points from historical data (train
    with all, predict with one stock combination of pivot points for
    index & pivot points for sector)
  - Evaluate statistical distribution of distance between pivot points
  - Implement other tolerance functions (absolute, time-dependent,
    adaptive, deviation from an arch/curve/taylor/fourier approximation
    instead of straight line etc)
  - Share functions as package

# Tests

``` r
file <- 
  list.files("./images") %>%
  str_subset("animation_recursive") %>% .[1]

file_full <-
  file.path("./images",file)
```

![](./images/animation_recursive_f2410a13adb.gif)
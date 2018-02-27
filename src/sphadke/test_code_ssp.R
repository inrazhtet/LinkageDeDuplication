#### This is a test code
## Trying out both RecordLinkage and fuzzyjoin

rm(list=ls())
gc()

library(dplyr)
library(fuzzyjoin)
library(RecordLinkage)

## fuzzyjoin

# difference_join
sepal_lengths <- data_frame(Sepal.Length = c(5, 6, 7),
                            Type = 1:3)

sepal_lengths <- data_frame(Sepal.Length = c(5, 6, 7), Type = 1:3)
join1 <- iris %>%
  difference_inner_join(sepal_lengths, max_dist = .5)
join2 <- iris %>%
  difference_left_join(sepal_lengths, max_dist = .5,
                       distance_col = 'dist')
join3 <- iris %>%
  difference_anti_join(sepal_lengths, max_dist = 0.2,
                       distance_col = "distance")
join4 <- iris %>%
  difference_semi_join(sepal_lengths, max_dist = .1)


# distance_join
sepal_lengths <- data_frame(Sepal.Length = c(5, 6, 7),
                            Sepal.Width = 1:3)

join5 <- iris %>%
  distance_inner_join(sepal_lengths, max_dist = 2,
                      method = "euclidean", #this is sqrt(dist1^2 + dist2^2)
                      distance_col = 'dist')

join6 <- iris %>%
  distance_inner_join(sepal_lengths, max_dist = 2,
                      method = "manhattan", #https://en.wikipedia.org/wiki/Taxicab_geometry is sum(abs(distances))
                      distance_col = 'dist')


# fuzzy_join









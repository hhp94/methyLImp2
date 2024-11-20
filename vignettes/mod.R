library(profvis)
library(ggplot2)

load_all()

# Changelog -----

# colsums once
# split internal into multiple functions
# change skip ids into vector of character instead because its more intuitive
# add validation for skip ids. right now -1 and ids not in the range are still imputed
# change passing around of id numbers to passing around characters
# change the look up apply loop with split function
# make sure the colnames and row.names are unique and annotation must have no repeat
# All methylations are now using bounded imputation between 0 and 1.
prev_internal <- function(s) {
  do.call(
    methyLImp2_internal,
    args = c(list(dat = s), methyLImp2_internal_args())
  )
}

aft_internal <- function(s) {
  do.call(
    mod_methyLImp2_internal,
    args = c(list(dat = s), methyLImp2_internal_args())
  )
}

# Test if two results are the same ----
set.seed(1234)
s <- sim_mat(5000, 20, nchr = 2, perc_NA = 0.2, perc_col_NA = 0.1)
# m <- prev_internal(s$input)
# m1 <- aft_internal(s$input)
# all(dplyr::near(m, m1))

microbenchmark::microbenchmark(aft_internal(s$input), times = 5)
p <- profvis(prev_internal(s$input))
p1 <- profvis(aft_internal(s$input))

# prev vs aft ----
bench::mark(prev_internal(s$input), aft_internal(s$input))

# Progress bar and parallel ----
# library(progressr)
# handlers(global = TRUE)
# mod_methyLImp2(input = s$input, type = "user", annotation = s$user)

# parallel ----
# has to install() first
library(methyLImp2)
library(future)
library(progressr)
library(future)
handlers(global = TRUE)

plan(multisession, workers = 6)
set.seed(1234)
s <- sim_mat(60000, 100, nchr = 6, perc_NA = 0.7, perc_col_NA = 0.05)
m <- mod_methyLImp2(
  input = s$input, 
  type = "user", 
  annotation = s$user, 
  parallel = TRUE
)
plan(sequential)

library(profvis)

load_all()

# colsums once
# split internal into multiple functions
# change skip ids into vector of character instead because its more intuitive
# change passing around of id numbers to passing around characters
# change the look up apply loop with split function
# make sure the colnames and row.names are unique and annotation must have no repeat

prev_internal <- function() {
  do.call(
    methyLImp2_internal,
    args = c(list(dat = s$input), methyLImp2_internal_args())
  )
}

aft_internal <- function() {
  do.call(
    mod_methyLImp2_internal,
    args = c(list(dat = s$input), methyLImp2_internal_args())
  )
}

set.seed(1234)
s <- sim_mat(20, 10, nchr = 1, perc_NA = 0.5, perc_col_NA = 0.5)
s

do.call(
  "mod_methyLImp2_internal",
  c(
    list(dat = s$input),
    purrr::list_modify(methyLImp2_internal_args(), skip_imputation_ids = purrr::zap()),
    skip_imputation_ids = "cg1"
  )
)

# Test if two results are the same
# set.seed(1234)
# s <- sim_mat(10000, 24, nchr = 2, perc_NA = 0.5, perc_col_NA = 0.5)
# 
# m <- prev_internal()
# m1 <- aft_internal()
# all(dplyr::near(m, m1))
# 
# p <- profvis(prev_internal())
# p1 <- profvis(aft_internal())
# 
# p
# p1

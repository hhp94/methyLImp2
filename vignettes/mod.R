library(profvis)
library(ggplot2)

load_all()

# colsums once
# split internal into multiple functions
# change skip ids into vector of character instead because its more intuitive
# add validation for skip ids. right now -1 and ids not in the range are still imputed
# change passing around of id numbers to passing around characters
# change the look up apply loop with split function
# make sure the colnames and row.names are unique and annotation must have no repeat

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

sim_tbl <- data.frame(
  n_cpg = rep(seq(5000, 40000, by = 5000), each = 2),
  fn = c("methyLImp2_internal", "mod_methyLImp2_internal")
) |>
  dplyr::as_tibble()

sim_tbl$results <- purrr::map2(
  sim_tbl$n_cpg,
  sim_tbl$fn,
  \(s, f) {
    set.seed(1234)
    d <- sim_mat(s, 50, nchr = 2, perc_NA = 0.02, perc_col_NA = 0.75)
    bench::mark(
      do.call(
        f, c(list(dat = d$input), methyLImp2_internal_args())
      ),
      iterations = 1
    )
  },
  .progress = TRUE
)

sim_tbl <- tidyr::unnest(sim_tbl, results)
sim_tbl |> 
  ggplot(aes(x = n_cpg, y = total_time)) +
  geom_line(aes(group = fn, color = fn))

# Test if two results are the same
# set.seed(1234)
# s <- sim_mat(10000, 24, nchr = 2, perc_NA = 0.7, perc_col_NA = 0.05)
#
# m <- prev_internal(s$input)
# m1 <- aft_internal(s$input)
# all(dplyr::near(m, m1))
#
# p <- profvis(prev_internal())
# p1 <- profvis(aft_internal())
#
# p
# p1

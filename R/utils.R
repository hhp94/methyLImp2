#' Simulate Data Matrix
#'
#' @param n Number of rows.
#' @param m Number of columns.
#' @param nchr Number of chromosomes.
#' @param perc_NA Proportion of NA in a specific column (0-1).
#' @param perc_col_NA Proportion of columns with NA (0-1).
#'
#' @return A list with a transposed matrix (`input`) and metadata (`user`).
#' @noRd
#' 
#' @keywords internal
sim_mat <- function(n = 100, m = 100, nchr = 2, perc_NA = 0.5, perc_col_NA = 0.5) {
  stopifnot(
    n > 1, m > 1, nchr >= 1, nchr <= 22, nchr < n,
    perc_NA > 0, perc_NA < 1, perc_col_NA > 0, perc_col_NA <= 1
  )
  
  # create and scale the matrix to between 0 and 1
  d_length <- n * m
  d <- matrix(rnorm(d_length), nrow = n, ncol = m)
  d <- apply(d, MARGIN = 2, function(x) (x - min(x)) / (max(x) - min(x)))
  
  # generate realistic cpg and sample names
  cpg <- seq_len(n)
  chr <- sample(paste0("chr", seq_len(nchr)), size = n, replace = TRUE)
  user <- data.frame(cpg = paste0("cg", cpg), chr = chr)
  colnames(d) <- paste0("s", seq_len(m))
  row.names(d) <- user$cpg
  d <- t(d)
  
  # remember, d is t()ed, so n and m are swapped
  col_miss_size <- max(floor(perc_col_NA * n), 1)
  NA_size <- max(floor(perc_NA * m), 1)
  
  if (col_miss_size > 0 && NA_size > 0) {
    col_miss <- sample.int(n, size = col_miss_size)
    for (i in col_miss) {
      NA_position <- sample.int(m, size = NA_size)
      d[NA_position, i] <- NA
    }
  }
  
  return(list(input = d, user = user))
}

head1 <- function(obj, n = 6, m = 6, ...) {
  head(obj, c(n, m), ...)
}
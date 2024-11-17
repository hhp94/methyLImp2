library(profvis)
library(matrixStats)

load_all()

# colsums once
# split internal into multiple functions
# change skip ids into vector of character instead because its more intuitive
# change passing around of id numbers to passing around characters
# change the look up apply loop with split function
# make sure the colnames and row.names are unique and annotation must have no repeat

s <- sim_mat(10000, 500, nchr = 2, perc_NA = 0.5)$input |> is.na()
# s <- is.na(mtcars)
s |> head1()
s <- matrix(as.numeric(s), nrow = nrow(s), ncol = ncol(s))
s |> head1()

s_pasted <- colCollapse(s)
s_pasted

bench::mark(
apply(s, 2, \(x){paste(x, collapse = "")}),
apply(t(s), 1, \(x){paste(x, collapse = "")})
)

split(as.integer(s), f = rep(colnames(s), each = nrow(s)))
apply()

na_index <- as.data.frame(which(s, arr.ind = TRUE, useNames = FALSE))
names(na_index) <- c("row_index", "col_index")
na_index |> head()
na_index
collapsed <- fsummarize(
  fgroup_by(na_index, col_index),
  row_index = paste(row_index, collapse = ",")
)
collapsed$row_index |> nchar() |> max()
collapsed |> head()

bench::mark(
  aggregate(row_index ~ col_index, data = na_index, function(x)
    paste(x, collapse = ",")),
  fsummarize(
    fgroup_by(na_index, col_index),
    row_index = paste(row_index, collapse = ",")
  )
)
hash

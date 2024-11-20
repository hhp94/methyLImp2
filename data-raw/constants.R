logis_low <- qlogis(.Machine$double.neg.eps)
logis_high <- qlogis(1 - .Machine$double.neg.eps)

usethis::use_data(logis_low, logis_high, internal = TRUE, overwrite = TRUE)

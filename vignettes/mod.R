data(beta)
dim(beta)

set.seed(1234)
beta_with_nas <- generateMissingData(beta, lambda = 3.5)$beta_with_nas

utils::data("EPIC.manifest.hg19", package = "ChAMPdata", envir = environment())
anno <- data.frame(cpg = rownames(EPIC.manifest.hg19), chr = EPIC.manifest.hg19$CpG_chrm)

library(profvis)
run_methyLImp2 <- function() {
  methyLImp2(
    input = beta_with_nas,
    type = "user",
    minibatch_frac = 1,
    annotation = anno,
    BPPARAM = BiocParallel::SerialParam()
  )
}

pv <- profvis(run_methyLImp2())
pv

library(bootSVD)
library(corpcor)

Y <- run_methyLImp2()
Y |> dim()
microbenchmark::microbenchmark(corpcor::fast.svd(Y), bootSVD::fastSVD(Y))

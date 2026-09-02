# tests/run_exhaustive.R
# 
# Run the entire test suite against the entire dataset in inst/extdata/NCRN
#
# Example usage:
#   Rscript tests/run_exhaustive.R
options(
  ncrnwater.test.exhaustive = TRUE,  # run every combo
  ncrnwater.test.n_shards   = 1,     # single shard locally (no parallel)
  ncrnwater.test.shard      = 1,
  ncrnwater.test.seed       = 42
)
devtools::test()

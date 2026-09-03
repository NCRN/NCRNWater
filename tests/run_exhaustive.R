# tests/run_exhaustive.R
# 
# Run the entire test suite against the entire dataset in inst/extdata/NCRN
#
# Example usage:
# Rscript tests/run_exhaustive.R
# 
# options(ncrnwater.test.exhaustive = TRUE)
# devtools::test()
# 
# Split long-running test sessions across two terminals
# Terminal 1:
# options(ncrnwater.test.exhaustive = TRUE, ncrnwater.test.n_shards = 2, ncrnwater.test.shard = 1)
# devtools::test()
# 
# Terminal 2:
# options(ncrnwater.test.exhaustive = TRUE, ncrnwater.test.n_shards = 2, ncrnwater.test.shard = 2)
# 
library(devtools)
options(
  ncrnwater.test.exhaustive = TRUE,  # run every combo
  ncrnwater.test.n_shards   = 1,     # single shard locally (no parallel)
  ncrnwater.test.shard      = 1,
  ncrnwater.test.seed       = 42
)
devtools::test()

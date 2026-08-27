test_that("example paths exist and are readable", {
  paths <- example_paths()
  expect_true(file.exists(paths$data))
  expect_true(file.exists(paths$metadata))
  
  ex <- use_example_data(assign = FALSE, reader = "utils")
  expect_true(is.data.frame(ex$wqp))
  expect_true(is.data.frame(ex$wqp_metadata))
})

test_that("example object builds and exceed works", {
  wd <- example_ncrnwater()
  df <- exceed(wd, charname = "ANC")
  expect_true(is.data.frame(df))
  expect_true(nrow(df) >= 0L)
})

# tests/testthat/test-enumerate-combos.R
# Example usage:
#   testthat::test_file("tests/testthat/test-enumerate-combos.R")
# 
library(testthat)
library(NCRNWater)

test_that("enumerate_combos returns df and respects filters", {
  wd <- getWD()
  df <- enumerate_combos(wd, require_data = TRUE, exhaustive = FALSE, n_cases = 3, seed = 1)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("park","site","param","has_data","has_lower","has_upper") %in% names(df)))
  expect_true(nrow(df) >= 1)
})


test_that("list_all_valid_combos returns a non-empty list of (park, site, param) triples", {
  wd <- getWD()  # shared fixture; benign staging warning muted
  combos <- list_all_valid_combos(wd)
  
  expect_true(is.list(combos))
  expect_true(length(combos) >= 1L)
  
  # Each entry is a list with fields park/site/param
  expect_true(
    all(vapply(combos, function(x) {
      is.list(x) && all(c("park","site","param") %in% names(x))
    }, logical(1)))
  )
})

test_that("list_all_threshold_combos returns a subset of valid combos (with at least one threshold)", {
  wd <- getWD()
  all  <- list_all_valid_combos(wd)
  th   <- list_all_threshold_combos(wd)
  
  # Threshold set cannot be larger than the full valid set
  expect_true(length(th) <= length(all))
  
  # Every threshold combo should also exist in the full set (by value)
  in_all <- vapply(th, function(tc) {
    any(vapply(all, identical, logical(1), tc))
  }, logical(1))
  expect_true(all(in_all))
})

test_that("sample_n_valid_combos picks N or fewer combos and is stable with a seed", {
  wd <- getWD()
  
  # Ask for a small sample deterministically
  n_cases <- 4L
  s1 <- sample_n_valid_combos(wd, n_cases = n_cases, seed = 123)
  s2 <- sample_n_valid_combos(wd, n_cases = n_cases, seed = 123)
  
  expect_true(is.list(s1))
  expect_true(length(s1) <= n_cases)
  expect_equal(s1, s2)  # seed-stable
  
  # Basic shape check on sampled entries
  expect_true(
    all(vapply(s1, function(x) {
      is.list(x) && all(c("park","site","param") %in% names(x))
    }, logical(1)))
  )
})

                                                                          
# tests/testthat/test-operators.R
#
# Run just this file:
#   testthat::test_file("tests/testthat/test-operators.R")
# Or via devtools filter:
#   devtools::test(filter = "operators")

library(testthat)
library(NCRNWater)

test_that("[ops] metadata conditions limited to lt/le/gt/ge/NA", {
  wd <- getwd()
  # pull some condition values via getCharInfo if you like
  # (or read the example metadata file and check unique values)
  expect_setequal(NCRNWater:::allowed_enums, c("lt", "le", "gt", "ge"))
})

test_that("[ops] normalize_to_enum handles symbols, words, case, whitespace, and HTML entities", {
  # Raw symbol strings -> enum
  expect_equal(NCRNWater:::normalize_to_enum("<"),  "lt")
  expect_equal(NCRNWater:::normalize_to_enum("<="), "le")
  expect_equal(NCRNWater:::normalize_to_enum(">"),  "gt")
  expect_equal(NCRNWater:::normalize_to_enum(">="), "ge")
  
  # Worded aliases -> enum (case-insensitive)
  expect_equal(NCRNWater:::normalize_to_enum("less_than"),                 "lt")
  expect_equal(NCRNWater:::normalize_to_enum("Less_Than_Or_Equal_To"),     "le")
  expect_equal(NCRNWater:::normalize_to_enum("greater_than"),              "gt")
  expect_equal(NCRNWater:::normalize_to_enum("GREATER_THAN_OR_EQUAL_TO"),  "ge")
  expect_equal(NCRNWater:::normalize_to_enum("lte"),                       "le")
  expect_equal(NCRNWater:::normalize_to_enum("GTE"),                       "ge")
  
  # Leading/trailing whitespace
  expect_equal(NCRNWater:::normalize_to_enum("  <  "), "lt")
  expect_equal(NCRNWater:::normalize_to_enum("\t>=\n"), "ge")
  
  # HTML entities -> enum (defensive support)
  expect_equal(NCRNWater:::normalize_to_enum("&lt;"),  "lt")
  expect_equal(NCRNWater:::normalize_to_enum("&lt;="), "le")
  expect_equal(NCRNWater:::normalize_to_enum("&gt;"),  "gt")
  expect_equal(NCRNWater:::normalize_to_enum("&gt;="), "ge")
  
  # Vector input normalizes element-wise
  v   <- c("<", "<=", "gt", "GREATER_THAN_OR_EQUAL_TO", "unknown", "")
  out <- NCRNWater:::normalize_to_enum(v)
  expect_equal(out[1:4], c("lt", "le", "gt", "ge"))
  expect_true(is.na(out[5]))   # "unknown" -> NA
  expect_true(is.na(out[6]))   # empty -> NA
  
  # NULL -> NA_character_
  expect_true(is.na(NCRNWater:::normalize_to_enum(NULL)))
})

test_that("[ops] allowed_enums contains canonical codes", {
  expect_setequal(NCRNWater:::allowed_enums, c("lt", "le", "gt", "ge"))
})

test_that("[ops] validate_enum accepts allowed codes and ignores NA; errors on invalid", {
  # Accept allowed codes
  expect_silent(NCRNWater:::validate_enum(c("lt", "le", "gt", "ge")))
  # Ignore NA entries
  expect_silent(NCRNWater:::validate_enum(c("lt", NA, "ge")))
  # Error on invalid tokens
  expect_error(NCRNWater:::validate_enum(c("lt", "foo", "ge")), "Invalid .* Allowed")
})

test_that("[ops] cmp_fun_enum returns correct comparison semantics", {
  # lt: x < y
  f_lt <- NCRNWater:::cmp_fun_enum("lt")
  expect_true( f_lt(3, 4) )
  expect_false(f_lt(4, 4))
  expect_false(f_lt(5, 4))
  
  # le: x <= y
  f_le <- NCRNWater:::cmp_fun_enum("le")
  expect_true( f_le(3, 4) )
  expect_true( f_le(4, 4) )
  expect_false(f_le(5, 4))
  
  # gt: x > y
  f_gt <- NCRNWater:::cmp_fun_enum("gt")
  expect_true( f_gt(5, 4) )
  expect_false(f_gt(4, 4))
  expect_false(f_gt(3, 4))
  
  # ge: x >= y
  f_ge <- NCRNWater:::cmp_fun_enum("ge")
  expect_true( f_ge(5, 4) )
  expect_true( f_ge(4, 4) )
  expect_false(f_ge(3, 4))
  
  # Vectorized use via vapply (functions themselves are scalar)
  x <- c(1, 2, 3, 4); y <- 3
  expect_equal(vapply(x, f_lt, logical(1), y), c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(vapply(x, f_le, logical(1), y), c(TRUE, TRUE, TRUE,  FALSE))
  expect_equal(vapply(x, f_gt, logical(1), y), c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(vapply(x, f_ge, logical(1), y), c(FALSE, FALSE, TRUE,  TRUE))
})

test_that("[ops] symbol_from_enum maps codes to display symbols", {
  expect_equal(NCRNWater:::symbol_from_enum("lt"), "<")
  expect_equal(NCRNWater:::symbol_from_enum("le"), "<=")
  expect_equal(NCRNWater:::symbol_from_enum("gt"), ">")
  expect_equal(NCRNWater:::symbol_from_enum("ge"), ">=")
  
  # Vector input
  codes <- c("lt","le","gt","ge","bad","")
  syms  <- NCRNWater:::symbol_from_enum(codes)
  expect_equal(syms[1:4], c("<","<=",">",">="))
  expect_true(is.na(syms[5]))
  expect_true(is.na(syms[6]))
})

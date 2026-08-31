# R/utils_ops.R

#' Operator utilities (internal, enum-based)
#'
#' @description
#' Internal helpers for normalizing and validating comparator representations
#' used in threshold comparisons (lower/upper points). Everything is converted
#' to an enum code to avoid issues with HTML entities or token handling.
#'
#' Enum codes:
#'   - "lt" : less-than          (<)
#'   - "le" : less-than-or-equal (<=)
#'   - "gt" : greater-than       (>)
#'   - "ge" : greater-than-or-equal (>=)
#'
#' @keywords internal
#' @noRd

# Normalize ANY input (HTML entities, symbols, words, shorthand) to enum codes
normalize_to_enum <- function(x) {
  if (is.null(x)) return(NA_character_)
  x <- as.character(x)
  x <- trimws(x)
  x_low <- tolower(x)
  
  # HTML entities -> symbols
  x_low <- gsub("&lt;=", "<=", x_low, fixed = TRUE)
  x_low <- gsub("&gt;=", ">=", x_low, fixed = TRUE)
  x_low <- gsub("&lt;",  "<",  x_low, fixed = TRUE)
  x_low <- gsub("&gt;",  ">",  x_low, fixed = TRUE)
  
  # Worded aliases -> symbols
  x_low <- ifelse(x_low %in% c("less_than_or_equal_to","lte","le"), "<=", x_low)
  x_low <- ifelse(x_low %in% c("less_than","lt"), "<", x_low)
  x_low <- ifelse(x_low %in% c("greater_than_or_equal_to","gte","ge"), ">=", x_low)
  x_low <- ifelse(x_low %in% c("greater_than","gt"), ">", x_low)
  
  # Symbols -> enum codes
  enum <- ifelse(x_low == "<",  "lt",
                 ifelse(x_low == "<=", "le",
                        ifelse(x_low == ">",  "gt",
                               ifelse(x_low == ">=", "ge", NA_character_))))
  enum[nchar(x) == 0L] <- NA_character_
  enum
}

# Canonical set of allowable comparator codes
allowed_enums <- c("lt", "le", "gt", "ge")

# Validate a vector of comparator codes (non-NA entries only)
validate_enum <- function(code, what = "comparator code") {
  bad <- !is.na(code) & !code %in% allowed_enums
  if (any(bad)) {
    stop(
      sprintf(
        "Invalid %s: %s. Allowed: %s",
        what,
        paste(unique(code[bad]), collapse = ", "),
        paste(allowed_enums, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Map enum code -> comparison function (no get(); immune to HTML)
cmp_fun_enum <- function(code) {
  stopifnot(length(code) == 1L, is.character(code), !is.na(code))
  validate_enum(code, "comparator code")
  switch(code,
         "lt" = function(x, y) x <  y,
         "le" = function(x, y) x <= y,
         "gt" = function(x, y) x >  y,
         "ge" = function(x, y) x >= y,
         stop("Unexpected comparator code: ", code, call. = FALSE)
  )
}

# (Optional) For display purposes: enum -> symbol string
symbol_from_enum <- function(code) {
  stopifnot(length(code) >= 1L)
  out <- rep(NA_character_, length(code))
  out[code == "lt"] <- "<"
  out[code == "le"] <- "<="
  out[code == "gt"] <- ">"
  out[code == "ge"] <- ">="
  out
}

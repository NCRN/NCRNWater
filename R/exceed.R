#' @include NCRNWater_NCRNWaterObj_Class_def.R
#' @include getCharInfo.R
#' @include getWData.R
#' @include waterseries.R
#' @include utils_ops.R
#' @importFrom purrr pmap
#' @importFrom dplyr filter group_by summarize bind_rows distinct
#' @importFrom magrittr %>%
#'
#' @title exceed
#' @description Determines if water data contains observations that exceed lower and/or upper assessment points.
#' Supports \code{mode="summary"} (default) and \code{mode="rows"}.
#' @inheritParams getChars
#' @inheritParams getWData
#' @param points One of \code{"lower"}, \code{"upper"}, \code{"both"} (default).
#' @param lower,upper Numeric thresholds; \code{NA} means use metadata thresholds.
#' @param all Logical, default \code{FALSE}. In summary mode, whether to include characteristics without applicable thresholds.
#' @param catsum Logical, default \code{FALSE}. In summary mode, summarize by Category instead of Characteristic.
#' @param ... Controls:
#'   \describe{
#'     \item{\code{mode}}{\code{"summary"} or \code{"rows"}}
#'     \item{\code{lower_op}, \code{upper_op}}{Comparator overrides; accepted forms:
#'       enum codes (\code{"lt","le","gt","ge"}), symbols (\code{"<","<=",">",">="}),
#'       HTML entities, or words (\code{"less_than"}, \code{"greater_than_or_equal_to"}).}
#'   }
#' @export
setGeneric(
  name = "exceed",
  function(object, parkcode = NA, sitecode = NA, charname = NA, category = NA,
           points = "both", lower = NA, upper = NA, all = FALSE, catsum = FALSE, ...) {
    standardGeneric("exceed")
  },
  signature = c("object")
)

# -------- NCRNWaterObj method --------
setMethod(
  f = "exceed",
  signature = c(object = "NCRNWaterObj"),
  function(object, parkcode = NA, sitecode = NA, charname = NA, category = NA,
           points = "both", lower = NA, upper = NA, all = FALSE, catsum = FALSE, ...) {
    
    dots <- list(...)
    mode <- if (!is.null(dots$mode)) match.arg(dots$mode, c("summary", "rows")) else "summary"
    
    # Do NOT forward controls meant for exceed() to getWData()
    safe_dots <- dots
    safe_dots[c("mode", "lower_op", "upper_op")] <- NULL
    
    # Build argument list for getWData without the unsupported args
    gw_args <- c(list(object   = object,
                      parkcode = parkcode,
                      sitecode = sitecode,
                      charname = charname,
                      category = category,
                      output   = "list"),
                 safe_dots)
    
    DataUse <- do.call(getWData, gw_args)
    NotNull <- !sapply(DataUse, is.null)
    DataUse <- DataUse[NotNull]
    
    if (length(DataUse) == 0L) {
      if (mode == "rows") {
        return(data.frame(
          Park = character(0), Site = character(0),
          Characteristic = character(0), Category = character(0),
          Date = as.Date(character(0)), Value = numeric(0),
          LowerPoint = numeric(0), UpperPoint = numeric(0),
          LowerPointCondition = character(0), UpperPointCondition = character(0),
          Exceed_Lower = logical(0), Exceed_Upper = logical(0),
          Exceed_Type = character(0),
          stringsAsFactors = FALSE
        ))
      }
      return(data.frame(Park = character(0), Site = character(0),
                        Characteristic = character(0), Category = character(0),
                        Total = integer(0), Acceptable = integer(0),
                        TooLow = integer(0), TooHigh = integer(0), AllExceed = integer(0),
                        stringsAsFactors = FALSE))
    }
    
    .first_scalar <- function(x) if (length(x) == 0L) NA else x[1]
    n <- length(DataUse)
    
    need_lower <- (points %in% c("lower", "both")) && (length(lower) == 1L && is.na(lower))
    need_upper <- (points %in% c("upper", "both")) && (length(upper) == 1L && is.na(upper))
    
    if (need_lower) {
      lower <- vapply(DataUse, function(df) {
        getCharInfo(object,
                    parkcode = .first_scalar(df$Park),
                    sitecode = .first_scalar(df$Site),
                    charname = .first_scalar(df$Characteristic),
                    category = .first_scalar(df$Category),
                    info     = "LowerPoint")
      }, FUN.VALUE = numeric(1))
    }
    if (need_upper) {
      upper <- vapply(DataUse, function(df) {
        getCharInfo(object,
                    parkcode = .first_scalar(df$Park),
                    sitecode = .first_scalar(df$Site),
                    charname = .first_scalar(df$Characteristic),
                    category = .first_scalar(df$Category),
                    info     = "UpperPoint")
      }, FUN.VALUE = numeric(1))
    }
    
    .recycle_to_n <- function(x, nn) {
      if (length(x) == nn) return(x)
      if (length(x) == 1L) return(rep(x, nn))
      stop(sprintf("Length of vector (%d) does not match number of data groups (%d).", length(x), nn))
    }
    lower <- if (!is.null(lower)) .recycle_to_n(lower, n) else rep(NA_real_, n)
    upper <- if (!is.null(upper)) .recycle_to_n(upper, n) else rep(NA_real_, n)
    
    # Enum codes for comparators
    need_lower_code <- (points %in% c("lower", "both"))
    need_upper_code <- (points %in% c("upper", "both"))
    
    if (need_lower_code) {
      lower_code <- if (!is.null(dots$lower_op)) .recycle_to_n(normalize_to_enum(dots$lower_op), n) else vapply(
        DataUse, function(df) {
          normalize_to_enum(getCharInfo(object,
                                        parkcode = .first_scalar(df$Park),
                                        sitecode = .first_scalar(df$Site),
                                        charname = .first_scalar(df$Characteristic),
                                        category = .first_scalar(df$Category),
                                        info     = "LowerPointCondition"))
        }, FUN.VALUE = character(1)
      )
    } else lower_code <- rep(NA_character_, n)
    
    if (need_upper_code) {
      upper_code <- if (!is.null(dots$upper_op)) .recycle_to_n(normalize_to_enum(dots$upper_op), n) else vapply(
        DataUse, function(df) {
          normalize_to_enum(getCharInfo(object,
                                        parkcode = .first_scalar(df$Park),
                                        sitecode = .first_scalar(df$Site),
                                        charname = .first_scalar(df$Characteristic),
                                        category = .first_scalar(df$Category),
                                        info     = "UpperPointCondition"))
        }, FUN.VALUE = character(1)
      )
    } else upper_code <- rep(NA_character_, n)
    
    fill_default_code <- function(code, default) { code[is.na(code) | code == ""] <- default; code }
    if (need_lower_code) lower_code <- fill_default_code(lower_code, "lt")
    if (need_upper_code) upper_code <- fill_default_code(upper_code, "gt")
    
    validate_enum(lower_code, "lower_code")
    validate_enum(upper_code, "upper_code")
    
    X <- purrr::pmap(
      .l = list(object   = DataUse,
                parkcode = rep(NA, n), sitecode = rep(NA, n),
                charname = rep(NA, n), category = rep(NA, n),
                points   = points, lower = lower, upper = upper,
                all      = FALSE, catsum = FALSE,
                mode     = mode, lower_op = lower_code, upper_op = upper_code),
      .f = exceed
    ) %>% dplyr::bind_rows()
    
    
    if (mode == "summary") {
      if (!all) {
        X <- dplyr::filter(X, !(is.na(TooLow) & is.na(TooHigh)))
      }
      if (catsum) {
        X <- X %>%
          dplyr::group_by(Park, Site, Category) %>%
          dplyr::summarize(
            Total      = sum(Total),
            Acceptable = sum(Acceptable),
            TooLow     = sum(TooLow),
            TooHigh    = sum(TooHigh),
            AllExceed  = sum(AllExceed),
            .groups    = "drop"
          )
        # Distinct on park/site/category only (Characteristic was collapsed)
        X <- dplyr::distinct(X, Park, Site, Category, .keep_all = TRUE)
      } else {
        # Distinct with Characteristic only when not catsum
        X <- dplyr::distinct(X, Park, Site, Characteristic, Category, .keep_all = TRUE)
      }
    }
    
    return(X)
  }
)

# -------- data.frame method --------
setMethod(
  f = "exceed",
  signature = c(object = "data.frame"),
  function(object, parkcode = NA, sitecode = NA, charname = NA, category = NA,
           points = "both", lower = NA, upper = NA, all = FALSE, catsum = FALSE, ...) {
    
    dots     <- list(...)
    mode     <- if (!is.null(dots$mode)) match.arg(dots$mode, c("summary", "rows")) else "summary"
    lower_cd <- normalize_to_enum(if (!is.null(dots$lower_op)) dots$lower_op else "lt")  # default lower "<"
    upper_cd <- normalize_to_enum(if (!is.null(dots$upper_op)) dots$upper_op else "gt")  # default upper ">"
    
    validate_enum(lower_cd, "lower_code")
    validate_enum(upper_cd, "upper_code")
    
    Park           <- if ("Park" %in% names(object)) unique(object$Park) else NA
    Site           <- if ("Site" %in% names(object)) unique(object$Site) else NA
    Characteristic <- if ("Characteristic" %in% names(object)) unique(object$Characteristic) else NA
    Category       <- if ("Category" %in% names(object)) unique(object$Category) else NA
    
    vals    <- object$Value
    Total   <- nrow(object)
    Missing <- sum(is.na(vals))
    
    points <- match.arg(points, c("lower", "upper", "both"))
    
    cmp_low  <- if (!is.na(lower)) cmp_fun_enum(lower_cd) else NULL
    cmp_high <- if (!is.na(upper)) cmp_fun_enum(upper_cd) else NULL
    
    low_flag  <- if (!is.null(cmp_low))  cmp_low(vals,  lower) else rep(FALSE, Total)
    high_flag <- if (!is.null(cmp_high)) cmp_high(vals, upper) else rep(FALSE, Total)
    
    na_mask <- is.na(vals)
    if (any(na_mask)) {
      low_flag[na_mask]  <- FALSE
      high_flag[na_mask] <- FALSE
    }
    
    if (points == "lower") high_flag <- rep(FALSE, Total)
    if (points == "upper") low_flag  <- rep(FALSE, Total)
    
    if (mode == "rows") {
      keep <- low_flag | high_flag
      
      template <- object[FALSE, , drop = FALSE]
      template$LowerPoint     <- numeric(0)
      template$UpperPoint     <- numeric(0)
      template$LowerPointCondition <- character(0)
      template$UpperPointCondition <- character(0)
      template$Exceed_Lower   <- logical(0)
      template$Exceed_Upper   <- logical(0)
      template$Exceed_Type    <- character(0)
      
      if (!any(keep)) {
        return(template)
      }
      
      out <- object[keep, , drop = FALSE]
      out$LowerPoint     <- if (!is.na(lower)) lower else NA_real_
      out$UpperPoint     <- if (!is.na(upper)) upper else NA_real_
      out$LowerPointCondition <- lower_cd
      out$UpperPointCondition <- upper_cd
      out$Exceed_Lower   <- low_flag[keep]
      out$Exceed_Upper   <- high_flag[keep]
      out$Exceed_Type    <- ifelse(out$Exceed_Lower & out$Exceed_Upper, "both",
                                   ifelse(out$Exceed_Lower, "lower",
                                          ifelse(out$Exceed_Upper, "upper", NA_character_)))
      return(out)
    }
    
    TooLow     <- if (points != "upper" && !is.na(lower)) sum(low_flag)  else if (points == "upper") NA_integer_ else NA_integer_
    TooHigh    <- if (points != "lower" && !is.na(upper)) sum(high_flag) else if (points == "lower") NA_integer_ else NA_integer_
    AllExceed  <- sum(TooLow, TooHigh, na.rm = TRUE)
    Acceptable <- Total - Missing - AllExceed
    
    data.frame(Park, Site, Characteristic, Category,
               Total, Acceptable, TooLow, TooHigh, AllExceed,
               stringsAsFactors = FALSE)
  }
)

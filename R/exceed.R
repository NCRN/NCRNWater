#' @include NCRNWater_NCRNWaterObj_Class_def.R 
#' @include getCharInfo.R
#' @include getWData.R
#' @include waterseries.R
#' @importFrom purrr pmap
#' @importFrom dplyr filter group_by summarize
#' 
#' @title exceed
#' 
#' @description Determines if water data contains observations that exceed a upper and/or lower assessment points. 
#' 
#' @inheritParams getChars
#' @inheritParams getWData
#' @param points What assessment points to use when determining if there are exceedances. There are three options
#' \describe{
#' \item{"lower"}{Only assess the data against the lower reference point}
#' \item{"upper"}{Only assess the data against the upper reference point}
#' \item{"both"}{The default. Assess the data against both reference points}
#' }
#' @param lower,upper The lower and uppers assessment points. Either a number specified by the user, or if \code{NA}, the default,
#'  the assessment point is determined by the \code{LowerPoint} and \code{UpperPoint} slots in the \code{Characteristic} objects. 
#' @param all Logical, defaults to \code{FALSE}. Not used when \code{object} is a \code{data.frame}. If \code{all} is \code{FALSE} 
#' characteristics without upper or lower references points will not be included in the results. If \code{all} is \code{TRUE} such characteristics will be included.
#' @param catsum Logical, defaults to \code{False}. When true summarizes by category rather than a characteristic. Not used if 
#' \code{object} is a \code{data.frame}
#' @param ... Additional commands passed to \code{\link{getWData}} for filtering or subsetting the data.
#' 
#' @export

setGeneric(name="exceed",function(object, parkcode=NA, sitecode=NA, charname=NA, category=NA, 
              points="both", lower=NA, upper=NA,all=F, catsum=F,...){standardGeneric("exceed")},signature=c("object") )

setMethod(f = "exceed", signature = c(object = "NCRNWaterObj"),
          function(object, parkcode = NA, sitecode = NA, charname = NA, category = NA,
                   points = "both", lower = NA, upper = NA, all = FALSE, catsum = FALSE, ...) {
            
            dots <- list(...)
            mode <- if (!is.null(dots$mode)) match.arg(dots$mode, c("summary", "rows")) else "summary"
            
            # 1) Gather per-site/characteristic data frames
            DataUse <- getWData(object,
                                parkcode = parkcode, sitecode = sitecode,
                                charname = charname, category = category,
                                output   = "list", ...)
            NotNull <- !sapply(DataUse, is.null)
            DataUse <- DataUse[NotNull]
            
            # If NO data groups at all: return appropriate zero-row schema
            if (length(DataUse) == 0L) {
              if (mode == "rows") {
                return(data.frame(
                  Park = character(0), Site = character(0),
                  Characteristic = character(0), Category = character(0),
                  Date = as.Date(character(0)),  # typical; ok to be empty
                  Value = numeric(0),
                  LowerPoint = numeric(0), UpperPoint = numeric(0),
                  LowerPointCondition = character(0), UpperPointCondition = character(0),
                  Exceed_Lower = logical(0), Exceed_Upper = logical(0),
                  Exceed_Type = character(0),
                  stringsAsFactors = FALSE
                ))
              }
              # Legacy summary schema
              return(data.frame(Park = character(0), Site = character(0),
                                Characteristic = character(0), Category = character(0),
                                Total = integer(0), Acceptable = integer(0),
                                TooLow = integer(0), TooHigh = integer(0), AllExceed = integer(0),
                                stringsAsFactors = FALSE))
            }
            
            .first_scalar <- function(x) if (length(x) == 0L) NA else x[1]
            
            # 2) Thresholds: derive from metadata only when not provided
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
            
            .recycle_to_n <- function(x, n) {
              if (length(x) == n) return(x)
              if (length(x) == 1L) return(rep(x, n))
              stop(sprintf("Length of vector (%d) does not match number of data groups (%d).", length(x), n))
            }
            lower <- if (!is.null(lower)) .recycle_to_n(lower, n) else rep(NA_real_, n)
            upper <- if (!is.null(upper)) .recycle_to_n(upper, n) else rep(NA_real_, n)
            
            # 3) Operators (metadata or overrides) + validation
            allowed_ops <- c("<", "<=", ">", ">=")
            need_lower_op <- (points %in% c("lower", "both"))
            need_upper_op <- (points %in% c("upper", "both"))
            
            if (need_lower_op) {
              lower_op <- if (!is.null(dots$lower_op)) .recycle_to_n(dots$lower_op, n) else vapply(
                DataUse, function(df) {
                  getCharInfo(object,
                              parkcode = .first_scalar(df$Park),
                              sitecode = .first_scalar(df$Site),
                              charname = .first_scalar(df$Characteristic),
                              category = .first_scalar(df$Category),
                              info     = "LowerPointCondition")
                }, FUN.VALUE = character(1)
              )
            } else lower_op <- rep(NA_character_, n)
            
            if (need_upper_op) {
              upper_op <- if (!is.null(dots$upper_op)) .recycle_to_n(dots$upper_op, n) else vapply(
                DataUse, function(df) {
                  getCharInfo(object,
                              parkcode = .first_scalar(df$Park),
                              sitecode = .first_scalar(df$Site),
                              charname = .first_scalar(df$Characteristic),
                              category = .first_scalar(df$Category),
                              info     = "UpperPointCondition")
                }, FUN.VALUE = character(1)
              )
            } else upper_op <- rep(NA_character_, n)
            
            # Default missing ops to legacy
            .fill_default <- function(op, default) { op[is.na(op) | op == ""] <- default; op }
            if (need_lower_op) lower_op <- .fill_default(lower_op, "<")
            if (need_upper_op) upper_op <- .fill_default(upper_op, ">")
            
            # Validate
            .validate_ops <- function(op) {
              bad <- !is.na(op) & !op %in% allowed_ops
              if (any(bad)) stop(sprintf("Invalid comparator(s): %s. Allowed: %s",
                                         paste(unique(op[bad]), collapse = ", "),
                                         paste(allowed_ops, collapse = ", ")))
            }
            .validate_ops(lower_op); .validate_ops(upper_op)
            
            # 4) Map to the data.frame method
            X <- purrr::pmap(
              .l = list(object = DataUse,
                        lower = lower, upper = upper,
                        lower_op = lower_op, upper_op = upper_op,
                        mode = mode, points = points),
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
              }
              X <- dplyr::distinct(X, Park, Site, Characteristic, Category, .keep_all = TRUE)
            } else {
              # rows mode: do not summarize; X already has the union schema
              # If absolutely NO groups produced any output, X will be a 0-row df
              # constructed by the per-group zero-row templates (see data.frame method).
            }
            
            return(X)
          })


setMethod(f = "exceed", signature = c(object = "data.frame"),
          function(object, lower, upper, lower_op = NULL, upper_op = NULL,
                   mode = "summary", points = "both", ...) {
            
            Park           <- if ("Park" %in% names(object)) unique(object$Park) else NA
            Site           <- if ("Site" %in% names(object)) unique(object$Site) else NA
            Characteristic <- if ("Characteristic" %in% names(object)) unique(object$Characteristic) else NA
            Category       <- if ("Category" %in% names(object)) unique(object$Category) else NA
            
            vals <- object$Value
            Total <- nrow(object)
            Missing <- sum(is.na(vals))
            
            mode   <- match.arg(mode,   c("summary", "rows"))
            points <- match.arg(points, c("lower", "upper", "both"))
            
            allowed_ops <- c("<", "<=", ">", ">=")
            if (is.null(lower_op) || is.na(lower_op) || lower_op == "") lower_op <- "<"
            if (is.null(upper_op) || is.na(upper_op) || upper_op == "") upper_op <- ">"
            stopifnot(lower_op %in% allowed_ops, upper_op %in% allowed_ops)
            
            .cmp <- function(op) get(op, mode = "function", inherits = FALSE)
            
            # Flags: missing thresholds => all FALSE; NA values => FALSE
            low_flag  <- if (!is.na(lower)) .cmp(lower_op)(vals, lower) else rep(FALSE, Total)
            high_flag <- if (!is.na(upper)) .cmp(upper_op)(vals, upper) else rep(FALSE, Total)
            na_mask <- is.na(vals)
            if (any(na_mask)) { low_flag[na_mask] <- FALSE; high_flag[na_mask] <- FALSE }
            
            # Respect 'points'
            if (points == "lower") high_flag <- rep(FALSE, Total)
            if (points == "upper") low_flag  <- rep(FALSE, Total)
            
            if (mode == "rows") {
              keep <- low_flag | high_flag
              
              # --- Zero-row schema initializer (per-group) ---
              # Start from the original measurement columns to preserve schema,
              # then append the context columns with correct types (length 0).
              template <- object[FALSE, , drop = FALSE]
              template$LowerPoint          <- numeric(0)
              template$UpperPoint          <- numeric(0)
              template$LowerPointCondition <- character(0)
              template$UpperPointCondition <- character(0)
              template$Exceed_Lower        <- logical(0)
              template$Exceed_Upper        <- logical(0)
              template$Exceed_Type         <- character(0)
              
              if (!any(keep)) {
                return(template)
              }
              
              out <- object[keep, , drop = FALSE]
              # Attach per-group scalars
              out$LowerPoint          <- if (!is.na(lower)) lower else NA_real_
              out$UpperPoint          <- if (!is.na(upper)) upper else NA_real_
              out$LowerPointCondition <- lower_op
              out$UpperPointCondition <- upper_op
              out$Exceed_Lower        <- low_flag[keep]
              out$Exceed_Upper        <- high_flag[keep]
              out$Exceed_Type         <- ifelse(out$Exceed_Lower & out$Exceed_Upper, "both",
                                                ifelse(out$Exceed_Lower, "lower",
                                                       ifelse(out$Exceed_Upper, "upper", NA_character_)))
              return(out)
            }
            
            # Legacy summary mode (computed from flags; keeps backward behavior)
            TooLow     <- if (points != "upper" && !is.na(lower)) sum(low_flag) else if (points == "upper") NA_integer_ else NA_integer_
            TooHigh    <- if (points != "lower" && !is.na(upper)) sum(high_flag) else if (points == "lower") NA_integer_ else NA_integer_
            AllExceed  <- sum(TooLow, TooHigh, na.rm = TRUE)
            Acceptable <- Total - Missing - AllExceed
            
            data.frame(Park, Site, Characteristic, Category,
                       Total, Acceptable, TooLow, TooHigh, AllExceed,
                       stringsAsFactors = FALSE)
          })
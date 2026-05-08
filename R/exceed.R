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
            
            # 1) Gather per-site/characteristic data frames
            DataUse <- getWData(object,
                                parkcode = parkcode, sitecode = sitecode,
                                charname = charname, category = category,
                                output   = "list", ...)
            NotNull <- !sapply(DataUse, is.null)
            DataUse <- DataUse[NotNull]
            
            # Empty short-circuit: return an empty data.frame with the same columns
            if (length(DataUse) == 0L) {
              return(data.frame(Park = character(0), Site = character(0),
                                Characteristic = character(0), Category = character(0),
                                Total = integer(0), Acceptable = integer(0),
                                TooLow = integer(0), TooHigh = integer(0), AllExceed = integer(0),
                                stringsAsFactors = FALSE))
            }
            
            # Helper to grab a scalar from each df's columns (first element)
            .first_scalar <- function(x) {
              if (length(x) == 0L) return(NA)
              x[1]
            }
            
            # 2) Build per-group threshold vectors ONLY when not provided by the user
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
            
            # 3) Validate/recycle user-provided lower/upper (if present)
            n <- length(DataUse)
            .recycle_to_n <- function(x, n) {
              if (length(x) == n) return(x)
              if (length(x) == 1L) return(rep(x, n))
              stop(sprintf("Length of threshold vector (%d) does not match number of data groups (%d).", length(x), n))
            }
            
            # If thresholds are not needed (e.g., points=="upper" and lower provided), fill with NA
            if (!("lower" %in% points)) {
              # Keep existing lower; if NULL, will be set below to NA vector
            }
            if (!("upper" %in% points)) {
              # Keep existing upper; if NULL, will be set below to NA vector
            }
            
            lower <- if (!is.null(lower)) .recycle_to_n(lower, n) else rep(NA_real_, n)
            upper <- if (!is.null(upper)) .recycle_to_n(upper, n) else rep(NA_real_, n)
            
            # 4) Compute exceedance per group and bind rows
            X <- purrr::pmap(.l = list(object = DataUse, lower = lower, upper = upper), .f = exceed) %>%
              dplyr::bind_rows() %>%
              { if (!all) dplyr::filter(., !(is.na(TooLow) & is.na(TooHigh))) else . }
            
            # 5) Optional per-category summary
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
            
            # 6) Ensure one row per Park/Site/Characteristic/Category (defensive)
            X <- dplyr::distinct(X, Park, Site, Characteristic, Category, .keep_all = TRUE)
            
            return(X)
          }
)

setMethod(f="exceed", signature=c(object="data.frame"),
          function(object,lower,upper,...){       
            Park<-if(exists("Park",object)) unique(object$Park) else NA
            Site<-if(exists("Site", object)) unique(object$Site) else NA
            Characteristic<-if(exists("Characteristic",object)) unique(object$Characteristic) else NA
            Category<-if(exists("Category",object)) unique(object$Category) else NA
            Total<-nrow(object)
            Missing<-sum(is.na(object$Value))
            TooLow<-if(is.na(lower)) NA else sum(object$Value<lower, na.rm=T)
            TooHigh<-if(is.na(upper)) NA else sum(object$Value>upper, na.rm = T)
            AllExceed<-sum(TooLow,TooHigh, na.rm=T)
            Acceptable=Total-Missing-AllExceed
            OutVec<-data.frame(Park, Site, Characteristic,Category, Total,Acceptable,TooLow,TooHigh, AllExceed, stringsAsFactors=F)
            return(OutVec)
            
          })
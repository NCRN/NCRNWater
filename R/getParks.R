#' @title getParks
#' @description Retrieves one or more parks from a \code{list} of such objects,
#' or filters a single \code{Park} object by its \code{ParkCode}.
#'
#' @param object Either a \code{Park} object or a \code{list} of such objects.
#' @param parkcode Park code of one or more parks, in quotes. A single NA means
#'   "unfiltered". In mixed vectors (e.g., c("NACE", NA)), NA is ignored and
#'   matching is done on the provided codes.
#' @return A list of one or more Park S4 objects, or a single Park S4 object.
#'   If \code{parkcode} is specified and no parks match, returns \code{NULL}.
#' @export
setGeneric(
  name = "getParks",
  function(object, parkcode = NA) {
    standardGeneric("getParks")
  },
  signature = c("object")
)

# List method: iterate only Park objects (prevents recursion)
setMethod(f = "getParks", signature = c(object = "list"),
          function(object, parkcode = NA) {
            # Keep only S4 Park objects; ignore anything else
            parks_only <- object[vapply(object, function(x) methods::is(x, "Park"), logical(1))]
            if (length(parks_only) == 0L) return(NULL)
            
            # Delegate to Park method for each Park
            out <- lapply(parks_only, FUN = getParks, parkcode = parkcode)
            
            # Drop NULLs; if all NULL, return NULL
            keep <- !vapply(out, is.null, logical(1))
            if (!any(keep)) return(NULL)
            
            out[keep]
          }
)

# Park method: single NA = unfiltered; otherwise match provided codes (ignoring NA)
setMethod(f = "getParks", signature = c(object = "Park"),
          function(object, parkcode = NA) {
            # Use slot directly to avoid cycles through accessors
            pc <- object@ParkCode
            
            # Unfiltered: exactly one NA means "return the Park"
            if (length(parkcode) == 1L && is.na(parkcode)) {
              return(object)
            }
            
            # Filtered: ignore NA and match provided codes
            filt <- if (length(parkcode)) parkcode[!is.na(parkcode)] else character(0)
            if (length(filt) == 0L) {
              # All NA in a vector -> strict behavior: no match
              return(NULL)
            }
            
            if (length(pc) >= 1L && pc %in% filt) {
              return(object)
            } else {
              return(NULL)
            }
          }
)
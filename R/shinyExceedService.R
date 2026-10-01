# R/shinyExceedService.R

#' Build a Shiny-ready exceedance package for a (park, site, characteristic)
#'
#' @description
#' Composes all data and presentation artifacts needed by the Shiny
#' *Exceedances* tab for a single `(park, site, char)` tuple. Internally, this
#' service:
#' \itemize{
#'   \item Retrieves base water-quality data via \code{\link[NCRNWater]{getWData}}.
#'   \item Looks up threshold metadata (descriptions and points) via
#'         \code{\link[NCRNWater]{getCharInfo}}.
#'   \item Computes per-sample **threshold exceedances** using
#'         \code{\link[NCRNWater]{exceed}} with \code{points = "both"} and
#'         inclusive comparators (\code{lower_op = "le"}; \code{upper_op = "ge"}).
#'   \item Summarizes counts by year (total vs. exceed) and percent exceeded.
#'   \item Builds a ggplot bar chart (your UI can wrap with \code{plotly::ggplotly()}).
#'   \item Composes short grammar strings and HTML bullet points for the summary text.
#' }
#'
#' The function returns a list with a stable shape that matches the existing Shiny
#' code paths (e.g., \code{df}, \code{exdf}, \code{histdata}, \code{Sitename},
#' \code{Characteristic}, \code{Unit}, \code{p}, etc.), so you can **replace** the
#' legacy logic (e.g., \code{exDUM()}) without changing UI rendering code.
#'
#' @details
#' **Threshold handling**
#' \itemize{
#'   \item If only \emph{one} threshold point (lower or upper) exists, \code{ExPoint}
#'         is set to that value; exceedances are computed against that side.
#'   \item If \emph{both} thresholds exist, exceedances are computed for both sides.
#'         \code{ExPoint} will prefer a single side if only that side has exceedances;
#'         otherwise it defaults to the upper point for display title purposes.
#'   \item If neither threshold point exists, \code{exdf} will be empty and text paths
#'         should reflect “no exceedances.”
#' }
#'
#' **Comparators**
#' \itemize{
#'   \item \code{lower_op = "le"} (less-than-or-equal) and \code{upper_op = "ge"}
#'         (greater-than-or-equal) match legacy behavior where values equal to a
#'         threshold are considered exceedances. Adjust if policy changes.
#' }
#'
#' **Plot**
#' \itemize{
#'   \item \code{p} is a ggplot bar chart of yearly percent exceeded (\code{percent_ex}).
#'         Your server/UI can convert to Plotly via \code{plotly::ggplotly(p)} and add
#'         hover text using \code{histdata$hover_text} if desired.
#' }
#'
#' @param wd An \strong{NCRNWater} object (list of Park S4 objects) produced by
#'   hydration (e.g., \code{\link[NCRNWater]{hydrate_network}}).
#' @param park Character. Park code (e.g., \code{"NCRN"}).
#' @param site Character. Site code at the given park (e.g., \code{"NCRN_ANTI_SHCK"}).
#' @param char Character. Characteristic/parameter name (e.g., \code{"ANC"}).
#' @param show_plot Logical. If \code{TRUE}, the returned structure includes \code{p}
#'   (ggplot). The Shiny UI can still decide visibility via a separate toggle.
#'
#' @return A named \strong{list} containing (at minimum):
#' \describe{
#'   \item{df}{Base water-quality data frame with columns:
#'             \code{MonitoringLocationName}, \code{Date}, \code{Characteristic},
#'             \code{Value}, \code{ResultMeasure.MeasureUnitCode}.}
#'   \item{total_obs}{Total rows in \code{df} prior to NA filtering.}
#'   \item{non_na_obs}{Rows in \code{df} after filtering out \code{NA} values.}
#'   \item{LowerThreshold, UpperThreshold}{Character strings describing thresholds (if present).}
#'   \item{LowerPoint, UpperPoint}{Numeric threshold points (if present).}
#'   \item{Sitename}{Human-readable site name.}
#'   \item{Characteristic}{Display name for the parameter.}
#'   \item{Unit}{Units string.}
#'   \item{exdf}{Per-sample exceedance rows (data.frame) returned by
#'               \code{\link[NCRNWater]{exceed}} and augmented with threshold descriptions.}
#'   \item{desc_exdf}{Renamed exceedance data frame for presentation (columns:
#'                    \code{Site}, \code{SampleDate}, \code{Parameter}, \code{Value},
#'                    \code{Difference} [optional], \code{Units}, plus threshold text if present).}
#'   \item{histdata}{Yearly summary with \code{Year}, \code{ntot}, \code{nex},
#'                   \code{percent_ex}, and \code{formatted_percent_ex}.}
#'   \item{recent_year, oldest_year}{Numeric year bounds computed from \code{histdata}.}
#'   \item{sum_nex, sum_ntot}{Totals across years for exceed and total counts.}
#'   \item{ExPoint}{A single numeric threshold (if chosen) for display labeling.}
#'   \item{grammar1, grammar2}{Short strings used in summary text for recent/all-year contexts.}
#'   \item{extext, extext_bullets}{HTML-friendly summary sentences and bullet list.}
#'   \item{p}{A ggplot bar chart of yearly percent exceeded (present if \code{show_plot = TRUE}).}
#' }
#'
#' @examples
#' \dontrun{
#' # Hydrate data (example paths)
#' cfg <- NCRNWater::load_app_config(profile = "NCRN")
#' dh  <- NCRNWater::hydrate_network(
#'   cfg$app$network_code,
#'   datadir      = cfg$files$datadir,
#'   dataname     = cfg$files$dataname,
#'   metadataname = cfg$files$metadataname,
#'   wqx          = cfg$wqx$enabled
#' )
#'
#' # Build the exceed package for one site/param
#' pkg <- build_exceed_site_package(
#'   wd   = dh$wd,
#'   park = cfg$app$network_code,
#'   site = "NCRN_ANTI_SHCK",
#'   char = "ANC",
#'   show_plot = TRUE
#' )
#'
#' # Use in Shiny server:
#' # output$exceed_plot <- renderPlot({ pkg$p })
#' # output$exceed_table <- renderDT({ pkg$desc_exdf })
#' # output$exceed_summary <- renderUI(HTML(paste0("<ul>", pkg$extext_bullets, "</ul>")))
#' }
#'
#' @seealso
#' \code{\link[NCRNWater]{exceed}} for per-sample exceedances,
#' \code{\link[NCRNWater]{getWData}} for base data retrieval,
#' \code{\link[NCRNWater]{getCharInfo}} for threshold metadata lookup.
#'
#' @keywords internal
#' 
build_exceed_site_package <- function(wd, park, site, char, show_plot = TRUE) {
  # Base data
  df <- NCRNWater::getWData(wd, parkcode = park, sitecode = site, charname = char, output = "data.frame")
  df <- df[, c("MonitoringLocationName", "Date", "Characteristic", "Value", "ResultMeasure.MeasureUnitCode")]
  total_obs <- nrow(df)
  df <- subset(df, !is.na(Value))
  non_na_obs <- nrow(df)
  
  # Threshold metadata
  lower_desc <- NCRNWater::getCharInfo(wd, parkcode = park, sitecode = site, charname = char, info = "LowerDescription")
  upper_desc <- NCRNWater::getCharInfo(wd, parkcode = park, sitecode = site, charname = char, info = "UpperDescription")
  lower_pt   <- NCRNWater::getCharInfo(wd, parkcode = park, sitecode = site, charname = char, info = "LowerPoint")
  upper_pt   <- NCRNWater::getCharInfo(wd, parkcode = park, sitecode = site, charname = char, info = "UpperPoint")
  sitename   <- NCRNWater::getCharInfo(wd, parkcode = park, sitecode = site, charname = char, info = "SiteName")
  dispname   <- NCRNWater::getCharInfo(wd, parkcode = park, sitecode = site, charname = char, info = "DisplayName")
  units      <- NCRNWater::getCharInfo(wd, parkcode = park, sitecode = site, charname = char, info = "Units")
  df$Characteristic <- dispname
  
  # Exceed rows via NCRNWater::exceed()
  rows <- NCRNWater::exceed(
    wd, parkcode = park, sitecode = site, charname = char,
    points = "both", mode = "rows", lower_op = "le", upper_op = "ge"
  )
  # Add threshold description columns (if present)
  if (!is.null(lower_desc)) rows$LowerThreshold <- lower_desc
  if (!is.null(upper_desc)) rows$UpperThreshold <- upper_desc
  exdf <- rows
  
  # Descriptive exceed dataframe (renames to match your UI)
  desc_exdf <- exdf %>%
    dplyr::rename(
      Units      = "ResultMeasure.MeasureUnitCode",
      # Site       = "MonitoringLocationName",
      Parameter  = "Characteristic",
      SampleDate = "Date"
    )
  
  # Year counts: total vs exceed
  totcount <- df %>% dplyr::mutate(Year = lubridate::year(Date)) %>% dplyr::count(Year, name = "ntot")
  totcount <- subset(totcount, !is.na(Year))
  excount  <- exdf %>% dplyr::mutate(Year = lubridate::year(Date)) %>% dplyr::count(Year, name = "nex")
  excount  <- subset(excount, !is.na(Year))
  
  histdata <- dplyr::left_join(totcount, excount, by = "Year")
  histdata[is.na(histdata)] <- 0
  histdata <- histdata %>%
    dplyr::mutate(percent_ex = (nex / ntot) * 100,
                  formatted_percent_ex = scales::percent(percent_ex / 100, accuracy = 1))
  
  # Text metrics
  recent_year  <- max(histdata$Year, na.rm = TRUE)
  oldest_year  <- min(histdata$Year, na.rm = TRUE)
  nex_recent   <- histdata[histdata$Year == recent_year, "nex", drop = TRUE]
  ntot_recent  <- histdata[histdata$Year == recent_year, "ntot", drop = TRUE]
  sum_nex      <- sum(histdata$nex)
  sum_ntot     <- sum(histdata$ntot)
  
  # Choose ExPoint (threshold shown in titles / alt text)
  ExPoint <- dplyr::case_when(
    is.na(upper_pt) && !is.na(lower_pt) ~ lower_pt,
    !is.na(upper_pt) && is.na(lower_pt) ~ upper_pt,
    !is.na(upper_pt) && !is.na(lower_pt) ~ {
      # Heuristic: prefer a single point if one side actually exceeded
      if (any(exdf$Value <= lower_pt, na.rm = TRUE) && !any(exdf$Value >= upper_pt, na.rm = TRUE)) lower_pt
      else if (any(exdf$Value >= upper_pt, na.rm = TRUE) && !any(exdf$Value <= lower_pt, na.rm = TRUE)) upper_pt
      else upper_pt # default if both exceeded; your UI treats both points anyway
    },
    TRUE ~ NA_real_
  )
  
  # Simple grammar (same as your implementation, condensed)
  grammar1 <- if (nex_recent == 0) {
    paste0("there were no ", dispname, " exceedances ")
  } else if (nex_recent == 1) {
    paste0("there was <b>", nex_recent, "</b> ", dispname, " exceedance (",
           histdata %>% dplyr::filter(Year == recent_year) %>% dplyr::pull(formatted_percent_ex), ") ")
  } else {
    paste0("there were <b>", nex_recent, "</b> ", dispname, " exceedances (",
           histdata %>% dplyr::filter(Year == recent_year) %>% dplyr::pull(formatted_percent_ex), ") ")
  }
  
  grammar2 <- if (sum_nex == 0) {
    paste0("there were no ", dispname, " exceedances ")
  } else if (sum_nex == 1) {
    paste0("there was <b>", sum_nex, "</b> ", dispname, " exceedance ")
  } else {
    paste0("there were <b>", sum_nex, "</b> ", dispname, " exceedances ")
  }
  
  # Plot (ggplot bar; your code already wraps in plotly with hover_text)
  p <- ggplot2::ggplot(histdata, ggplot2::aes(x = Year, y = percent_ex)) +
    ggplot2::geom_bar(stat = "identity", fill = "lightgray") +
    ggplot2::scale_x_continuous(breaks = seq(min(histdata$Year), max(histdata$Year), by = 1)) +
    ggplot2::ylim(0, 100) +
    ggplot2::labs(
      title = paste0(
        sitename, ", ", dispname, ", ", oldest_year, "-", recent_year, "\n",
        sum_nex, " of ", non_na_obs, " measurements (",
        round(100 * (sum_nex / non_na_obs), 1), "%) exceeded ", ExPoint, " ", units
      ),
      x = "Year", y = "% of measurements"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5))
  
  # Assemble text bullets (keeps your HTML wrapper compatible)
  extext <- c(
    paste0("In the most recent year of data, ", recent_year, ", ", grammar1, " at ", sitename, "."),
    paste0("In all years of data, ", oldest_year, " through ", recent_year, ", ", grammar2, " at ", sitename, ".")
  )
  extext_bullets <- paste0("<li>", extext, "</li>", collapse = "")
  
  # Return the package for this site (same names your UI expects)
  list(
    df             = df,
    total_obs      = total_obs,
    non_na_obs     = non_na_obs,
    LowerThreshold = lower_desc,
    UpperThreshold = upper_desc,
    LowerPoint     = lower_pt,
    UpperPoint     = upper_pt,
    Sitename       = sitename,
    Characteristic = dispname,
    Unit           = units,
    exdf           = exdf,
    desc_exdf      = desc_exdf,
    histdata       = histdata,
    recent_year    = recent_year,
    oldest_year    = oldest_year,
    sum_nex        = sum_nex,
    sum_ntot       = sum_ntot,
    ExPoint        = ExPoint,
    grammar1       = grammar1,
    grammar2       = grammar2,
    extext         = extext,
    extext_bullets = extext_bullets,
    p              = p
  )
}

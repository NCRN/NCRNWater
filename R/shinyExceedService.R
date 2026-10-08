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
#'         comparators derived from metadata (\code{LowerPointCondition},
#'         \code{UpperPointCondition}: one of \code{"lt"}, \code{"le"},
#'         \code{"gt"}, \code{"ge"}). If a condition is missing but a threshold
#'         point is present, the service defaults to inclusive comparators.
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
#'   \item Comparators are derived from metadata: \code{LowerPointCondition} and
#'         \code{UpperPointCondition} (one of \code{"lt"}, \code{"le"}, \code{"gt"}, \code{"ge"}).
#'   \item These comparators are passed to \code{\link[NCRNWater]{exceed}} and also drive
#'         the human-readable narrative (e.g., “less than (<)”, “greater than (>)”).
#'   \item If a condition is missing but a threshold point exists, the service defaults to
#'         inclusive comparators (\code{"le"} for lower, \code{"ge"} for upper).
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
#'   \item{html_extext}{A html un-ordered list that summarizes the data shown in plot \code{p}.}
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

# -------------------------------------------------------------------
# Main service: build the exceedance package for one (park, site, char)
# -------------------------------------------------------------------

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
  reference  <- NCRNWater::getCharInfo(wd, parkcode = park, sitecode = site, charname = char, info = "AssessmentDetails")
  
  # Use the same comparators for precise narrative
  lower_op  <- .map_cond_to_op(NCRNWater::getCharInfo(wd, parkcode=park, sitecode=site, charname=char, info="LowerPointCondition"))
  upper_op  <- .map_cond_to_op(NCRNWater::getCharInfo(wd, parkcode=park, sitecode=site, charname=char, info="UpperPointCondition"))
  
  # If a site/parameter has a threshold point but no *PointCondition in metadata, set a sensible default so both exceed() and your text don’t break
  if (is.na(lower_op) && !is.na(lower_pt)) lower_op <- "le"  # default inclusive
  if (is.na(upper_op) && !is.na(upper_pt)) upper_op <- "ge"
  
  lower_phrase <- .op_phrase(lower_op)
  upper_phrase <- .op_phrase(upper_op)
  lower_symbol <- .op_symbol(lower_op)
  upper_symbol <- .op_symbol(upper_op)
  
  lower_symbol_a11y <- symbol_with_aria(lower_symbol, lower_phrase)
  upper_symbol_a11y <- symbol_with_aria(upper_symbol, upper_phrase)
  
  # Exceed rows via NCRNWater::exceed() with the comparators you want reflected in text
  rows <- NCRNWater::exceed(
    wd,
    parkcode = park,
    sitecode = site,
    charname = char,
    points   = "both",
    mode     = "rows",
    lower_op = lower_op,  # now metadata-driven
    upper_op = upper_op
  )
  
  can_have_exceedances <- (!is.na(lower_pt) || !is.na(upper_pt))
  has_exceedances      <- (nrow(rows) > 0)
  
  # Comparator-aware policy lines (prefer metadata if present)
  lower_line <- NULL
  upper_line <- NULL
  if (!is.na(lower_pt)) {
    lower_line <- .nonempty(lower_desc)
    if (is.null(lower_line)) {
      lower_line <- sprintf(
        "Acceptable %s is %s %s %s",
        tolower(dispname), lower_phrase, lower_pt, units
      )
    }
  }
  if (!is.na(upper_pt)) {
    upper_line <- .nonempty(upper_desc)
    if (is.null(upper_line)) {
      upper_line <- sprintf(
        "Acceptable %s is %s %s %s",
        tolower(dispname), upper_phrase, upper_pt, units
      )
    }
  }
  
  if (isTRUE(has_exceedances)) {
    # Normalize dispname in rows
    rows$Characteristic <- dispname
    
    # Add threshold description columns only when they are present (non-NA and non-empty)
    if (!is.na(lower_desc) && nzchar(lower_desc)) rows$LowerThreshold <- lower_desc
    if (!is.na(upper_desc) && nzchar(upper_desc)) rows$UpperThreshold <- upper_desc
    
    exdf <- rows
    threshold_cols_present <- intersect(c("UpperThreshold", "LowerThreshold"), names(exdf))
    
    # Descriptive exceed dataframe (renames to match UI) with row-wise difference from exceeded side
    desc_exdf <- rows %>%
      dplyr::select(-Site) %>%
      dplyr::rename(
        Units      = "ResultMeasure.MeasureUnitCode",
        Site       = "MonitoringLocationName",
        Parameter  = "Characteristic",
        SampleDate = "Date"
      ) %>%
      dplyr::mutate(
        Difference = dplyr::case_when(
          !is.na(UpperPoint) &  is.na(LowerPoint)                ~ Value - UpperPoint,
          is.na(UpperPoint) & !is.na(LowerPoint)                ~ Value - LowerPoint,
          !is.na(UpperPoint) & !is.na(LowerPoint) & Exceed_Upper ~ Value - UpperPoint,
          !is.na(UpperPoint) & !is.na(LowerPoint) & Exceed_Lower ~ Value - LowerPoint,
          TRUE                                                  ~ NA_real_
        ),
        Difference = round(Difference, 4)
      ) %>%
      dplyr::arrange(dplyr::desc(SampleDate)) %>%
      dplyr::select(
        "Site", "SampleDate", "Parameter", "Value", "Difference", "Units",
        dplyr::any_of(threshold_cols_present)   # include only present threshold description columns
      )
    
    # Year counts: total vs exceed
    totcount <- df %>%
      dplyr::mutate(Characteristic = dispname, Year = lubridate::year(Date)) %>%
      dplyr::count(Year, name = "ntot")
    totcount <- subset(totcount, !is.na(Year))
    
    excount  <- exdf %>%
      dplyr::mutate(Year = lubridate::year(Date)) %>%
      dplyr::count(Year, name = "nex")
    excount  <- subset(excount, !is.na(Year))
    
    histdata <- dplyr::left_join(totcount, excount, by = "Year")
    histdata[is.na(histdata)] <- 0
    histdata <- histdata %>%
      dplyr::mutate(
        percent_ex           = (nex / ntot) * 100,
        formatted_percent_ex = scales::percent(percent_ex / 100, accuracy = 1)
      )
    
    # Text metrics
    recent_year <- max(histdata$Year, na.rm = TRUE)
    oldest_year <- min(histdata$Year, na.rm = TRUE)
    nex_recent  <- histdata[histdata$Year == recent_year, "nex",  drop = TRUE]
    ntot_recent <- histdata[histdata$Year == recent_year, "ntot", drop = TRUE]
    sum_nex     <- sum(histdata$nex)
    sum_ntot    <- sum(histdata$ntot)
    
    # Choose ExPoint (threshold shown in titles / alt text)
    ExPoint <- dplyr::case_when(
      is.na(upper_pt) && !is.na(lower_pt) ~ lower_pt,
      !is.na(upper_pt) && is.na(lower_pt) ~ upper_pt,
      !is.na(upper_pt) && !is.na(lower_pt) ~ {
        if (any(exdf$Value <= lower_pt, na.rm = TRUE) && !any(exdf$Value >= upper_pt, na.rm = TRUE)) lower_pt
        else if (any(exdf$Value >= upper_pt, na.rm = TRUE) && !any(exdf$Value <= lower_pt, na.rm = TRUE)) upper_pt
        else upper_pt
      },
      TRUE ~ NA_real_
    )
    
    # Grammars (recent year vs all years)
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
    
    # Plot (ggplot bar; wrap with plotly in your UI if desired)
    p <- if (isTRUE(show_plot)) {
      ggplot2::ggplot(histdata, ggplot2::aes(x = Year, y = percent_ex)) +
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
        ggplot2::theme(
          panel.grid.major.x = ggplot2::element_blank(),
          plot.title         = ggplot2::element_text(hjust = 0.5)
        )
    } else {
      ggplot2::ggplot()
    }
    
    # Summary bullets + optional per-year bullets
    extext <- c(
      paste0("In the most recent year of data, ", recent_year, ", ", grammar1, " at ", sitename, "."),
      paste0("In all years of data, ", oldest_year, " through ", recent_year, ", ", grammar2, " at ", sitename, ".")
    )
    extext_bullets <- paste0("<li>", extext, "</li>", collapse = "")
    
    alt_raw <- character(0)
    if (sum_nex > 0) {
      for (i in seq_len(nrow(histdata))) {
        nex  <- histdata$nex[i]
        ntot <- histdata$ntot[i]
        alt_raw <- c(
          alt_raw,
          paste0(
            "<b>", histdata$Year[i], "</b>: ",
            nex, " of ", ntot, " measurement", if (ntot > 1) "s", " (",
            histdata$formatted_percent_ex[i], ") exceeded ",
            ExPoint, " ", units
          )
        )
      }
    }
    alt_bullets  <- if (length(alt_raw)) paste0("<li>", alt_raw, "</li>", collapse = "") else ""
    alt_bullets2 <- if (length(alt_raw)) paste0("<ul>", alt_bullets, "</ul>") else ""
    
    # Exceedances Summary (for has_exceedances)
    html_extext <- paste0(
      "<p><b><span style='font-size: 18px;'>Exceedances Summary:</span></b></p>",
      "<ul>",
      extext_bullets,  # already contains <li>...</li>
      alt_bullets2,
      "</ul>"
    )
    
    # Return (same shape your UI expects)
    return(list(
      df              = df,
      total_obs       = total_obs,
      non_na_obs      = non_na_obs,
      LowerThreshold  = lower_desc,
      UpperThreshold  = upper_desc,
      LowerPoint      = lower_pt,
      UpperPoint      = upper_pt,
      Sitename        = sitename,
      Characteristic  = dispname,
      Unit            = units,
      exdf            = exdf,
      desc_exdf       = desc_exdf,
      histdata        = histdata,
      recent_year     = recent_year,
      oldest_year     = oldest_year,
      sum_nex         = sum_nex,
      sum_ntot        = sum_ntot,
      ExPoint         = ExPoint,
      grammar1        = grammar1,
      grammar2        = grammar2,
      extext          = extext,
      extext_bullets  = extext_bullets,
      html_extext     = html_extext,
      p               = p,
      has_exceedances = TRUE
    ))
  }
  
  # -------------------------
  # No exceedances observed
  # -------------------------
  if (isTRUE(can_have_exceedances)) {
    if (!is.na(lower_pt) && is.na(upper_pt)) {
      # LOWER ONLY: precise comparator-negation
      html_extext <- sprintf(
        "<p><b><span style='font-size: 18px;'>%s (%s)</span></b><br/>No %s measurements were %s (%s) %s %s at %s.</p>",
        lower_line, reference,
        dispname,
        lower_phrase,                # e.g., "less than or equal to"
        lower_symbol_a11y, lower_pt, # e.g., "≤ 600"
        units,
        sitename
      )
    } else if (is.na(lower_pt) && !is.na(upper_pt)) {
      # UPPER ONLY: precise comparator-negation
      html_extext <- sprintf(
        "<p><b><span style='font-size: 18px;'>%s (%s)</span></b><br/>No %s measurements were %s (%s) %s %s at %s.</p>",
        upper_line, reference,
        dispname,
        upper_phrase,                # e.g., "greater than or equal to"
        upper_symbol_a11y, upper_pt, # e.g., "≥ 600"
        units,
        sitename
      )
    } else {
      # BOTH: precise negation using *metadata-based* comparators
      html_extext <- sprintf(
        "<p><b><span style='font-size: 18px;'>%s</span></b><br/>
   <b><span style='font-size: 18px;'>%s</span></b><br/>
   <b><span style='font-size: 18px;'>(%s)</span></b><br/>
   No %s measurements were %s (%s) %s or %s (%s) %s %s at %s.</p>",
        lower_line,
        upper_line,
        reference,
        dispname,
        lower_phrase, lower_symbol_a11y, lower_pt,
        upper_phrase, upper_symbol_a11y, upper_pt,
        units,
        sitename
      )
      # If you prefer explicit disjunction instead of "outside the range", use:
      # html_extext <- sprintf(
      #   "<p><b><span style='font-size: 18px;'>%s</span></b><br/><b><span style='font-size: 18px;'>%s</span></b><br/>No %s measurements were %s (%s %s) or %s (%s %s) at %s.</p>",
      #   lower_line, upper_line, dispname,
      #   lower_phrase, lower_symbol, format(lower_pt, trim = TRUE),
      #   upper_phrase, upper_symbol, format(upper_pt, trim = TRUE),
      #   sitename
      # )
    }
  } else {
    html_extext <- sprintf(
      "<p><b><span style='font-size: 18px;'>There is no water quality threshold for parameter %s at %s.</span></b></p>",
      dispname, sitename
    )
  }
  
  # Return a consistent shape (placeholders where not applicable)
  list(
    df              = df,
    total_obs       = total_obs,
    non_na_obs      = non_na_obs,
    LowerThreshold  = lower_desc,
    UpperThreshold  = upper_desc,
    LowerPoint      = lower_pt,
    UpperPoint      = upper_pt,
    Sitename        = sitename,
    Characteristic  = dispname,
    Unit            = units,
    exdf            = rows,       # no exceedances rows
    desc_exdf       = rows,       # placeholder (not used in no-exceedances UI)
    histdata        = rows,       # placeholder (not used in no-exceedances UI)
    recent_year     = NA,
    oldest_year     = NA,
    sum_nex         = 0,
    sum_ntot        = 0,
    ExPoint         = NA,
    grammar1        = "",
    grammar2        = "",
    extext          = "",
    extext_bullets  = "",
    html_extext     = html_extext,
    p               = ggplot2::ggplot(),
    has_exceedances = FALSE
  )
}

# -------------------------------------------------------------------
# Helpers: operator -> phrase/symbol, and non-empty guard
# -------------------------------------------------------------------

# Map comparator code to readable phrase
.op_phrase <- function(op) {
  switch(tolower(op),
         "lt" = "less than",
         "le" = "less than or equal to",
         "gt" = "greater than",
         "ge" = "greater than or equal to",
         op
  )
}

# Map comparator code to math symbol
.op_symbol <- function(op) {
  switch(tolower(op),
         "lt" = "<",
         "le" = "≤",
         "gt" = ">",
         "ge" = "≥",
         op
  )
}

# Guard: return NULL if empty/missing
.nonempty <- function(x) {
  if (is.null(x) || is.na(x) || (is.character(x) && !nzchar(x))) return(NULL)
  x
}

# Map NCRNWater condition string -> internal op code
.map_cond_to_op <- function(cond) {
  # cond is like "lt", "le", "gt", "ge"
  switch(tolower(cond),
         "lt" = "lt",
         "le" = "le",
         "gt" = "gt",
         "ge" = "ge",
         NA    # fallback
  )
}

# Screen readers sometimes read operators like > and < oddly. You can add aria-label to your comparator symbols.
symbol_with_aria <- function(sym, phrase) sprintf("<span aria-label='%s'>%s</span>", phrase, sym)

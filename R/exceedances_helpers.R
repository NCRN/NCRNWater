# R/exceedances_helpers.R
# Small, composable helpers for Shiny panel logic that lean on NCRNWater::exceed()

# --- Metadata & data fetch ----------------------------------------------------

# Fetch site/characteristic metadata for one (park, site, param)
site_info <- function(WaterData, park, site, param) {
  list(
    SiteName             = NCRNWater::getCharInfo(WaterData, parkcode = park, sitecode = site, charname = param, info = "SiteName"),
    DisplayName          = NCRNWater::getCharInfo(WaterData, parkcode = park, sitecode = site, charname = param, info = "DisplayName"),
    Units                = NCRNWater::getCharInfo(WaterData, parkcode = park, sitecode = site, charname = param, info = "Units"),
    LowerPoint           = NCRNWater::getCharInfo(WaterData, parkcode = park, sitecode = site, charname = param, info = "LowerPoint"),
    UpperPoint           = NCRNWater::getCharInfo(WaterData, parkcode = park, sitecode = site, charname = param, info = "UpperPoint"),
    LowerPointCondition  = NCRNWater::getCharInfo(WaterData, parkcode = park, sitecode = site, charname = param, info = "LowerPointCondition"),
    UpperPointCondition  = NCRNWater::getCharInfo(WaterData, parkcode = park, sitecode = site, charname = param, info = "UpperPointCondition"),
    LowerDescription     = NCRNWater::getCharInfo(WaterData, parkcode = park, sitecode = site, charname = param, info = "LowerDescription"),
    UpperDescription     = NCRNWater::getCharInfo(WaterData, parkcode = park, sitecode = site, charname = param, info = "UpperDescription")
  )
}

# Pull raw measurements for one (park, site, param)
site_data <- function(WaterData, park, site, param) {
  df <- NCRNWater::getWData(WaterData,
                            parkcode = park, sitecode = site,
                            charname = param, output = "data.frame")
  # Basic schema checks
  if (!"Date" %in% names(df))  stop("Expected 'Date' column.")
  if (!"Value" %in% names(df)) stop("Expected 'Value' column.")
  df
}

# Row-level exceedances (points = "both")
exceed_rows <- function(WaterData, park, site, param) {
  rows <- NCRNWater::exceed(WaterData,
                            parkcode = park, sitecode = site,
                            charname = param, points = "both",
                            mode = "rows")
  needed <- c("Date","Value","LowerPoint","UpperPoint",
              "LowerPointCondition","UpperPointCondition",
              "Exceed_Lower","Exceed_Upper","Exceed_Type")
  miss <- setdiff(needed, names(rows))
  if (length(miss)) stop("Rows-mode exceed schema missing columns: ", paste(miss, collapse = ", "))
  rows
}

# --- Threshold helpers --------------------------------------------------------

# Choose a single threshold to display in titles/alt text, based on actual exceedances
choose_ex_point <- function(rows, lower_point, upper_point) {
  has_lower <- any(rows$Exceed_Lower %in% TRUE)
  has_upper <- any(rows$Exceed_Upper %in% TRUE)
  if (has_lower && !has_upper) return(lower_point)
  if (has_upper && !has_lower) return(upper_point)
  # If both occurred, return NA and refer to "threshold(s)" in text
  NA_real_
}

# Generate threshold descriptions when metadata descriptions are missing
fill_threshold_descriptions <- function(info) {
  lower_desc <- info$LowerDescription
  upper_desc <- info$UpperDescription
  
  if (is.na(lower_desc) && is.na(upper_desc)) {
    if (!is.na(info$UpperPoint)) {
      upper_desc <- paste0(
        "Acceptable ", tolower(info$DisplayName), " is ",
        if (info$UpperPointCondition %in% c("lt","le")) "below " else "above ",
        info$UpperPoint, " ", info$Units, "."
      )
    } else if (!is.na(info$LowerPoint)) {
      lower_desc <- paste0(
        "Acceptable ", tolower(info$DisplayName), " is ",
        if (info$LowerPointCondition %in% c("gt","ge")) "above " else "below ",
        info$LowerPoint, " ", info$Units, "."
      )
    }
  }
  
  list(LowerDescription = lower_desc, UpperDescription = upper_desc)
}

# --- Data wrangling for display ----------------------------------------------

# Distance from exceeded threshold
add_difference <- function(rows) {
  rows$Difference <- dplyr::case_when(
    rows$Exceed_Type == "upper" & !is.na(rows$UpperPoint) ~ rows$Value - rows$UpperPoint,
    rows$Exceed_Type == "lower" & !is.na(rows$LowerPoint) ~ rows$Value - rows$LowerPoint,
    TRUE ~ NA_real_
  )
  rows$Difference <- round(rows$Difference, 4)
  rows
}

# Yearly totals and exceed counts with formatted percentages
yearly_summary <- function(all_df, exceed_rows_df) {
  df_non_na <- dplyr::filter(all_df, !is.na(Value))
  totcount  <- df_non_na %>%
    dplyr::mutate(Year = lubridate::year(Date)) %>%
    dplyr::count(Year, name = "ntot") %>%
    dplyr::filter(!is.na(Year))
  
  excount <- exceed_rows_df %>%
    dplyr::mutate(Year = lubridate::year(Date)) %>%
    dplyr::count(Year, name = "nex") %>%
    dplyr::filter(!is.na(Year))
  
  histdata <- dplyr::left_join(totcount, excount, by = "Year")
  histdata[is.na(histdata)] <- 0
  histdata <- histdata %>%
    dplyr::mutate(percent_ex = (nex / ntot) * 100,
                  formatted_percent_ex = scales::percent(percent_ex / 100, accuracy = 1))
  histdata
}

vec_format <- function(vec) {
  vec <- as.character(vec)
  n <- length(vec)
  
  if (n == 0) {
    ""
  } else if (n == 1) {
    vec[1]
  } else if (n == 2) {
    paste(vec, collapse = " and ")
  } else {
    paste0(paste(vec[-n], collapse = ", "), ", and ", vec[n])
  }
}

# Compose natural language snippets for the panel
compose_text <- function(histdata, info, ex_point, sum_nex, non_na_obs) {
  recent_year  <- max(histdata$Year)
  oldest_year  <- min(histdata$Year)
  nex_recent   <- histdata$nex[histdata$Year == recent_year]
  
  # Highest/lowest % across all years
  highest_rate <- histdata$formatted_percent_ex[which.max(histdata$percent_ex)]
  lowest_rate  <- histdata$formatted_percent_ex[which.min(histdata$percent_ex)]
  
  # All years tied for highest/lowest %
  hry_years <- histdata$Year[histdata$percent_ex == max(histdata$percent_ex)]
  lry_years <- histdata$Year[histdata$percent_ex == min(histdata$percent_ex)]
  
  # Build text snippets for the panel
  grammar1 <- if (nex_recent == 0) {
    paste0("there were no ", info$DisplayName, " exceedances")
  } else if (nex_recent == 1) {
    paste0(
      "there was ", "<b>", nex_recent, "</b>", " ", info$DisplayName,
      " exceedance (", histdata$formatted_percent_ex[histdata$Year == recent_year], ")"
    )
  } else {
    paste0(
      "there were ", "<b>", nex_recent, "</b>", " ", info$DisplayName,
      " exceedances (", histdata$formatted_percent_ex[histdata$Year == recent_year], ")"
    )
  }
  
  grammar2 <- if (sum_nex == 0) {
    paste0("there were no ", info$DisplayName, " exceedances")
  } else if (sum_nex == 1) {
    paste0("there was ", "<b>", sum_nex, "</b>", " ", info$DisplayName, " exceedance")
  } else {
    paste0("there were ", "<b>", sum_nex, "</b>", " ", info$DisplayName, " exceedances")
  }
  
  list(
    recent_year        = recent_year,
    oldest_year        = oldest_year,
    highest_rate       = highest_rate,
    lowest_rate        = lowest_rate,
    highest_rate_years = vec_format(hry_years),
    lowest_rate_years  = vec_format(lry_years),
    grammar1           = grammar1,
    grammar2           = grammar2
  )
}



# Build the histogram plot
build_plot <- function(histdata, sitename, display_name, unit, ex_point, sum_nex, non_na_obs) {
  ttl <- paste0(
    sitename, ", ", display_name, ", ",
    min(histdata$Year), "-", max(histdata$Year), "\n",
    sum_nex, " of ", non_na_obs, " measurements (",
    round(100 * (sum_nex / non_na_obs), 1), "%) exceeded ",
    if (!is.na(ex_point)) ex_point else "threshold(s)", " ", unit
  )
  
  ggplot2::ggplot(histdata, ggplot2::aes(x = Year, y = percent_ex)) +
    ggplot2::geom_bar(stat = "identity", fill = "lightgray") +
    ggplot2::scale_x_continuous(breaks = seq(min(histdata$Year), max(histdata$Year), by = 1)) +
    ggplot2::ylim(0, 100) +
    ggplot2::labs(title = ttl, x = "Year", y = "% of measurements") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5))
}

# --- Orchestrator for one site -----------------------------------------------

# Build a per-site package used by the Shiny panel
build_site_package <- function(WaterData, park, site, param) {
  info        <- site_info(WaterData, park, site, param)
  df_all      <- site_data(WaterData, park, site, param)
  non_na_obs  <- nrow(dplyr::filter(df_all, !is.na(Value)))
  rows_ex     <- exceed_rows(WaterData, park, site, param)
  
  # Fill threshold descriptions when missing
  descs       <- fill_threshold_descriptions(info)
  
  # Display table (rename to UI-friendly labels)
  df_display <- df_all %>%
    dplyr::transmute(
      Site       = info$SiteName,
      SampleDate = Date,
      Parameter  = info$DisplayName,
      Value,
      Units      = info$Units
    )
  
  # Exceeded rows table
  exdf <- rows_ex %>%
    add_difference() %>%
    dplyr::mutate(
      UpperThreshold = descs$UpperDescription,
      LowerThreshold = descs$LowerDescription
    ) %>%
    dplyr::arrange(dplyr::desc(Date)) %>%
    dplyr::transmute(
      Site        = info$SiteName,
      SampleDate  = Date,
      Parameter   = info$DisplayName,
      Value,
      Difference,
      Units       = info$Units,
      UpperThreshold,
      LowerThreshold
    )
  
  histdata <- yearly_summary(df_all, rows_ex)
  sum_nex  <- sum(histdata$nex)
  ex_point <- choose_ex_point(rows_ex, info$LowerPoint, info$UpperPoint)
  
  txt   <- compose_text(histdata, info, ex_point, sum_nex, non_na_obs)
  plot  <- build_plot(histdata, info$SiteName, info$DisplayName, info$Units, ex_point, sum_nex, non_na_obs)
  
  # Optional: hover text for bars
  histdata$hover_text <- paste0(
    histdata$Year, ", ", info$DisplayName, "\n",
    histdata$ntot, " total observation(s)\n",
    histdata$formatted_percent_ex, " exceeded ",
    if (!is.na(ex_point)) ex_point else "threshold(s)", " ", info$Units
  )
  
  list(
    df              = df_display,
    total_obs       = nrow(df_all),
    non_na_obs      = non_na_obs,
    LowerThreshold  = descs$LowerDescription,
    UpperThreshold  = descs$UpperDescription,
    LowerPoint      = info$LowerPoint,
    UpperPoint      = info$UpperPoint,
    Sitename        = info$SiteName,
    Characteristic  = info$DisplayName,
    Unit            = info$Units,
    exdf            = exdf,
    histyears       = data.frame(Year = lubridate::year(df_all$Date)),
    totcount        = dplyr::select(histdata, Year, ntot),
    excount         = dplyr::select(histdata, Year, nex),
    histdata        = histdata,
    recent_year     = txt$recent_year,
    oldest_year     = txt$oldest_year,
    n_ex_year       = sum(histdata$nex != 0),
    highest_ex_rate = txt$highest_rate,
    lowest_ex_rate  = txt$lowest_rate,
    highest_rate_year = txt$highest_rate_years,
    lowest_rate_year  = txt$lowest_rate_years,
    grammar1        = txt$grammar1,
    grammar2        = txt$grammar2,
    ExPoint         = ex_point,
    p               = plot,
    alt_raw         = NULL,
    alt_bullets     = NULL,
    alt_bullets2    = NULL,
    html_extext     = NULL
  )
}

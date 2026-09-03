# R/congruency.R

#' Check structural and semantic congruency between WQP data and NCRNWater metadata
#'
#' Validates whether a user-provided WQP data file and NCRNWater metadata file are
#' structurally compatible with each other and with the expected NCRNWater templates
#' located under inst/extdata/templates/. Prints a human-readable report and halts
#' with an error if problems exist.
#'
#' @examples
#' \dontrun{
#' data_fp <- "path/to/wqp.csv"
#' meta_fp <- "path/to/wqp_ncrnwater_metadata.csv"
#' congruency(data_fp, meta_fp)
#' }
#' @export
congruency <- function(
    data_filename,
    metadata_filename,
    data_template = system.file("extdata", "templates/wqp.csv", package = "NCRNWater"),
    metadata_template = system.file("extdata", "templates/wqp_ncrnwater_metadata.csv", package = "NCRNWater")
) {
  # Guard: template files must exist in the installed package
  if (identical(data_template, "") || identical(metadata_template, "")) {
    stop("Template files not found under inst/extdata/templates/. Reinstall NCRNWater or contact maintainers.")
  }
  
  cat(sprintf("Checking congruency for user-files %s and %s ...\n\n", data_filename, metadata_filename))
  
  results <- list(problems = 0L, msgs = character(0))
  
  # existence + extension checks
  results <- check_congruency_files_exist(c(data_filename, metadata_filename), results)
  results <- check_congruency_files_are_csvs(c(data_filename, metadata_filename), results)
  
  # if problems were flagged, stop BEFORE reading user files
  
  if (is.list(results) && !is.null(results$problems) && results$problems > 0L) {
    msgs <- paste(results$msgs, collapse = "\n")
    cat(msgs, "\n", sep = "")
    stop(sprintf("Your files have %d congruency problem(s). Resolve those problems before proceeding.\n",
                 results$problems))
  }
  
  
  # Build file containers (user and template) — safe now because files exist and are CSVs
  files <- list(
    user = list(
      data     = list(df = utils::read.csv(data_filename, stringsAsFactors = FALSE),     fname = data_filename),
      metadata = list(df = utils::read.csv(metadata_filename, stringsAsFactors = FALSE), fname = metadata_filename)
    ),
    template = list(
      data     = list(df = utils::read.csv(data_template, stringsAsFactors = FALSE),     fname = data_template),
      metadata = list(df = utils::read.csv(metadata_template, stringsAsFactors = FALSE), fname = metadata_template)
    )
  )
  
  
  # Column name congruency (user vs template)
  files_to_check <- list(
    c(files$user$data$fname,     files$template$data$fname),
    c(files$user$metadata$fname, files$template$metadata$fname)
  )
  results <- congruency_helper_column_names(files, files_to_check, results)
  
  # Value congruency across known pairs (user data vs user metadata)
  results <- check_congruency_user_versus_user(files, results)
  
  msgs <- paste(results$msgs, collapse = "\n")
  if (results$problems == 0L) {
    cat(msgs, "\n\nOK to proceed!", sep = "")
  } else {
    cat(msgs)
    stop(sprintf("Your files have %d congruency problem(s). Resolve those problems before proceeding.\n", results$problems))
  }
}

# ---- Helpers (internal) ------------------------------------------------------

#' @keywords internal
check_congruency_files_exist <- function(files, results) {
  tmp <- list(msgs = character(0), problems = 0L)
  for (f in files) {
    if (!file.exists(f)) {
      msg <- sprintf("'%s' does not exist. Check for typos or missing information and try again.", f)
      tmp$problems <- tmp$problems + 1L
    } else {
      msg <- sprintf("%s exists", f)
    }
    tmp$msgs <- c(tmp$msgs, msg)
  }
  results$problems <- results$problems + tmp$problems
  results$msgs     <- c(results$msgs, tmp$msgs)
  results
}

#' @keywords internal
check_congruency_files_are_csvs <- function(files, results) {
  tmp <- list(msgs = character(0), problems = 0L)
  for (f in files) {
    is_csv <- tolower(tools::file_ext(f)) == "csv"
    if (!is_csv) {
      msg <- sprintf("'%s' is not a CSV file. Check for typos or missing information and try again.", f)
      tmp$problems <- tmp$problems + 1L
    } else {
      msg <- sprintf("%s is a CSV", f)
    }
    tmp$msgs <- c(tmp$msgs, msg)
  }
  results$problems <- results$problems + tmp$problems
  results$msgs     <- c(results$msgs, tmp$msgs)
  results
}

#' @keywords internal
congruency_helper_column_names <- function(files, files_to_check, results) {
  tmp <- list(msgs = character(0), problems = 0L)
  
  # template locals
  data_template_df <- files$template$data$df
  data_template_fn <- files$template$data$fname
  meta_template_df <- files$template$metadata$df
  meta_template_fn <- files$template$metadata$fname
  
  # user locals
  data_df <- files$user$data$df
  data_fn <- files$user$data$fname
  meta_df <- files$user$metadata$df
  meta_fn <- files$user$metadata$fname
  
  # Compare (user data vs template data) and (user metadata vs template metadata)
  pairs <- list(
    list(user_df = data_df, user_label = data_fn, templ_df = data_template_df, templ_label = data_template_fn),
    list(user_df = meta_df, user_label = meta_fn, templ_df = meta_template_df, templ_label = meta_template_fn)
  )
  
  for (p in pairs) {
    user_cols  <- colnames(p$user_df)
    templ_cols <- colnames(p$templ_df)
    miss_user  <- which(!(user_cols %in% templ_cols))
    miss_templ <- which(!(templ_cols %in% user_cols))
    
    if (length(miss_user) > 0L || length(miss_templ) > 0L) {
      tmp$msgs <- c(tmp$msgs,
                    sprintf("\nThe column names %s do not match the column names in %s\n",
                            p$user_label, p$templ_label))
      if (length(miss_templ) > 0L) {
        cols <- templ_cols[miss_templ]
        tmp$msgs <- c(tmp$msgs,
                      sprintf("%d column(s) are in %s but not in %s:\n%s\n",
                              length(cols), p$templ_label, p$user_label, paste(cols, collapse = ", ")))
        tmp$problems <- tmp$problems + 1L
      }
      if (length(miss_user) > 0L) {
        cols <- user_cols[miss_user]
        tmp$msgs <- c(tmp$msgs,
                      sprintf("%d column(s) are in %s but not in %s:\n%s\n",
                              length(cols), p$user_label, p$templ_label, paste(cols, collapse = ", ")))
        tmp$problems <- tmp$problems + 1L
      }
    }
  }
  
  if (tmp$problems == 0L) {
    tmp$msgs <- c(tmp$msgs,
                  sprintf("\n%d pairs of files passed column-congruency checks:\n%s columns match %s\n%s columns match %s",
                          length(files_to_check),
                          data_fn, data_template_fn,
                          meta_fn, meta_template_fn))
  }
  
  results$problems <- results$problems + tmp$problems
  results$msgs     <- c(results$msgs, tmp$msgs)
  results
}

#' @keywords internal
check_congruency_user_versus_template <- function(files, results) {
  files_to_check <- list(
    c(files$user$data$fname,     files$template$data$fname),
    c(files$user$metadata$fname, files$template$metadata$fname)
  )
  congruency_helper_column_names(files, files_to_check, results)
}

#' @keywords internal
check_congruency_user_versus_user <- function(files, results) {
  # map each data column to its corresponding metadata column
  cols_to_check <- list(
    c("MonitoringLocationIdentifier", "SiteCode"),
    c("MonitoringLocationIdentifier", "SiteCodeWQX"),
    c("MonitoringLocationName",       "SiteName"),
    c("CharacteristicName",           "DataName")
  )
  congruency_helper_values(files, cols_to_check, results)
}

#' @keywords internal
congruency_helper_values <- function(files, cols_to_check, results) {
  tmp <- list(msgs = character(0), problems = 0L)
  
  # user locals
  data_df    <- files$user$data$df
  data_fn    <- files$user$data$fname
  metadata_df <- files$user$metadata$df
  metadata_fn <- files$user$metadata$fname
  
  for (i in seq_along(cols_to_check)) {
    data_col     <- cols_to_check[[i]][1]
    metadata_col <- cols_to_check[[i]][2]
    
    data_vals     <- unique(data_df[[data_col]])
    metadata_vals <- unique(metadata_df[[metadata_col]])
    
    in_data_not_metadata     <- which(!(data_vals %in% metadata_vals))
    in_metadata_not_data     <- which(!(metadata_vals %in% data_vals))
    
    if (length(in_data_not_metadata) > 0L || length(in_metadata_not_data) > 0L) {
      tmp$msgs <- c(tmp$msgs,
                    sprintf("\nThe values %s$%s do not match the values in %s$%s\n",
                            data_fn, data_col, metadata_fn, metadata_col))
      
      if (length(in_data_not_metadata) > 0L) {
        vals <- data_vals[in_data_not_metadata]
        tmp$msgs <- c(tmp$msgs,
                      sprintf("%d value(s) are in %s but not in %s:\n%s\n",
                              length(vals), data_fn, metadata_fn, paste(vals, collapse = ", ")))
        tmp$problems <- tmp$problems + 1L
      }
      
      if (length(in_metadata_not_data) > 0L) {
        vals <- metadata_vals[in_metadata_not_data]
        tmp$msgs <- c(tmp$msgs,
                      sprintf("%d value(s) are in %s but not in %s:\n%s\n",
                              length(vals), metadata_fn, data_fn, paste(vals, collapse = ", ")))
        tmp$problems <- tmp$problems + 1L
      }
    }
  }
  
  # Success message if no problems
  if (tmp$problems == 0L) {
    msg_begining <- sprintf("\n%d pairs of columns passed value-congruency checks:\n", length(cols_to_check))
    msg_middle <- character(0)
    for (i in seq_along(cols_to_check)) {
      msg_middle <- c(msg_middle,
                      sprintf("%s$%s values match %s$%s",
                              data_fn, cols_to_check[[i]][1], metadata_fn, cols_to_check[[i]][2]))
    }
    tmp$msgs <- c(tmp$msgs, paste0(msg_begining, paste(msg_middle, collapse = "\n")))
  }
  
  results$problems <- results$problems + tmp$problems
  results$msgs     <- c(results$msgs, tmp$msgs)
  results
}

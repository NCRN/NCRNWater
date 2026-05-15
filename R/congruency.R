#' Check structural and semantic congruency between WQP data and NCRNWater metadata
#'
#' @description
#' `congruency()` validates whether a user-provided WQP data file and NCRNWater
#' metadata file are structurally compatible with each other and with the
#' expected NCRNWater templates. The function checks:
#'
#' * that both user files exist and are CSVs
#' * that the **columns** in the user data and metadata match those in the
#'   official NCRNWater template files
#' * that key **value pairs** (e.g., SiteCode vs SiteCodeWQX, CharacteristicName vs DataName)
#'   are consistent across files
#' * that the user data is internally consistent with the user metadata
#'
#' The function prints a human‑readable report to the console and halts with an error
#'
#' @examples
#' \dontrun{
#' library(NCRNWater)
#' my_data <- 'data/wqp.csv'
#' my_metadata <- 'data/metadata.csv'
#' NCRNWater::congruency(my_data, my_metadata)
#' }
#' 
#' @export
#'


congruency <- function(
    data_filename,
    metadata_filename,
    data_template = system.file("templates", "wqp.csv", package = "NCRNWater"),
    metadata_template = system.file("templates", "wqp_ncrnwater_metadata.csv", package = "NCRNWater")
    ) {
  
  
  # Optional: guard if system.file didn't find the files
  if (identical(data_template, "") || identical(metadata_template, "")) {
    stop("Template files not found in installed package. Reinstall NCRNWater or contact maintainers.")
  }
  
  
  cat(paste0('Checking congruency for user-files ', data_filename, ' and ', metadata_filename, ' ...\n\n'))
  
  # package together for convenience
  files <- c(data_filename, metadata_filename)
  
  # instantiate
  results <- list()
  results[['problems']] <- 0
  results[['msgs']] <- c()
  
  # do the files exist?
  results <- check_congruency_files_exist(files, results)
  
  # are the files csv?
  results <- check_congruency_files_are_csvs(files, results)
  
  # if the files exist and are the correct filetype, we're ready to test the files themselves
  # package together for convenience
  files <- list(
    'user' = list(
      'data' = list(
        'df' = read.csv(data_filename)
        ,'fname' = data_filename
      )
      ,'metadata' = list(
        'df' = read.csv(metadata_filename)
        ,'fname' = metadata_filename
      )
    )
    ,'template' = list(
      'data' = list(
        'df' = read.csv(data_template)
        ,'fname' = ifelse(data_template=='templates/wqp.csv', 'WQP format', data_template)
      )
      ,'metadata' = list(
        'df' = read.csv(metadata_template)
        ,'fname' = ifelse(metadata_template=='templates/wqp_ncrnwater_metadata.csv', 'NCRNWater metadata format', metadata_template)
      )
    )
  )
  
  # check user data against template data
  # check user metadata against template metadata
  results <- check_congruency_user_versus_template(files, results)
  
  # check user data against user metadata
  results <- check_congruency_user_versus_user(files, results)
  
  msgs <- paste(results[['msgs']], collapse = '\n')
  if (results$problems == 0){
    cat(msgs)
    cat('\n')
    cat('\nOK to proceed!')
  } else {
    cat(msgs)
    stop(paste0('Your files have ',results[['problems']],' congruency problem(s). Resolve those problems before proceeding.\n'))
  }
  
}

check_congruency_user_versus_template <- function(files, results) {
  
  files_to_check <- list(
    c(files[['user']][['data']][['fname']], files[['template']][['data']][['fname']])
    ,c(files[['user']][['metadata']][['fname']], files[['template']][['metadata']][['fname']])
  )
  
  # are the columns present and named properly?
  results <- congruency_helper_column_names(files, files_to_check, results)
  
  return(results)
  
}

check_congruency_user_versus_user <- function(files, results) {
  
  # map each data column to its corresponding metadata column
  cols_to_check <- list(
    c('MonitoringLocationIdentifier', 'SiteCode')
    ,c('MonitoringLocationIdentifier', 'SiteCodeWQX')
    ,c('MonitoringLocationName', 'SiteName')
    ,c('CharacteristicName', 'DataName')
  )
  
  # are the values in column-pairs consistent?
  results <- congruency_helper_values(files, cols_to_check, results)
  
  # TODO: type checking. Check that the columns that NCRNWater needs to be certain formats are actually those types.
  # e.g., numbers are numeric, dates are formatted properly, factors, characters, etc.
  # is.numeric(metadata$LowerPoint)
  # is.numeric(metadata$UpperPoint)
  # is.numeric(metadata$Lat)
  # is.numeric(metadata$Long)
  # parse metadata
  # metadata %>% dplyr::select(DataName, DataType) %>% dplyr::distinct(DataName, DataType)
  # does the metadata#DataType agree with data$ResultMeasureValue?
  # make a lookup for (metadata$DataName, metadata$DataType)
  # make a table of (data$CharacteristicName, data$ResultMeasureValue)
  # try to change the column's data type and see what breaks
  
  # TODO: null checking. Check that required columns have values.
  # e.g., thresholds (metadata$UpperPoint, metadata$LowerPoint) are required because things break otherwise
  # the only column that you're allowed to provide NAs:
  # LowerPoint, UpperPoint, LowerDescription, UpperDescription
  # These can't be ALL NAs, but some NAs are allowed
  
  # TODO: coverage
  # are there duplicates in the metadata file
  # (SiteCode, DataName)
  # is every combination of (MonitoringLocationIdentifier, CharacteristicName) covered with a combination of (SiteCode, DataName)?
  
  
  return(results)
  
}

congruency_helper_values <- function(files, cols_to_check, results) {
  
  tmp <- list(
    'msgs' = c()
    ,problems = 0
  )
  
  # template local variables
  data_template <- files[['template']][['data']][['df']]
  data_template_filename <- files[['template']][['data']][['fname']]
  metadata_template <- files[['template']][['metadata']][['df']]
  metadata_template_filename <- files[['template']][['metadata']][['fname']]
  
  # user local variables
  data <- files[['user']][['data']][['df']]
  data_filename <- files[['user']][['data']][['fname']]
  metadata <- files[['user']][['metadata']][['df']]
  metadata_filename <- files[['user']][['metadata']][['fname']]
  
  for (i in seq_along(cols_to_check)){
    data_col <- cols_to_check[[i]][1]
    metadata_col <- cols_to_check[[i]][2]
    
    if (any(data[[data_col]] %>% unique %in% metadata[[metadata_col]] %>% unique == F)){
      
      msg_beginning <- paste0("\nThe values ", data_filename, "$",data_col, " do not match ")
      msg_end <- paste0('the values in ', metadata_filename, "$",metadata_col, '\n')
      msg <- paste0(msg_beginning, msg_end)
      tmp[['msgs']] <- c(tmp[['msgs']], msg)
      
      in_data_not_metadata <- which(data[[data_col]] %>% unique %in% metadata[[metadata_col]] %>% unique == F)
      in_metadata_not_data <- which(metadata[[metadata_col]] %>% unique %in% data[[data_col]] %>% unique == F)
      
      if (length(in_data_not_metadata)>0){
        values_in_data_not_metadata <- (data[[data_col]] %>% unique)[in_data_not_metadata]
        msg_beginning <- paste0(length(in_data_not_metadata), " value(s) are in ",data_filename," but not in ",metadata_filename,":\n")
        msg_end <- "\n"
        msg <- paste0(msg_beginning, paste(values_in_data_not_metadata, collapse = ', '), msg_end)
        tmp[['problems']] <- tmp[['problems']] + 1
        tmp[['msgs']] <- c(tmp[['msgs']], msg)
      }
      
      if (length(in_metadata_not_data)>0){
        values_in_metadata_not_data <- (metadata[[metadata_col]] %>% unique)[in_metadata_not_data]
        msg_beginning <- paste0(length(in_metadata_not_data), " value(s) are in ",metadata_filename," but not in ",data_filename,":\n")
        msg_end <- "\n"
        msg <- paste0(msg_beginning, paste(values_in_metadata_not_data, collapse = ', '), msg_end)
        tmp[['problems']] <- tmp[['problems']] + 1
        tmp[['msgs']] <- c(tmp[['msgs']], msg)
      }
    }
  }
  
  if (tmp[['problems']]==0){
    msg_begining <- paste0('\n',length(cols_to_check), ' pairs of columns passed value-congruency checks:\n')
    msg_middle <- c()
    for (i in seq_along(cols_to_check)){
      data_col <- paste0(data_filename, '$',cols_to_check[[i]][1])
      metadata_col <- paste0(metadata_filename, '$', cols_to_check[[i]][2])
      msg_tmp <- paste0(data_col, ' values match ', metadata_col)
      msg_middle <- c(msg_middle, msg_tmp)
    }
    msg <- paste0(msg_begining, paste(msg_middle, collapse = '\n'))
    tmp[['msgs']] <- c(tmp[['msgs']], msg)
  }
  
  results[['problems']] <- results[['problems']] + tmp[['problems']]
  results[['msgs']] <- c(results[['msgs']], tmp[['msgs']])
  
  return(results)
}

check_congruency_files_exist <- function(files, results) {
  tmp <- list(
    'msgs' = c()
    ,problems = 0
  )
  
  for (f in files){
    if (file.exists(f)==F){
      msg_beginning <- "'" 
      msg_end <- ' does not exist. Check for typos or missing information and try again.'
      msg <- paste0(msg_beginning, f, msg_end)
      tmp[['problems']] <- tmp[['problems']] + 1
    } else {
      msg_beginning <- ""
      msg_end <- ' exists'
      msg <- paste0(msg_beginning, f, msg_end)
    }
    tmp[['msgs']] <- c(tmp[['msgs']], msg)
  }
  
  results[['problems']] <- results[['problems']] + tmp[['problems']]
  results[['msgs']] <- c(results[['msgs']], tmp[['msgs']])
  return(results)
}

check_congruency_files_are_csvs <- function(files, results) {
  tmp <- list(
    'msgs' = c()
    ,problems = 0
  )
  
  for (f in files){
    if (endsWith(f, '.csv')==F){
      msg_beginning <- "'" 
      msg_end <- ' is not a CSV file. Check for typos or missing information and try again.'
      msg <- paste0(msg_beginning, f, msg_end)
      tmp[['problems']] <- tmp[['problems']] + 1
    } else {
      msg_beginning <- ""
      msg_end <- ' is a CSV'
      msg <- paste0(msg_beginning, f, msg_end)
    }
    tmp[['msgs']] <- c(tmp[['msgs']], msg)
  }
  
  results[['problems']] <- results[['problems']] + tmp[['problems']]
  results[['msgs']] <- c(results[['msgs']], tmp[['msgs']])
  return(results)
}

congruency_helper_column_names <- function(files, files_to_check, results) {
  
  tmp <- list(
    'msgs' = c()
    ,problems = 0
  )
  
  # template local variables
  data_template <- files[['template']][['data']][['df']]
  data_template_filename <- files[['template']][['data']][['fname']]
  metadata_template <- files[['template']][['metadata']][['df']]
  metadata_template_filename <- files[['template']][['metadata']][['fname']]
  
  # user local variables
  data <- files[['user']][['data']][['df']]
  data_filename <- files[['user']][['data']][['fname']]
  metadata <- files[['user']][['metadata']][['df']]
  metadata_filename <- files[['user']][['metadata']][['fname']]
  
  for (i in seq_along(files_to_check)){
    user_file <- files_to_check[[i]][1]
    template_file <- files_to_check[[i]][2]
    
    if (any(colnames(user_file) %in% colnames(template_file) == F) | any(colnames(template_file) %in% colnames(user_file) == F)){
      msg_beginning <- paste0("\nThe column names ", user_file, " do not match ")
      msg_end <- paste0('the column names in ', template_file, '\n')
      msg <- paste0(msg_beginning, msg_end)
      tmp[['msgs']] <- c(tmp[['msgs']], msg)
      
      in_template_not_user <- which(colnames(data_template) %in% colnames(data) == F)
      in_user_not_template <- which(colnames(data) %in% colnames(data_template) == F)
      
      if (length(in_template_not_user)>0){
        values_in_template_not_user <- colnames(data_template)[in_template_not_user]
        msg_beginning <- paste0(length(in_template_not_user), " column(s) are in ",template_file," but not in ",user_file,":\n")
        msg_end <- "\n"
        msg <- paste0(msg_beginning, paste(values_in_data_not_metadata, collapse = ', '), msg_end)
        tmp[['problems']] <- tmp[['problems']] + 1
        tmp[['msgs']] <- c(tmp[['msgs']], msg)
      }
      if (length(in_user_not_template)>0){
        values_in_template_not_user <- colnames(data)[in_user_not_template]
        msg_beginning <- paste0(length(in_user_not_template), " column(s) are in ",user_file," but not in ",template_file,":\n")
        msg_end <- "\n"
        msg <- paste0(msg_beginning, paste(values_in_data_not_metadata, collapse = ', '), msg_end)
        tmp[['problems']] <- tmp[['problems']] + 1
        tmp[['msgs']] <- c(tmp[['msgs']], msg)
      }
    }
  }
  
  if (tmp[['problems']]==0){
    msg_begining <- paste0('\n',length(files_to_check), ' pairs of files passed column-congruency checks:\n')
    msg_middle <- c()
    for (i in seq_along(files_to_check)){
      data_col <- paste0(files_to_check[[i]][1])
      metadata_col <- paste0(files_to_check[[i]][2])
      msg_tmp <- paste0(data_col, ' columns match ', metadata_col)
      msg_middle <- c(msg_middle, msg_tmp)
    }
    msg <- paste0(msg_begining, paste(msg_middle, collapse = '\n'))
    tmp[['msgs']] <- c(tmp[['msgs']], msg)
  }
  
  results[['problems']] <- results[['problems']] + tmp[['problems']]
  results[['msgs']] <- c(results[['msgs']], tmp[['msgs']])
  
  return(results)
}

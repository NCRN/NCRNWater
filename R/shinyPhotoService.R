# R/shinyPhotoService.R

#' Parse NCRN water-quality photo filenames into a nested index
#'
#' @description
#' Builds a nested list of photo metadata from a directory of JPEG images
#' following NCRN water-quality naming conventions. The hierarchy is:
#' `park -> site -> year -> sitevisit -> filename`, where each filename node
#' stores:
#' - `rel_fpath`: full path to the image file
#' - `sortorder`: an index or timestamp extracted from the filename used
#'   for ordering within a site visit.
#'   
#' - The function operates on the **top-level** of `directory`. To process
#'   subdirectories, you could replace `list.files(directory)` with
#'   `list.files(directory, recursive = TRUE)` and adjust path handling.
#' - `sortorder` comes from either the parenthetical index (conventions 1 & 3)
#'   or the terminal timestamp segment (convention 2).
#' - If `getSiteInfo()` fails to resolve a site name (e.g., unknown park/site),
#'   the `sitevisit` string may be malformed or cause an error.
#' - Filenames must follow one of the three supported conventions; deviations
#'   may lead to incorrect parsing or dropped files.
#'
#' @param directory Character scalar. Path to the directory containing image
#'   files. Only files with a `.jpg`/`.JPG` extension are processed. Subdirectories
#'   are **not** traversed (see Notes for making it recursive).
#' @param object An `NCRNWater` network/site object (or compatible) that can be
#'   passed to `NCRNWater::getSiteInfo()` to resolve a human-readable site name via
#'   `getSiteInfo(object, parkcode = <park>, sitecode = <site>, info = "SiteName")`.
#'
#' @details
#' The function recognizes **three** filename conventions and extracts
#' `park`, `site`, `date`, and `index` accordingly:
#'
#' 1. **Water program prefix**  
#'    Example: `"WATER_ANTI_SHCK_20240201 (1).JPG"`  
#'    Pattern: `WATER_<PARK>_<SITE>_<YYYYMMDD> (<index>).JPG`  
#'    - `park` = `ANTI`  
#'    - `site` = `SHCK`  
#'    - `date` = `20240201` (YYYYMMDD)  
#'    - `index` = number in parentheses (e.g., `1`)
#'
#' 2. **DWQ/CWQ prefixed, with hyphenated date and timestamp**  
#'    Example: `"dwq_NCRN_MONO_BUCK_2024-06-04_20240604-084406.jpg"`  
#'    Pattern: `dwq|cwq_NCRN_<PARK>_<SITE>_<YYYY-MM-DD>_<YYYYMMDD-HHMMSS>.jpg`  
#'    - `park` = `MONO`  
#'    - `site` = `BUCK`  
#'    - `date` = `20240604` (derived from `YYYY-MM-DD`)  
#'    - `index` = timestamp portion after the last underscore (e.g., `20240604-084406`)
#'
#' 3. **Park-site with date and index**  
#'    Example: `"ANTI_SHCK_20181210 (8).JPG"`  
#'    Pattern: `<PARK>_<SITE>_<YYYYMMDD> (<index>).JPG`  
#'    - `park` = `ANTI`  
#'    - `site` = `SHCK`  
#'    - `date` = `20181210` (YYYYMMDD)  
#'    - `index` = number in parentheses (e.g., `8`)
#'
#' After parsing, `site` is normalized to the format `NCRN_<PARK>_<SITE>`. The
#' date is split into `year-month-day`, and a display key for the visit is built as:
#' `<SiteName> YYYY-MM-DD`, where `<SiteName>` is retrieved from
#' `NCRNWater::getSiteInfo(object, parkcode = park, sitecode = site, info = "SiteName")`.
#'
#' Files with extensions other than `.jpg`/`.JPG` are ignored. Case-insensitive
#' matching is used for the extension check.
#'
#' @return
#' A nested list with the structure:
#'
#' ```
#' imgs[[park]][[site]][[year]][[sitevisit]][[filename]]$rel_fpath  # full file path
#' imgs[[park]][[site]][[year]][[sitevisit]][[filename]]$sortorder  # index/timestamp
#' ```
#'
#' Where:
#' - `park` is the park code extracted from the filename (e.g., `"ANTI"`, `"MONO"`).
#' - `site` is normalized to `"NCRN_<PARK>_<SITE>"`.
#' - `year` is the four-digit year derived from the file's date.
#' - `sitevisit` is `"<SiteName> YYYY-MM-DD"`, resolved via `getSiteInfo(...)`.
#' - `filename` is the original filename.
#'
#' @examples
#' \dontrun{
#'   # Assuming `net` is an NCRNWater object and `img_dir` contains photos:
#'   imgs <- parsePhotos(directory = "path/to/photos", object = net)
#'
#'   # Access all photos for park ANTI, site NCRN_ANTI_SHCK, year 2024:
#'   anti_2024 <- imgs[["ANTI"]][["NCRN_ANTI_SHCK"]][["2024"]]
#'
#'   # Inspect a specific site visit (name depends on getSiteInfo result):
#'   visit_names <- names(anti_2024)
#'   first_visit <- anti_2024[[visit_names[1]]]
#'
#'   # Retrieve file path and sort order for a given filename:
#'   fn <- names(first_visit)[1]
#'   first_visit[[fn]]$rel_fpath
#'   first_visit[[fn]]$sortorder
#' }
#'
#' @seealso
#' \code{\link[NCRNWater]{getSiteInfo}}
#'
#' @export
parsePhotos <- function(directory, object) {
  imgs <- list()
  for (f in list.files(directory)){
    # we need the park, site, and date given a filename
    
    # we have three naming conventions to deal with
    # 1. "WATER_ANTI_SHCK_20240201 (1).JPG"
    # 2. "dwq_NCRN_MONO_BUCK_2024-06-04_20240604-084406.jpg"
    # 3. "ANTI_SHCK_20181210 (8).JPG"
    
    # to start with, we'll use what the string starts with
    if (tolower(tools::file_ext(f)) %in% c("jpg", "jpeg")){
      parts <- base::strsplit(f, '_')[[1]]
      
      # step 1: break the filename into pieces
      
      if (base::startsWith(f, 'WATER')){ # 1. "WATER_ANTI_SHCK_20240201 (1).JPG"
        
        # park and site
        park <- parts[2]
        site <- parts[3]
        # date and index
        tmp <- base::strsplit(parts[4], ' ')[[1]]
        dt <- tmp[1]
        idx <- base::sub(".*\\(([^)]*)\\).*", "\\1", tmp[2], ignore.case = T)
        
      } else if(base::startsWith(f, 'dwq') | base::startsWith(f, 'cwq')){ # 2. "dwq_NCRN_MONO_BUCK_2024-06-04_20240604-084406.jpg"
        
        # park and site
        park <- parts[3]
        site <- parts[4]
        # date and index
        idx <- base::sub('\\.jpg$', '', parts[6], ignore.case = TRUE)  # "20240604-084406"
        dt <- base::sub('-.*$', '', idx)                          # "20240604"

      } else { # 3. "ANTI_SHCK_20181210 (8).JPG"
        
        # park and site
        park <- parts[1]
        site <- parts[2]
        # date and index
        tmp <- base::strsplit(parts[3], ' ')[[1]]
        dt <- tmp[1]
        idx <- base::sub(".*\\(([^)]*)\\).*", "\\1", tmp[2], ignore.case = TRUE)
        
      }
      
      site <- paste0('NCRN_',park,'_',site)
      yr <- base::substr(dt, 1,4)
      mo <- base::substr(dt, 5,6)
      day <- base::substr(dt, 7,8)
      sitevisit <- paste0(NCRNWater::getSiteInfo(object, parkcode=park, sitecode=site, info="SiteName"), ' ', yr, '-', mo, '-', day)
      
      # step 2: build the data structure
      
      # add the park if it does not exist
      if (park %in% names(imgs)==F){
        imgs[[park]] <- list()
      }
      # add the site if it does not exist
      if (site %in% names(imgs[[park]])==F) {
        imgs[[park]][[site]] <- list()
      }
      # add the year if it does not exist
      if (yr %in% names(imgs[[park]][[site]])==F) {
        imgs[[park]][[site]][[yr]] <- list()
      }
      # add the date if it does not exist
      if (sitevisit %in% names(imgs[[park]][[site]][[yr]])==F) {
        imgs[[park]][[site]][[yr]][[sitevisit]] <- list()
      }
      # add the filename if it does not exist
      if (f %in% names(imgs[[park]][[site]][[yr]][[sitevisit]])==F) {
        imgs[[park]][[site]][[yr]][[sitevisit]][[f]] <- list()
      }
      
      imgs[[park]][[site]][[yr]][[sitevisit]][[f]]$rel_fpath <- file.path(directory, f)
      imgs[[park]][[site]][[yr]][[sitevisit]][[f]]$sortorder <- idx
      
    }
  }
  return(imgs)
}

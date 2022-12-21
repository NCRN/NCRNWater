# this script scrubs information from NCRN's water metadata and data so those files can become example data in the NCRNWater package
# Identifying information, like park name, site name, lat and lon, etc. are herein anonymized with random values (i.e., character strings or numbers)
# The anonymized datasets are then saved in a .rda file to become part of NCRNWater
# Anonymized datasets are available in NCRNWater via data() (i.e., data(metadata) or data(waterdata))
# reference: https://youtu.be/Bj0gHafa4GE

# install library(usethis)
renv::install("usethis")
library(usethis)
renv::install("stringi")
library(stringi)
library(dplyr)
library(data.table)

##############################
# metadata
##############################
# the original, real metadata
metadata <- read.csv("water_data/metadata.csv") # directory excluded from github via .gitignore
metadata_colnames <- colnames(metadata) # save colnames as vector for use later
# $Network
length(unique(metadata$Network))
metadata$Network <- do.call(paste0, Map(stri_rand_strings, n=1, length=c(4),
                                        pattern = c('[A-Z]'))) # replace all $Network values with 4-letter random string

# $ParkCode, ShortName, LongName
# anonymize parks; replace each unique park identifier with a random string
# create a lookup table to cross-reference random strings against real strings
n_parks <- length(unique(metadata$ParkCode))
lookup <- data.frame(ParkCode = unique(metadata$ParkCode),
                     ParkCode2 = do.call(paste0, Map(stri_rand_strings, n=n_parks, length=c(4),
                                                      pattern = c('[A-Z]'))),
                     ShortName = unique(metadata$ShortName),
                     ShortName2 = paste(do.call(paste0, Map(stri_rand_strings, n=n_parks, length=c(7),
                                                             pattern = c('[A-Z]'))), "park"),
                     LongName = unique(metadata$LongName),
                     LongName2 = paste(do.call(paste0, Map(stri_rand_strings, n=n_parks, length=c(7),
                                                            pattern = c('[A-Z]'))), "National Park"))
# join columns from `lookup` to `metadata`
metadata <- dplyr::left_join(metadata, lookup %>% select(ParkCode, ParkCode2), by = "ParkCode")
metadata <- dplyr::left_join(metadata, lookup %>% select(ShortName, ShortName2), by = "ShortName")
metadata <- dplyr::left_join(metadata, lookup %>% select(LongName, LongName2), by = "LongName")
# delete original columns
metadata$ParkCode <- NULL
metadata$ShortName <- NULL
metadata$LongName <- NULL
# rename newly joined columns
setnames(metadata, "ParkCode2", "ParkCode")
setnames(metadata, "ShortName2", "ShortName")
setnames(metadata, "LongName2", "LongName")
# put columns back in their original order
metadata <- metadata %>% select(metadata_colnames)

# $SiteCode, SiteCodeWQX, SiteName
# anonymize sites; replace each unique site identifier with a random strings
n_sites <- length(unique(metadata$SiteCode))
lookup <- data.frame(SiteCode = unique(metadata$SiteCode),
                     SiteCode2 = rep(unique(metadata$Network), n_sites))
# need to find 1:many $ParkCode to $SiteCode relationshp for lookup$ParkCode below
lookup2 <- metadata %>% select(ParkCode, SiteCode) # extract the many-to-many
lookup2 <- lookup2[!duplicated(lookup2$SiteCode),] # de-dupe to produce one-to-many
lookup <- data.frame(SiteCode = unique(metadata$SiteCode),
                     SiteCode2 = paste(rep(unique(metadata$Network), n_sites),
                                       lookup2$ParkCode,
                                       do.call(paste0, Map(stri_rand_strings,
                                                           n=n_sites,
                                                           length=c(4),
                                                           pattern = c('[A-Z]')
                                                           )
                                               ),
                                       sep = "_"
                                       ),
                     SiteCodeWQX = unique(metadata$SiteCodeWQX),
                     SiteCodeWQX2 = paste("11PPPWRD_WQX", lookup$SiteCode2, sep = "-"),
                     SiteName = unique(metadata$SiteName),
                     SiteName2 = paste(do.call(paste0, Map(stri_rand_strings, n=n_sites, length=c(7),
                                                           pattern = c('[A-Z]'))), "creek"))

# join columns from `lookup` to `metadata`
metadata <- dplyr::left_join(metadata, lookup %>% select(SiteCode, SiteCode2), by = "SiteCode")
metadata <- dplyr::left_join(metadata, lookup %>% select(SiteCodeWQX, SiteCodeWQX2), by = "SiteCodeWQX")
metadata <- dplyr::left_join(metadata, lookup %>% select(SiteName, SiteName2), by = "SiteName")
# delete original columns
metadata$SiteCode <- NULL
metadata$SiteCodeWQX <- NULL
metadata$SiteName <- NULL
# rename newly joined columns
setnames(metadata, "SiteCode2", "SiteCode")
setnames(metadata, "SiteCodeWQX2", "SiteCodeWQX")
setnames(metadata, "SiteName2", "SiteName")
# put columns back in their original order
metadata <- metadata %>% select(metadata_colnames)

# $Lat, Long
# anonymize sites; replace lat & lon with rnorm() values
head(metadata$Lat)
lookup <- data.frame(Lat = unique(metadata$Lat),
                     Lat2 = rnorm(n_sites, (mean(metadata$Lat)-rnorm(1,15,5))),
                     Long = unique(metadata$Long),
                     Long2 = rnorm(n_sites, (mean(metadata$Long)+rnorm(1,15,5))))
# join columns from `lookup` to `metadata`
metadata <- dplyr::left_join(metadata, lookup %>% select(Lat, Lat2), by = "Lat")
metadata <- dplyr::left_join(metadata, lookup %>% select(Long, Long2), by = "Long")
# delete original columns
metadata$Lat <- NULL
metadata$Long <- NULL
# rename newly joined columns
setnames(metadata, "Lat2", "Lat")
setnames(metadata, "Long2", "Long")
# put columns back in their original order
metadata <- metadata %>% select(metadata_colnames)
# $AssessmentDetails
lookup <- data.frame(AssessmentDetails = unique(metadata$AssessmentDetails),
                     AssessmentDetail2 = c("",
                                          "Author et al 2156",
                                          "Author et al 2122",
                                          "Author A and Author B 2432",
                                          "Author 5 et al 2655"))
# join columns from `lookup` to `metadata`
metadata <- dplyr::left_join(metadata, lookup, by = "AssessmentDetails")
# delete original columns
metadata$AssessmentDetail <- NULL
# rename newly joined columns
setnames(metadata, "AssessmentDetail2", "AssessmentDetail")
# put columns back in their original order
metadata <- metadata %>% select(metadata_colnames)

usethis::use_data(metadata, overwrite = TRUE, compress = "xz")

##############################
# water data
##############################
# the original, real data
waterdata <- read.csv("water_data/waterdata.csv") # directory excluded from github via .gitignore

# $OrganizationIdentifier
head(waterdata$OrganizationIdentifier)
waterdata$OrganizationIdentifier <- paste0(unique(metadata$SiteCodeWQX), "_")

# $OrganizationFormalName

# $ActivityIdentifier
# need to match this format: "11NPSWRD_WQX-NCRN_MONO_GAMI_20050523011501_02"
# components:
### 1) unique(metadata$SiteCodeWQX)
### 2) cross-ref last two parts of metadata$SiteCode against regex target below
### 3) YYYMMDD date of waterdata$ActivityStartDate

# working on regex matching:
# try: [^A-Z]*(-)[^0-9]* at https://regexr.com/
# target: "MONO", "GAMI"
# string: 11NPSWRD_WQX-NCRN_MONO_GAMI_20050523011501_02

# $OrganizationFormalName
head(waterdata$OrganizationFormalName)
org_choices <- paste("Organization",
                     do.call(paste0,
                             Map(stri_rand_strings,
                                 n=5,
                                 length=c(2,3),
                                 pattern = c('[A-Z]',
                                             '[0-9]')
                                 )
                             ),
                     sep = " "
                     )
org_samples <- sample(org_choices, 100, replace = TRUE)
waterdata$OrganizationFormalName <- sample(org_choices, length(waterdata$OrganizationFormalName), replace = TRUE)
head(waterdata$OrganizationFormalName)

# $ActivityIdentifier
head(waterdata$ActivityIdentifier)
waterdata$ActivityIdentifier <- paste0(waterdata$OrganizationIdentifier,
                                       "-loc_code-",
                                       gsub("-", "", waterdata$ActivityStartDate))
head(waterdata$ActivityIdentifier)

# $ProjectIdentifier
head(waterdata$ProjectIdentifier)
length(unique(waterdata$ProjectIdentifier))
waterdata$ProjectIdentifier <- "ABCDWQ01"
head(waterdata$ProjectIdentifier)

# $ProjectName
head(waterdata$ProjectName)
length(unique(waterdata$ProjectName))
waterdata$ProjectName <- "ABCD water quality project 01"
head(waterdata$ProjectName)

# $MonitoringLocationIdentifier
head(waterdata$MonitoringLocationIdentifier)
head(waterdata$OrganizationIdentifier)


# $MonitoringLocationName
# $SampleCollectionMethod.MethodIdentifier
# $SampleCollectionMethod.MethodIdentierContexst
# $ResultIdentifier

# scrub lat/lon (changed to random numbers)
# $ActivityLocation.LatitudeMeasure
head(waterdata$ActivityLocation.LatitudeMeasure)
waterdata$ActivityLocation.LatitudeMeasure <- rnorm(length(waterdata$ActivityLocation.LatitudeMeasure),50)
head(waterdata$ActivityLocation.LatitudeMeasure)

# $ActivityLocation.LongitudeMeasure
head(waterdata$ActivityLocation.LongitudeMeasure)
waterdata$ActivityLocation.LongitudeMeasure <- rnorm(length(waterdata$ActivityLocation.LongitudeMeasure),-50)
head(waterdata$ActivityLocation.LongitudeMeasure)

# save scrubbed data to be included as part of NCRNWater package
usethis::use_data(waterdata, overwrite = TRUE, compress = "xz")

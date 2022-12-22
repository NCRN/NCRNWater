# this script scrubs information from NCRN's water metadata and data so those files can become example data in the NCRNWater package
# Identifying information, like park name, site name, lat and lon, etc. are herein anonymized with random values (i.e., character strings or numbers)
# The anonymized datasets are then saved in a .rda file to become part of NCRNWater
# Anonymized datasets are available in NCRNWater via data() (i.e., data(metadata) or data(waterdata))
# reference: https://youtu.be/Bj0gHafa4GE

# packages
library(usethis)
library(dplyr)
library(stringi)
library(stringr)
library(data.table)
set.seed(1)

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
                     LongName = unique(metadata$LongName))
for (i in 1:length(lookup$LongName)){
  lookup$LongName2[i] <- paste(stringr::str_extract(lookup$ShortName2[i], "(^[A-Z]{7})"), "National Park")
}
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
                     SiteCodeWQX2 = "11PPPWRD_WQX",
                     SiteName = unique(metadata$SiteName))

for (i in 1:length(lookup$SiteName)){
  lookup$SiteName2[i] <- paste(stringr::str_extract(lookup$SiteCode2[i], "([A-Z]{4}$)"), "creek")
}
lookup$SiteCodeWQX2 <- paste0(lookup$SiteCodeWQX2, "-", lookup$SiteCode2)
lookup_wqx <- lookup
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
lookup <- data.frame(AssessmentDetails = unique(metadata$AssessmentDetails))
for (i in 1:length(lookup$AssessmentDetails)){
  lookup$AssessmentDetails2[i] <- ifelse(lookup$AssessmentDetails[i] == "",
                                         "",
                                         paste("Author et al", rpois(1, 2100)))
}
# join columns from `lookup` to `metadata`
metadata <- dplyr::left_join(metadata, lookup, by = "AssessmentDetails")
# delete original columns
metadata$AssessmentDetails <- NULL
# rename newly joined columns
setnames(metadata, "AssessmentDetails2", "AssessmentDetails")
# put columns back in their original order
metadata <- metadata %>% select(metadata_colnames)

usethis::use_data(metadata, overwrite = TRUE, compress = "xz")

##############################
# water data
##############################
# the original, real data
waterdata <- read.csv("water_data/waterdata.csv") # directory excluded from github via .gitignore
waterdata_colnames <- colnames(waterdata) # save colnames as vector for use later

# $OrganizationIdentifier
head(waterdata$OrganizationIdentifier)
length(unique(waterdata$OrganizationIdentifier))
# waterdata$OrganizationIdentifier should match the beginning of metadata$SiteCodeWQX
# regex to extract the string we need
# extract a string that matches these criteria:
# 1) string starts with (^) two {2} characters [0-9]. Next,
# 2) string contains six {6} characters [A-Z]. Next,
# 3) string contains one {1} underscore '_'. Next,
# 4) string contains three {3} characters [A-Z].
waterdata$OrganizationIdentifier <- stringr::str_extract(unique(metadata$SiteCodeWQX)[1], "(^[0-9]{2}[A-Z]{6}_{1}[A-Z]{3})") 

# $OrganizationFormalName
head(waterdata$OrganizationFormalName)
length(unique(waterdata$OrganizationFormalName))
# this is not an identifying piece of data, so we leave as-is

# $ActivityIdentifier
head(waterdata$ActivityIdentifier)
# concatenation:
### 1) waterdata$OrganizationIdentifier
### 2) "-"
### 3) lookup_wqx to cross-ref
### 4) "_"
### 5) YYYMMDD date of waterdata$ActivityStartDate
### 6) ?? is this a timestamp?

# 1) create dummy variable waterdata$SiteCodeWQX
waterdata$SiteCodeWQX <- NA
# 2) regex to extract characters from waterdata$ActivityIdentifier into waterdata$SiteCodeWQX
waterdata$SiteCodeWQX <- stringr::str_extract(waterdata$ActivityIdentifier, "(^[0-9]{2}[A-Z]{6}_{1}[A-Z]{3}-{1}[A-Z]{4}_[A-Z]{4}_[A-Z0-9]{4})")
# testdf <- waterdata %>% # find any $ActivityIdentifier that the regex couldn't parse
#   select(ActivityIdentifier, SiteCodeWQX) %>%
#   subset(is.na(SiteCodeWQX))
# 3) create dummy variable waterdata$activity_dummy_keep
waterdata$activity_dummy_keep <- NA
# 4) regex to extract date string from waterdata$ActivityIdentifier into waterdata$activity_dummy_keep
waterdata$activity_dummy_keep <- stringr::str_extract(waterdata$ActivityIdentifier, "([0-9]+$)|([0-9]+_{1}[0-9].$)|([0-9]+_{1}[A-Z]+$)|([0-9]+/[0-9]+$)")
# testdf <- waterdata %>% # find any $ActivityIdentifier that the regex couldn't parse
#   select(ActivityIdentifier, activity_dummy_keep) %>%
#   subset(is.na(activity_dummy_keep))
# 5) delete waterdata$ActivityIdentifier
waterdata$ActivityIdentifier <- NULL
# 6) left join waterdata$activity_dummy_match to lookup_wqx$SiteCodeWQX2
# metadata <- dplyr::left_join(waterdata, lookup_wqx, by.x = "activity_dummy_match", by.y = "SiteCodeWQX2")
testdf <- dplyr::left_join(waterdata, lookup_wqx %>% select(SiteCodeWQX, SiteCodeWQX2), by = "SiteCodeWQX") %>%
  select(SiteCodeWQX, SiteCodeWQX2) %>%
  subset(is.na(SiteCodeWQX2))
# 7) waterdata$activity_dummy_match <- NULL
# 8) setnames(waterdata, "SiteCodeWQX2", "ActivityIdentifier")
# 9) paste0(waterdata$ActivityIdentifier, "_", waterdata$activity_dummy_keep)
# 10) waterdata$activity_dummy_keep <- NULL
# 11) waterdata <- waterdata %>% select(waterdata_colnames)




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

# Pipeline on how to use the FACTAGE package
# (this is the envisioned usage)

# Have your longitudinal SILC UDB files in a folder structured like this:
#
# |-- SILC_UDB_Longitudinal
# |   |-- AT
# |   |   |-- 2016
# |   |   |   |-- UDB_lAT16D.csv
# |   |   |   |-- UDB_lAT16P.csv
# |   |   |   |-- UDB_lAT16H.csv
# |   |   |   |-- UDB_lAT16R.csv
# |   |   |-- 2015
# |   |   |   |-- UDB_lAT15D.csv
# |   |   |   |-- UDB_lAT15P.csv
# |   |   |   |-- UDB_lAT15H.csv
# |   |   |   |-- UDB_lAT15R.csv
# |   |   |-- 2014
# |   |   |   |-- ...
# |   |   |-- ...
# |   |-- BE
# |   |   |-- 2016
# |   |   |-- ...
# |   |-- ...
#
# Then use the functions in the following manner.

# Specify the path to your data:
path <- "/path/to/my/SILC_UDB_Longitudinal"
# Specify the year range which you want to extract
# Be aware that this extraction (2008-2016) will include data from the calender year 2005
# since 2005, 2006, 2007 and 2008 are part of the longitudinal file from 2008.
# Think of the years as "release years", so in which year the data was released.
# One release year inlcudes four calender years.
year_from <- 2008
year_to <- 2016
# Select your countries. Use any two letter codes. Or specify NULL for all available countries.
countries <- c("AT", "BE", "FR")
# Choose your analytical variables from the SILC UDB!
vars <- c("PH010","PH030", "HY020")

# read in the data
SILC_UDB <- readUDB(path,year_from,year_to,countries,vars)

# merge data
SILC_UDB_M <- mergeUDB(copy(SILC_UDB))

# reassign IDs
SILC_UDB_MC <- recodeID(copy(SILC_UDB_M))

# flag for eligibilty for follow up
SILC_UDB_E <- setEligibility(copy(SILC_UDB_MC))

# optional:
# save non-eligible respondents
SILC_UDB_nonEligs <- SILC_UDB_E[ELIGIBLEforFU==FALSE]

# calculate entry and exit dates
SILC_UDB_D <- calcDates(copy(SILC_UDB_E))

### here you could calculate eg. a mean over the years..

# get final data set
SILC_UDB_X <- castUDB(copy(SILC_UDB_D),
                      analyticalVars=c("PH010","PH030","HY020"),
                      extractMethod="baseline",
                      DurationUnits="years")
# get the first survey year
# this is recommended to use as a controlling variable in Cox models
SILC_UDB_X$FirstSurveyYear <- year(SILC_UDB_X$EntryDate)

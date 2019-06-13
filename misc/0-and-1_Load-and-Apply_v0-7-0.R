########################################
##  Version 0.7.0                     ##
##  Date: 03.09.2018                  ##
##  created by: Tobias Göllner        ##
##  and the generous help of          ##
##  our inhouse R-Support Team        ##
##  and the power of the internet     ##
##  @TobiasGold on GitHub             ##
########################################

# install packages 
install.packages("tidyverse", "data.table")

# load packages
library(tidyverse)
library(data.table)

# create a function "create.SILCdata" which handles the initial data creation
# things that need improvement: 
# create a default value for countries, which should load all available countries
# the import cuts off data (which is OK) if the data provided is incorrect
# this should maybe be more clear to the user
# also wanted to do this with data.table but fread() is not able to handle this 
create.SILCdata <- function(path, year_from, year_to, vars, countries) {
  
  # set the pattern for which the folder should be screened #
  UDBpatrn <- "^UDB_l\\d{2}(D|H|P|R).csv$"
  # create a list with all the files that fit the pattern #
  list.correct.filenames <- list.files(path, pattern=UDBpatrn)
  # output a message with all the files that match the pattern #
  message.list.correct.filenames <- paste(list.correct.filenames, collapse=", ")
  message("Found the following csv files: ", message.list.correct.filenames) 
  
  # create empty lists for each filetype to recieve data #
  list.D.SILCdata<-list()
  list.H.SILCdata<-list()
  list.P.SILCdata<-list()
  list.R.SILCdata<-list()
  
  # split the specified user variables (vars) into the respective filetypes #
  # then message the user about which variables they chose #
  D.user.vars <- grep("^D", vars, value = TRUE)
  message.D.user.vars <- paste(D.user.vars, collapse=", ")
  message("Chosen variables of the D file: ", message.D.user.vars)
  H.user.vars <- grep("^H", vars, value = TRUE)
  message.H.user.vars <- paste(H.user.vars, collapse=", ")
  message("Chosen variables of the H file: ", message.H.user.vars)
  P.user.vars <- grep("^P", vars, value = TRUE)
  message.P.user.vars <- paste(P.user.vars, collapse=", ")
  message("Chosen variables of the P file: ", message.P.user.vars)
  R.user.vars <- grep("^R", vars, value = TRUE)
  message.R.user.vars <- paste(R.user.vars, collapse=", ")  
  message("Chosen variables of the R file: ", message.R.user.vars)
  
  # these are the essential variables needed to run the code successfully #
  # do not change! #
  D.essential.vars <- c("DB010", "DB020", "DB030", "DB110")
  H.essential.vars <- c("HB010", "HB020", "HB030", "HB050", "HB060")
  P.essential.vars <- c("PB010", "PB020", "PB030", "PB100", "PB110")
  R.essential.vars <- c("RB010", "RB020", "RB030", "RB040", "RB070", "RB080", "RB090", "RB110", "RB140", "RB150", "RX010")
  
  # combine the user with the essential variables #
  # message the user about those #
  D.vars <- c(D.essential.vars, D.user.vars)
  message.D.vars <- paste(D.vars, collapse=", ")  
  message("D.vars: ", message.D.vars)
  H.vars <- c(H.essential.vars, H.user.vars)
  message.H.vars <- paste(H.vars, collapse=", ")  
  message("H.vars: ", message.H.vars)
  P.vars <- c(P.essential.vars, P.user.vars)
  message.P.vars <- paste(P.vars, collapse=", ")  
  message("P.vars: ", message.P.vars)
  R.vars <- c(R.essential.vars, R.user.vars)
  message.R.vars <- paste(R.vars, collapse=", ")  
  message("R.vars: ", message.R.vars)
  
  # split the complete list of csv files into the respective filetype lists #
  list.D.filenames <- grep("^UDB_l\\d{2}D.csv$", list.correct.filenames, value=TRUE)
  list.H.filenames <- grep("^UDB_l\\d{2}H.csv$", list.correct.filenames, value=TRUE)
  list.P.filenames <- grep("^UDB_l\\d{2}P.csv$", list.correct.filenames, value=TRUE)
  list.R.filenames <- grep("^UDB_l\\d{2}R.csv$", list.correct.filenames, value=TRUE)
  
  # create a variable with all the years between "year_from" and "year_to" in the correct format #
  year_range <- sprintf('%0.2d', year_from:year_to)
  
  # create lists per year of each filetype #
  # message the user about it # 
  list.D.filenames.yrs <- as.list(grep(paste(year_range,collapse="|"), list.D.filenames, value=TRUE))
  message.list.D.filenames.yrs <- paste(list.D.filenames.yrs, collapse=", ")  
  message("Chosen csv files of D: ", message.list.D.filenames.yrs)
  list.H.filenames.yrs <- as.list(grep(paste(year_range,collapse="|"), list.H.filenames, value=TRUE))
  message.list.H.filenames.yrs <- paste(list.H.filenames.yrs, collapse=", ")  
  message("Chosen csv files of H: ", message.list.H.filenames.yrs)
  list.P.filenames.yrs <- as.list(grep(paste(year_range,collapse="|"), list.P.filenames, value=TRUE))
  message.list.P.filenames.yrs <- paste(list.P.filenames.yrs, collapse=", ")  
  message("Chosen csv files of P: ", message.list.P.filenames.yrs)
  list.R.filenames.yrs <- as.list(grep(paste(year_range,collapse="|"), list.R.filenames, value=TRUE))
  message.list.R.filenames.yrs <- paste(list.R.filenames.yrs, collapse=", ")  
  message("Chosen csv files of R: ", message.list.R.filenames.yrs)
  
  
  #####    
  #### this is what does not work properly yet! 
  ### even with ", fill=TRUE" it does not work
  ##
  # list.D.SILCdata <- lapply(list.D.filenames.yrs, function(i) {
  #   dat <- fread(file.path(path, i), select = D.vars)
  #   dat[dat$DB020 %in% countries,]
  # })
  # names(list.D.SILCdata)<-list.D.filenames.yrs
  # 
  # list.H.SILCdata <- lapply(list.H.filenames.yrs, function(i) {
  #   dat <- fread(file.path(path, i), select = H.vars)
  #   dat[dat$HB020 %in% countries,]
  # })
  # names(list.H.SILCdata)<-list.H.filenames.yrs
  # 
  # 
  # list.P.SILCdata <- lapply(list.P.filenames.yrs, function(i) {
  #   dat <- fread(file.path(path, i), select = P.vars)
  #   dat[dat$PB020 %in% countries,]
  # })
  # names(list.P.SILCdata)<-list.P.filenames.yrs
  # 
  # 
  # list.R.SILCdata <- lapply(list.R.filenames.yrs, function(i) {
  #   dat <- fread(file.path(path, i), select = R.vars)
  #   dat[dat$RB020 %in% countries,]
  # })
  # names(list.R.SILCdata)<-list.R.filenames.yrs
  ##
  ###    
  ####
  #####
  
  ## workaround for now 
  list.D.SILCdata <- lapply(list.D.filenames.yrs, function(i) {
    dat <- read.csv(file.path(path, i), header=TRUE)
    dat <- select(dat, one_of(D.vars))
    dat <- dat[dat$DB020 %in% countries,]
  })
  names(list.D.SILCdata)<-list.D.filenames.yrs
  
  list.H.SILCdata <- lapply(list.H.filenames.yrs, function(i) {
    dat <- read.csv(file.path(path, i), header=TRUE)
    dat <- select(dat, one_of(H.vars))
    dat <- dat[dat$HB020 %in% countries,]
  })
  names(list.H.SILCdata)<-list.H.filenames.yrs
  
  list.P.SILCdata <- lapply(list.P.filenames.yrs, function(i) {
    dat <- read.csv(file.path(path, i), header=TRUE)
    dat <- select(dat, one_of(P.vars))
    dat <- dat[dat$PB020 %in% countries,]
  })
  names(list.P.SILCdata)<-list.P.filenames.yrs
  
  list.R.SILCdata <- lapply(list.R.filenames.yrs, function(i) {
    dat <- read.csv(file.path(path, i), header=TRUE)
    dat <- select(dat, one_of(R.vars))
    dat <- dat[dat$RB020 %in% countries,]
  })
  names(list.R.SILCdata)<-list.R.filenames.yrs
  
  list.total.SILCdata <- list("D.files"=list.D.SILCdata, "H.files"=list.H.SILCdata, "P.files"=list.P.SILCdata, "R.files"=list.R.SILCdata)
  
  return(list.total.SILCdata)
}

#example call
mySILCdat <- create.SILCdata(path="C:/folder/to/mydata/", year_from=9, year_to=16, vars=c("PH010", "HY020"), countries = c("AT", "RO", "NL", "HU", "UK"))


# create a dataframe from all imported files with an id column that contains the name of the origin csv file
D.files.raw <- rbindlist(mySILCdat$D.files, fill=TRUE, idcol="data.origin")
H.files.raw <- rbindlist(mySILCdat$H.files, fill=TRUE, idcol="data.origin")
P.files.raw <- rbindlist(mySILCdat$P.files, fill=TRUE, idcol="data.origin")
R.files.raw <- rbindlist(mySILCdat$R.files, fill=TRUE, idcol="data.origin")


# the large list can be removed  
###rm(mySILCdat)

# get only the number of the data.origin and create a release_year variable
D.files.raw$release_year <- parse_number(D.files.raw$data.origin)
D.files.raw$data.origin <- NULL
H.files.raw$release_year <- parse_number(H.files.raw$data.origin)
H.files.raw$data.origin <- NULL
P.files.raw$release_year <- parse_number(P.files.raw$data.origin)
P.files.raw$data.origin <- NULL
R.files.raw$release_year <- parse_number(R.files.raw$data.origin)
R.files.raw$data.origin <- NULL


#rename Greece! convert "EL" to "GR"
# "GR" is the code (ISC)
D.files.raw$DB020 <- as.character(D.files.raw$DB020)
D.files.raw$DB020[D.files.raw$DB020=="EL"] <- "GR"
H.files.raw$HB020 <- as.character(H.files.raw$HB020)
H.files.raw$HB020[H.files.raw$HB020=="EL"] <- "GR"
P.files.raw$PB020 <- as.character(P.files.raw$PB020)
P.files.raw$PB020[P.files.raw$PB020=="EL"] <- "GR"
R.files.raw$RB020 <- as.character(R.files.raw$RB020)
R.files.raw$RB020[R.files.raw$RB020=="EL"] <- "GR"


# only use the most recent entry of every household/person
D.files.base <- D.files.raw %>% 
  group_by(DB020, DB030, DB010) %>% 
  slice(which.max(release_year))
message("Number of observations of D.files.base is ", nrow(D.files.raw), ". After using only the most recent observations ", nrow(D.files.base), " remain.")
###rm(D.files.raw)


H.files.base <- H.files.raw %>% 
  group_by(HB020, HB030, HB010) %>% 
  slice(which.max(release_year))
message("Number of observations of H.files.base is ", nrow(H.files.raw), ". After using only the most recent observations ", nrow(H.files.base), " remain.")
###rm(H.files.raw)

P.files.base <- P.files.raw %>% 
  group_by(PB020, PB030, PB010) %>% 
  slice(which.max(release_year))
message("Number of observations of P.files.base is ", nrow(P.files.raw), ". After using only the most recent observations ", nrow(P.files.base), " remain.")
###rm(P.files.raw)  

R.files.base <- R.files.raw %>% 
  group_by(RB020, RB030, RB010) %>% 
  slice(which.max(release_year))
message("Number of observations of R.files.base is ", nrow(R.files.raw), ". After using only the most recent observations ", nrow(R.files.base), " remain.")
###rm(R.files.raw)

# dropping release_year variable
D.files.base$release_year <- NULL
H.files.base$release_year <- NULL
P.files.base$release_year <- NULL
R.files.base$release_year <- NULL

# renaming
# initially P and R file use the original Personal ID (PB020 / RB020 == tmp_PS_ID), which will be changed later
D.files.base <- setnames(D.files.base, old=c("DB010","DB020", "DB030"), new=c("Year_Survey", "Country", "HH_ID"))
H.files.base <- setnames(H.files.base, old=c("HB010","HB020", "HB030"), new=c("Year_Survey", "Country", "HH_ID"))   
P.files.base <- setnames(P.files.base, old=c("PB010","PB020", "PB030"), new=c("Year_Survey", "Country", "tmp_PS_ID"))
R.files.base <- setnames(R.files.base, old=c("RB010","RB020", "RB030", "RB040"), new=c("Year_Survey", "Country", "tmp_PS_ID", "HH_ID"))


# merging, new ID and getting households with db110=5
# using a left join
# e.g. all entries which are in the register (household or person) files (D or R)
households <- merge(x = D.files.base, y = H.files.base, by=c("Country","HH_ID", "Year_Survey"), all.x = TRUE)
persons <- merge(x = R.files.base, y = P.files.base, by=c("Country","tmp_PS_ID", "Year_Survey"), all.x = TRUE)

# creating a key for the new Personal ID (PS_ID) from key variables
persons.keyed <- data.table(persons, key=c("Country", "tmp_PS_ID", "RB090", "RB080", "RB070", "HH_ID", "Year_Survey"))
persons.newID <- persons.keyed[, PS_ID:=.GRP, by=c("Country", "tmp_PS_ID", "RB090", "RB080", "RB070")]

# create a file with all entire household deceased cases (DB110=5)
households.deceased <- households[households$DB110==5, ]

# create the workingfile with information from all four filetypes
workfile <- merge(households, persons.newID, by=c("Country","HH_ID", "Year_Survey"), all=TRUE)

# now we start to fix the cases of deceased households
# first only select the chronologically first observation of each deceased household
households.deceased.firstobs <- households.deceased %>% group_by(Country, HH_ID) %>% slice(which.min(Year_Survey))

# we want the information of the year before the household dies
households.deceased.firstobs.yrpr <- households.deceased.firstobs
households.deceased.firstobs.yrpr$Year_Survey <- households.deceased.firstobs.yrpr$Year_Survey - 1
###this could probably be done in one line...

# we only need the three by variables
by.vars_C.HH.YS <- c("Country", "HH_ID", "Year_Survey")
households.deceased.firstobs.yrpr.cut <-  households.deceased.firstobs.yrpr %>% select(by.vars_C.HH.YS)

# now remerge with the workfile to get the information from the year prior (more cases since more persons can live in one household!)
households.to.replace <- merge(x = households.deceased.firstobs.yrpr.cut, y = workfile, by=c("Country","HH_ID", "Year_Survey"), all.x = TRUE)
# this works. the file now has more observations because there can be more than one person in a household!

# remove unused data
rm(households.deceased.firstobs, households.deceased.firstobs.yrpr, households.deceased.firstobs.yrpr.cut)

# create selection variables
# we want cases where RB110 eq 1,2,3 or 4 AND where the PS_ID is not missing 
valid.RB110 <- c(1,2,3,4)
nomiss.PS_ID <- na.omit(households.to.replace$PS_ID)

# create the data
households.to.replace.corrRB110 <- households.to.replace[households.to.replace$RB110 %in% valid.RB110, ]
households.to.replace.final <- households.to.replace.corrRB110[households.to.replace.corrRB110$PS_ID %in% nomiss.PS_ID, ]
# remove
rm(households.to.replace.corrRB110)

# set the DB110 variable to 5 for all those cases
households.to.replace.final$DB110 <- 5
# add one year to Year_Survey
households.to.replace.final$Year_Survey <- households.to.replace.final$Year_Survey + 1

# convert to data.table
setDT(workfile)
setDT(households.to.replace.final)
# ensure DB110 is an integer
households.to.replace.final[,DB110:=as.integer(DB110)]

# create update.cols and use correct ones
update.cols <- union(colnames(households.to.replace.final),colnames(workfile))
update.cols <- update.cols[!update.cols%in%by.vars_C.HH.YS]
# create this four keyed by-variable for later
by.vars_C.HH.PS.YS <- c("Country","HH_ID", "PS_ID", "Year_Survey")

# create the workfile with the values of household.to.replace.final using the three keyed by-variable
workfile.mod0 <- workfile[households.to.replace.final,c(update.cols):=mget(paste0("i.",update.cols)),on=c(by.vars_C.HH.YS)]

# if it just exists in household.to.replace.final, then simply append to the workfile (using the three keyed by-variable)
rbind.data <- households.to.replace.final[!workfile.mod0,on=c(by.vars_C.HH.PS.YS)]
workfile.mod1 <- rbind(workfile.mod0,rbind.data,use.names=TRUE)

# new workfile for the next steps
workfile.mod2 <- workfile.mod1

# create the variable Month of Survey 
# look at PB100 (which is in quarters) and assume the middle month of every quarter
workfile.mod2$M_survey[workfile.mod2$PB100==1] <- 2
workfile.mod2$M_survey[workfile.mod2$PB100==2] <- 5
workfile.mod2$M_survey[workfile.mod2$PB100==3] <- 8
workfile.mod2$M_survey[workfile.mod2$PB100==4] <- 11

# create the variable Month of Birth
# look at RB070 (which is in quarters) and assume the middle month of every quarter
workfile.mod2$M_birth[workfile.mod2$RB070==1] <- 2
workfile.mod2$M_birth[workfile.mod2$RB070==2] <- 5
workfile.mod2$M_birth[workfile.mod2$RB070==3] <- 8
workfile.mod2$M_birth[workfile.mod2$RB070==4] <- 11

# create the Month of Death or Move
# this is more complex
# first check if the country is Beglium or not 
workfile.mod2$M_deadORmove[workfile.mod2$Country=="BE"] <- workfile.mod2$RB140
workfile.mod2$M_deadORmove[!workfile.mod2$Country=="BE" & workfile.mod2$RB140==1] <- 2
workfile.mod2$M_deadORmove[!workfile.mod2$Country=="BE" & workfile.mod2$RB140==2] <- 5
workfile.mod2$M_deadORmove[!workfile.mod2$Country=="BE" & workfile.mod2$RB140==3] <- 8
workfile.mod2$M_deadORmove[!workfile.mod2$Country=="BE" & workfile.mod2$RB140==4] <- 11

# then check if the household moved or died and overwrite if there is no data in it
DB110_moveORdeath_status <- c(3,4,5)
RB110_moveORdeath_status <- c(5,6)
workfile.mod2$M_deadORmove[workfile.mod2$DB110%in%DB110_moveORdeath_status & !workfile.mod2$RB110%in%RB110_moveORdeath_status & workfile.mod2$HB050==1] <- 8
workfile.mod2$M_deadORmove[workfile.mod2$DB110%in%DB110_moveORdeath_status & !workfile.mod2$RB110%in%RB110_moveORdeath_status & workfile.mod2$HB050==2] <- 11
workfile.mod2$M_deadORmove[workfile.mod2$DB110%in%DB110_moveORdeath_status & !workfile.mod2$RB110%in%RB110_moveORdeath_status & workfile.mod2$HB050==3] <- 2
workfile.mod2$M_deadORmove[workfile.mod2$DB110%in%DB110_moveORdeath_status & !workfile.mod2$RB110%in%RB110_moveORdeath_status & workfile.mod2$HB050==4] <- 5

# create the variables Dat_Survey and Dat_Birth
workfile.mod2$Dat_Survey <- as.Date( paste(15, workfile.mod2$M_survey, workfile.mod2$PB110, sep = "."), format = "%d.%m.%Y")
workfile.mod2$Dat_Birth <- as.Date( paste(15, workfile.mod2$M_birth, workfile.mod2$RB080, sep = "."), format = "%d.%m.%Y")

# create the Dat_deadORmove variable
# months are correctly assigned above, years are done here
# assume half a year of survival
# for cases where only household (D or H) data is available
# using the dplyr if_else() (not R base iflese() )
HB050_1or2_status <- c(1,2)
HB050_3or4_status <- c(3,4)
workfile.mod2$Dat_deadORmove <- if_else(workfile.mod2$RB110%in%RB110_moveORdeath_status, as.Date( paste(15, workfile.mod2$M_deadORmove, workfile.mod2$RB150, sep = "."), format = "%d.%m.%Y"), if_else( 
  workfile.mod2$DB110%in%DB110_moveORdeath_status & workfile.mod2$HB050%in%HB050_1or2_status, as.Date( paste(15, workfile.mod2$M_deadORmove, workfile.mod2$HB060, sep = "."), format = "%d.%m.%Y"), if_else( 
    workfile.mod2$DB110%in%DB110_moveORdeath_status & workfile.mod2$HB050%in%HB050_3or4_status, as.Date( paste(15, workfile.mod2$M_deadORmove, workfile.mod2$HB060 + 1, sep = "."), format = "%d.%m.%Y"), as.Date( paste(1, 1, 1600, sep = "."), format = "%d.%m.%Y") ) ) )

workfile.mod2$Dat_deadORmove[workfile.mod2$Dat_deadORmove=="1600-01-01"] <- NA

# drop M_xyz variables
drop.M_vars <- c("M_survey", "M_birth", "M_deadORmove")
workfile.mod3 <- workfile.mod2 %>% select(-one_of(drop.M_vars))
# rename RB090 and RX010 to Sex and Age_Survey
workfile.mod3 <- setnames(workfile.mod3, old=c("RB090","RX010"), new=c("Sex", "Age_Survey"))

# create the universal Died variable
workfile.mod3$Died[workfile.mod3$RB110==6] <- 1
workfile.mod3$Died[workfile.mod3$RB110==5] <- 9
workfile.mod3$Died[!workfile.mod3$RB110%in%RB110_moveORdeath_status] <- 0
workfile.mod3$Died[workfile.mod3$DB110==5] <- 1
DB110_move_status <- c(3,4)
workfile.mod3$Died[workfile.mod3$DB110%in%DB110_move_status] <- 9

# create the Dat_Death and Dat_Cens variable
workfile.mod4 <- workfile.mod3 %>% 
  group_by(PS_ID, Year_Survey) %>% 
  mutate(Dat_Death = case_when (which.max(Year_Survey) & Died==1  ~ Dat_deadORmove), 
         Dat_Censoring = if_else(which.max(Year_Survey) & Died==0, Dat_Survey, 
                                 if_else(which.max(Year_Survey) & Died==9, Dat_deadORmove, as.Date(paste(1, 1, 1600, sep = "."), format = "%d.%m.%Y")) )  )

workfile.mod4$Dat_Censoring[workfile.mod4$Dat_Censoring=="1600-01-01"] <- NA        
# slow but works...

# now the first and last observation is taken from this file and will be re-merged
# user now has to specify where to extract their variables 
# first-last function

first.last.merge <- function (first.vars=NULL, last.vars=NULL) {  
  
  first.essential.vars <- c("Country", "Year_Survey", "PS_ID", "Age_Survey", "Sex", "Dat_Survey", "Dat_Birth")
  last.essential.vars <- c("PS_ID", "Died", "Year_Survey", "Dat_Death", "Dat_Censoring")  
  
  keep.first <- c(first.essential.vars, first.vars)
  keep.last <- c(last.essential.vars, last.vars)
  
  workfile.first <- workfile.mod4 %>%
    select(one_of(keep.first)) %>%
    group_by(PS_ID) %>% 
    arrange(Year_Survey, .by_group = TRUE) %>%
    filter(row_number()==1)
  
  workfile.last <- workfile.mod4 %>%
    select(one_of(keep.last)) %>%
    group_by(PS_ID) %>% 
    arrange(Year_Survey, .by_group = TRUE) %>%
    filter(row_number()==n())
  workfile.last$Year_Survey <- NULL
  
  
  workfile.mod5 <- merge(x = workfile.first, y = workfile.last, by="PS_ID")
  
  return(workfile.mod5)
} 

# first.last.merge call
workfile.mod5 <- first.last.merge(first.vars=c("PH010", "HY020"))

# create the variable Verweildauer in days
workfile.mod5$Verweildauer_days <- ifelse(workfile.mod5$Died==1, difftime(workfile.mod5$Dat_Death, workfile.mod5$Dat_Survey, units = "days"),
                                          difftime(workfile.mod5$Dat_Censoring, workfile.mod5$Dat_Survey, units = "days") )

# only keep entries where there is a Verweildauer greater than 0
workfile.mod6 <- workfile.mod5 %>%
  filter(workfile.mod5$Verweildauer_days>0)

# create Verweildauer_years to assess correct Verweildauers more easily
workfile.mod6$Verweildauer_years <- workfile.mod6$Verweildauer_days/365.25

# check all lines if the Verweildauers are apropriate for the respective country
# maximum value of Verweildauer is 8 years for NO, 9 for FR, indefinetly for LU, and 4 for all other countries 
special_Verweildauer_Countries <- c("FR", "LU", "NO")
LU_year_range <- max(workfile.mod6$Year_Survey) - min(workfile.mod6$Year_Survey) + 1
workfile.mod6$eligible[workfile.mod6$Country=="FR" & workfile.mod6$Verweildauer_years<9] <- 1
workfile.mod6$eligible[workfile.mod6$Country=="LU" & workfile.mod6$Verweildauer_years<LU_year_range] <- 1
workfile.mod6$eligible[workfile.mod6$Country=="NO" & workfile.mod6$Verweildauer_years<8] <- 1
workfile.mod6$eligible[!workfile.mod6$Country%in%special_Verweildauer_Countries & workfile.mod6$Verweildauer_years<4] <- 1

# only select eligibles
workfile.mod7 <- workfile.mod6 %>%
  filter(workfile.mod6$eligible==1)

# only select persons in valid age range from 16 to 79
workfile.mod8 <- workfile.mod7 %>%
  filter(workfile.mod7$Age_Survey>=16 & workfile.mod7$Age_Survey<=79)

# recode 9 to 0 in Died
workfile.mod8$Died[workfile.mod8$Died==9] <- 0

# create Dat_Exit of Dat_Death or Dat_Censoring
workfile.final <- workfile.mod8 %>% mutate(Dat_Exit = if_else(is.na(workfile.mod8$Dat_Censoring), workfile.mod8$Dat_Death,workfile.mod8$Dat_Censoring))
workfile.final$eligible <- NULL

# done

write.csv(workfile.final, "C:/folder/to/mydata/output/workfile_final.csv")

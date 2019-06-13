#' Recode IDs
#'
#' recodeID() does three things:
#' 1. It defines unique persons through certain variables. (Default values are: PID Country RB090 RB080 RB070)
#' 2. It creates new household IDs
#' 3. It renames the personal IDs
#'
#' @param dat The data created from mergeUDB().
#' @param hid Default value = "HHID". No change recommended!
#' @param pid Default value = "PID". No change recommended!
#' @param year Default value = "SurveyYear". No change recommended!
#' @param country Default value = "Country". No change recommended!
#' @param uniqueIdentifier Default values are "RB090", "RB080", "RB070". No change recommended!
#' @param verbose Default==TRUE. Be chatty?
#'
#' @return
#' @export
#'
#' @examples
#' # Standard call:
#' SILC_UDB_MC <- recodeID(copy(SILC_UDB_M))
#' # copy() is not mandatory


recodeID <- function(dat,hid="HHID",pid="PID",year="SurveyYear",country="Country",
                     uniqueIdentifier=c("RB090", "RB080", "RB070"),verbose=TRUE){

  dat <- copy(dat)
  pidOrig <- paste(pid,"ORIGINAL",sep="_")
  hidOrig <- paste(hid,"ORIGINAL",sep="_")

  setnames(dat,c(pid,hid),c(pidOrig,hidOrig))

  if(verbose){
    cat("Defining unique persons through:",c(pid ,country,uniqueIdentifier))
  }
  dat[,c(pid):=.GRP,by=c(pidOrig ,country,uniqueIdentifier)]
  dat[,c(hid):=get(hidOrig)]
  if(verbose){
    cat("...finished!\n")
  }

  if(verbose){
    cat("Creating new household IDs")
  }
  # help data to check if household for year+1
  # contains at least one person from last year
  datHelp <- dat[,mget(c(pid,hid,year,country))]
  datHelp[,c(year):=get(year)-1]

  dat[datHelp,existsNextYear:=TRUE,on=c(year,pid,hid,country)]
  dat[is.na(existsNextYear),existsNextYear:=FALSE]
  dat[existsNextYear==FALSE,existsNextYear:=get(year)==max(get(year)),by=c(country,hid)]
  dat[,existsNextYear:=any(existsNextYear),by=c(year,hid,country)]

  # create new household id
  # household gets new id for next year
  # if all household members swap - no household member present next year
  # household ID is only unique within each country
  setorderv(dat,c(country,hid,year))
  dat[,c(hid):=.SD[,(.GRP)*1000L+rleid(existsNextYear)*10L,by=c(hid)][,V1],by=c(country)]
  dat[,c(hid):=.SD[,rep((.GRP)*100L,.N),by=c(hid)][,V1],by=c(country)]
  if(verbose){
    cat("...finished!\n")
  }

  # create new personal id
  # person gets ID from similar to first household in
  # which they occur - person id stays fix even when person moves
  if(verbose){
    cat("Renaming personal IDs")
  }
  pidHelp <- paste0(pid,"_HELP")
  datHelp <- dat[,.(1:uniqueN(get(pid)),unique(get(pid))),by=c(country,hid)]
  setnames(datHelp,c("V1","V2"),c(pidHelp,pid))
  dat[datHelp,c(pidHelp):=get(pidHelp),on=c(pid,hid,country)]
  setorderv(dat,c(country,pid,year))
  dat[,c(pid):=get(hid)[1]*100L+get(pidHelp)[1],by=c(country,pid)]
  if(verbose){
    cat("...finished!\n")
  }
  # remove added columns
  dat[,c(pidHelp,"existsNextYear"):=NULL]

  return(dat)
}

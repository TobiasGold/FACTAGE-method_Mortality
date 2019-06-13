#' Set Eligibility Status
#'
#' This sets the "ELIGIBLEforFU" variable to FALSE if
#' a): There is only one observation (line) of the respondent
#' b): There are two lines and the second line contains the DB110 codes 6,7 or 11.
#' In both cases (a & b) no vitality status can be assessed, thus there is no eligiblity for mortality follow-up.
#' Should you not want to exclude those cases for some reason, simply create the variable "ELIGIBLEforFU" for all lines and set it to true.
#' e.g. with data.table: dt[, ELIGIBLEforFU := TRUE]
#'
#' @param dat The data created from recodeID().
#' @param pid Default value = "PID". No change recommended!
#' @param country Default value = "Country". No change recommended!
#' @param eligVar Default value = "DB110". No change recommended!
#'
#' @return
#' @export
#'
#' @examples
#' # Standard call:
#' SILC_UDB_E <- setEligibility(copy(SILC_UDB_MC))
#' # copy() is not mandatory

setEligibility <- function(dat,pid="PID",country="Country",eligVar="DB110"){

  dat[,ELIGIBLEforFU:=helpEligibility(get(eligVar)),by=c(pid,country)]
  return(dat)
}

helpEligibility <- function(DB110){

  n <- length(DB110)
  if(n==1){
    return(FALSE)
  }
  if(n==2&DB110[n]%in%c(6,7,11)){
    return(FALSE)
  }

  return(TRUE)
}

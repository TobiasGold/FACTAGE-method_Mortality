#' Calculate Duration Times
#'
#' Calculates the entry and exit date for each respondent.
#' If no valid exit date can be read in the last line, then the date of the penultimate line is read and half a year is added.
#' This logic is the same as in the previous SAS installment of the code.
#'
#' @param dat The data created from setEligibility().
#' @param year Default value = "SurveyYear". No change recommended!
#' @param country Default value = "Country". No change recommended!
#' @param hid Default value = "HHID". No change recommended!
#' @param pid Default value = "PID". No change recommended!
#' @param hDat Default values = c("HB050","HB060"). No change recommended!
#' @param pDat Default values = c("PB100","PB110"). No change recommended!
#' @param rDat Default values = c("RB140","RB150"). No change recommended!
#' @param eligible Default value = "ELIGIBLEforFU". No change recommended!
#'
#' @return
#' @export
#'
#' @examples
#' # Standard call:
#' SILC_UDB_D <- calcDates(copy(SILC_UDB_E))
#' # copy() is not mandatory
calcDates <- function(dat,year="SurveyYear",country="Country",hid="HHID",pid="PID",
                      hDat=c("HB050","HB060"),pDat=c("PB100","PB110"),rDat=c("RB140","RB150"),eligible="ELIGIBLEforFU"){

  dateVars <- c(hDat,pDat,rDat)

  datWork <- dat[get(eligible)==TRUE,mget(c(year,country,hid,pid,hDat,pDat,rDat))]

  # create help variables
  dateVarsOrig <- paste0(dateVars,"ORIGINAL")
  datWork[,c(dateVarsOrig):=mget(dateVars)]

  # set quarters to month
  # if only quarters present in each varialbe
  datesMonth <- c(hDat[1],pDat[1],rDat[1])
  for(i in datesMonth){
    i_help <- paste0(i,"help")
    datWork[!is.na(get(i)),c(i_help):=all(get(i)<5),by=c(country,year)]
    datWork[get(i_help)==TRUE,c(i):=quarter2Month(get(i))]
    datWork[,c(i_help):=NULL]
  }

  # get year and month for calculation
  listDat <- list(rDat,pDat,hDat)

  for(i in listDat){
    datWork[is.na(get(i[1]))|is.na(get(i[2])),c(i):=NA]
  }

  setorderv(datWork,c(country,pid,year))
  datEntry <-  unique(datWork,by=c(country,pid))
  datExit <-  datWork[datWork[,.I[.N],by=c(country,pid)][,V1]]

  helpEntry <- c("EntryMonth","EntryYear")
  helpExit <- c("ExitMonth","ExitYear")
  datEntry[,c(helpEntry):=NA_integer_]
  datExit[,c(helpExit):=NA_integer_]
  for(i in 1:length(listDat)){
    ii <- listDat[[i]]
    if(i>1){
      datEntry[(!is.na(get(ii[1])))&(!is.na(get(ii[2])))&is.na(EntryMonth),c(helpEntry):=mget(ii)]
    }
    datExit[(!is.na(get(ii[1])))&(!is.na(get(ii[2])))&is.na(ExitMonth),c(helpExit):=mget(ii)]
  }

  # get records with no information on exit date
  # and take second to last obersvation for those records
  datExit2 <- datExit[is.na(ExitMonth),mget(c(country,pid))]
  datExit2 <- datWork[datExit2,,on=c(country,pid)]
  datExit2 <- datExit2[datExit2[,.I[.N-1],by=c(country,pid)][,V1]]

  datExit2[,c(helpExit):=NA_integer_]
  for(i in 1:length(listDat)){
    ii <- listDat[[i]]
    index <- datExit2[(!is.na(get(ii[1])))&(!is.na(get(ii[2])))&is.na(ExitMonth),which=TRUE]
    datExit2[index,c(helpExit):=mget(ii)]
    if(i>1){
      # add half a year to the date
      # when using pDat or hDat
      datExit2[index,c(helpExit):=add6Months(ExitMonth,ExitYear)]
    }
  }

  datExit[datExit2[,mget(c(pid,country,helpExit))],c(helpExit):=mget(paste0("i.",helpExit)),on=c(country,pid)]
  rm(datExit2)

  datDates <- merge(datEntry[,mget(c(country,pid,helpEntry))],datExit[,mget(c(country,pid,helpExit))],by=c(country,pid),all=TRUE)
  datDates[is.na(EntryMonth),c(helpEntry,helpExit):=NA]

  datDates[!is.na(EntryYear),EntryDate:=as.Date(paste(EntryYear,EntryMonth,"15",sep="-"))]
  datDates[!is.na(ExitYear),ExitDate:=as.Date(paste(ExitYear,ExitMonth,"15",sep="-"))]

  dat <- merge(dat,datDates[,mget(c(pid,country,"EntryDate","ExitDate"))],by=c(pid,country),all.x=TRUE)

  rm(datDates,datEntry,datExit,datWork)

  return(dat)
}


# helpfunction to convert quarter to month (middle of quarter)
quarter2Month <- function(x){
  (x)*3L-1L
}

# helpfunction to add 6 months
add6Months <- function(m,y){
  m <- m+6L
  y[m>12] <- y[m>12]+1L
  m <- m%%12L
  m[m==0] <- 1L
  return(list(m,y))
}


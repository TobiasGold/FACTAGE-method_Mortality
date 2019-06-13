#' Cast UDB-Data
#'
#' Casts the data in a way that each respondent is only one line. Calculates the DurationTime (time at risk)
#' of each respondent. Also extracts the analytical variables at baseline or end of the follow-up period.
#'
#' @param dat The data created from calcDates()
#' @param pid Default value = "PID". No change recommended!
#' @param country Default value = "Country". No change recommended!
#' @param year Default value = "SurveyYear". No change recommended!
#' @param analyticalVars Specify the analytical variables. Normally you should simply use the variables stated in the "vars" argument in readUDB().
#' @param extractMethod Specify on which point you want to extract the data from. Valid values are: "all", "baseline", "end".
#' @param DurationUnits Specify which time units to use for calculating the duration time, e.g. "year", "months", "days",... Argument passed down to \code{\link[lubridate]{time_length}}
#' @param EntryDate Default value = "EntryDate". No change recommended!
#' @param ExitDate Default value = "ExitDate". No change recommended!
#' @param age Default value = "RX010". No change recommended!
#' @param sex Default value = "RB090". No change recommended!
#' @param keepVars Default value = "HHID". Specify the variables which you want to keep after casting.
#'
#' @return
#' @export
#'
#' @examples
#' SILC_UDB_X <- castUDB(copy(SILC_UDB_D),
#' analyticalVars=c("PH010","PH030", "HY020"),
#' extractMethod="baseline",
#' DurationUnits="years",keepVars ="HHID")
castUDB <- function(dat,pid="PID",country="Country",year="SurveyYear",
                    analyticalVars=c("PH010", "PH030"),
                    extractMethod="all",DurationUnits="days",
                    EntryDate="EntryDate",ExitDate="ExitDate",
                    # birthYear="RB080",birthMonth="RB070",
                    age="RX010",sex="RB090",
                    keepVars = c("HHID","RB090","EntryDate","ExitDate")){

  setorderv(dat,c(country,pid))

  if(length(extractMethod)==1){
    extractMethod <- rep(extractMethod,length(analyticalVars))
  }

  # define flags for excluding rows
  dat[,dropage:=is.na(get(age)[1]),by=c(country,pid)]
  dat[,dropsex:=!get(sex)%in%c(1,2)]

  expr <- paste0("interval(",EntryDate,",",ExitDate,")")

  dat[,DurationTime:=time_length(eval(parse(text=expr)),"years")]

  dat[,dropDuration:=DurationTime>=4]
  dat[get(country)=="FR",dropDuration:=DurationTime>=9]
  dat[get(country)=="NO",dropDuration:=DurationTime>=8]

  maxEntry <- dat[,diff(range(get(year)))]+1
  dat[get(country)=="LU",dropDuration:=DurationTime>maxEntry]

  # exclude rows
  dat <- dat[dropage==FALSE&dropsex==FALSE&dropDuration==FALSE]
  dat[,c("dropage","dropsex","dropDuration"):=NULL]

  # produce additional variables
  dat[,AgeBaseline:=get(age)[1],by=c(country,pid)]
  # dat[,BirthDate:=quarter2Month(get(birthMonth))]
  # dat[,BirthDate:=as.IDate(paste(get(birthYear),get(birthMonth),"15",sep="-"))]
  dat[,Died:=any(RB110==6|DB110==5),by=c(country,pid)]
  if(DurationUnits!="year"){
    dat[,DurationTime:=time_length(eval(parse(text=expr)),DurationUnits)]
  }


  # extract first and last observations of each country and pid
  dat[,helpFirstLast:=c("baseline",rep(NA_character_,.N-2),"end"),by=c(country,pid)]
  dat <- dat[helpFirstLast%in%c("baseline","end")]


  # start buidling final data set
  analyticalVars_all <- analyticalVars[extractMethod=="all"]
  analyticalVars_baseline <- analyticalVars[extractMethod=="baseline"]
  analyticalVars_end <- analyticalVars[extractMethod=="end"]

  id.vars <- c(pid,country,"AgeBaseline","Died","DurationTime",keepVars)

  datOutput <- list()

  if(length(analyticalVars_baseline)>0){

    datHelp <- dat[helpFirstLast=="baseline",mget(c(id.vars,analyticalVars_baseline))]
    setnames(datHelp,analyticalVars_baseline,paste0(analyticalVars_baseline,"_baseline"))
    datOutput <- c(datOutput,list(datHelp))
  }
  if(length(analyticalVars_end)>0){

    datHelp <- dat[helpFirstLast=="end",mget(c(id.vars,analyticalVars_end))]
    setnames(datHelp,analyticalVars_end,paste0(analyticalVars_end,"_end"))
    datOutput <- c(datOutput,list(datHelp))
  }
  if(length(analyticalVars_all)>0){

    dcast.form <- as.formula(paste(paste(id.vars,collapse="+"),"helpFirstLast",sep="~"))
    datHelp <- dcast(dat,dcast.form,value.var = analyticalVars_all)

    if(length(analyticalVars_all)==1){
      orignames <- colnames(datHelp)[!colnames(datHelp)%in%id.vars]
      setnames(datHelp,orignames,paste0(analyticalVars_all,c("_baseline","_end")))
    }
    datOutput <- c(datOutput,list(datHelp))
  }


  datOutput <- Reduce(function(...){merge(...,by=id.vars)},datOutput)

  # return output
  return(datOutput)
}

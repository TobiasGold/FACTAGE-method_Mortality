/************************************/
/* Version 1.0.0  					*/
/* Date: 22.05.2018 				*/
/* created by: Johannes Klotz		*/
/************************************/

/************************************/
/* This is an exemplary analysis in */
/* SAS. This is to illustrate a 	*/
/* potential application.			*/
/************************************/


/* Specify the working directory */
%let silcpath = C:\path\to-your\directory ;
libname eusilc "&silcpath"; 


/* Have a look at the first 6 observations in the data */

proc print data = eusilc.silc_full (obs = 6);
	run;

/* Descriptive statistics 1: per country, number and percentage of survivors and decedents */

proc freq data = eusilc.silc_full;
	table country * died /nocol nopercent;
	run;

/* Descriptive statistics 2: overall, number and percentage of survivors and decedents by age at baseline */

proc freq data = eusilc.silc_full;
	table age * died /nocol nopercent;
	run;


/* Model I: Estimating mortality hazard ratio by household income category */

/* First, data modification */

data analysis;
	set eusilc.silc_full;
	where (age ge 30 and age le 79) and hy020 gt 0;
	if hy020 lt 10000 then income_group = 1;
		else if hy020 ge 10000 and hy020 le 30000 then income_group = 2;
		else income_group = 3;
	run;

/* Then, the Proportional Hazards Regression */

proc phreg data = analysis;
	class income_group (param = ref ref = "2");
	model verweildauer * died (0) = age income_group;
	run;


/* Model II: Stratified by sex and additionally controlling for country and survey year */

/* First, data modification */

proc sort data = analysis out = analysis_II;
	by sex;
	run;

/* Then, the Proportional Hazards Regression */

proc phreg data = analysis_II;
	by sex;
	class income_group (param = ref ref = "2") country;
	model verweildauer * died (0) = age income_group year_survey country;
	run;


/* Model III: Restricted to Mediterranean countries (PT, ES, IT, MT, SI, HR, GR and CY) and additionally controlling for self-rated health (pseudo-metric) */

/* First, data modification */

data analysis_III;
	set analysis;
	where country in ("PT" "ES" "IT" "MT" "SI" "HR" "GR" "CY") and ph010 in (1 2 3 4 5);
	run;

/* Then, the Proportional Hazards Regression */

proc phreg data = analysis_III;
	class income_group (param = ref ref = "2");
	model verweildauer * died (0) = age income_group ph010;
	run;

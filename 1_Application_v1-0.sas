/************************************/
/* Version 1.0  					*/
/* Date: 29.03.2018 				*/
/* created by: Tobias Göllner 		*/
/* @TobiasGold on GitHub 			*/
/************************************/

/************************************/
/* This code excecutes the macros.	*/
/* Edit the apropriate lines to 	*/
/* your needs and save it.			*/
/************************************/


	/*e*/ 		/* here you should edit the code */
	/*e!*/		/* here you MUST edit the code */
	/*e?*/		/* optional edit */


/* I suggest you turn on the following option */
/* This makes working with large datasets quicker */
option compress=yes;

/* set library location */

	/*e!!*/ %let silcpath = C:\path\to-your\library-folder ;

libname eusilc "&silcpath"; 
/* DON'T CHANGE THE LIBNAME COMMAND */


/* if you want only certain countries in your analyses you can specify them here */
/* please list them according to the example below */
/* eg: %let clist='ES', 'BE'; */
/* If you want to include Greece you have to specify both codes: 'EL' and 'GR' */
/* Greece got renamed during EU-SILC. We will rename it in one of the macros to 'GR' */

	/*e?*/ %let clist='AT', 'BE', 'BG', 'CH', 'CY', 'CZ', 'EE', 'EL', 'ES', 'FI', 'FR', 'GR', 'HR', 'HU', 'IT', 'LT', 'LU', 'LV', 'MT', 'NL', 'NO', 'PL', 'PT', 'RO', 'SE', 'SI', 'SK', 'UK';

/*create list with letters of files */
%let list=d h p r;


/* The data files have to follow a certain naming convention! */
/* If this naming convention is not met, the code won't work  */

/* if you want to choose fileoption=1 then type in the paths to your include files here */

	/*e?!*/	%let dincl = C:\path\to-your\D-file-codes;
	/*e?!*/	%let hincl = C:\path\to-your\H-file-codes;
	/*e?!*/	%let pincl = C:\path\to-your\P-file-codes;
	/*e?!*/	%let rincl = C:\path\to-your\R-file-codes;

/*	Every included .sas program has to be edited manually! 
	You have to edit the name (data WORK.XFILEYY; X is D, H, P or R, and YY is the Year 8, 9, 10, etc.)
	And you have to check if the infile statement refers to the correct (newest) version of the file
	(eg. "infile 'D:\EUROSTAT\SILC_UDB\2008L\2008-4\UDB_l08D_ver 2008-4 from 01-03-2012.csv'") */
/* Do this for every .sas program and then continue with the code */


/* if you want to choose fileoption=2 then type in the paths to your files here */

	/*e?!*/ libname dfiles "C:\path\to-your\d-file-data";
	/*e?!*/ libname hfiles "C:\path\to-your\h-file-data";
	/*e?!*/ libname pfiles "C:\path\to-your\p-file-data";
	/*e?!*/ libname rfiles "C:\path\to-your\r-file-data";

/* All the files must have the naming structure dfile9, hfile10, rfile11, etc. and be in their proper folder! */
/* It will not work if all the files are in one folder togehter. */


/* small comment for each macro, to explain what happens */

	/*e*/ %createdata(yr_from= , yr_to= , fileoption= , countries= , vars= )

/* example: %createdata(yr_from=6, yr_to=15, fileoption=1, countries=(&clist), vars=hy020 PH020) */
/* every statement except "vars=" has a default value */


/* merges the four file types together */
/* D + H = household // R + P = person */
/* person + household = work file */
%mergefiles()

/* this is needed to fix the number of deaths when a multiple person household dies */
%hhdeathfix()

/* this is the first data processing step */
/* mainly date variables get calculated and one unique death identifier is created */
%dataproc()

/* here the first and last observation of each person get merged together */

	/*e*/%infos(firstvars= , lastvars= )

/* example: %infos(firstvars=PH020 HY020) */
/* usually you want to extract the information from the baseline so "firstvars=XY123 XZ234" */
/* if you do not specify all analysis variables from %createdata(vars=...) then those will get dropped! */
/* so simply copy all variables down */

/* the second data processing step */
/* checks for data errors and creates the output file */
%dataproc2() 
/* the output file only includes persons aged from 16 to 79 (at baseline */

/* if you want to export the data into a csv file */
/* use this code */
proc export data=eusilc.silc_full
  dbms=csv 
  outfile="&silcpath.\silc_full.csv" 
  replace;
run;

/* the date variables will be in the format which was specified earlier in the %dataproc2 macro */
/* the format used is the DDMMYYYY10. format, which looks like this 21/05/1990. */
/* keep this in mind when importing into different programmes */

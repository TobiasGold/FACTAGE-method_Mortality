/* Date 28.02.2018*/
/*	Every included .sas program has to be edited manually! (Should be already done in this version)
	You have to edit the name (data WORK.XFILEYY; X is D, H, P or R, and YY is the Year 08, 09, 10, etc.)
	And you have to check if the infile statement refers to the correct (newest) version of the file
	(eg. infile 'N:\EUROSTAT\40_UDB\SILC_UDB\2008L\2008-4\UDB_l08D_ver 2008-4 from 01-03-2012.csv') */

/*	the names "dfile6", "dfile7", "dfileX" etc are set in the INCLUDEd codes!
	if they are named differently in there, then the following code wont work! */

/* we suggest you turn on the following option */
option compress=yes;

/* set library location */
%let silcpath = C:\path\to-your\library-folder ;
libname eusilc "&silcpath"; 

/*if you want only certain countries in your analyses you can specify them here*/
/*please list them according to the example below*/
/*eg: %let clist='ES', 'BE'; */
/* This is the list for all possible countries (of 2015) */
/* 'AT', 'BE', 'BG', 'CH', 'CY', 'CZ', 'DK', 'EE', 'EL', 'ES', 'FI', 'FR', 'GR', 'HR', 'HU', 'IE', 
'IS', 'IT', 'LT', 'LU', 'LV', 'MT', 'NL', 'NO', 'PL', 'PT', 'RO', 'SE', 'SI', 'SK', 'UK' */
/* If you want to include Greece you have to specify both codes: 'EL' and 'GR' */
/* Greece got renamed during EU-SILC. We will rename it in one of the macros to 'GR' */
%let clist='AT', 'BE', 'BG', 'CH', 'CY', 'CZ', 'EE', 'EL', 'ES', 'FI', 'FR', 'GR', 'HR', 'HU', 'IT', 'LT', 'LU', 'LV', 'MT', 'NL', 'NO', 'PL', 'PT', 'RO', 'SE', 'SI', 'SK', 'UK';

/*create list with letters of files */
%let list=d h p r;

/* if you chose fileoption=1 then type in the paths to your include files here */
%let dincl = C:\path\to-your\D-file-codes;
%let hincl = C:\path\to-your\H-file-codes;
%let pincl = C:\path\to-your\P-file-codes;
%let rincl = C:\path\to-your\R-file-codes;


/* if you chose fileoption=2 then type in the paths to your files here */
/* note that the files must have the naming structure dfile9, hfile10, rfile11, etc. and be in their proper folder! */
libname dfiles "C:\path\to-your\d-file-data";
libname hfiles "C:\path\to-your\h-file-data";
libname pfiles "C:\path\to-your\p-file-data";
libname rfiles "C:\path\to-your\r-file-data";


/* small comment for each macro, to explain what happens */
%createdata(yr_from=6, yr_to=15, fileoption=1, countries=(&clist), vars=hy020 PH020)


/* small comment for each macro, to explain what happens */
%mergefiles()

/* small comment for each macro, to explain what happens */
%hhdeathfix()

/* small comment for each macro, to explain what happens */
%dataproc()

/* small comment for each macro, to explain what happens */
%infos(firstvars=PH020, lastvars=HY020)

/* small comment for each macro, to explain what happens */
%dataproc2() 

/* last comment */
/* data structure, possible applications etc */

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

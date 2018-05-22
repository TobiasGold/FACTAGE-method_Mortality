/************************************/
/* Version 1.0.2  					*/
/* Date: 04.04.2018 				*/
/* created by: Tobias Göllner 		*/
/* @TobiasGold on GitHub 			*/
/************************************/

/************************************/
/* This code creates all the macros.*/
/* Run this first and don't close 	*/
/* the SAS session.					*/
/************************************/

/* This is from http://support.sas.com/kb/45/805.html 	*/
/* The only change is the %include statement 			*/
%macro drive(dir,ext);                                                                                                                  
  %local filrf rc did memcnt name f;                                                                                                    
                                                                                                                                                                                               
  %let rc=%sysfunc(filename(filrf,&dir));                                                                                               
  %let did=%sysfunc(dopen(&filrf));                                                                                                     
                                                                                                                                                                                                                                
  %if &did eq 0 %then %do;                                                                                                              
   %put Directory &dir cannot be open or does not exist;                                                                                
   %return;                                                                                                                             
  %end;                                                                                                                                 
                                                                                                                                                                                                                                        
   %do f = 1 %to %sysfunc(dnum(&did));                                                                                                  
                                                                                                                                                                                                                                     
     %let name=%qsysfunc(dread(&did,&f));                                                                                                                                                                                                                                                                                                       
      %if %qupcase(%qscan(&name,-1,.)) = %upcase(&ext) %then %do;                                                                       
/*changed here*/  %include "&dir\&name";                                                                                                                
      %end;                                                                                                                                                                                                                    
      %else %if %qscan(&name,2,.) = %then %do;                                                                                          
        %drive(&dir\%unquote(&name),&ext)                                                                                               
      %end;                                                                                                                             
                                                                                                                                        
   %end;                                                                                                                                
                                                                                                                                                                                                                          
  %let rc=%sysfunc(dclose(&did));                                                                                                       
  %let rc=%sysfunc(filename(filrf));                                                                                                    
                                                                                                                                        
%mend drive;


/* This is the data creation macro */
%macro createdata(yr_from=5, yr_to=15, fileoption=1, countries=all, vars=);

%local n i;

%global dvars hvars pvars rvars;

%global luyr;
%let luyr = %eval(&yr_to - &yr_from + 4); /* this could use some rethinking */

%let n = %sysfunc(countw(&list));
%do i=1 %to &n;
	%let file = %scan(&list,&i);

filename zdat temp;
proc stream outfile=zdat prescol; begin
&vars
;;;;

data vars;
infile zdat;
input &vars;
run;

proc contents data=vars (keep=&file.:) out=&file.var (keep=name) noprint;
run;

	data &file.var;
	set &file.var;
	name=upcase(name);
	run;

	proc sql noprint;
	select distinct name into :&file.vars separated by ' ' from &file.var order by name;
	quit;

/* if files have to be created */
	%if &fileoption=1 %then %do;
		
		%drive(&&&file.incl,sas)

		data &file.full;
		set %do num=&yr_from %to &yr_to;
			&file.file&num (in=&file.&num.) 
			%end; ;
				%do num=&yr_from %to &yr_to;
				if &file.&num then &file._release = &num ; 
				%end; 
		%if &countries = all %then ; 
		%else where &file.b020 in &countries.; ;

		%if &file=d  %then %do;
	keep DB010 DB020 DB030 DB110 d_release &dvars;
		%end; 
		%else %if &file=h %then %do;
	keep HB010 HB020 HB030 HB050 HB060 h_release &hvars;
		%end;
		%else %if &file=p %then %do;	
	keep PB010 PB020 PB030 PB100 PB110 p_release &pvars;
		%end;
		%else %if &file=r %then %do;
	keep RB010 RB020 RB030 RB040 RB070 RB080 RB090 RB110 RB140 RB150 RX010 r_release &rvars;
		%end;
		run;

		data &file.full_re;
		set &file.full;
		if &file.b020 = 'EL' then &file.b020 = 'GR';
		run;

		proc sql;
		create table &file.full_s as
		select *
		from &file.full_re
		group by &file.b020, &file.b030, &file.b010
		having &file._release = max(&file._release);
		quit;


		data eusilc.&file._base;
		set &file.full_s;

%if &file=d  %then %do;
		rename DB010=Year_Survey DB020=Country DB030=HH_ID ;
%end; 
	%else %if &file=h %then %do;
		rename HB010=Year_Survey HB020=Country HB030=HH_ID ;
	%end;
	%else %if &file=p %then %do;	
		rename PB010=Year_Survey PB020=Country PB030=PS_ID ;
	%end;
	%else %if &file=r %then %do;
		rename RB010=Year_Survey RB020=Country RB030=PS_ID RB040=HH_ID ;
	%end;
		run;

		proc datasets lib=work nolist kill; 
		quit; 
		run; 
	%end;

/*if files are already created */
	%else %if &fileoption=2 %then %do;

		data &file.full;
		set %do num=&yr_from %to &yr_to;
			&file.files.&file.file&num (in=&file.&num.)
			%end; ;
				%do num=&yr_from %to &yr_to;
				if &file.&num then &file._release = &num ; 
				%end;
		%if &countries = all %then ;
		%else where &file.b020 in &countries.; ;

	%if &file=d  %then %do;
		keep DB010 DB020 DB030 DB110 d_release &dvars;
	%end; 
	%else %if &file=h %then %do;
		keep HB010 HB020 HB030 HB050 HB060 h_release &hvars;
	%end;
	%else %if &file=p %then %do;	
		keep PB010 PB020 PB030 PB100 PB110 p_release &pvars;
	%end;
	%else %if &file=r %then %do;
		keep RB010 RB020 RB030 RB040 RB070 RB080 RB090 RB110 RB140 RB150 RX010 r_release &rvars;
	%end;
		run;

		data &file.full_re;
		set &file.full;
		if &file.b020 = 'EL' then &file.b020 = 'GR';
		run;

		proc sql;
		create table &file.full_s as
		select *
		from &file.full_re
		group by &file.b020 &file.b030 &file.b010
		having &file._release = max(&file._release);
		quit;


		data eusilc.&file._base;
		set &file.full_s;

%if &file=d  %then %do;
		rename DB010=Year_Survey DB020=Country DB030=HH_ID ;
%end; 
	%else %if &file=h %then %do;
		rename HB010=Year_Survey HB020=Country HB030=HH_ID ;
	%end;
	%else %if &file=p %then %do;	
		rename PB010=Year_Survey PB020=Country PB030=PS_ID ;
	%end;
	%else %if &file=r %then %do;
		rename RB010=Year_Survey RB020=Country RB030=PS_ID RB040=HH_ID ;
	%end;
		run;

		proc datasets lib=work nolist kill; 
		quit; 
		run; 
	%end;

%end;

%mend createdata;

/* This merges the four files together */
%macro mergefiles();

/* re-rolling to a prior version */
/* hashing had some unexpected side-effects */
/* might add this again later */ 

proc sort data=eusilc.d_base;
by Country HH_ID Year_Survey;
run;

proc sort data=eusilc.h_base;
by Country HH_ID Year_Survey;
run;

data households;
merge eusilc.h_base eusilc.d_base (in=inD);
by Country HH_ID Year_Survey;
if inD;
run;

proc sort data=eusilc.r_base;
by Country PS_ID Year_Survey;
run;

proc sort data=eusilc.p_base;
by Country PS_ID Year_Survey;
run;

data persons;
merge eusilc.p_base eusilc.r_base (in=inR);
by Country PS_ID Year_Survey;
if inR;
run;

/* fix reassigned IDs */

	proc sort data=persons out=persons_sort (rename=(PS_ID=old_PS_ID));
	by Country PS_ID rb090 rb080 rb070 HH_ID Year_Survey;
	run;

	data persons_newID ;
	set persons_sort;
	by Country old_PS_ID rb090 rb080 rb070 HH_ID Year_Survey;
	  	if first.rb070 then PS_ID+1;
	run;

/* end */

proc sort data=households;
by Country HH_ID Year_Survey;
run;

	data eusilc.household_deaths;
	set households;
	where db110 = 5;
	keep country hh_id year_survey;
	run;

proc sort data=persons_newID;
by Country HH_ID Year_Survey;
run;

data eusilc.merge_all_work;
merge households persons_newID;
by Country HH_ID Year_Survey;
run;

%mend mergefiles;

/* This fixes the number of deaths if an entrie household dies */
%macro hhdeathfix();

/* first fix the deaths where an entire household dies */
	data household_deaths_IDs;
	set eusilc.household_deaths (rename = (year_survey = year_died));
	by country hh_id;
	year_survey = year_died - 1;
	if first.hh_id;	
	run;

/* create a dataset for re-merging */
	proc sort data = eusilc.merge_all_work out = household_deaths_info;
	by country hh_id year_survey ps_id;
	run;

/* merge the two datasets */
/* adds the information of the year before */
	data hh_to_replace;
	merge	household_deaths_info
			household_deaths_IDs (in = inD);
	by country hh_id year_survey;
	if inD;
	run;

/* select persons with ID and that are current household members */
/* this is from the year before the household dies */
	data hh_to_replace;
	set hh_to_replace;
	where ps_id ne . and rb110 in (1 2 3 4);
	year_survey + 1;
	db110 = 5;
	run;

/* sort... */
	proc sort data = hh_to_replace;
	by Country HH_ID PS_ID Year_Survey;
	run;

	proc sort data = eusilc.merge_all_work;
	by Country HH_ID PS_ID Year_Survey;
	run;


/* now use the created data to replace the cases where the entire household dies */
	data eusilc.merge_all_work_mod;
	merge	eusilc.merge_all_work (where = (db110 ne 5))
			hh_to_replace;
	by Country HH_ID PS_ID Year_Survey;
	run;

%mend hhdeathfix;

/* The first data processing step */
%macro dataproc();

	/*Add Perso_ID variable and create date Variables */ 
	data merge_all_work_mod1;	
	set eusilc.merge_all_work_mod;
	if Country ne 'BE' then do;
		
		/*survey month*/
		if pb100 = 1 then M_survey = 2;
			else if pb100 = 2 then M_survey = 5;
			else if pb100 = 3 then M_survey = 8;
			else if pb100 = 4 then M_survey = 11;
			else M_survey=.;

		/*deathORmove month*/
		if rb140 = 1 then M_deadORmove = 2;
			else if rb140 = 2 then M_deadORmove = 5;
			else if rb140 = 3 then M_deadORmove = 8;
			else if rb140 = 4 then M_deadORmove = 11;
			else M_deadORmove=.;

		/*add half a year to death or move. we assume the date of death or move is half a year after last interview */
		if db110 in (3 4 5) then do;
			if hb050 = 1 then M_deadORmove=8;
			else if hb050 = 2 then M_deadORmove=11;
			else if hb050 = 3 then M_deadORmove=2;
			else if hb050 = 4 then M_deadORmove=5;
			else M_deadORmove=.;
			end;

		/*birth month*/
		if rb070 = 1 then M_birth = 2;
			else if rb070 = 2 then M_birth = 5;
			else if rb070 = 3 then M_birth = 8;
			else if rb070 = 4 then M_birth = 11;
			else M_birth=.;

	end; /*BE only*/

	else if Country eq 'BE' then do;

		/*survey month*/
		if pb100 = 1 then M_survey = 2;
			else if pb100 = 2 then M_survey = 5;
			else if pb100 = 3 then M_survey = 8;
			else if pb100 = 4 then M_survey = 11;
			else M_survey=.;

		/*BE has full month data*/
		M_deadORmove=rb140;

		/*hb050 should be in quarters. no remark about a change in the differences... docx.*/
		if db110 in (3 4 5) then do;
			if hb050 = 1 then M_deadORmove=8;
			else if hb050 = 2 then M_deadORmove=11;
			else if hb050 = 3 then M_deadORmove=2;
			else if hb050 = 4 then M_deadORmove=5;
			else M_deadORmove=.;
			end;

		/*birth month*/
		if rb070 = 1 then M_birth = 2;
			else if rb070 = 2 then M_birth = 5;
			else if rb070 = 3 then M_birth = 8;
			else if rb070 = 4 then M_birth = 11;
			else M_birth=.;
	end;
	run;



	/*Create Variables*/ 
	data merge_all_work_mod2;
	set merge_all_work_mod1;

	/*assume middle of quarter for survey date and birth date*/
	Dat_Survey = mdy(M_survey, 15, pb110);
	Dat_birth = mdy(M_birth, 15, rb080);

	/*assume middle of quarter for date of death or move; date between last interview and would be next interview*/
	if rb110 in (5 6) then Dat_deadORmove = mdy(M_deadORmove, 15, rb150);
			else if db110 in (3 4 5) AND hb050 in (3 4) then Dat_deadORmove = mdy(M_deadORmove, 15, hb060+1);
			else if db110 in (3 4 5) then Dat_deadORmove = mdy(M_deadORmove, 15, hb060);
			else Dat_deadORmove=.;
		RUN; 


	/*Rename Variables*/ 
	/*IE, MT, NL, SI and UK all do no provide the Month of Birth (RB070) at all. */ 
	/*We fix this by assuming the person lived half a year before the interview */ 
	/*Some other countries also have this problem but for far less cases. These get fixed with this as well */ 
	data merge_all_work_mod3;
	set merge_all_work_mod2 (rename=(rb090=Sex rx010=Age_Survey));

	if Dat_birth=. then do;

		if month(Dat_Survey) = 8 then do
			Dat_birth=mdy(2 ,15, (year(Dat_Survey)-Age_Survey));
			end;
		else if month(Dat_Survey) = 11 then do;
			Dat_birth=mdy(5, 15, (year(Dat_Survey)-Age_Survey));
			end;
		else if month(Dat_Survey) = 2 then do;
			Dat_birth=mdy(8, 15, (year(Dat_Survey)-Age_Survey-1));
			end;
		else if month(Dat_Survey) = 5 then do;
			Dat_birth=mdy(11, 15, (year(Dat_Survey)-Age_Survey-1));
			end;

	end;
	else do;
		calc_age = yrdif(Dat_birth, Dat_Survey, 'act/act');
	end;

	run;

	/*Sort*/ 
	proc sort data=merge_all_work_mod3 out=merge_all_work_mod4;
	by PS_ID Year_Survey;
	run;

	/*Create Variables*/ 
	data merge_all_work_mod5;
	set merge_all_work_mod4;
	by PS_ID;
	Died=.;

	if rb110=6 then Died = 1; 	/* died */
	if rb110=5 then Died = -1; 	/* moved out or last interview if not contacted previous wave */
	if rb110=1 then Died = 0; 	/* current household member, was in HH prev wave */
	if rb110=2 then Died = 0; 	/* moved into this HH from other sample HH */
	if rb110=3 then Died = 0; 	/* moved into this HH from outside sample */
	if rb110=4 then Died = 0; 	/* newly born into HH */
	if rb110=7 then Died = 0; 	/* lived in HH for at least 3 monhts (inc ref per) not recorded in the register of this HH */

	if last.PS_ID then do;
		if db110=5 then Died=1;
		else if db110 in (3 4) then Died=-1;
		end;
	RUN;

	/*Create Variables*/ 
	data merge_all_work_mod6;
	set merge_all_work_mod5;
	by PS_ID;
	Dat_Death=.;
	Dat_Cens=.;

	if last.PS_ID then do;
		if Died=1 then Dat_Death = Dat_deadORmove;
		if Died=0 then Dat_Cens = Dat_Survey;
		if Died=-1 then Dat_Cens = Dat_deadORmove;
		end;
	RUN;

	/* Sort & save file */
	proc sort data=merge_all_work_mod6  out=eusilc.mortality_SILC_0;
	by PS_ID Year_Survey;
	Run;

%mend dataproc;

/* This creates two files, one with the first observation and one with the last observation of a person */
%macro infos(firstvars=, lastvars=);
		
	/*create first entry for every person*/
	data first (keep=PS_ID old_PS_ID calc_age Age_Survey Sex Dat_Survey Dat_birth Year_Survey Country &firstvars);
	set eusilc.mortality_SILC_0;
	by PS_ID;
	if first.PS_ID;
	RUN;

	/*create last entry for every person*/
	data last (keep= PS_ID Died Dat_Death Dat_Cens rb110 db110 &lastvars);
	set eusilc.mortality_SILC_0;
	by PS_ID;
	if last.PS_ID;
	Run;

	/* merge first and last */
	data eusilc.mortality_SILC_1;
	merge first last;
	by PS_ID;
	RUN;

%mend infos;

/* The second data processing step */
%macro dataproc2();
	/*add variable Verweildauer */
	data mortality_SILC_2;
	set eusilc.mortality_SILC_1;
	if Died=1 then Verweildauer = yrdif(Dat_Survey, Dat_Death, 'act/act');
		else Verweildauer = yrdif(Dat_Survey, Dat_Cens, 'act/act');
	if Verweildauer >0;
	RUN;

	/*compute new age variable*/
	data mortality_SILC_agefix;
	set mortality_SILC_2;
	age=.;

		if Age_Survey=. then age=int(calc_age);
		else if Age_Survey=-1 then age=int(calc_age);
		else if Age_Survey=-2 then age=int(calc_age);
		else if Age_Survey=81 then age=int(calc_age);
			else age=Age_Survey;

	run;

	/* LU has permanent panel. change to apropriate value */
	data mortality_SILC_vdfix;
	set mortality_SILC_agefix;
	where (age between 16 and 79);
	if Country = "FR" then do;
		if Verweildauer < 9 then eligible = 1;
		else eligible = 0;
	end;
	else if Country = "LU" then do;
		if Verweildauer < &luyr then eligible = 1;
		else eligible = 0;
	end; 
	else if  Country = "NO" then do;
		if Verweildauer < 8 then eligible = 1;
		else eligible = 0;
	end;
	else do;
		if Verweildauer < 4 then eligible = 1;
		else eligible = 0;
	end;
	if eligible = 1;
	run;

	data eusilc.SILC_full;
	set mortality_SILC_vdfix (drop= db110 rb110 Age_Survey calc_age eligible);
	IF Died = -1 then Died = 0;
	IF Dat_Death then dt_exit=Dat_Death;
	IF Dat_Cens then dt_exit=Dat_Cens;
	format dt_exit Dat_Death Dat_Cens Dat_Survey Dat_Birth ddmmyy10.;
	run;

%mend dataproc2;

/***********/
/*** END ***/
/***********/

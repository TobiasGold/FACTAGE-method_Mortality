/** Version 1.0.0 ** Date: 22.05.2018 ** created by: Johannes Klotz **/
/** This is an exemplary analysis in SPSS. This is to illustrate a potential application. **/

*Set working directory.
cd "C:\path\to-your\directory".

*Import the data from SAS and save the SPSS file.
set unicode on.
get sas data = "silc_full.sas7bdat".
save outfile "silc_full.sav".


* Model I: Estimating mortality hazard ratio by household income category

* First, data modification.

select if (age >= 30 and age <= 79) and hy020 > 0.
compute income_group = 1.
   if (hy020 >= 10000 and hy020 <= 30000) income_group = 2.
   if (hy020 > 30000) income_group = 3.
save outfile "analysis.sav".

* Then, the Proportional Hazards Regression.

coxreg Verweildauer
   /status = died (1)
   /contrast (income_group) = indicator (2)
   /method = enter age income_group
   /print = ci (95).

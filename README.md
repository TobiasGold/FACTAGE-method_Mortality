# Editing EU-SILC UDB Longitudinal Data for Differential Mortality Analyses

This SAS code is a deliverable of the Fairer Active Ageing for Europe (FACTAGE) project. FACTAGE is exploring emerging inequalities associated with longer working lives. It is a Joint Programming Initiative - More Years Better Lives (MYBL) - project. 
Details on the project can be found on [www.factage.eu](http://www.factage.eu). 
Details on the Joint Programming Initiative can be found on [www.jp-demographic.eu](http://www.jp-demographic.eu).

The SAS code is free to use at your own risk. The same applies for the R code.

Suggested citation: Göllner, T., and Klotz, J.: Editing EU-SILC UDB Longitudinal Data for Differential Mortality Analyses. SAS code and documentation. FACTAGE project report, May 2018.


## Abstract

This SAS code extracts data from EU-SILC User Database (UDB) longitudinal files and edits it such that a file is produced that can be further used for differential mortality analyses. Information from the original D, R, H and P files is merged per person and possibly pooled over several longitudinal data releases. Vital status information is extracted from target variables DB110 and RB110, and time at risk between the first interview and either death or censoring is estimated based on quarterly date information.

Apart from path specifications, the SAS code consists of several SAS macros. Two of them require parameter specification from the user. The other ones are just executed. The code was written in Base SAS, Version 9.4.

By default, the output file contains several variables which are necessary for differential mortality analyses, such as sex, age, country, year of first interview, and vital status information. In addition, the user may specify the analytical variables by which mortality risk should be compared later, for example educational level or occupational class. These analytical variables may be measured either at the first interview (the baseline) or at the last interview of a respondent. The output file is available in SAS format and by default also in csv format.

The R code is analogous to the SAS code, a seperate extensive manual is not planned, but a small guide.

## How to use

The provided pdf document should answer all questions regarding the SAS code.  Please read it carefully before submitting any questions. Alternatively, read the wiki page.

To understand the R code please also consult the manual to get an idea of the code. There probably will not be an extensive manual like for the SAS code, but I am planning for a small guide.

## News

17/07/2018: Updated the pdf document. These changes are not yet in the wiki.

03/09/2018: Uploaded the first draft of the R code, which is a "translation" from the SAS code. (No guide yet!)

21/05/2019: [b]We are currently revising the draft of the R code. Expect news and a release soon![/b]


## FACTAGE report on differential mortality

Klotz, J. & Göllner, T. (2017). Estimating Differential Mortality from EU-SILC Longitudinal Data. A Feasibility Study. FACTAGE project report. 

Download the [research report](https://www.factage.eu/pubs/FACTAGE_STAT_D4-1_Report_final.pdf) or the [policy brief](https://www.factage.eu/pubs/FACTAGE_STAT_D4-1_Policy_Brief_final.pdf).

http://dx.doi.org/10.11587/ZOOBKE

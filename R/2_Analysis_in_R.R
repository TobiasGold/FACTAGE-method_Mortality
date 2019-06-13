## Version 1.1.0 # Date: 13.06.2019 # created by: Johannes Klotz, Tobias Göllner ##
## This is an exemplary analysis in R. This is to illustrate a potential application.	##

# optional: to get the first survey year
SILC_UDB_X$FirstSurveyYear <- year(SILC_UDB_X$EntryDate)

# restrict useable ages
SILC_UDB_X_age <- SILC_UDB_X[between(AgeBaseline, 16, 79), ]
# only use valid duration times
SILC_UDB_X_age <- SILC_UDB_X_age[DurationTime > 0, ]

library (haven)
library (survival)

# Model I: Estimating mortality hazard ratio by household income category
analysis <- SILC_UDB_X_age[ which(AgeBaseline >= 30 & AgeBaseline <= 79 & HY020_baseline > 0),]

analysis$income_group[analysis$HY020_baseline < 10000] <- 1
analysis$income_group[analysis$HY020_baseline >= 10000 & analysis$HY020_baseline <= 30000] <- 2
analysis$income_group[analysis$HY020_baseline > 30000] <- 3

# Then, the Proportional Hazards Regression
Model_I <- coxph(Surv(time=DurationTime, event=Died) ~ AgeBaseline + factor(income_group, levels = c(2,1,3)), data = analysis)
summary(Model_I)

# An alternative model with Sex as stratifier
Model_IIa <- coxph(Surv(time=DurationTime, event=Died) ~ AgeBaseline + factor(income_group, levels = c(2,1,3)), subset = Country=="AT", data = analysis)
summary(Model_IIa)
Model_IIb <- coxph(Surv(time=DurationTime, event=Died) ~ AgeBaseline + factor(income_group, levels = c(2,1,3)), subset = Country=="PL", data = analysis)
summary(Model_IIb)

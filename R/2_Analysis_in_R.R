## Version 1.0.0 # Date: 22.05.2018 # created by: Johannes Klotz ##
## This is an exemplary analysis in R. This is to illustrate a potential application.	##

# Set the working directory
setwd ("C:/path/to-your/directory")

# (Install and) load special packages
install.packages (c("haven", "survival"))
library (haven)
library (survival)

# Import the sas file
silc_full <- read_sas ("silc_full.sas7bdat")


# Model I: Estimating mortality hazard ratio by household income category

# First, data modification
# Attention: R is case-sensitive!
attach (silc_full)
analysis <- silc_full [ which(age >= 30 & age <= 79 & HY020 > 0),]
detach (silc_full)

attach (analysis)
analysis$income_group [HY020 < 10000] <- 1
analysis$income_group [HY020 >= 10000 & HY020 <= 30000] <- 2
analysis$income_group [HY020 > 30000] <- 3
detach (analysis)

# Then, the Proportional Hazards Regression
Model_I <- coxph (Surv(time=Verweildauer, event=Died) ~ age + factor(income_group, levels = c(2,1,3)), data = analysis)
summary(Model_I)

library("tidyverse")
library("haven")
library("broom")
my_data <- read.csv("https://reifjulian.github.io/illinois-wellness-data/data/csv/claims.csv")

############################################################################
#Question 4
############################################################################





############################################################################
#Question 5: Treatment Vs. Control
############################################################################

#y = beta*x + e, y: outcome, x: treat/!treat

###year following randomization: 0816-0717
###without demographics

#Avg monthly spending
tidy(lm(spend_0816_0717~treat, my_data), conf.int = T, conf.level = 0.95)

#Avg monthly spending (Rx)
tidy(lm(spendRx_0816_0717~treat, my_data), conf.int = T, conf.level = 0.95)

#Avg monthly spending (office care)
tidy(lm(spendOff_0816_0717~treat, my_data), conf.int = T, conf.level = 0.95)

#Avg monthly spending (hospital care)
tidy(lm(spendHosp_0816_0717~treat, my_data), conf.int = T, conf.level = 0.95)

#Avg nonzero medical spending
tidy(lm(nonzero_spend_0816_0717~treat, my_data), conf.int = T, conf.level = 0.95)

############################################################################
###year following randomization: 0816-0717
###with demographics

#Avg monthly spending
tidy(lm(spend_0816_0717~treat+age50+male+age37_49+white, my_data), conf.int = T, conf.level = 0.95)

#Avg monthly spending (Rx)
tidy(lm(spendRx_0816_0717~treat+age50+male+age37_49+white, my_data), conf.int = T, conf.level = 0.95)

#Avg monthly spending (office care)
tidy(lm(spendOff_0816_0717~treat+age50+male+age37_49+white, my_data), conf.int = T, conf.level = 0.95)

#Avg monthly spending (hospital care)
tidy(lm(spendHosp_0816_0717~treat+age50+male+age37_49+white, my_data), conf.int = T, conf.level = 0.95)

#Avg nonzero medical spending
tidy(lm(nonzero_spend_0816_0717~treat+age50+male+age37_49+white, my_data), conf.int = T, conf.level = 0.95)


############################################################################
#Question 6: Participant vs. Non-Participant
############################################################################

###year following randomization: 0816-0717
###without demographics
my_data$participant=case_when(my_data$hra_c_yr1==1 & my_data$completed_screening_nomiss_2016==1 ~1,
                              my_data$hra_c_yr1==0 | my_data$completed_screening_nomiss_2016==0 ~0)
table(my_data$participant==my_data$hra_c_yr1) #TRUE
my_data_new <- filter(my_data, treat==1)


#Avg monthly spending
tidy(lm(spend_0816_0717~participant, my_data_new), conf.int = T, conf.level = 0.95)

#Avg monthly spending (Rx)
tidy(lm(spendRx_0816_0717~participant, my_data_new), conf.int = T, conf.level = 0.95)

#Avg monthly spending (office care)
tidy(lm(spendOff_0816_0717~participant, my_data_new), conf.int = T, conf.level = 0.95)

#Avg monthly spending (hospital care)
tidy(lm(spendHosp_0816_0717~participant, my_data_new), conf.int = T, conf.level = 0.95)

#Avg nonzero medical spending
tidy(lm(nonzero_spend_0816_0717~participant, my_data_new), conf.int = T, conf.level = 0.95)

############################################################################
###with demographics

#Avg monthly spending
tidy(lm(spend_0816_0717~participant+age50+male+age37_49+white, my_data_new), conf.int = T, conf.level = 0.95)

#Avg monthly spending (Rx)
tidy(lm(spendRx_0816_0717~participant+age50+male+age37_49+white, my_data_new), conf.int = T, conf.level = 0.95)

#Avg monthly spending (office care)
tidy(lm(spendOff_0816_0717~participant+age50+male+age37_49+white, my_data_new), conf.int = T, conf.level = 0.95)

#Avg monthly spending (hospital care)
tidy(lm(spendHosp_0816_0717~participant+age50+male+age37_49+white, my_data_new), conf.int = T, conf.level = 0.95)

#Avg nonzero medical spending
tidy(lm(nonzero_spend_0816_0717~participant+age50+male+age37_49+white, my_data_new), conf.int = T, conf.level = 0.95)




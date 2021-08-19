# Supplementary 4 – Hurdle and OLS models before the SMM and after the SMM
# 
# I implemented all OLS models and the wetland credit sales models individually for the periods 
# before and after SMM implementation (October 2013), thereby dropping the SMM binary variable and 
# interaction effect. I regressed each model normally and included the coefficients in Table S4.

# Wetland credit market before the SMM (Supp. 4) -------------------------

library(tidyverse)

all <- read_csv("C:\\Users\\Owner\\Documents\\Fort Worth Research\\Research\\3.14_research_redone_R_code\\Essential\\all_5.20_prime_with_zeros.csv")
# First - let's look at how the wetland credit market was growing before SMM was implemented - so from 2007 to 2013
all2 <- all %>% filter(month_as_num > 93 & month_as_num < 166)
all2 <- all %>% filter(month_as_num >= 166 & month_as_num <= 237)

full_glms_all2 <- function(beta1) {
  
  non_zero <- ifelse(beta1 > 0, 1, 0)
  
  d <- tibble(beta1 =beta1, non_zero, month = all2$month_as_num, after_SMM = all2$after_SMM)
  d <- d %>% mutate(after_SMM_true = ifelse(after_SMM == 1, TRUE, FALSE))
  x <- summary(glm(beta1 ~ month, data = subset(d, non_zero == 1), family = Gamma(link = "log")))
  print(x)
  print(exp(x$coefficients))
  
  # printing full summary
  
  y <- summary(glm(non_zero ~ month * after_SMM, data = d, family = binomial(link = logit)))
  print(y)
  print(exp(y$coefficients))
  # printing full summary
  z = glm(beta1 ~ month * after_SMM, data = subset(d, non_zero == 1), family = Gamma(link = log))
  print((exp(confint(z))))
  z2 = glm(non_zero ~ month * after_SMM, data = d, family = binomial(link = logit))
  print(exp(confint(z2)))
  print(logLik(z))
  print(logLik(z2))
}

full_glms_all2(all2$rib_pal_credits)

# Credit supply before and after the SMM (Supp. 4) -------------------------------

all_test <- read_csv("C:\\Users\\Owner\\Documents\\Fort Worth Research\\Research\\3.14_research_redone_R_code\\credit_potential_rel_5.22.csv")
# riv cred change and pal cred change will be my two variables
#all <- read_csv("C:\\Users\\Owner\\Documents\\Fort Worth Research\\Research\\3.14_research_redone_R_code\\Essential\\all_4_4.16_prime_with_zeros.csv")
all_test2 <- all_test %>% filter(month_as_num > 93 & month_as_num <238)

# # Per Figure 2, pre-SMM, wetland credit supplies grew at a rate of X additional credits added to the 
# market per month, but declined at a rate of XYZ upon the release of the SMM.

summary(lm(pal_cred_P ~ month_as_num * after_SMM, all_test2))

summary(lm(pal_cred_P ~ month_as_num, all_test2[1:72,]))
confint(lm(pal_cred_P ~ month_as_num, all_test2[1:72,]))
AIC(lm(pal_cred_P ~ month_as_num, all_test2[1:72,]))
logLik(lm(pal_cred_P ~ month_as_num, all_test2[1:72,]))

summary(lm(pal_cred_P ~ month_as_num, all_test2[73:144,]))
confint(lm(pal_cred_P ~ month_as_num, all_test2[73:144,]))
AIC(lm(pal_cred_P ~ month_as_num, all_test2[73:144,]))
logLik(lm(pal_cred_P ~ month_as_num, all_test2[73:144,]))

all_test3 <- all_test2 %>% filter(riv_cred_P> 0 & after_SMM_T== FALSE)
summary(lm(riv_cred_P ~ month_as_num , all_test3))
confint(lm(riv_cred_P ~ month_as_num , all_test3))
AIC(lm(riv_cred_P ~ month_as_num , all_test3))
logLik(lm(riv_cred_P ~ month_as_num , all_test3))

all_test4 <- all_test2 %>% filter(after_SMM_T== T)
summary(lm(riv_cred_P ~ month_as_num , all_test4))
confint(lm(riv_cred_P ~ month_as_num , all_test4))
AIC(lm(riv_cred_P ~ month_as_num , all_test4))
logLik(lm(riv_cred_P ~ month_as_num , all_test4))

# Coincident Economic Growth index (Supp. 2) ----------------------
# I attempted to determine if a relationship existed between economic growth and shifts within
# the mitigation market in the Upper Trinity River Basin. I used the Coincident Economic Index 
# (CEI; Figure S1), developed by Crone and Clayton-Matthews (2005) to assess the economic conditions
# within Texas between October 2007 and September 2019. Concerns of redundancy due to high correlation 
# (r = 0.99) between time and the CEI and problems with interpretability of the CEI led me to reject 
# the CEI for my primary analysis. 
#

all2 <- all %>% filter(month_as_num > 93 & month_as_num < 238)

full_glms_coincident <- function(beta1) {
  
  non_zero <- ifelse(beta1 > 0, 1, 0)
  
  d <- tibble(beta1 =beta1, non_zero, coincident = all2$coincident)
  x <- summary(glm(beta1 ~ coincident, data = subset(d, non_zero == 1), family = Gamma(link = "log")))
  print(x)
  print(exp(x$coefficients))
  
  # printing full summary
  
  y <- summary(glm(non_zero ~ coincident, data = d, family = binomial(link = logit)))
  print(y)
  print(exp(y$coefficients))
  # printing full summary
  z = glm(beta1 ~ coincident, data = subset(d, non_zero == 1), family = Gamma(link = log))
  print((exp(confint(z))))
  z2 = glm(non_zero ~ coincident, data = d, family = binomial(link = logit))
  print(exp(confint(z2)))
  print(logLik(z))
  print(logLik(z2))
}

full_glms_coincident(all2$rib_pal_credits) #  statistically sig
full_glms_coincident(all2$rib_riv_credits) # not statistically sig
full_glms_coincident(all2$orm_pal_acres_new) # not stat

all_test <- read_csv("C:\\Users\\Owner\\Documents\\Fort Worth Research\\Research\\3.14_research_redone_R_code\\credit_potential_rel_5.22.csv")
all_test2 <- all_test %>% filter(month_as_num > 93 & month_as_num <238)
summary(lm(pal_cred_P ~ coincident, all_test2))#  statistically sig
summary(lm(riv_cred_P ~ coincident, all_test2))#  statistically sig

all4 <- read_csv("C:\\Users\\Owner\\Documents\\Career Resources\\Portfolio\\Thesis\\Code\\all_4_4.16.csv")
all4 <- all4 %>% mutate(dateX = as.Date(Date_test, "%m/%d/%Y")) %>% mutate(after_SMM_true = ifelse(after_SMM == 1, TRUE, FALSE))
dateX = as.Date(all4$Date_test, "%m/%d/%Y")
all4 <- all4[94:242, ]

all4 %>% ggplot(aes(x = dateX,
                    y = coincident,
                    family = after_SMM_true))+
  geom_line(size = 1)+
  #geom_smooth(method = "lm", se = F, color = "#404040") + 
  scale_x_date(date_breaks = "3 years", date_labels = "%Y")+
  geom_vline(xintercept = as.numeric(all4$dateX[73]), color = "black", linetype = "dashed", size = 1)+
  xlab("Year")+ylab("Coincident Economic Index\n(Texas)")+ ggthemes::theme_few()+
  theme(plot.title = element_text(size=20, face="bold.italic"),
        axis.title.x = element_text(size=20, face="bold"),
        axis.title.y = element_text(size=20, face="bold"), axis.text=element_text(size=18))

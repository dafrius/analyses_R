# Last updated 21/04/22

library(tidyverse)
library(tibble)
library(brms)
library(rstan)



load("fit_invlogit_1234th.rda")


sumcoefs <- coef(fit_invlogit_1234th)$subject # Obtaining summary coefficient values from the model

sumco1 <- as_tibble(sumcoefs, rownames="subject") # Wrangling the data

sumco2 <- sumco1 %>% select(subject, starts_with("Esti")) %>% # Only getting the "Estimate" values
  pivot_longer(2:19, names_to="name", values_to="val") %>% #long format
  mutate(name = stringr::str_remove(name,"Estimate.eta_")) %>% # removing the recurring and unnecessary part in the name
  pivot_wider(names_from="name", values_from="val") %>% # wide format again
  mutate(int_inv_diff = Intercept,
         slp_inv_diff = iv3,
         int_low_diff = Intercept + tasklow,
         slp_low_diff = iv3 + `iv3:tasklow`,
         int_up_diff = Intercept + taskupright,
         slp_up_diff = iv3 + `iv3:taskupright`,
         int_inv_same = Intercept + condsame,
         slp_inv_same = iv3 + `iv3:condsame`,
         int_low_same = Intercept + `tasklow` + `condsame` + `tasklow:condsame`,
         slp_low_same = iv3 + `iv3:tasklow` + `iv3:condsame` + `iv3:tasklow:condsame`,
         int_up_same = Intercept + `taskupright` + `condsame` + `taskupright:condsame`,
         slp_up_same = iv3 + `iv3:taskupright` + `iv3:condsame` + `iv3:taskupright:condsame`,
         int_inv_iso = Intercept + condiso,
         slp_inv_iso = iv3 + `iv3:condiso`,
         int_low_iso = Intercept + tasklow + condiso + `tasklow:condiso`,
         slp_low_iso = iv3 + `iv3:tasklow` + `iv3:condiso` + `iv3:tasklow:condiso`,
         int_up_iso = Intercept + `taskupright` + `condiso` + `taskupright:condiso`,
         slp_up_iso = iv3 + `iv3:taskupright` + `iv3:condiso` + `iv3:taskupright:condiso`) %>% #calculating the actual slope and intercept values
  select(subject, starts_with("int_"), starts_with("slp")) %>%
  pivot_longer(2:19, names_to="param", values_to="val") %>%
  separate(param, into=c("param","task","cond"), sep="_") %>%
  pivot_wider(names_from="param", values_from="val") %>%
  mutate(location=-int/slp,
         scale=1/slp) %>%
  rename(intercept=int,
         slope=slp)

brmseta_1234 <- sumco2

save(brmseta_1234, file="brmslsETA_1234th.rda")




#summary etas


summ_etas <- fixef(fit_invlogit_1234th) # Obtaining summary coefficient values from the model

summ_etas1 <- as_tibble(summ_etas, rownames="etas") # Wrangling the data

summ_etas2 <- summ_etas1 %>% select(etas, starts_with("Esti")) %>% # Only getting the "Estimate" values
  #pivot_longer(2:19, names_to="name", values_to="val") %>% #long format
  mutate(etas = stringr::str_remove(etas,"eta_")) %>% # removing the recurring and unnecessary part in the name
  pivot_wider(names_from="etas", values_from="Estimate") %>% # wide format again
  mutate(int_inv_diff = Intercept,
         slp_inv_diff = iv3,
         int_low_diff = Intercept + tasklow,
         slp_low_diff = iv3 + `iv3:tasklow`,
         int_up_diff = Intercept + taskupright,
         slp_up_diff = iv3 + `iv3:taskupright`,
         int_inv_same = Intercept + condsame,
         slp_inv_same = iv3 + `iv3:condsame`,
         int_low_same = Intercept + `tasklow` + `condsame` + `tasklow:condsame`,
         slp_low_same = iv3 + `iv3:tasklow` + `iv3:condsame` + `iv3:tasklow:condsame`,
         int_up_same = Intercept + `taskupright` + `condsame` + `taskupright:condsame`,
         slp_up_same = iv3 + `iv3:taskupright` + `iv3:condsame` + `iv3:taskupright:condsame`,
         int_inv_iso = Intercept + condiso,
         slp_inv_iso = iv3 + `iv3:condiso`,
         int_low_iso = Intercept + tasklow + condiso + `tasklow:condiso`,
         slp_low_iso = iv3 + `iv3:tasklow` + `iv3:condiso` + `iv3:tasklow:condiso`,
         int_up_iso = Intercept + `taskupright` + `condiso` + `taskupright:condiso`,
         slp_up_iso = iv3 + `iv3:taskupright` + `iv3:condiso` + `iv3:taskupright:condiso`) %>% #calculating the actual slope and intercept values
  select(starts_with("int_"), starts_with("slp")) %>%
  pivot_longer(1:18, names_to="param", values_to="val") %>%
  separate(param, into=c("param","task","cond"), sep="_") %>%
  pivot_wider(names_from="param", values_from="val") %>%
  mutate(location=-int/slp,
         scale=1/slp) %>%
  rename(intercept=int,
         slope=slp)

summary_etas_1234th <- summ_etas2

save(summary_etas_1234th,file="summary_etas_1234th.rda")


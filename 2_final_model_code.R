library(tidyverse)
library(tibble)
library(brms)
library(rstan)

load("d_lohi12345.rda")

df <-  d_lohi12345

df_trns <- df %>% mutate(iv2= case_when(task=="low"~log10(iv),
                                        TRUE~iv)) %>%
  group_by(task) %>%
  mutate(iv3=(iv2-mean(iv2))/sd(iv2),
         task=as.factor(task),
         guess=case_when(task=="low"~1,
                         task!="low"~0))

fit_invlogit_12345th <- brm(
  bf(dv ~ 0.5*guess + (1-0.5*guess)*inv_logit(eta),
     eta ~ iv3*task*cond + (iv3*task*cond||subject), nl=TRUE),
  data=df_trns,
  family=bernoulli("identity"),
  prior=c(prior(normal(0,5), nlpar="eta"),
          prior(lognormal(0,.1), nlpar="eta", class="sd", group="subject")),
  cores=12,
  warmup = 3000,
  iter=6000)

save(fit_invlogit_12345th, file="fit_invlogit_12345th.rda", compress="xz")


pred_df <- expand.grid(iv3 = seq(-3,3, l = 101),
                       task = unique(df_trns$task),
                       subject = unique(df_trns$subject),
                       cond = unique(df_trns$cond))

pred_df <- pred_df %>% mutate(guess=case_when(task=="low"~1,
                                              task!="low"~0))
pred_df <- bind_cols(pred_df, as_tibble(fitted(fit_invlogit_12345th, newdata = pred_df, 
                                               re_formula = NULL,scale="linear")))
save(pred_df, file="preds_invlogit_12345th.rda", compress="xz")

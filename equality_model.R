library(INLA)
library(dplyr)

# Load data
fin_data <- readRDS(file="toy_data.RDS")
## Remove duplicate rows
fin_data <- fin_data %>% 
  group_by(LTLA_ID, date_ID, age_class) %>% 
  filter(row_number()==1) %>% ungroup()

# Main analysis for disaggregated BAME subgroups 

formula_main <- counts ~ 1  + rural_urban + vax_prop_stand +
  IMD_stand + 
  bame_black_stand + 
  bame_southasian_stand + 
  bame_other_stand + 
  f(age_class, model='iid', constr=TRUE) +
  f(date_ID, model = "rw2") +
  f(LTLA_ID, model = "bym2", graph = "W.adj", scale.model = TRUE, constr =
      TRUE, hyper = list(theta1 = list("PCprior", c(1,   0.01)), theta2 =
                           list("PCprior", c(0.5, 0.5)))) +
  f(date_LTLA_ID, model = "iid") + 
  f(date_month_Black, bame_black_stand, model= "rw1", constr = TRUE) +
  f(date_month_SA, bame_southasian_stand, model= "rw1", constr = TRUE) +
  f(date_month_Other, bame_other_stand, model= "rw1", constr = TRUE) +
  f(date_month_IMD, IMD_stand, model= "rw1", constr = TRUE) 

res_main <- inla(formula_main, data = fin_data, family = "binomial",
                 Ntrials = tot_pop,
                 verbose = TRUE, 
                 num.threads = 2, 
                 control.fixed=list(prec=1,prec.intercept=1),
                 control.inla=list(int.strategy="eb", strategy="adaptive"),
                 control.compute=list(config = TRUE))

summary(res_main)
save(res_main, file='res_main.RData')

# Main analysis for aggregated BAME 

formula_main2 <- counts ~ 1  + rural_urban + vax_prop_stand + 
  IMD_stand + BAME_stand + 
  f(age_class, model='iid', constr=TRUE) +
  f(date_ID, model = "rw2") +
  f(LTLA_ID, model = "bym2", graph = "W.adj", scale.model = TRUE, constr =
      TRUE, hyper = list(theta1 = list("PCprior", c(1,   0.01)), theta2 =
                           list("PCprior", c(0.5, 0.5)))) +
  f(date_LTLA_ID, model = "iid")  +
  f(date_month, BAME_stand, model= "rw1", constr = TRUE) +
  f(date_month_IMD, IMD_stand, model= "rw1", constr = TRUE)

res_main2 <- inla(formula_main2, data = fin_data, family = "binomial",
                 Ntrials = tot_pop,
                 verbose = TRUE, 
                 num.threads = 2, 
                 control.fixed=list(prec=1,prec.intercept=1),
                 control.inla=list(int.strategy="eb", strategy="adaptive"),
                 control.compute=list(config = TRUE))

summary(res_main2)
save(res_main2, file='res_main2.RData')

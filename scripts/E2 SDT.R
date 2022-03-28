
library(readxl)
library(ggplot2)
library(tidyverse)
library(MASS)
library(brms)
library(ggthemes)
library(emmeans)
library("easystats")
library(readr)
library(logspline)
library(loo)

###

theme_set(
  jtools::theme_apa(x.font.size = 11, y.font.size = 11,
                    legend.font.size = 12, facet.title.size = 13) +
    theme(text = element_text(family = "Times"),
          strip.background = element_blank(),
          legend.key.width = unit(1, "cm"),
          legend.key.height = unit(.5, "cm"),
          legend.position = "top",
          legend.justification = "right"))

####

d <- read_csv("data/Experiment 1 + 2 - Cleaned Data.csv")
de2 <- d %>%filter(parade == "six")

####process data to SDT format####

d_sdt <- de2 %>%
  mutate(target = tolower(TP),
         resp_expec = order,
         resp_given = parade_response,
         exposure = factor(exposure, levels = c("15", "30", "60"), ordered = F),
         age = as.numeric(gsub(" years", "", age))) %>%
  mutate(suspect_id = resp_given == resp_expec & resp_given != 0, # say target (old response)
         filler_id  = resp_given != resp_expec & resp_given != 0,
         no_id = resp_given == 0 | is.na(resp_given),
         is_target = target == "present",
         say_target = (resp_given != 0 & target == "absent") | (resp_given == resp_expec & target == "present"),
         is_corr = resp_given == resp_expec) 

count(d_sdt, is_target, say_target, suspect_id, filler_id, no_id)

#### remove TP foil IDs###

d <- d_sdt %>%
  mutate(condition = factor(paste(exposure, sep = "_"))) %>%
  filter(!(is_target & filler_id))

count(d, is_target, say_target, suspect_id, filler_id, no_id)

# Fit model
nchains = ncores = 3
iter = 30000

# Make adjustment to FA rate
d <- mutate(d, adjustment = ifelse(is_target, 1/6, 6)) 

# Model
m <- bf(say_target ~ Phi( dprime * is_target - c * adjustment),  
        c + dprime ~ 0 + condition + (1 |p| lineup),
        nl = TRUE,
        family = bernoulli(link="identity")) 

# Priors
#get_prior(m, data = d)
priors <- c(prior(normal(0, 3), class = "b", nlpar = "dprime", lb = 0),
            prior(normal(0, 3), class = "b", nlpar = "c"),
            prior(student_t(10, 0, 1), class = "sd", nlpar = "dprime"),
            prior(student_t(10, 0, 1), class = "sd", nlpar = "c"))

# Model fit
fit <- brm(formula = m, data = d,
           prior = priors, sample_prior = TRUE,
           cores = ncores, chains = nchains,
           iter = iter, warmup = iter/2, 
           refresh = iter / 2,
           control = list(adapt_delta = .99), seed = 123)

round(fixef(fit),2)

###inference###

##criterion

h01c15 <- hypothesis(fit, "c_condition15 = 0")
h01c15$hypothesis$Evid.Ratio #bf01
1/h01c15$hypothesis$Evid.Ratio #bf10

h01c30 <-hypothesis(fit, "c_condition30 = 0")
h01c30$hypothesis$Evid.Ratio #bf01
1/h01c30$hypothesis$Evid.Ratio #bf10

h01c60 <-hypothesis(fit, "c_condition60 = 0")
h01c60$hypothesis$Evid.Ratio #bf01
1/h01c60$hypothesis$Evid.Ratio #bf10

#pairwise

h01c1530 <- hypothesis(fit, "c_condition15 = c_condition30")
h01c1530$hypothesis$Evid.Ratio #bf01
1/h01c1530$hypothesis$Evid.Ratio #bf10

h01c1560 <- hypothesis(fit, "c_condition15 = c_condition60")
h01c1560$hypothesis$Evid.Ratio #bf01
1/h01c1560$hypothesis$Evid.Ratio #bf10

h01c3060 <- hypothesis(fit, "c_condition30 = c_condition60")
h01c3060$hypothesis$Evid.Ratio #bf01
1/h01c3060$hypothesis$Evid.Ratio #bf10


##sensitivity

h01d15 <- hypothesis(fit, "dprime_condition15 = 0")
h01d15$hypothesis$Evid.Ratio #bf01
1/h01d15$hypothesis$Evid.Ratio #bf10

h01d30 <-hypothesis(fit, "dprime_condition30 = 0")
h01d30$hypothesis$Evid.Ratio #bf01
1/h01d30$hypothesis$Evid.Ratio #bf10

h01d60 <-hypothesis(fit, "dprime_condition60 = 0")
h01d60$hypothesis$Evid.Ratio #bf01
1/h01d60$hypothesis$Evid.Ratio #bf10

#pairwise

h01d1530 <- hypothesis(fit, "dprime_condition15 = dprime_condition30")
h01d1530$hypothesis$Evid.Ratio #bf01
1/h01d1530$hypothesis$Evid.Ratio #bf10

h01d1560 <- hypothesis(fit, "dprime_condition15 = dprime_condition60")
h01d1560$hypothesis$Evid.Ratio #bf01
1/h01d1560$hypothesis$Evid.Ratio #bf10

h01d3060 <- hypothesis(fit, "dprime_condition30 = dprime_condition60")
h01d3060$hypothesis$Evid.Ratio #bf01
1/h01d3060$hypothesis$Evid.Ratio #bf10

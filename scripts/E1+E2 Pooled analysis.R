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

####wrangling###

d <- read_csv("data/Experiment 1 + 2 - Cleaned Data.csv") %>%
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
         is_corr = resp_given == resp_expec) %>%
  mutate(is_target = ifelse(is_target == TRUE, "present", "absent"),
         is_corr = ifelse(is_corr == TRUE, "correct", "incorrect")) %>%
  mutate(condition = factor(paste(exposure, parade, sep = "_")))

summary(d$condition)

contrasts(d$condition) <- contr.treatment(levels(d$condition),
                                           base=which(levels(d$condition) == '60_nine'))

summary(d$condition)
(nrow <- length(levels(d$condition)))

#####

m <- bf(acc ~ condition + (1 | lineup), family = bernoulli())

get_prior(m, data = d) %>% tibble() %>% unique() %>% as.data.frame()

prior <- set_prior("normal(0, 1)", class = "b")
prior("student_t(3, 0, 2.5)", class = "Intercept")
prior("student_t(3, 0, 2.5)", class = "sd")

fit <- brm(m, data = d, 
                #prior = prior,#use flat prior
                inits = 0,
                iter = 30000, 
                sample_prior = TRUE,
                chains = 6,
                cores = 6,
                seed = 365,
                control = list(adapt_delta = 0.99, 
                               max_treedepth = 16))

summary(fit_null) 



###combined data


d$exposure <- as.factor(d$exposure)
contrasts(d$exposure) <- contr.treatment(levels(d$exposure),
                                         base=which(levels(d$exposure) == '60'))

get_prior(m, data = d) %>% tibble() %>% unique() %>% as.data.frame()

prior <- set_prior("normal(0, 1)", class = "b")
prior("student_t(3, 0, 2.5)", class = "Intercept")
prior("student_t(3, 0, 2.5)", class = "sd")


m.null <- bf(acc ~ 1 + (1 | lineup), family = bernoulli())
m.main.exposure <- bf(acc ~ exposure + (1 | lineup), family = bernoulli())
m.main.tp <- bf(acc ~ TP + (1 | lineup), family = bernoulli())
m.main.size <- bf(acc ~ parade + (1 | lineup), family = bernoulli())
m.main.all <- bf(acc ~ exposure + TP + parade + (1 | lineup), family = bernoulli())
m.inter2 <- bf(acc ~ (exposure + TP + parade)^2 + (1 | lineup), family = bernoulli())
m.inter3 <- bf(acc ~ (exposure + TP + parade)^3 + (1 | lineup), family = bernoulli())

m.inter2 <- bf(acc ~ (TP + parade)^2 + (1 | lineup), family = bernoulli())

fit_null <- brm(m.null, data = d, 
                #prior = prior,#use flat prior
                inits = 0,
                iter = 30000, 
                sample_prior = TRUE,
                chains = 6,
                cores = 6,
                seed = 365,
                control = list(adapt_delta = 0.99, 
                               max_treedepth = 16))

summary(fit_null) 

fit_main.exposure  <- brm(m.main.exposure, data = d, 
                          prior = prior,
                          inits = 0,
                          iter = 30000,
                          save_all_pars = TRUE,
                          sample_prior = TRUE,
                          chains = 6,
                          cores = 6,
                          seed = 365,
                          control = list(adapt_delta = 0.99, 
                                         max_treedepth = 16))
summary(fit_main.exposure) 

fit_main.tp  <- brm(m.main.tp, data = d, 
                    prior = prior,
                    inits = 0,
                    iter = 30000,
                    save_all_pars = TRUE,
                    sample_prior = TRUE,
                    chains = 6,
                    cores = 6,
                    seed = 365,
                    control = list(adapt_delta = 0.99, 
                                   max_treedepth = 16))

summary(fit_main.tp) 

fit_main_size  <- brm(m.main.size, data = d, 
                      prior = prior,
                      inits = 0,
                      iter = 30000,
                      save_all_pars = TRUE,
                      sample_prior = TRUE,
                      chains = 6,
                      cores = 6,
                      seed = 365,
                      control = list(adapt_delta = 0.99, 
                                     max_treedepth = 16))

summary(fit_main_size) 



fit_main_all  <- brm(m.main.all, data = d, 
                     prior = prior,
                     inits = 0,
                     iter = 30000,
                     save_all_pars = TRUE,
                     sample_prior = TRUE,
                     chains = 6,
                     cores = 6,
                     seed = 365,
                     control = list(adapt_delta = 0.99, 
                                    max_treedepth = 16))

summary(fit_main_all) 

fit_inter2 <- brm(m.inter2, data = d, 
                  prior = prior,
                  inits = 0,
                  iter = 30000,
                  save_all_pars = TRUE,
                  sample_prior = TRUE,
                  chains = 6,
                  cores = 6,
                  seed = 365,
                  control = list(adapt_delta = 0.99, 
                                 max_treedepth = 16))

summary(fit_inter2) 

fit_inter3 <- brm(m.inter3, data = d, 
                  prior = prior,
                  inits = 0,
                  iter = 30000,
                  save_all_pars = TRUE,
                  sample_prior = TRUE,
                  chains = 6,
                  cores = 6,
                  seed = 365,
                  control = list(adapt_delta = 0.99, 
                                 max_treedepth = 16))

summary(fit_inter3) 

conditional_effects(fit_inter, effects = NULL,
                    conditions = NULL, int_conditions = NULL, re_formula = NA,
                    robust = TRUE)


loo.null <- loo(fit_null)
loo.main_all <- loo(fit_main_all)
loo.tp <- loo(fit_main.tp)
loo.size <- loo(fit_main_size)
loo.exposure <- loo(fit_main.exposure)
loo.inter2 <- loo(fit_inter2)
loo.inter3 <- loo(fit_inter3)

loo::loo_compare(loo.null, loo.tp , loo.exposure , loo.size, loo.main_all, loo.inter2, loo.inter3)


####

describe_posterior(
  fit_main_all,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance", "bf"),
  centrality = "all")

p_map(
  fit_main_all,
  precision = 2^10,
  method = "kernel",
  effects = c("fixed", "random", "all"),
  component = c("conditional", "zi", "zero_inflated", "all"),
  parameters = NULL
)


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

###
d <- read_csv("data/Experiment 1 + 2 - Cleaned Data.csv")
###

###Experiment 1###

###wrangling###

de2 <- d %>%filter(parade == "six")
de2$COND <- paste(de2$TP, de2$exposure, sep = "_")
de2$COND <- as.factor(de2$COND)
summary(de2$COND)
de2$TP <- factor(de2$TP)
levels(de2$TP)
de2$TP <- factor(de2$TP)
levels(de2$TP)
de2$exposure <- factor(de2$exposure)
levels(de2$exposure)
de2$exposure <- relevel(de2$exposure, ref="60")
de2$sex <- factor(de2$sex)
levels(de2$sex)

mean(de2$age)
sd(de2$age)
table(de2$sex)

contrasts(de2$exposure) <- contr.treatment(levels(de2$exposure),
                                           base=which(levels(de2$exposure) == '60'))
#Accuracy models 

###defining the models

m.null <- bf(acc ~ 1 + (1 | lineup), family = bernoulli())
m.main.exposure <- bf(acc ~ exposure + (1 | lineup), family = bernoulli())
m.main.tp <- bf(acc ~ TP + (1 | lineup), family = bernoulli())
m.main.all <- bf(acc ~ exposure + TP + (1 | lineup), family = bernoulli())
m.inter <- bf(acc ~ exposure + TP + exposure:TP + (1 | lineup), family = bernoulli())

get_prior(m.inter, data = de2) %>% tibble() %>% unique() %>% as.data.frame()

prior <- set_prior("normal(0, 1)", class = "b")
prior("student_t(3, 0, 2.5)", class = "Intercept")
prior("student_t(3, 0, 2.5)", class = "sd")

###running the models

fit_null <- brm(m.null, data = de2, 
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

fit_main.exposure  <- brm(m.main.exposure, data = de2, 
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

fit_main.tp  <- brm(m.main.tp, data = de2, 
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


fit_main_all  <- brm(m.main.all, data = de2, 
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

fit_inter <- brm(m.inter, data = de2, 
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

summary(fit_inter) 

###check models

pp_check(fit_inter)
conditional_effects(fit_inter, effects = NULL,
                    conditions = NULL, int_conditions = NULL, re_formula = NA,
                    robust = TRUE)

####compare models

loo.null <- loo(fit_null)
loo.main_all <- loo(fit_main_all)
loo.tp <- loo(fit_main.tp)
loo.exposure <- loo(fit_main.exposure)
loo.inter <- loo(fit_inter)

loo::loo_compare(loo.null, loo.tp , loo.exposure , loo.main_all, loo.inter)

#####inference from interaction model####

describe_posterior(
  fit_inter,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance", "bf"),
  centrality = "all")

hdi(
  fit_inter,
  ci = 0.95,
  effects = c("fixed", "random", "all"),
  component = c("conditional", "zi", "zero_inflated", "all"),
  parameters = NULL,
  verbose = TRUE)

p_map(
  fit_inter,
  precision = 2^10,
  method = "kernel",
  effects = c("fixed", "random", "all"),
  component = c("conditional", "zi", "zero_inflated", "all"),
  parameters = NULL
)


####plot interaction

effects <- conditional_effects(fit_inter)
interactionME <- effects$`exposure:TP`
interactionME$exposure <- factor(interactionME$exposure, levels = c("15", "30", "60"))

interactionME %>% 
  mutate(exposure = recode(exposure, `60` = "60s duration",
                           '15' = "15s duration",
                           '30' = "30s duration" ),
         TP = str_to_sentence(TP)) %>%
  ggplot(aes(y = estimate__, ymin=lower__, ymax=upper__, colour = TP, shape = TP, x = exposure)) +
  geom_point(position = position_dodge(.25), size = 4.5, fill = "white") +
  geom_errorbar(position = position_dodge(.25), width = .1) +
  labs(y = "Response Accuracy", x = "Sample Duration") +
  scale_y_continuous(limits = c(0,1), labels = scales::percent_format(accuracy = 1)) +
  scale_shape_manual(values = c(21,24)) +
  scale_color_colorblind() +
  geom_hline(yintercept=0.1428571, linetype="dashed", color = "red", size=1)
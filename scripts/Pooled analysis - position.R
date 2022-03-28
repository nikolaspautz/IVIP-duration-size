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

d <- d %>%filter(TP == "Present")

d <- d %>% mutate(position = ifelse(order %in% 2:3, "early",
                                    ifelse(order %in% 5:7, "late", NA)))
d$position <- as.factor(d$position)
#Accuracy models 

###defining the models

m.null <- bf(acc ~ 1 + (1 | lineup), family = bernoulli())
m.main.position <- bf(acc ~ position+ (1 | lineup), family = bernoulli())
m.main.parade <- bf(acc ~ parade + (1 | lineup), family = bernoulli())
#m.main.exposure <- bf(acc ~ exposure + (1 | lineup), family = bernoulli())
m.inter <- bf(acc ~ position*parade + (1 | lineup), family = bernoulli())

get_prior(m.inter, data = d) %>% tibble() %>% unique() %>% as.data.frame()

prior <- set_prior("normal(0, 1)", class = "b")
prior("student_t(3, 0, 2.5)", class = "Intercept")
prior("student_t(3, 0, 2.5)", class = "sd")

###running the models

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

fit_position <- brm(m.main.position , data = d, 
                prior = prior,
                inits = 0,
                iter = 30000, 
                sample_prior = TRUE,
                chains = 6,
                cores = 6,
                seed = 365,
                control = list(adapt_delta = 0.99, 
                               max_treedepth = 16))

fit_parade <- brm(m.main.parade , data = d, 
                    prior = prior,
                    inits = 0,
                    iter = 30000, 
                    sample_prior = TRUE,
                    chains = 6,
                    cores = 6,
                    seed = 365,
                    control = list(adapt_delta = 0.99, 
                                   max_treedepth = 16))

#fit_exposure <- brm(m.main.exposure , data = d, 
#prior = prior,
#inits = 0,
#iter = 30000, 
#sample_prior = TRUE,
#chains = 6,
#cores = 6,
#seed = 365,
#control = list(adapt_delta = 0.99, 
#               max_treedepth = 16))

fit_inter <- brm(m.inter , data = d, 
                  prior = prior,
                  inits = 0,
                  iter = 30000, 
                  sample_prior = TRUE,
                  chains = 6,
                  cores = 6,
                  seed = 365,
                  control = list(adapt_delta = 0.99, 
                                 max_treedepth = 16))

###comparing models

loo.null <- loo(fit_null)
loo.parade <- loo(fit_parade)
loo.position <- loo(fit_position)
loo.inter <- loo(fit_inter)

loo::loo_compare(loo.null, loo.parade,loo.position, loo.inter )

###

describe_posterior(
  fit_inter,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance", "bf"),
  centrality = "all")

####

####plot interaction

effects <- conditional_effects(fit_inter)
interactionME <- effects$`position:parade`
interactionME$position <- factor(interactionME$position, levels = c("early", "late"))
interactionME$parade <- factor(interactionME$parade, levels = c("nine", "six"))

interactionME %>% 
  ggplot(aes(y = estimate__, ymin=lower__, ymax=upper__, colour = parade, shape = parade, x = position)) +
  geom_point(position = position_dodge(.25), size = 4.5, fill = "white") +
  geom_errorbar(position = position_dodge(.25), width = .1) +
  labs(y = "Response Accuracy", x = "Target Position") +
  scale_y_continuous(limits = c(0,1), labels = scales::percent_format(accuracy = 1)) +
  scale_shape_manual(values = c(21,24)) +
  scale_color_colorblind() 

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
library(rethinking)

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

de1 <- d %>%filter(parade == "nine")
de1$COND <- paste(de1$TP, de1$exposure, sep = "_")
de1$COND <- as.factor(de1$COND)
summary(de1$COND)
de1$TP <- factor(de1$TP)
levels(de1$TP)
de1$TP <- factor(de1$TP)
levels(de1$TP)
de1$exposure <- factor(de1$exposure)
levels(de1$exposure)
de1$exposure <- relevel(de1$exposure, ref="60")
de1$sex <- factor(de1$sex)
levels(de1$sex)

mean(de1$age)
sd(de1$age)
table(de1$sex)

contrasts(de1$exposure) <- contr.treatment(levels(de1$exposure),
                                          base=which(levels(de1$exposure) == '60'))
#Accuracy models 

###defining the models

m.null <- bf(acc ~ 1 + (1 | lineup), family = bernoulli())
m.main.exposure <- bf(acc ~ exposure + (1 | lineup), family = bernoulli())
m.main.tp <- bf(acc ~ TP + (1 | lineup), family = bernoulli())
m.main.all <- bf(acc ~ exposure + TP + (1 | lineup), family = bernoulli())
m.inter <- bf(acc ~ exposure + TP + exposure:TP + (1 | lineup), family = bernoulli())

get_prior(m.inter, data = de1) %>% tibble() %>% unique() %>% as.data.frame()

prior <- set_prior("normal(0, 1)", class = "b")
prior("student_t(3, 0, 2.5)", class = "Intercept")
prior("student_t(3, 0, 2.5)", class = "sd")

###running the models

fit_null <- brm(m.null, data = de1, 
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

fit_main.exposure  <- brm(m.main.exposure, data = de1, 
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

fit_main.tp  <- brm(m.main.tp, data = de1, 
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


fit_main_all  <- brm(m.main.all, data = de1, 
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

fit_inter <- brm(m.inter, data = de1, 
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
  geom_hline(yintercept=0.1, linetype="dashed", color = "red", size=1)




#####experiment 2

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

get_prior(m.null, data = de2) %>% tibble() %>% unique() %>% as.data.frame()

prior <- set_prior("normal(0, 1)", class = "b")

m.null <- bf(acc ~ 1 + (1 | lineup), family = bernoulli())
m.main.exposure <- bf(acc ~ exposure + (1 | lineup), family = bernoulli())
m.main.tp <- bf(acc ~ TP + (1 | lineup), family = bernoulli())
m.main.all <- bf(acc ~ exposure + TP + (1 | lineup), family = bernoulli())
m.inter <- bf(acc ~ exposure + TP + exposure:TP + (1 | lineup), family = bernoulli())



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

conditional_effects(fit_inter, effects = NULL,
                    conditions = NULL, int_conditions = NULL, re_formula = NA,
                    robust = TRUE)


hdi(
  fit_inter,
  ci = 0.95,
  effects = c("fixed", "random", "all"),
  component = c("conditional", "zi", "zero_inflated", "all"),
  parameters = NULL,
  verbose = TRUE,
)

desc <- describe_posterior(
  fit_inter,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance", "bf"),
  centrality = "all"
)

map <- p_map(
  fit_inter,
  precision = 2^10,
  method = "kernel",
  effects = c("fixed", "random", "all"),
  component = c("conditional", "zi", "zero_inflated", "all"),
  parameters = NULL
)



effects <- conditional_effects(fit_inter)

loo.null <- loo(fit_null)
loo.main_all <- loo(fit_main_all)
loo.tp <- loo(fit_main.tp)
loo.exposure <- loo(fit_main.exposure)
loo.inter <- loo(fit_inter)

loo::loo_compare(loo.null, loo.tp , loo.exposure , loo.main_all, loo.inter)

icc(fit_inter, by_group = T)

##hypothesis testing to confirm

#hypothesis(fit_inter, "Intercept = 0")
hypothesis(fit_inter, "exposure15 = 0")
1/2.46
hypothesis(fit_inter, "exposure30 = 0")
1/2.08
hypothesis(fit_inter, "TPPresent = 0")
1/0.04
hypothesis(fit_inter, "exposure15:TPPresent = 0")
1/1.68
hypothesis(fit_inter, "exposure30:TPPresent = 0")
1/1.8



###combined data

d <- read_csv("IVIP Paper 1/Experiment 1 + 2 - Cleaned Data.csv")

d$exposure <- as.factor(d$exposure)
contrasts(d$exposure) <- contr.treatment(levels(d$exposure),
                                         base=which(levels(d$exposure) == '60'))

prior <- set_prior("normal(0, 1)", class = "b")

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


#####SDT Exp 1

library(readr)
d <- read_csv("IVIP Paper 1/exp1_sdt.csv")

# Fit model
nchains = ncores = 5
iter = 30000


# Cell means
# Model

priors <- c(prior(normal(0, 3), class = "b", nlpar = "dprime", lb = 0),
            prior(normal(0, 3), class = "b", nlpar = "c"),
            prior(student_t(10, 0, 1), class = "sd", nlpar = "dprime"),
            prior(student_t(10, 0, 1), class = "sd", nlpar = "c"),
            prior(lkj(4), class = "cor")) ###try take out

m <- bf(conf_is_target ~ Phi(dprime*is_target - c * 8/9), nl = TRUE,
        lf(dprime ~ 0 + as.factor(exposure) + (1 |s| lineup), cmc = TRUE),
        lf(c ~ 0 + as.factor(exposure) +(1 |s| lineup), cmc = TRUE),
        family = bernoulli(link="identity"))



# Model fit

fit_a <- brm(formula = m, data = d,
             cores = ncores, chains = nchains,
             iter = iter, warmup = iter/2,
             prior = priors, sample_prior = TRUE,
             control = list(adapt_delta = .99), seed = 123)

###confidence SDT [USING FA DATA]

##

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}


##


d <- read_csv("IVIP Paper 1/exp1.csv") %>% 
  mutate(condition = factor(paste(parade, fawarning, sep = "_")),
         conf_is_target = ifelse(!say_target, 10 - conf, conf),
         conf_is_target = as.ordered(conf_is_target)) %>%
  filter(normal_hearing == "Yes", !(is_target & filler_id)) %>%  
  dplyr::select(subj, condition, is_target, conf_is_target)

d$is_target <- as.integer(d$is_target == "TRUE")


m <- bf(conf_is_target ~ is_target * condition, 
        family =  cumulative(link = "probit")
) 

m <- bf(conf_is_target ~ is_target, 
        condition ~ 0 + is_target, 
        nl = TRUE,
        family =  cumulative(link = "probit")
        ) 



m <- bf(conf_is_target ~ is_target * condition, family =  cumulative(link = "probit")) 

get_prior(data = d, m)


fit_b <- brm(formula = m, data = d,
             cores = ncores, chains = nchains,
             iter = iter, warmup = iter/2,
             sample_prior = TRUE,
             control = list(adapt_delta = .99), seed = 123)


p1 <- ggplot(data.frame(x = seq(-5, 6, by = .01)), aes(x = x)) +
  coord_cartesian(ylim = c(0, 0.5), xlim = c(-3.5, 4), expand = 0) +
  labs(x = "Signal strength", y = "Probability")
# Add noise & signal distributions, and criterion
crits <- tibble(
  crit = round(fixef(fit_b)[1:10, 1], 2),
  zcrit = round(pnorm(crit), 2)
)
p1 + stat_function(
  fun = dnorm, geom = "line", lty = 2,
  args = list(mean = 0, sd = 1)
) +
  stat_function(
    fun = dnorm, geom = "line",
    args = list(fixef(fit_b)[11, 1], sd = 1)
  ) +
  geom_vline(xintercept = crits$crit, lty = 3) +
  geom_label(
    data = crits, aes(x = crit, y = .02 + seq(0, .1, length = 10), label = zcrit),
    size = 3, hjust = 1
  )

m <- bf(conf_is_target ~ is_target, disc ~ 0 + is_target, family =  cumulative(link = "probit")) 

get_prior(data = d, m)


fit_b <- brm(formula = m, data = d,
             cores = ncores, chains = nchains,
             iter = iter, warmup = iter/2,
             sample_prior = TRUE,
             control = list(adapt_delta = .99), seed = 123)



p2 <- as.data.frame(fit_b, pars = "is_target") %>%
  ggplot(aes(b_is_target, exp(-b_disc_is_target))) +
  scale_fill_viridis_c() +
  stat_density_2d(aes(fill = ..density..),
                  geom = "raster", contour = F, show.legend = FALSE, n = 301
  ) +
  geom_point(aes(x = 1.52, y = exp(.35)), col = "red", size = 2) +
  scale_x_continuous(breaks = pretty_breaks(5)) +
  scale_y_continuous(breaks = pretty_breaks(5)) +
  coord_cartesian(expand = 0) +
  labs(x = "d'", y = "SD(signal) [= exp(-disc_is_target)]") +
  theme(aspect.ratio = 1)
p2 + labs(
  title = "UVSDT model's posterior distribution",
  caption = "Model fitted to one subject's data"
)


p3 <- ggplot(data.frame(x = seq(-5, 6, by = .01)), aes(x = x)) +
  coord_cartesian(ylim = c(0, 0.5), xlim = c(-3.5, 4), expand = 0) +
  labs(x = "Signal strength", y = "Probability")
# Add noise & signal distributions, and criterion
crits <- tibble(
  crit = round(fixef(fit_b)[1:10, 1], 2),
  zcrit = round(pnorm(crit), 2)
)
p3 + stat_function(
  fun = dnorm, geom = "line", lty = 2,
  args = list(mean = 0, sd = 1)
) +
  stat_function(
    fun = dnorm, geom = "line",
    args = list(fixef(fit_b)[11, 1], sd = exp(-fixef(fit2)[12, 1]))
  ) +
  geom_vline(xintercept = crits$crit, lty = 3) +
  geom_label(
    data = crits, aes(x = crit, y = .02 + seq(0, .1, length = 10), label = zcrit),
    size = 3, hjust = 1
  )


m <- bf(conf_is_target ~ is_target, 
        disc ~ 0 + is_target * condition, 
        family =  cumulative(link = "probit")) 


fit_b <- brm(formula = m, data = d,
             cores = ncores, chains = nchains,
             iter = iter, warmup = iter/2,
             sample_prior = TRUE,
             control = list(adapt_delta = .99), seed = 123)

m <- bf(conf_is_target ~ is_target, 
        disc ~ 0 + is_target, 
        family =  cumulative(link = "probit")) 


fit_b <- brm(formula = m, data = d,
             cores = ncores, chains = nchains,
             iter = iter, warmup = iter/2,
             sample_prior = TRUE,
             control = list(adapt_delta = .99), seed = 123)



######

d <- d %>% mutate(conf_cat = ifelse(conf_is_target %in% 0:0, "no",
                             ifelse(conf_is_target %in% 1:3, "low",
                             ifelse(conf_is_target %in% 4:7, "med",
                                    ifelse(conf_is_target %in% 8:10, "high", NA)))),
conf_cat = factor(conf_cat, levels = c("no", "low", "med", "high"), ordered = T))

d1 <- d %>%filter(condition == "serial_no warning")
d2 <- d %>%filter(condition == "serial_warning")
d3 <- d %>%filter(condition == "sequential_no warning")
d4 <- d %>%filter(condition == "sequential_warning")

m <- bf(conf_cat ~ is_target, family =  cumulative(link = "probit")) 

fit_1 <- brm(formula = m, data = d1,
             cores = ncores, chains = nchains,
             iter = iter, warmup = iter/2,
             sample_prior = TRUE,
             control = list(adapt_delta = .99), seed = 123)



#

m <- bf(conf_cat ~ is_target, family =  cumulative(link = "probit")) 

fit_2 <- brm(formula = m, data = d2,
             cores = ncores, chains = nchains,
             iter = iter, warmup = iter/2,
             sample_prior = TRUE,
             control = list(adapt_delta = .99), seed = 123)



#

m <- bf(conf_cat ~ is_target, family =  cumulative(link = "probit")) 

fit_3 <- brm(formula = m, data = d3,
             cores = ncores, chains = nchains,
             iter = iter, warmup = iter/2,
             sample_prior = TRUE,
             control = list(adapt_delta = .99), seed = 123)



m <- bf(conf_cat ~ is_target, family =  cumulative(link = "probit")) 

fit_4 <- brm(formula = m, data = d4,
             cores = ncores, chains = nchains,
             iter = iter, warmup = iter/2,
             sample_prior = TRUE,
             control = list(adapt_delta = .99), seed = 123)


marginal_effects(fit_1, "is_target", categorical = T)
####

p1 <- ggplot(data.frame(x = seq(-5, 6, by = .01)), aes(x = x)) +
  coord_cartesian(ylim = c(0, 0.5), xlim = c(-3.5, 4), expand = 0) +
  labs(x = "Signal strength", y = "Probability")
# Add noise & signal distributions, and criterion
crits <- tibble(
  crit = round(fixef(fit_1)[1:3, 1], 2),
  zcrit = round(pnorm(crit), 2)
)
p1 + stat_function(
  fun = dnorm, geom = "line", lty = 2,
  args = list(mean = 0, sd = 1)
) +
  stat_function(
    fun = dnorm, geom = "line",
    args = list(fixef(fit_1)[4, 1], sd = 1)
  ) +
  geom_vline(xintercept = crits$crit, lty = 3) +
  geom_label(
    data = crits, aes(x = crit, y = .02 + seq(0, .1, length = 1), label = zcrit),
    size = 3, hjust = 1
  )

p2 <- ggplot(data.frame(x = seq(-5, 6, by = .01)), aes(x = x)) +
  coord_cartesian(ylim = c(0, 0.5), xlim = c(-3.5, 4), expand = 0) +
  labs(x = "Signal strength", y = "Probability")
# Add noise & signal distributions, and criterion
crits <- tibble(
  crit = round(fixef(fit_2)[1:3, 1], 2),
  zcrit = round(pnorm(crit), 2)
)
p2 + stat_function(
  fun = dnorm, geom = "line", lty = 2,
  args = list(mean = 0, sd = 1)
) +
  stat_function(
    fun = dnorm, geom = "line",
    args = list(fixef(fit_2)[4, 1], sd = 1)
  ) +
  geom_vline(xintercept = crits$crit, lty = 3) +
  geom_label(
    data = crits, aes(x = crit, y = .02 + seq(0, .1, length = 3), label = zcrit),
    size = 3, hjust = 1
  )

p3 <- ggplot(data.frame(x = seq(-5, 6, by = .01)), aes(x = x)) +
  coord_cartesian(ylim = c(0, 0.5), xlim = c(-3.5, 4), expand = 0) +
  labs(x = "Signal strength", y = "Probability")
# Add noise & signal distributions, and criterion
crits <- tibble(
  crit = round(fixef(fit_3)[1:3, 1], 2),
  zcrit = round(pnorm(crit), 2)
)
p3 + stat_function(
  fun = dnorm, geom = "line", lty = 2,
  args = list(mean = 0, sd = 1)
) +
  stat_function(
    fun = dnorm, geom = "line",
    args = list(fixef(fit_3)[4, 1], sd = 1)
  ) +
  geom_vline(xintercept = crits$crit, lty = 3) +
  geom_label(
    data = crits, aes(x = crit, y = .02 + seq(0, .1, length = 3), label = zcrit),
    size = 3, hjust = 1
  )

p4 <- ggplot(data.frame(x = seq(-5, 6, by = .01)), aes(x = x)) +
  coord_cartesian(ylim = c(0, 0.5), xlim = c(-3.5, 4), expand = 0) +
  labs(x = "Signal strength", y = "Probability")
# Add noise & signal distributions, and criterion
crits <- tibble(
  crit = round(fixef(fit_4)[1:3, 1], 2),
  zcrit = round(pnorm(crit), 2)
)
p4 + stat_function(
  fun = dnorm, geom = "line", lty = 2,
  args = list(mean = 0, sd = 1)
) +
  stat_function(
    fun = dnorm, geom = "line",
    args = list(fixef(fit_4)[4, 1], sd = 1)
  ) +
  geom_vline(xintercept = crits$crit, lty = 3) +
  geom_label(
    data = crits, aes(x = crit, y = .02 + seq(0, .1, length = 3), label = zcrit),
    size = 3, hjust = 1
  )

###
crits <- tibble(
  crit = round(fixef(fit_1)[1:3, 1], 2),
  zcrit = round(pnorm(crit), 2))

p1 <- ggplot(data.frame(x = seq(-5, 6, by = .01)), aes(x = x)) +
  coord_cartesian(ylim = c(0, 0.5), xlim = c(-3.5, 4), expand = 0) +
  labs(x = "Signal strength", y = "Probability") + stat_function(
  fun = dnorm, geom = "line", lty = 2,
  args = list(mean = 0, sd = 1)
) +
  stat_function(
    fun = dnorm, geom = "line",
    args = list(fixef(fit_1)[4, 1], sd = 1)
  ) +
  geom_vline(xintercept = crits$crit, lty = 3) +
  geom_label(
    data = crits, aes(x = crit, y = .02 + seq(0, .1, length = 1), label = zcrit),
    size = 3, hjust = 1
  )


# Add noise & signal distributions, and criterion
crits <- tibble(
  crit = round(fixef(fit_2)[1:3, 1], 2),
  zcrit = round(pnorm(crit), 2))


p2 <- ggplot(data.frame(x = seq(-5, 6, by = .01)), aes(x = x)) +
  coord_cartesian(ylim = c(0, 0.5), xlim = c(-3.5, 4), expand = 0) +
  labs(x = "Signal strength", y = "Probability") + stat_function(
  fun = dnorm, geom = "line", lty = 2,
  args = list(mean = 0, sd = 1)
) +
  stat_function(
    fun = dnorm, geom = "line",
    args = list(fixef(fit_2)[4, 1], sd = 1)
  ) +
  geom_vline(xintercept = crits$crit, lty = 3) +
  geom_label(
    data = crits, aes(x = crit, y = .02 + seq(0, .1, length = 3), label = zcrit),
    size = 3, hjust = 1
  )

# Add noise & signal distributions, and criterion
crits <- tibble(
  crit = round(fixef(fit_3)[1:3, 1], 2),
  zcrit = round(pnorm(crit), 2)
)

p3 <- ggplot(data.frame(x = seq(-5, 6, by = .01)), aes(x = x)) +
  coord_cartesian(ylim = c(0, 0.5), xlim = c(-3.5, 4), expand = 0) +
  labs(x = "Signal strength", y = "Probability") + stat_function(
  fun = dnorm, geom = "line", lty = 2,
  args = list(mean = 0, sd = 1)
) +
  stat_function(
    fun = dnorm, geom = "line",
    args = list(fixef(fit_3)[4, 1], sd = 1)
  ) +
  geom_vline(xintercept = crits$crit, lty = 3) +
  geom_label(
    data = crits, aes(x = crit, y = .02 + seq(0, .1, length = 3), label = zcrit),
    size = 3, hjust = 1
  )

# Add noise & signal distributions, and criterion
crits <- tibble(
  crit = round(fixef(fit_4)[1:3, 1], 2),
  zcrit = round(pnorm(crit), 2)
)

p4 <- ggplot(data.frame(x = seq(-5, 6, by = .01)), aes(x = x)) +
  coord_cartesian(ylim = c(0, 0.5), xlim = c(-3.5, 4), expand = 0) +
  labs(x = "Signal strength", y = "Probability") + stat_function(
  fun = dnorm, geom = "line", lty = 2,
  args = list(mean = 0, sd = 1)
) +
  stat_function(
    fun = dnorm, geom = "line",
    args = list(fixef(fit_4)[4, 1], sd = 1)
  ) +
  geom_vline(xintercept = crits$crit, lty = 3) +
  geom_label(
    data = crits, aes(x = crit, y = .02 + seq(0, .1, length = 3), label = zcrit),
    size = 3, hjust = 1
  )



###

p1 <- p1

p2 <- p2
library(gridExtra)
grid.arrange(p1, p2, p3, p4,
             ncol = 2,
             nrow = 2)


####SDT e2 

d <- read_csv("IVIP Paper 1/exp2_sdt.csv")
d$exposure <- as.factor(d$exposure)


d$filter <- ifelse(d$target == "present" & d$say_target == FALSE & d$is_target == TRUE & 
                     d$suspect_id == FALSE & d$filler_id == TRUE & d$no_id == FALSE, 
                   1,0)
d %<>% filter(filter == 0) %>% dplyr::select(-filter)

count(d, is_target, say_target, suspect_id, filler_id, no_id)

options (mc.cores=parallel::detectCores ())

# Fit model
nchains = ncores = 5
iter = 30000


# Cell means
# Model


priors <- c(prior(normal(0, 3), class = "b", nlpar = "dprime", lb = 0),
            prior(normal(0, 3), class = "b", nlpar = "c"),
            prior(student_t(10, 0, 1), class = "sd", nlpar = "dprime"),
            prior(student_t(10, 0, 1), class = "sd", nlpar = "c"),
            prior(lkj(4), class = "cor")) ###try take out

m <- bf(say_target ~ Phi(dprime*is_target - c * 5/6), nl = TRUE,
        lf(dprime ~ 0 + exposure + (1 |s| lineup), cmc = TRUE),
        lf(c ~ 0 + exposure +(1 |s| lineup), cmc = TRUE),
        family = bernoulli(link="identity"))

# Model fit

fit_a <- brm(formula = m, data = d,
             cores = ncores, chains = nchains,
             iter = iter, warmup = iter/2,
             prior = priors, sample_prior = TRUE,
             control = list(adapt_delta = .99), seed = 123); fit_a



####

describe_posterior(
  fit_a,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance", "bf"),
  centrality = "MAP"
)

hdi(
  fit_a,
  ci = 0.95,
  effects = c("fixed", "random", "all"),
  component = c("conditional", "zi", "zero_inflated", "all"),
  parameters = NULL,
  verbose = TRUE,
)


##hypotheses
hypothesis(fit_a, "dprime_exposure15 = 0", robust = T)

hypothesis(fit_a, "dprime_exposure30 = 0", robust = T)

hypothesis(fit_a, "dprime_exposure60 = 0", robust = T)

hypothesis(fit_a, "c_exposure15 = 0", robust = T)

hypothesis(fit_a, "c_exposure30 = 0", robust = T)

hypothesis(fit_a, "c_exposure60 = 0", robust = T)

hypothesis(fit_a, "dprime_exposure15 = dprime_exposure30", robust = T)
hypothesis(fit_a, "dprime_exposure15 = dprime_exposure60", robust = T)
hypothesis(fit_a, "dprime_exposure30 = dprime_exposure60", robust = T)

hypothesis(fit_a, "c_exposure15 = c_exposure30", robust = T)
hypothesis(fit_a, "c_exposure15 = c_exposure60", robust = T)
hypothesis(fit_a, "c_exposure30 = c_exposure60", robust = T)

####


samples <- posterior_samples(fit_a, "b_") %>% 
  dplyr::select(-contains("prior")) %>%
  as_tibble() 


mcmc_areas(samples, point_est = "mean", prob = .95)

plot_data <- samples  %>%
  pivot_longer(everything()) %>%
  mutate(name = gsub("b_|condition", "", name)) %>%
  separate(name, into = c("param", "sample_length"))  

plot_data$param <- factor(plot_data$param, labels = c(bquote("Response criterion"~italic("c")), bquote("Signal sensitivity"~italic("d'"))))

plot_data$sample_length <- gsub("exposure15", "15s duration", plot_data$sample_length)
plot_data$sample_length <- gsub("exposure30", "30s duration", plot_data$sample_length)
plot_data$sample_length <- gsub("exposure60", "60s duration", plot_data$sample_length)

plot_data %>% mutate(sample_length = str_to_sentence(sample_length),
                     condition = paste0(sample_length, " ")) %>%
  ggplot(aes(x = value, y = sample_length, colour = condition, fill = condition, linetype = condition)) +
  ggridges::geom_density_ridges(alpha = .9, size = .1) + facet_wrap(~param, scales = "free_x", labeller = label_parsed) +
  labs(x = "Signal strength", y = "Posterior density", fill = "", colour = "", linetype = "") + 
  scale_color_grey() + scale_fill_grey() + theme_apa(base_size = 13) + theme(legend.position = "bottom")



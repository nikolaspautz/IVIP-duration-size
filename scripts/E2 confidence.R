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

d <- read_csv("data/Experiment 1 + 2 - Cleaned Data.csv")
de2 <- d %>%filter(parade == "six")

de2 <- de2 %>%
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
         is_corr = ifelse(is_corr == TRUE, "correct", "incorrect"),
         condition = factor(paste(is_corr, exposure, sep = "_")),
         conf = ordered(conf)) 


# Contrasts
summary(de2$condition)
(nrow <- length(levels(de2$condition)))

cmat <- fractions(matrix(c(
  1/2, 0, 0,-1/2, 0, 0, 
  0, 1/2, 0, 0,-1/2, 0,  
  0, 0, 1/2, 0, 0,-1/2),
  nrow = nrow, byrow = F))

rown <- levels(de2$condition)
rownames(cmat) <- rown

ivs <- unique(gsub("correct_|incorrect_|", "", levels(de2$condition)))


coln <- c(paste0("corr_",ivs))

colnames(cmat)<- coln
cmat; colSums(cmat)

d_cmat <- cmat %>%
  as.data.frame() %>%
  rownames_to_column("condition") %>%
  as_tibble()

# Assign contrasts
de2 <- left_join(de2, d_cmat)  

rown <- levels(de2$condition)
rownames(cmat) <- rown

# Model fit params
nchains <- ncores <- 3
niter <- 30000
warmup <- niter/2

# Formula
m <- bf(conf ~ corr_15 + corr_30 + corr_60 + (1|lineup),   
        family = brmsfamily("cumulative")) 

# Get priors
get_prior(m, data = de2) %>% tibble() %>% unique() %>% as.data.frame()

# Priors
prior <- set_prior("normal(0, 2)", class = "b")
prior("student_t(3, 0, 2.5)", class = "Intercept")
prior("student_t(3, 0, 2.5)", class = "sd")

# Sampling
fit <- brm(m, data = de2, 
           prior = prior,
           inits = 0,
           warmup = warmup,
           iter = niter, 
           sample_prior = TRUE,
           refresh = niter/4,
           chains = nchains,
           cores = ncores,
           seed = 365,
           control = list(adapt_delta = 0.99, 
                          max_treedepth = 16))

####

describe_posterior(
  fit,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance", "bf"),
  centrality = "all")

hdi(
  fit,
  ci = 0.95,
  effects = c("fixed", "random", "all"),
  component = c("conditional", "zi", "zero_inflated", "all"),
  parameters = NULL,
  verbose = TRUE)

####cell means####

inv_logit <- function(x) 1 / (1 + exp(-x))

# Model fit params
nchains <- ncores <- 3
niter <- 30000
warmup <- niter/2

# Load data

d <- read_csv("data/Experiment 1 + 2 - Cleaned Data.csv")
de2 <- d %>%filter(parade == "six")

de2 <- de2 %>%
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
         is_corr = ifelse(is_corr == TRUE, "correct", "incorrect"),
         condition = factor(paste(exposure, sep = "_")),
         conf = ordered(conf)) 

#### Contrasts ####
summary(de2$condition)
(nrow <- length(levels(de2$condition)))

cmat <- fractions(matrix(c(
  1, 0, 0,
  0, 1, 0, 
  0, 0, 1), 
  nrow = nrow, byrow = F))

rown <- levels(de2$condition)
rownames(cmat) <- rown
coln <- levels(de2$condition)
colnames(cmat)<- coln
cmat; colSums(cmat)

# transpose and inverse matrix to get contrast between the expected levels
inv.cmat<-fractions(t(ginv(cmat)))
rownames(inv.cmat) <- rown
colnames(inv.cmat) <- coln
inv.cmat

# Assign contrasts
contrasts(de2$condition) <- inv.cmat

# Formula
m <- bf(conf ~ condition + (1|lineup),   
        family = brmsfamily("cumulative")) 

# Get priors
get_prior(m, data = de2) %>% tibble() %>% unique() %>% as.data.frame()

# Priors
prior <- set_prior("normal(0, 2)", class = "b")
prior("student_t(3, 0, 2.5)", class = "Intercept")
prior("student_t(3, 0, 2.5)", class = "sd")

# Sampling
fit2 <- brm(m, data = de2, 
            prior = prior,
            inits = 0,
            warmup = warmup,
            iter = niter, 
            sample_prior = TRUE,
            refresh = niter/4,
            chains = nchains,
            cores = ncores,
            seed = 365,
            control = list(adapt_delta = 0.99, 
                           max_treedepth = 16))


newdata <- make_conditions(de2, "condition")
me <- conditional_effects(fit2, conditions = newdata)$condition
me %<>% as_tibble() %>% dplyr::select(effect1__:upper__) %>% 
  unique() %>%
  rename(est = estimate__,
         se = se__,
         lo = lower__,
         up = upper__) %>%
  separate(effect1__, into = c("duration"), sep = "_")


###figure

me %>%
  mutate(duration = recode(duration, '15' = "15s duration", 
                           '30' = "30s duration",
                           '60' = "60s duration")) %>%
  ggplot(aes(y=est, x = duration, ymin=lo, ymax=up, color = duration))+
  geom_point(position = position_dodge(.45), size = 4.5, fill = "white") +
  geom_errorbar(position = position_dodge(.45), width = .2) +
  scale_shape_manual(values = c(21,24, 17)) +
  labs(y = "Confidence rating", x = "Sample Duration") + ggthemes::scale_color_colorblind("") + theme(legend.position = "none")

conditional_effects(fit2)

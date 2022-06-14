## packages
library(bayesrules)
library(rstan)
library(rstanarm)
library(bayesplot)
library(tidyverse)
library(tidybayes)
library(broom.mixed)
##
library(loo)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# for posterior plots
source("helperfunct.R")


data <- abs.complete %>% 
  select(EmployeeID = ID,
         Months.in.Service = Service.time.months,
         Season,
         Disciplinary.failure,
         Social.drinker,
         Social.smoker,
         BMI,
         Absenteeism.hours)

data <- data %>%
  mutate(offset = log(Months.in.Service),
         Season = as.integer(Season),
         Smoker = as.integer(Social.smoker),
         Drinker = as.integer(Social.drinker),
         Disciplinary = as.integer(Disciplinary.failure),
         BMI = as.integer(BMI),
         Employee = as.integer(EmployeeID))

## For counting
data_count <- data %>%
  rename(y = Absenteeism.hours) %>%
  count(y)

X <- model.matrix(object = Absenteeism.hours ~ BMI + Season + Smoker + Drinker + Disciplinary, data = data)
y <- data$Absenteeism.hours

### Poisson Model

poisson_covs <- stan("poisson.stan",
                     data = list(s = 10,
                                 N = nrow(X), p = ncol(X),
                                 offset = log(data$Months.in.Service),
                                 X = X,
                                 y = y))

## model checks
pars = c("beta","lp__")
pairs(poisson_covs, pars = pars)

## check convergence
traceplot(poisson_covs, inc_warmup = TRUE, pars = pars)


### Negative Binomial Model

negbinomial_covs <- stan("negBinomial.stan",
                         data = list(a = 10^(-3), b = 10^(-3),
                                     s = 10,
                                     N = nrow(X), p = ncol(X),
                                     offset = log(data$Months.in.Service),
                                     X = X,
                                     y = y))


pars = c("beta","a_gamma","phi","lp__")
pairs(negbinomial_covs, pars = pars)


traceplot(negbinomial_covs, inc_warmup = TRUE, pars = pars)

### Parameter Estimates

cat("Poisson\n")
print(poisson_covs, pars = c("beta"))
cat("Negative binomial\n")
print(negbinomial_covs, pars = c("beta","a_gamma"))



### Model Comparison


loo_list <- list(loo(poisson_covs, moment_match = TRUE), loo(negbinomial_covs, moment_match = TRUE))
loo_compare(loo_list)
loo_model_weights(loo_list)




both_ppd <- bind_rows(extract_post_pred(poisson_covs) %>%
                        mutate(model = "Poisson"),
                      extract_post_pred(negbinomial_covs) %>%
                        mutate(model = "Negative Binomial"))

both_ppd %>%
  group_by(model, .chain, .iteration, .draw) %>%
  count(value) %>%
  group_by(model, value) %>%
  tidybayes::median_qi(n, .width = c(0.95, 0.80, 0.50)) %>%
  ggplot(mapping = aes(x = value, y = n)) +
  tidybayes::geom_interval( aes(ymin = .lower, ymax = .upper) ) +
  geom_point(data = data_count,
             mapping = aes(x = y, y = n)) +
  geom_line(data = data_count,
            mapping = aes(x = y, y = n)) +
  scale_color_brewer() +
  facet_wrap( ~ model) +
  coord_cartesian(xlim = c(0, 50)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank())

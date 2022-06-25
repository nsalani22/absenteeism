## packages
library(bayesrules)
library(rstan)
library(rstanarm)
library(bayesplot)
library(dplyr)
library(tidyverse)
library(tidybayes)
library(broom.mixed)
##
library(loo)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# for posterior plots
source("helperfunct.R")

# import and clean 

absent <-  readr::read_delim( "data/Absenteeism_at_work.csv",
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

# correct
names(absent ) <- gsub(" ", ".", names(absent ))
names(absent ) <- gsub("/", ".", names(absent ))

# shorter names
absent  <- absent  %>% rename(Month =  Month.of.absence,
                              Day = Day.of.the.week,
                              Season = Seasons,
                              Cost.of.Commute = Transportation.expense,
                              Distance.to.work = Distance.from.Residence.to.Work,
                              Average.Workload.day = Work.load.Average.day,
                              Hit.Rate.Workload = Hit.target,
                              Children = Son,                           
                              BMI = Body.mass.index,
                              Service.time.months = Service.time,
                              Absenteeism.hours = Absenteeism.time.in.hours
)



# Data Preparation
absent <- absent %>% 
  mutate(
    
    ID  = sub("^","ID",ID),
    
    Absence.reason = sub("^","Reason",Reason.for.absence),
    
    Month = sub("^","Month",Month),
    
    Day = case_when(Day == 1 ~ "Sunday",
                    Day == 2 ~ "Monday",
                    Day == 3 ~ "Tuesday",
                    Day == 4 ~ "Wednesday",
                    Day == 5 ~ "Thursday",
                    Day == 6 ~ "Friday",
                    Day == 7 ~ "Saturday"),
    
    Season = case_when(Season == 1 ~ "Summer",
                       Season == 2 ~ "Autumn",
                       Season == 3 ~ "Winter",
                       Season == 4 ~ "Spring"),
    
    Education = case_when(Education == 1 ~ "High.School",
                          Education == 2 ~ "Graduate",
                          Education == 3 ~ "Post.Graduate",
                          Education == 4 ~ "Masters.Doctor"),
    
    Social.drinker = case_when(Social.drinker == 1 ~  "Yes",
                               Social.drinker == 0 ~  "No"),
    
    Social.smoker= case_when(Social.smoker == 1 ~  "Yes",
                             Social.smoker == 0 ~  "No"),
    
    Disciplinary.failure = case_when(Disciplinary.failure == 1 ~  "Yes",
                                     Disciplinary.failure == 0 ~  "No")
    
  )



# Remove: Employee ID 
cols.preprocess <- c("ID",
                     "Absence.reason",  
                     "Month",
                     "Day",
                     "Season",
                     "Cost.of.Commute",
                     "Distance.to.work",
                     "Service.time.months",       
                     "Age",  
                     "Average.Workload.day",
                     "Hit.Rate.Workload",   
                     "Disciplinary.failure",
                     "Education",                       
                     "Children",                           
                     "Social.drinker",         
                     "Social.smoker",           
                     "Pet",                
                     "Weight",                      
                     "Height",   
                     "BMI",
                     "Absenteeism.hours")

abs.preprocess <- absent[cols.preprocess ]



# preparing complete data for modelling

# nominal vars --------

nominal.vars <- c("ID","Absence.reason","Season","Month","Day")  
abs.preprocess[, nominal.vars] <- lapply(abs.preprocess[, nominal.vars], factor)

# ordinal vars --------

## Education
edu.levels = c("High.School", "Graduate", "Post.Graduate", "Masters.Doctor")
abs.preprocess[, "Education"] <- lapply(abs.preprocess[,  "Education"], factor, levels = edu.levels)

# binary vars --------------
binary.vars <- c("Social.drinker", "Social.smoker","Disciplinary.failure" )
abs.preprocess[, binary.vars ] <- lapply(abs.preprocess[,  binary.vars], factor, labels=c("No","Yes") )

# Considering Outcome as a count
abs.complete <- abs.preprocess

#  prepped data
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


### Negative Binomial Model

negbinomial_covs <- stan("negBinomial.stan",
                         data = list(a = 10^(-3), b = 10^(-3),
                                     s = 10,
                                     N = nrow(X), p = ncol(X),
                                     offset = log(data$Months.in.Service),
                                     X = X,
                                     y = y))

## model checks
pars = c("beta","a_gamma","phi","lp__")
pairs(negbinomial_covs, pars = pars)


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



# import and clean 

absent <-  readr::read_delim( "~/absenteeism/data/Absenteeism_at_work.csv",
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

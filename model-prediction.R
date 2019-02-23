## Reading NCLEX data
setwd('D:/UMASSD/Nursing Department/data-analysis/original-data/');
data <- read.csv(file="SAT Score - NCLEX Review with Admissions and Demographic data.csv", header = T)

# Removing unncessory attributes
data$X <- NULL
data$Student.ID <- NULL
data$FirstName <- NULL
data$LastName <- NULL
data$Birthdate <- NULL
data$HESI_ID <- NULL
data$HESI.Exam <- NULL
data$Date <- NULL
data$X.1 <- NULL 
data$X.2<-NULL 
data$X.3<-NULL 
data$X.4<-NULL 
data$X.5<-NULL 
data$X.6<-NULL 
data$X.7<-NULL
data$Admitted.Major <- NULL

#model <- glm(NCLEX.Outcome ~ HESI.Score + Gender + Cum.GPA + High.School.GPA + SAT.Math +
#                SAT.Verbal + Total.SAT + Accuplacer.Algebra + Accuplacer.Calculus,
#                data, family = binomial)

model <- glm(NCLEX.Outcome ~ HESI.Score + Gender + Cum.GPA + High.School.GPA +
               Total.SAT + Accuplacer.Algebra + Accuplacer.Calculus,
               data, family = binomial)


predict(model, newdata = new.df, type="response")

output <- round(predict(model, new.df, type="response"))
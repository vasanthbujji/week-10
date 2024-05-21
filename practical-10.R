salary_data<-read.csv("Salary.csv", na="")
str(salary_data)
View(salary_data)

# Rename the last column
names(salary_data)[names(salary_data) == "Number.of.Subjects"] <- "num_of_subjects"

# Verify the change by displaying the names of the columns
names(salary_data)

salary_data<-salary_data[c(2,3,4,5)]
View(salary_data)
correlations <- cor(salary_data[, c("Salary", "Years", "Rating", "num_of_subjects")])
correlations

salary_data<-salary_data[c(2,3,4,5)]
attach(salary_data)
model <- lm(Salary ~ Years + Rating + num_of_subjects)
model

library(psych)
windows(20,10)
pairs.panels(salary_data,
             smooth = FALSE,     
             scale = FALSE,     
             density = TRUE,    
             ellipses = FALSE,  
             method = "spearman",
             pch = 21,          
             lm = FALSE,        
             cor = TRUE,       
             jiggle = FALSE,    
             factor = 2,        
             hist.col = 4,       
             stars = TRUE,      
             ci = TRUE)

windows(20,12)
par(mfrow= c(2,3))

scatter.smooth(x = salary_data$Years,
               y = salary_data$Salary,
               xlab = "Years",
               ylab = "Salary %", main = "Correlation of Salary~Years")
scatter.smooth(x = salary_data$Rating,
               y = salary_data$Salary,
               xlab = "Rating",
               ylab = "Salary %", main = "Correlation of Salary~Rating")
scatter.smooth(x = salary_data$num_of_subjects,
               y = salary_data$Salary,
               xlab = "No.of.Subjects",
               ylab = "Salary %", main = "Correlation of Salary~Num.of.Subjects")

windows(20,10)
par(mfrow = c(2, 2)) 
attach(salary_data)

boxplot(Salary,
        main = "Salary")

boxplot(Years,
        main = "Years") 

boxplot(Rating,
        main = "Rating")

boxplot(num_of_subjects,
        main = "Num_Subjects") 

shapiro.test(Salary)
shapiro.test(Years)
shapiro.test(Rating)
shapiro.test(num_of_subjects)


shapiro.test(residuals(model))
t.test(residuals(model),mu=0) 
AIC(model)
AIC(model)
BIC(model)
BIC(model)

install.packages("faraway")
library(faraway)
v2<-vif(model)
v2

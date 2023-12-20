library(rpart)
library(rpart.plot)
library(ggplot2)
library(dplyr)

#Read the dataset file in data frame 
promotion <- read.csv("D:/Files/train.csv",na.strings = c("","NA"))
#The na.strings argument specifies the values that will be considered missing values--> the empty string and NA will be considered missing values.

print(promotion)
#Shows All Data
################################################################################

#Explore data

promotion$is_promoted <- as.logical(promotion$is_promoted) #convert data type of column
View(promotion) #view the table or display it from environment
head(promotion) #View the first 6 Rows
tail(promotion) #View the last 6 Rows
summary(promotion)
class(promotion)
str(promotion) #Display the Structure
dim(promotion) #Rows Columns
names(promotion) #Get columns names
unique(promotion$department)
table(promotion$department)

################################################################################


#Clean Data

#1) Duplications

sum(duplicated(promotion)) #how many duplicated rows in data frame

duplicated(promotion)

promotion[duplicated(promotion),] #which rows is duplictaed

promotion<-distinct(promotion) #if we want to print the data frame with no duplications
#--------------------------------
#2)Missing Valuse (Na)

sum(is.na(promotion)) # how many Na values in data 

is.na(promotion)
cleaned_data <- na.omit(promotion) #Handle the missing value with deleting their rows

sum(is.na(cleaned_data)) # how many Na values in data 
View(cleaned_data)
View(cleaned_data_withoutOutliers)

#  remove the outliers from age
cleaned_data_withoutOutliers_age<-cleaned_data
for (i in 1:nrow(cleaned_data_withoutOutliers_age)) {
  tmp <- cleaned_data_withoutOutliers_age$age[i]
  if (tmp > 47) {
    cleaned_data_withoutOutliers_age$age[i] <- NA
  }
}
cleaned_data_withoutOutliers_age <- na.omit(cleaned_data_withoutOutliers_age)
#---------------------------------------------------------------------
#---------------------------------------------------------------------
#  remove the outliers from length_of_service
cleaned_data_withoutOutliers_length_of_service<-cleaned_data_withoutOutliers_age
for(i in 1:nrow(cleaned_data_withoutOutliers_length_of_service)){
  tmp <- cleaned_data_withoutOutliers_length_of_service$length_of_service[i]
  if(tmp > 13){
    cleaned_data_withoutOutliers_length_of_service$length_of_service[i] <- NA
  }
}
cleaned_data_withoutOutliers_length_of_service <- na.omit(cleaned_data_withoutOutliers_length_of_service)
#---------------------------------------------------------------------
#---------------------------------------------------------------------
#  remove the outliers from previous_year_rating
cleaned_data_withoutOutliers<-cleaned_data_withoutOutliers_length_of_service
for(i in 1:nrow(cleaned_data_withoutOutliers)){
  tmp <- cleaned_data_withoutOutliers$previous_year_rating[i]
  if(tmp == 1){
    cleaned_data_withoutOutliers$previous_year_rating[i] <- NA
  }
}
cleaned_data_withoutOutliers <- na.omit(cleaned_data_withoutOutliers)


################################################################################

#Visualization

#boxplot for age

boxplot(x=cleaned_data_withoutOutliers$age,main="Age Range",xlab="Age")
boxplot(x=cleaned_data_withoutOutliers$age)$out #print outliers
boxplot_stats<-(boxplot(x=cleaned_data_withoutOutliers$age))$stats
min_value <- boxplot_stats[1]  # Minimum
max_value <- boxplot_stats[5]  # Maximum
median_value <- boxplot_stats[3]  # Median
q1 <- boxplot_stats[2]  
q3 <- boxplot_stats[4]  
min_value
max_value
median_value
q1
q3

#-----------------------------------------------------------------------------------------

#box plot for length of service

boxplot(x=cleaned_data_withoutOutliers$length_of_service,main="length of service",xlab="length")
boxplot(x=cleaned_data_withoutOutliers$length_of_service)$out #print outliers
boxplot_stats<-(boxplot(x=cleaned_data_withoutOutliers$length_of_service))$stats
min_value <- boxplot_stats[1]  # Minimum
max_value <- boxplot_stats[5]  # Maximum
median_value <- boxplot_stats[3]  # Median
q1 <- boxplot_stats[2]  
q3 <- boxplot_stats[4]  
min_value
max_value
median_value
q1
q3

#------------------------------------------
#box plot for avg training score

boxplot(x=cleaned_data_withoutOutliers$avg_training_score,main="Training score",xlab="Score")
boxplot(x=cleaned_data_withoutOutliers$avg_training_score)$out #print outliers
boxplot_stats<-(boxplot(x=cleaned_data_withoutOutliers$avg_training_score))$stats
min_value <- boxplot_stats[1]  # Minimum
max_value <- boxplot_stats[5]  # Maximum
median_value <- boxplot_stats[3]  # Median
q1 <- boxplot_stats[2]  
q3 <- boxplot_stats[4]  
min_value
max_value
median_value
q1
q3

#-------------------------------------------------

#box plot for previous_year_rating
boxplot(x=cleaned_data_withoutOutliers$previous_year_rating,main="Training score",xlab="Score")
boxplot(x=cleaned_data_withoutOutliers$previous_year_rating)$out #print outliers
boxplot_stats<-(boxplot(x=cleaned_data_withoutOutliers$previous_year_rating))$stats
min_value <- boxplot_stats[1]  # Minimum
max_value <- boxplot_stats[5]  # Maximum
median_value <- boxplot_stats[3]  # Median
q1 <- boxplot_stats[2]  
q3 <- boxplot_stats[4]  
min_value
max_value
median_value
q1
q3
#hist Age

table(cleaned_data_withoutOutliers$age)
hist(cleaned_data_withoutOutliers$age,main = "Age",col="black",border = "white",xlab = "age")
#--------------------------------
#hist length_of_service

table(cleaned_data_withoutOutliers$length_of_service)
hist(cleaned_data_withoutOutliers$length_of_service,main = "Length of service",col="green",border = "blue",xlab = "Length of service")
#--------------------------------
#hist no_of_trainings

table(cleaned_data_withoutOutliers$no_of_trainings)
hist(cleaned_data_withoutOutliers$no_of_trainings,main = "Number Of Training",col="yellow",border = "green",xlab = "training",xlim = c(0,10),ylim = c(0,50000))
#--------------------------------
#hist avg_training_score

table(cleaned_data_withoutOutliers$avg_training_score)
hist(cleaned_data_withoutOutliers$avg_training_score,main = "avg_training_score",col="dodgerblue3",border = "darkslateblue",xlab = "Avg Score",ylim = c(0,10000))
#--------------------------------
#barplot previous_year_rating

table(cleaned_data_withoutOutliers$previous_year_rating)
barplot(table(cleaned_data_withoutOutliers$previous_year_rating),mian = "previous_year_rating" ,xlab = "Rating" , ylabel = "Frequency" ,col = "firebrick4" )
#--------------------------------
#barplot department

table(cleaned_data_withoutOutliers$department)
barplot(table(cleaned_data_withoutOutliers$department),xlab = "Departments" , ylab = "Frequency",col = "lightblue4")
#--------------------------------
#barplot recruitment_channel

table(cleaned_data_withoutOutliers$recruitment_channel)
barplot(table(cleaned_data_withoutOutliers$recruitment_channel) , xlab = "recruitment_channel" , ylab = "Frequency" , col = "darkseagreen1",ylim = c(0,30000))
#--------------------------------

#pie chart education

x<-table(cleaned_data_withoutOutliers$education)
percentage=paste0(round(100*(x/sum(x))),"%")
pie(table(cleaned_data_withoutOutliers$education),main="education",labels=percentage,col=c("azure3","gold","burlywood4"))
legend("bottomright", legend = c("Bachelor's", "Master's & above","below secondary"), fill = c("azure3","gold","burlywood4"))
#--------------------------------
#pie chart gender

table(cleaned_data_withoutOutliers$gender)
x<-table(cleaned_data_withoutOutliers$gender)
percentage=paste0(round(100*(x/sum(x))),"%")
pie(table(cleaned_data_withoutOutliers$gender),main="Gender",labels=percentage,col=c("pink","lightblue"))
legend("bottomright", legend = c("Female", "Male"), fill = c("pink","lightblue"))
#--------------------------------
#pie chart is promoted

table(cleaned_data_withoutOutliers$is_promoted)
x<-table(cleaned_data_withoutOutliers$is_promoted)
percentage=paste0(round(100*(x/sum(x))),"%")
pie(table(cleaned_data_withoutOutliers$is_promoted),main="Promotion",labels=percentage,col=c("yellow","white"))
legend("bottomright", legend = c("Not promoted", "Promoted"), fill = c("yellow","white"))
#--------------------------------
#pie chart award

table(cleaned_data_withoutOutliers$awards_won.)
x<-table(cleaned_data_withoutOutliers$awards_won.)
percentage=paste0(round(100*(x/sum(x))),"%")
pie(table(cleaned_data_withoutOutliers$awards_won.),main="Award",labels=percentage,col=c("red","green"))
legend("bottomright", legend = c("Not Awarded", "Awarded"), fill = c("red","green"))
#--------------------------------
#pie chart recruitment_channel

table(cleaned_data_withoutOutliers$recruitment_channel)
x<-table(cleaned_data_withoutOutliers$recruitment_channel)
percentage=paste0(round(100*(x/sum(x))),"%")
pie(table(cleaned_data_withoutOutliers$recruitment_channel),main="recruitment_channel",labels=percentage,col=c("cyan","yellow","green"))
legend("bottomright", legend = c("Other", "Referred","Sourcing"), fill = c("cyan","yellow","green"))
#--------------------------------

#5 y-axis must be numeric
mean_age_male <- tapply(cleaned_data_withoutOutliers$age, cleaned_data_withoutOutliers$gender, mean)
mean_age_female <- tapply(cleaned_data_withoutOutliers$age, cleaned_data_withoutOutliers$gender, mean)
ggplot(data.frame(gender = c("male", "female"), mean_age = c(mean_age_male, mean_age_female)), aes(x = gender, y = mean_age)) +
  geom_col(aes(fill = gender), position = "dodge", stat = "identity")
#نستخدم الدالة ggplot() لإنشاء رسم بياني. نستخدم وسيطة aes() لتحديد متغيرات x و y. نستخدم وسيطة geom_col() لإنشاء رسم بياني شريطي. نستخدم وسيطة fill = gender لتحديد لون كل عمود. نستخدم وسيطة position = "dodge" لفصل الأعمدة عن بعضها البعض. نستخدم وسيطة stat = "identity" لتحديد أن قيم العمود y هي قيم البيانات الأصلية.
#6
# احصل على متوسط العمر لكل فئة جوائز
mean_age_by_award <- tapply(cleaned_data_withoutOutliers$age, cleaned_data_withoutOutliers$awards, mean)
ggplot(data.frame(awards = c("0", "1"), mean_age = c(mean_age_by_award[1], mean_age_by_award[2])), aes(x = awards, y = mean_age)) +
  geom_col(aes(fill = awards), position = "dodge", stat = "identity")
#--------------------------------------------------------------------------------------------------

correlation<-cor(cleaned_data_withoutOutliers$age, cleaned_data_withoutOutliers$avg_training_score)
age <- cleaned_data_withoutOutliers$age
avg_training_score <- cleaned_data_withoutOutliers$avg_training_score

plot(age, avg_training_score)

abline(lm(avg_training_score ~ age))
correlation
#------------------------------------------

correlation<-cor(cleaned_data_withoutOutliers$age, cleaned_data_withoutOutliers$length_of_service)
age <- cleaned_data_withoutOutliers$age
length_of_service <- cleaned_data_withoutOutliers$length_of_service

plot(age, length_of_service)

abline(lm(length_of_service ~ age))
correlation
#------------------------------------------

correlation<-cor(cleaned_data_withoutOutliers$no_of_trainings, cleaned_data_withoutOutliers$age)
no_of_trainings <- cleaned_data_withoutOutliers$no_of_trainings
age <- cleaned_data_withoutOutliers$age

plot(no_of_trainings, age)

abline(lm(age ~no_of_trainings))
correlation
#------------------------------------------

correlation<-cor(cleaned_data_withoutOutliers$no_of_trainings, cleaned_data_withoutOutliers$length_of_service)
no_of_trainings <- cleaned_data_withoutOutliers$no_of_trainings
length_of_service <- cleaned_data_withoutOutliers$length_of_service

plot(no_of_trainings, length_of_service)

abline(lm(length_of_service ~no_of_trainings))
correlation
#------------------------------------------

correlation<-cor(cleaned_data_withoutOutliers$no_of_trainings, cleaned_data_withoutOutliers$avg_training_score)
no_of_trainings <- cleaned_data_withoutOutliers$no_of_trainings
avg_training_score <- cleaned_data_withoutOutliers$avg_training_score

plot(no_of_trainings, avg_training_score)

abline(lm(avg_training_score ~no_of_trainings))
correlation
#------------------------------------------
# حفظت البيانات بعد التعديل في ملف
new_data <- cleaned_data_withoutOutliers
write.csv(new_data, file="D:/Files/cleaned_data_withoutOutliers.csv")
#------------------------------------------
mydata <- cleaned_data_withoutOutliers[c(7, 8, 9, 10, 11, 12, 13)]
kdata <- kmeans(mydata, centers = 2)

# Assuming 'kdata' contains the results of kmeans clustering
mydata$cluster <- as.factor(kdata$cluster)

# Check the column names of kdata$centers
colnames_kdata <- colnames(kdata$centers)
print(colnames_kdata)  # Print column names for debugging
required_columns <- c("age", "avg_training_score")  # Assuming these are the column names

# Check if required columns are present
if (all(required_columns %in% colnames_kdata)) {
  # Create a scatter plot
  ggplot(mydata, aes(x = mydata[, "age"], y = mydata[, "avg_training_score"], color = cluster)) +
    geom_point() +
    geom_point(data = as.data.frame(kdata$centers), aes(x = age, y = avg_training_score), color = "black", size = 3, shape = 4) +
    labs(title = "K-means Clustering",
         x = "Age",
         y = "Average Training Score",
         color = "Cluster") +
    theme_minimal()
} else {
  print("Required columns not present in kdata$centers.")
}


# Decision Tree


tree<-rpart(is_promoted ~ department + previous_year_rating  +  awards_won. + avg_training_score  , data = cleaned_data_withoutOutliers , minsplit=2  )
rpart.plot(tree,type = 2, extra = 100, cex = 0.8, box.col=c("lightpink","mistyrose","seashell","thistle","thistle1","seashell2"))

#to predict data                                                                       
data_to_predict<-data.frame(department="HR",previous_year_rating =6,  awards_won.=2, avg_training_score=99)
if(predict(tree,newdata=data_to_predict)>0.5){
  print("Promoted")
}else{
  print("not Promoted")
}
#------------------------------------------
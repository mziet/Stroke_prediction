##############################################LOADING PACKAGES######################################

library(dplyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(caTools)

#############################################DATA TRANSFORMATION########################################

data<-read.csv(file="C:\\Users\\domin\\OneDrive\\Pulpit\\Statistical data science\\healthcare-dataset-stroke-data.csv", header=TRUE, sep=",")

summary(data)

#Proportion of strokes
sum(data$stroke)/length(data$stroke) 

#Convert some datas
data$bmi<-as.numeric(data$bmi)
data$stroke<-as.factor(data$stroke)

#Check values

table(data$gender)
data<-data %>% filter(gender!="Other") #Delete row with gender="Other"
table(data$smoking_status) #We have 1544 "Unknown" positions
table(data$work_type)
table(data$ever_married)
table(data$Residence_type) 
table(data$hypertension)
data$bmi[is.na(data$bmi)]<-mean(data$bmi, na.rm=TRUE)#BMI filled by mean

############################################EXPLORATORY DATA ANALYSIS#####################################

#Connection between age and bmi
ggplot(data=data, aes(x=age, y=bmi,colour=stroke))+geom_point(size=0.75)+ggtitle("Relation between age and bmi")+theme(plot.title = element_text(hjust = 0.5)) 
#Patients with BMI>60 are young, so this is suspicious. 
ggplot(data=data, aes(y=bmi))+geom_boxplot(size=0.75)+ggtitle("BMI")+theme(plot.title = element_text(hjust = 0.5)) 
data<-data %>% filter(bmi<65)
#We remove bmi outliers.

stroke_only<-data %>% filter(stroke==1)

#Histograms and density plot

#Check BMI vs stroke

par(mfrow=c(1,2))
hist(data$bmi,
     main="All people",
     xlab="BMI",
     ylab="Counts",
     col="blue")

hist(as.numeric(stroke_only$bmi),
     main="People with strokes",
     xlab="BMI",
     ylab="Counts",
     col="red")

ggplot(data,aes(x=bmi,fill=gender))+geom_density(alpha=0.4)+ggtitle("Density of bmi")+theme(plot.title = element_text(hjust = 0.5))
ggplot(data=data, aes(x=bmi))+geom_histogram(colour="black", fill="lightblue")
#Check age vs stroke

par(mfrow=c(1,2))
hist(stroke_only$age, 
     main="People with strokes",
     xlab="Age",
     ylab="Counts",
     col="red")
hist(data$age,
     main="All people",
     xlab="Age",
     ylab="Counts",
     col="blue")

#Easy to notice that between age and possibility of stroke occur a positive correlation.
ggplot(data=data, aes(x=age))+geom_histogram(colour="black", fill="lightblue")
ggplot(data,aes(x=age,fill=gender))+geom_density(alpha=0.4)+ggtitle("Density of age")+theme(plot.title = element_text(hjust = 0.5))+xlim(0,100)

#Check glucose level vs stroke

par(mfrow=c(1,2))
hist(stroke_only$avg_glucose_level, 
     main="People with strokes",
     xlab="AVG glucose level",
     ylab="Counts of strokes",
     col="red")
hist(data$avg_glucose_level,
     main="All people",
     xlab="AVG glucose level",
     ylab="Counts of strokes",
     col="blue")
ggplot(data=data, aes(x=avg_glucose_level))+geom_histogram(colour="black", fill="lightblue")
ggplot(data,aes(x=avg_glucose_level,fill=gender))+geom_density(alpha=0.4)+ggtitle("Density of glucose level")+theme(plot.title = element_text(hjust = 0.5))

#Check smoking status vs stroke

data %>%
  count(smoking_status, stroke) %>% 
  group_by(smoking_status) %>%
  mutate(percentage = n / sum(n)*100) %>%
  ggplot(aes(x = smoking_status, y = percentage, fill = stroke)) +
  geom_col(position = "dodge2")  +
  geom_text(aes(label = round(percentage,2)),position=position_dodge2(width=0.9),vjust=-0.25)

#Check marital status vs stroke

data %>%
  count(ever_married, stroke) %>% 
  group_by(ever_married) %>%
  mutate(percentage = n / sum(n)*100) %>%
  ggplot(aes(x = ever_married, y = percentage, fill = stroke)) +
  geom_col(position = "dodge2")  +
  geom_text(aes(label = round(percentage,2)),position=position_dodge2(width=0.9),vjust=-0.25)

#Check residence vs stroke

data %>%
  count(Residence_type, stroke) %>% 
  group_by(Residence_type) %>%
  mutate(percentage = n / sum(n)*100) %>%
  ggplot(aes(x = Residence_type, y = percentage, fill = stroke)) +
  geom_col(position = "dodge2")  +
  geom_text(aes(label = round(percentage,2)),position=position_dodge2(width=0.9),vjust=-0.25) 
  

#Check hypertension vs stroke

data %>%
  count(hypertension, stroke) %>% 
  group_by(hypertension) %>%
  mutate(percentage = n / sum(n)*100) %>%
  ggplot(aes(x = hypertension, y = percentage, fill = stroke)) +
  geom_col(position = "dodge2")  +
  geom_text(aes(label = round(percentage,2)),position=position_dodge2(width=0.9),vjust=-0.25)

#Hypertension clearly impact on the probability of stroke.

#Check heart disease vs stroke

data %>%
  count(heart_disease, stroke) %>% 
  group_by(heart_disease) %>%
  mutate(percentage = n / sum(n)*100) %>%
  ggplot(aes(x = heart_disease, y = percentage, fill = stroke)) +
  geom_col(position = "dodge2")  +
  geom_text(aes(label = round(percentage,2)),position=position_dodge2(width=0.9),vjust=-0.25)

#Heart disease also impact on the probability of stroke.

#Check gender vs stroke

data %>%
  count(gender, stroke) %>% 
  group_by(gender) %>%
  mutate(percentage = n / sum(n)*100) %>%
  ggplot(aes(x = gender, y = percentage, fill = stroke)) +
  geom_col(position = "dodge2")  +
  geom_text(aes(label = round(percentage,2)),position=position_dodge2(width=0.9),vjust=-0.25)

#Seems to gender doesn't impact significantly.

#Work type vs stroke

data %>%
  count(work_type, stroke) %>% 
  group_by(work_type) %>%
  mutate(percentage = n / sum(n)*100) %>%
  ggplot(aes(x = work_type, y = percentage, fill = stroke)) +
  geom_col(position = "dodge2")  +
  geom_text(aes(label = round(percentage,2)),position=position_dodge2(width=0.9),vjust=-0.25)

###############################################GENERAL MODEL#############################################

step_0 <- glm(stroke ~ age+gender+hypertension+heart_disease+ever_married+work_type+Residence_type+avg_glucose_level+bmi+smoking_status, family = "binomial", data = data)
summary(step_0)

#Taking into account P value we see that the most significant predictor is an age, so we start our model with age.

step_1<-glm(stroke~age, data=data, family="binomial")
summary(step_1)

prob_logit_step_1 <- predict(step_1, type = "response")

logit_step_1<- log(prob_logit_step_1/(1-prob_logit_step_1))

data %>% 
  ggplot(aes(age, logit_step_1)) +
  geom_point() + 
  geom_smooth() +
  labs(x = "Age (years)", 
       y = "Logit", 
       title = "Logit of stroke vs age") 

data %>% 
  mutate(prob=ifelse(stroke==1,1,0)) %>% 
  ggplot(aes(age, prob)) +
  geom_point(alpha=0.2) +
  geom_smooth(mapping=aes(age, exp(coef(step_1)[1]+coef(step_1)[2]*age)/(1+exp(coef(step_1)[1]+coef(step_1)[2]*age))))+
  labs(title="Logistic regression stroke vs age", x="Age", y="Probability of having stroke")

#Now we can add second variable - in our case it is an average glucose level.

step_2<-glm(stroke~age+avg_glucose_level, data=data, family="binomial")
summary(step_2)

#The last step is adding a hypertension to our model. Because it's discrete variable we apply grouping.

data$G1<-ifelse(data$hypertension==1,1,0)
data$G2<-ifelse(data$hypertension!=1,1,0)

data$G1_Age<-data$G1*data$age
data$G2_Age<-data$G2*data$age

data$G1_Glucose<-data$G1*data$avg_glucose_level
data$G2_Glucose<-data$G2*data$avg_glucose_level

step_3<-glm(stroke~G1+G2+G1_Age+G2_Age+G1_Glucose+G2_Glucose-1,data=data, family='binomial')
summary(step_3)
qt(0.975, 5104-6) #treshhold interval [-1.960429,1.960429] t value for G1_Glucose = 0.974 so we accept null hypothesis
#As we can see for patients with hypertension we can resign from coefficient G1_Glucose. For fixed age=40 we can plot our regression function.

data %>% 
  mutate(prob=ifelse(stroke==1,1,0)) %>% 
  ggplot(aes(avg_glucose_level, prob)) +
  geom_smooth(mapping=aes(avg_glucose_level, coef(step_3)[1]+coef(step_3)[3]*40+coef(step_3)[5]*avg_glucose_level,col='Hypertension'))+
  geom_smooth(mapping=aes(avg_glucose_level, coef(step_3)[2]+coef(step_3)[4]*40+coef(step_3)[6]*avg_glucose_level,col="No hypertension"))+
  labs(title="Regression lines for glucose level group by hypertension", x="Avg_glucose_level", y="Exponent")+
  scale_color_manual(name='',
                     breaks=c('Hypertension', 'No hypertension'),
                     values=c('Hypertension'='red', "No hypertension"='green'))

step_4<-glm(stroke~G1+G2+G1_Age+G2_Age+G2_Glucose-1,data=data, family='binomial')
summary(step_4)

#Now we can plot our results. Firstly we plot the probability of stroke for patient with and without a hypertension with average glucose on level 120.

data %>% 
  mutate(prob = ifelse(stroke == 1, 1, 0)) %>% 
  ggplot(aes(x = age, y = prob)) +
  geom_line(aes(y = exp(coef(step_4)[1] + coef(step_4)[3] * age) / (1 + exp(coef(step_4)[1] + coef(step_4)[3] * age)), color = "Hypertension"), linewidth = 1) +
  geom_line(aes(y = exp(coef(step_4)[2] + coef(step_4)[4] * age+coef(step_4)[5]*120) / (1 + exp(coef(step_4)[2] + coef(step_4)[4] * age+coef(step_4)[5]*120)), color = "No hypertension"), linewidth = 1) +
  labs(title = "Logistic regression stroke vs age for glucose on level=120", x = "Age", y = "Probability of having stroke") +
  scale_color_manual(values = c("Hypertension" = "red", "No hypertension" = "green"))+
  labs(color=NULL)


#Analogously we can plot results for patients with and without a hypertension which are 40.

w1 <- data %>% 
  mutate(prob = ifelse(stroke == 1, 1, 0)) %>% 
  ggplot(aes(x = avg_glucose_level, y = prob)) +
  geom_line(aes(y = exp(coef(step_4)[1] + coef(step_4)[3] * 40) / (1 + exp(coef(step_4)[1] + coef(step_4)[3] * 40)), color = "Hypertension"), size = 1) +
  geom_line(aes(y = exp(coef(step_4)[2] + coef(step_4)[4]*40+coef(step_4)[5]*avg_glucose_level) / (1 + exp(coef(step_4)[2] + coef(step_4)[4] *40+coef(step_4)[5]*avg_glucose_level)), color = "No hypertension"), size = 1) +
  labs(title = "Logistic regression stroke vs glucose level for age=40", x = "Glucose", y = "Probability of having stroke") +
  scale_color_manual(values = c("Hypertension" = "red", "No hypertension" = "green"))+
  labs(color=NULL)


x <- seq(10,100, by = 10)
lines(data$avg_glucose_level,exp(coef(step_4)[2] + coef(step_4)[4]*50+coef(step_4)[5]*data$avg_glucose_level) / (1 + exp(coef(step_4)[2] + coef(step_4)[4] *50+coef(step_4)[5]*data$avg_glucose_level)))

#######################################CONFIRMATION OF GENERAL MODEL#########################################

#Dividing data set into five parts

StrokeYes <- data %>% filter(stroke == 1)
StrokeNo <- data %>% filter(stroke == 0)
set.seed(200)

split1 = sample.split(StrokeYes$stroke, SplitRatio = 49 )
Set1 = subset(StrokeYes, split1 == TRUE)
Set = subset(StrokeYes, split1 == FALSE)
split2 = sample.split(Set$stroke, SplitRatio = 50 )
Set2 = subset(Set, split2 == TRUE)
Set = subset(Set, split2 == FALSE)
split3 = sample.split(Set$stroke, SplitRatio = 50)
Set3 = subset(Set, split3 == TRUE)
Set = subset(Set, split3 == FALSE)
split4 = sample.split(Set$stroke, SplitRatio = 50 )
Set4 = subset(Set, split4 == TRUE)
Set5 = subset(Set, split4 == FALSE)

split6 = sample.split(StrokeNo$stroke, SplitRatio = 971)
Set6 = subset(StrokeNo,split6 == TRUE)
Set = subset(StrokeNo,split6 == FALSE)
split7 = sample.split(Set$stroke, SplitRatio = 971)
Set7 = subset(Set, split7 == TRUE)
Set = subset(Set, split7 == FALSE)
split8 = sample.split(Set$stroke, SplitRatio = 971)
Set8 = subset(Set, split8 == TRUE)
Set = subset(Set, split8 == FALSE)
split9 = sample.split(Set$stroke, SplitRatio = 971)
Set9 = subset(Set, split9 == TRUE)
Set10 = subset(Set, split9  == FALSE)


CheckSet1 <- rbind(Set1,Set6)
CheckSet2 <- rbind(Set2,Set7)
CheckSet3 <- rbind(Set3,Set8)
CheckSet4 <- rbind(Set4,Set9)
CheckSet5 <- rbind(Set5,Set10)


#Creating model for each CheckSet
CheckStep_01 <- glm(stroke ~ age+gender+hypertension+heart_disease+ever_married+work_type+Residence_type+avg_glucose_level+bmi+smoking_status, family = "binomial", data = CheckSet1)
summary(CheckStep_01)
CheckStep_02 <- glm(stroke ~ age+gender+hypertension+heart_disease+ever_married+work_type+Residence_type+avg_glucose_level+bmi+smoking_status, family = "binomial", data = CheckSet2)
summary(CheckStep_02)
CheckStep_03 <- glm(stroke ~ age+gender+hypertension+heart_disease+ever_married+work_type+Residence_type+avg_glucose_level+bmi+smoking_status, family = "binomial", data = CheckSet3)
summary(CheckStep_03)
CheckStep_04 <- glm(stroke ~ age+gender+hypertension+heart_disease+ever_married+work_type+Residence_type+avg_glucose_level+bmi+smoking_status, family = "binomial", data = CheckSet4)
summary(CheckStep_04)
CheckStep_05 <- glm(stroke ~ age+gender+hypertension+heart_disease+ever_married+work_type+Residence_type+avg_glucose_level+bmi+smoking_status, family = "binomial", data = CheckSet5)
summary(CheckStep_05)


predict_train1=predict(step_4, newdata=rbind(CheckSet1,CheckSet2,CheckSet3,CheckSet4),type='response')
train_table1<-table(rbind(CheckSet1,CheckSet2,CheckSet3,CheckSet4)$stroke, predict_train1>0.2)
(train_table1[1,1]+train_table1[2,2])/nrow(rbind(CheckSet1,CheckSet2,CheckSet3,CheckSet4))

predict_test1 = predict(step_4, newdata=CheckSet5, type = 'response')
test_table1<-table(CheckSet5$stroke, predict_test1>0.2)
p1 <- (test_table1[1,1]+test_table1[2,2]) / nrow(CheckSet5)



predict_train2=predict(step_4, newdata=rbind(CheckSet1,CheckSet2,CheckSet3,CheckSet5),type='response')
train_table2<-table(rbind(CheckSet1,CheckSet2,CheckSet3,CheckSet5)$stroke, predict_train2>0.2)
(train_table2[1,1]+train_table2[2,2])/nrow(rbind(CheckSet1,CheckSet2,CheckSet3,CheckSet5))

predict_test2 = predict(step_4, newdata=CheckSet4, type = 'response')
test_table2<-table(CheckSet4$stroke, predict_test2>0.2)
p2 <- (test_table2[1,1]+test_table2[2,2]) / nrow(CheckSet4)


predict_train3=predict(step_4, newdata=rbind(CheckSet1,CheckSet2,CheckSet4,CheckSet5),type='response')
train_table3<-table(rbind(CheckSet1,CheckSet2,CheckSet4,CheckSet5)$stroke, predict_train3>0.2)
(train_table3[1,1]+train_table3[2,2])/nrow(rbind(CheckSet1,CheckSet2,CheckSet4,CheckSet5))

predict_test3 = predict(step_4, newdata=CheckSet3, type = 'response')
test_table3<-table(CheckSet3$stroke, predict_test3>0.2)
p3 <- (test_table3[1,1]+test_table3[2,2]) / nrow(CheckSet3)


predict_train4=predict(step_4, newdata=rbind(CheckSet1,CheckSet4,CheckSet3,CheckSet5),type='response')
train_table4<-table(rbind(CheckSet1,CheckSet4,CheckSet3,CheckSet5)$stroke, predict_train4>0.2)
(train_table4[1,1]+train_table4[2,2])/nrow(rbind(CheckSet1,CheckSet4,CheckSet3,CheckSet5))

predict_test4 = predict(step_4, newdata=CheckSet2, type = 'response')
test_table4<-table(CheckSet2$stroke, predict_test4>0.2)
p4 <- (test_table4[1,1]+test_table4[2,2]) / nrow(CheckSet2)


predict_train5=predict(step_4, newdata=rbind(CheckSet4,CheckSet2,CheckSet3,CheckSet5),type='response')
train_table5<-table(rbind(CheckSet4,CheckSet2,CheckSet3,CheckSet5)$stroke, predict_train5>0.2)
(train_table5[1,1]+train_table5[2,2])/nrow(rbind(CheckSet4,CheckSet2,CheckSet3,CheckSet5))

predict_test5 = predict(step_4, newdata=CheckSet1, type = 'response')
test_table5<-table(CheckSet1$stroke, predict_test5>0.2)
p5 <- (test_table5[1,1]+test_table5[2,2]) / nrow(CheckSet1)


mean(p1,p2,p3,p4,p5)


#this value is 0.927522 so we have very good prediction
################################################OTHER MODELS#######################################


#Model with age grouped by marital status.
data$M1<-ifelse(data$ever_married=="Yes",1,0)
data$M2<-ifelse(data$ever_married!="Yes",1,0)

data$M1_Age<-data$M1*data$age
data$M2_Age<-data$M2*data$age


out3<-glm(stroke~M1+M2+M1_Age+M2_Age-1,data=data, family='binomial')
summary(out3)

prob_logit3 <- predict(out3, type = "response")

logit3<- log(prob_logit3/(1-prob_logit3))

data %>% 
  ggplot(aes(age, logit3)) +
  geom_point() + 
  labs(x = "Age (years)", 
       y = "Logit", 
       title = "Logit of stroke vs age")

data %>% 
  mutate(prob = ifelse(stroke == 1, 1, 0)) %>% 
  ggplot(aes(x = age, y = prob)) +
  geom_line(aes(y = exp(coef(out3)[1] + coef(out3)[3] * age) / (1 + exp(coef(out3)[1] + coef(out3)[3] * age)), color = "Yes"), size = 1) +
  geom_line(aes(y = exp(coef(out3)[2] + coef(out3)[4] * age) / (1 + exp(coef(out3)[2] + coef(out3)[4] * age)), color = "No"), size = 1) +
  labs(title = "Logistic regression stroke vs age by group", x = "Age", y = "Probability of having stroke") +
  scale_color_manual(values = c("Yes" = "red", "No" = "green"))+
  labs(color=NULL)


#Model with age grouped by heart disease.

data$H1<-ifelse(data$heart_disease==1,1,0)
data$H2<-ifelse(data$heart_disease!=1,1,0)

data$H1_Age<-data$H1*data$age
data$H2_Age<-data$H2*data$age


out4<-glm(stroke~H1+H2+H1_Age+H2_Age-1,data=data, family='binomial')
summary(out4)

prob_logit4 <- predict(out4, type = "response")

logit4<- log(prob_logit4/(1-prob_logit4))

data %>% 
  ggplot(aes(age, logit4)) +
  geom_point() + 
  labs(x = "Age (years)", 
       y = "Logit", 
       title = "Logit of stroke vs age")

data %>% 
  mutate(prob = ifelse(stroke == 1, 1, 0)) %>% 
  ggplot(aes(x = age, y = prob)) +
  geom_line(aes(y = exp(coef(out4)[1] + coef(out4)[3] * age) / (1 + exp(coef(out4)[1] + coef(out4)[3] * age)), color = "With heart disease"), size = 1) +
  geom_line(aes(y = exp(coef(out4)[2] + coef(out4)[4] * age) / (1 + exp(coef(out4)[2] + coef(out4)[4] * age)), color = "Without heart disease"), size = 1) +
  labs(title = "Logistic regression stroke vs age by group", x = "Age", y = "Probability of having stroke") +
  scale_color_manual(values = c("With heart disease" = "red", "Without heart disease" = "green"))+
  labs(color=NULL)
#Model with age grouped by hypertension.
data$G1_Glucose<-data$G1*data$avg_glucose_level
data$G2_Glucose<-data$G2*data$avg_glucose_level

out11<-glm(stroke~G1+G2+G1_Age+G2_Age-1,data=data, family='binomial')
summary(out11)

prob_logit11 <- predict(out11, type = "response")

logit11<- log(prob_logit11/(1-prob_logit11))

data %>% 
  ggplot(aes(age, logit11)) +
  geom_point() + 
  labs(x = "Age (years)", 
       y = "Logit", 
       title = "Logit of stroke vs age")

data %>% 
  mutate(prob = ifelse(stroke == 1, 1, 0)) %>% 
  ggplot(aes(x = age, y = prob)) +
  geom_line(aes(y = exp(coef(out11)[1] + coef(out11)[3] * age) / (1 + exp(coef(out11)[1] + coef(out11)[3] * age)), color = "With hypertension"), size = 1) +
  geom_line(aes(y = exp(coef(out11)[2] + coef(out11)[4] * age) / (1 + exp(coef(out11)[2] + coef(out11)[4] * age)), color = "Without hypertension"), size = 1) +
  labs(title = "Logistic regression stroke vs age by group", x = "Age", y = "Probability of having stroke") +
  scale_color_manual(values = c("With hypertension" = "red", "Without hypertension" = "green"))+
  labs(color=NULL)


#Model with avg_glucose_level grouped by hypertension.
data$G1_Glucose<-data$G1*data$avg_glucose_level
data$G2_Glucose<-data$G2*data$avg_glucose_level

out5<-glm(stroke~G1+G2+G1_Glucose+G2_Glucose-1,data=data, family='binomial')
summary(out5)

prob_logit5 <- predict(out5, type = "response")

logit5<- log(prob_logit5/(1-prob_logit5))

data %>% 
  ggplot(aes(avg_glucose_level, logit5)) +
  geom_point() + 
  labs(x = "Average glucose level", 
       y = "Logit", 
       title = "Logit of stroke vs Glucose")

data %>% 
  mutate(prob = ifelse(stroke == 1, 1, 0)) %>% 
  ggplot(aes(x = avg_glucose_level, y = prob)) +
  geom_line(aes(y = exp(coef(out5)[1] + coef(out5)[3] * avg_glucose_level) / (1 + exp(coef(out5)[1] + coef(out5)[3] * avg_glucose_level)), color = "With hypertension"), size = 1) +
  geom_line(aes(y = exp(coef(out5)[2] + coef(out5)[4] * avg_glucose_level) / (1 + exp(coef(out5)[2] + coef(out5)[4] * avg_glucose_level)), color = "Without hipertension"), size = 1) +
  labs(title = "Logistic regression stroke vs Glucose by group", x = "Glucose", y = "Probability of having stroke") +
  scale_color_manual(values = c("With hypertension" = "red", "Without hipertension" = "green"))+
  labs(color=NULL)


#Model with avg_glucose_level grouped by marital status.

data$M1_Glucose<-data$M1*data$avg_glucose_level
data$M2_Glucose<-data$M2*data$avg_glucose_level


out6<-glm(stroke~M1+M2+M1_Glucose+M2_Glucose-1,data=data, family='binomial')
summary(out6)

prob_logit6 <- predict(out6, type = "response")

logit6 <- log(prob_logit6/(1-prob_logit6))

data %>% 
  ggplot(aes(avg_glucose_level, logit6)) +
  geom_point() + 
  labs(x = "Average glucose level", 
       y = "Logit", 
       title = "Logit of stroke vs Glucose")

data %>% 
  mutate(prob = ifelse(stroke == 1, 1, 0)) %>% 
  ggplot(aes(x = avg_glucose_level, y = prob)) +
  geom_line(aes(y = exp(coef(out6)[1] + coef(out6)[3] * avg_glucose_level) / (1 + exp(coef(out6)[1] + coef(out6)[3] * avg_glucose_level)), color = "Yes"), size = 1) +
  geom_line(aes(y = exp(coef(out6)[2] + coef(out6)[4] * avg_glucose_level) / (1 + exp(coef(out6)[2] + coef(out6)[4] * avg_glucose_level)), color = "No"), size = 1) +
  labs(title = "Logistic regression stroke vs Glucose by group", x = "Glucose", y = "Probability of having stroke") +
  scale_color_manual(values = c("Yes" = "red", "No" = "green"))+
  labs(color=NULL)



#Model with avg_glucose_level grouped by heart disease.

data$H1_Glucose<-data$H1*data$avg_glucose_level
data$H2_Glucose<-data$H2*data$avg_glucose_level


out7<-glm(stroke~H1+H2+H1_Glucose+H2_Glucose-1,data=data, family='binomial')
summary(out7)

prob_logit7 <- predict(out7, type = "response")

logit7 <- log(prob_logit7/(1-prob_logit7))

data %>% 
  ggplot(aes(avg_glucose_level, logit7)) +
  geom_point() + 
  labs(x = "Average glucose level", 
       y = "Logit", 
       title = "Logit of stroke vs Glucose")

data %>% 
  mutate(prob = ifelse(stroke == 1, 1, 0)) %>% 
  ggplot(aes(x = avg_glucose_level, y = prob)) +
  geom_line(aes(y = exp(coef(out7)[1] + coef(out7)[3] * avg_glucose_level) / (1 + exp(coef(out7)[1] + coef(out7)[3] * avg_glucose_level)), color = "With heart disease"), size = 1) +
  geom_line(aes(y = exp(coef(out7)[2] + coef(out7)[4] * avg_glucose_level) / (1 + exp(coef(out7)[2] + coef(out7)[4] * avg_glucose_level)), color = "Without heart disease"), size = 1) +
  labs(title = "Logistic regression stroke vs Glucose by group", x = "Glucose", y = "Probability of having stroke") +
  scale_color_manual(values = c("With heart disease" = "red", "Without heart disease" = "green"))+
  labs(color=NULL)


#Model with bmi grouped by hypertension.
data$G1_Bmi<-data$G1*data$bmi
data$G2_Bmi<-data$G2*data$bmi

out8 <-glm(stroke~G1+G2+G1_Bmi+G2_Bmi-1,data=data, family='binomial')
summary(out8)

prob_logit8 <- predict(out8, type = "response")

logit8<- log(prob_logit8/(1-prob_logit8))

data %>% 
  ggplot(aes(bmi, logit8)) +
  geom_point() + 
  labs(x = "Bmi", 
       y = "Logit", 
       title = "Logit of stroke vs Bmi")

data %>% 
  mutate(prob = ifelse(stroke == 1, 1, 0)) %>% 
  ggplot(aes(x = bmi, y = prob)) +
  geom_line(aes(y = exp(coef(out8)[1] + coef(out8)[3] * bmi) / (1 + exp(coef(out8)[1] + coef(out8)[3] * bmi)), color = "With hypertension"), size = 1) +
  geom_line(aes(y = exp(coef(out8)[2] + coef(out8)[4] * bmi) / (1 + exp(coef(out8)[2] + coef(out8)[4] * bmi)), color = "Without hipertension"), size = 1) +
  labs(title = "Logistic regression stroke vs Bmi by group", x = "Bmi", y = "Probability of having stroke") +
  scale_color_manual(values = c("With hypertension" = "red", "Without hipertension" = "green"))+
  labs(color=NULL)

#Model with bmi grouped by marital status.
data$M1_Bmi<-data$M1*data$bmi
data$M2_Bmi<-data$M2*data$bmi

out9 <-glm(stroke~M1+M2+M1_Bmi+M2_Bmi-1,data=data, family='binomial')
summary(out9)

prob_logit9 <- predict(out9, type = "response")

logit9 <- log(prob_logit9/(1-prob_logit9))

data %>% 
  ggplot(aes(bmi, logit9)) +
  geom_point() + 
  labs(x = "Bmi", 
       y = "Logit", 
       title = "Logit of stroke vs Bmi")
  

data %>% 
  mutate(prob = ifelse(stroke == 1, 1, 0)) %>% 
  ggplot(aes(x = bmi, y = prob)) +
  geom_line(aes(y = exp(coef(out9)[1] + coef(out9)[3] * bmi) / (1 + exp(coef(out9)[1] + coef(out9)[3] * bmi)), color = "Yes"), size = 1) +
  geom_line(aes(y = exp(coef(out9)[2] + coef(out9)[4] * bmi) / (1 + exp(coef(out9)[2] + coef(out9)[4] * bmi)), color = "No"), size = 1) +
  labs(title = "Logistic regression stroke vs Bmi by group", x = "Bmi", y = "Probability of having stroke") +
  scale_color_manual(values = c("Yes" = "red", "No" = "green"))+
  labs(color=NULL)


#Model with bmi grouped by heart disease.
data$H1_Bmi<-data$H1*data$bmi
data$H2_Bmi<-data$H2*data$bmi

out10 <-glm(stroke~H1+H2+H1_Bmi+H2_Bmi-1,data=data, family='binomial')
summary(out10)

prob_logit10 <- predict(out10, type = "response")

logit10 <- log(prob_logit10/(1-prob_logit10))

data %>% 
  ggplot(aes(bmi, logit10)) +
  geom_point() + 
  labs(x = "Bmi", 
       y = "Logit", 
       title = "Logit of stroke vs Bmi")

data %>% 
  mutate(prob = ifelse(stroke == 1, 1, 0)) %>% 
  ggplot(aes(x = bmi, y = prob)) +
  geom_line(aes(y = exp(coef(out10)[1] + coef(out10)[3] * bmi) / (1 + exp(coef(out10)[1] + coef(out10)[3] * bmi)), color = "With heart disease"), size = 1) +
  geom_line(aes(y = exp(coef(out10)[2] + coef(out10)[4] * bmi) / (1 + exp(coef(out10)[2] + coef(out10)[4] * bmi)), color = "Without heart disease"), size = 1) +
  labs(title = "Logistic regression stroke vs Bmi by group", x = "Bmi", y = "Probability of having stroke") +
  scale_color_manual(values = c("With heart disease" = "red", "Without heart disease" = "green"))+
  labs(color=NULL)

##############################################BACKWARD SELECTION#########################################

fullmodel <- glm(stroke~ gender+age+hypertension+heart_disease+ever_married+avg_glucose_level+bmi+smoking_status, data=data, family="binomial")
summary(fullmodel)

#The least significant variable as seen is gender with P value of 0.876692, so we will remove gender.

log_regression_2 <- glm(stroke~ age+hypertension+heart_disease+ever_married+avg_glucose_level+bmi+smoking_status, data=data, family="binomial")
summary(log_regression_2)
#The least significant variable as seen is bmi with P value of 0.82980, so we will remove bmi.

log_regression_3 <- glm(stroke~ age+hypertension+heart_disease+ever_married+avg_glucose_level+smoking_status, data=data, family="binomial")
summary(log_regression_3)
#The least significant variable as seen is smoking_status, so we will remove smoking_status.

log_regression_4 <- glm(stroke~ age+hypertension+heart_disease+ever_married+avg_glucose_level, data=data, family="binomial")
summary(log_regression_4)
#The least significant variable as seen is ever_married with P-value of 0.400410, so we will remove ever_married.

log_regression_5 <- glm(stroke~ age+hypertension+heart_disease+avg_glucose_level, data=data, family="binomial")
summary(log_regression_5)
#The least significant variable as seen is heart_disease with P value of 0.078859, so we will remove heart_disease.

log_regression_6 <- glm(stroke~ age+hypertension+avg_glucose_level, data=data, family="binomial")
summary(log_regression_6)
#We get the three most significant variables having P-values less than 0.05.

backward=step(fullmodel) #Compare with function.
summary(backward)

#######################################SPLIT SET TO TRAIN AND TEST#########################################


split = sample.split(data$stroke, SplitRatio = 0.7)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

predict_train=predict(step_4, newdata=train,type='response')
train_table<-table(train$stroke, predict_train>0.2)
(train_table[1,1]+train_table[2,2])/nrow(train)

predict_test = predict(step_4, newdata=test, type = 'response')
test_table<-table(test$stroke, predict_test>0.2)
(test_table[1,1]+test_table[2,2]) / nrow(test)


qt(0.975, 5104-6)


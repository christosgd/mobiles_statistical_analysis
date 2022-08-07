install.packages("summarytools")
install.packages("moments")
install.packages("devtools")
install.packages("mgcv-package")
install.packages("tree")
install.packages("DescTools")
library(DescTools)
library(tree)
library(mgcv)
library(moments)
library(tibble)
library("summarytools")


attach(Cell_Phones_labels)



# Νumerical Variables
# q18 , q20 , q26 , age , mobileprice


#q18

q18 = deframe(Cell_Phones_labels[19][,1])
psych::describe(q18)
summary(q18)
hist(q18, xlim = c(0, 100))
boxplot(q18, xlab="q18")
qqnorm(q18, main="q18")
qqline(q18)
new_q18 = na.omit(q18)
b<-numeric(10000)
for (i in 1:10000) 
{  b[i]<-mean(sample(new_q18,length(new_q18),replace=T)) # ακόμα και χωρίς το na.rm = true βγάζει ίδιο αποτέλεσμα

}
hist(b,freq=FALSE)
quantile(b,c(0.025,0.975))
abline(v=quantile(b,0.025),col="red")
abline(v=quantile(b,0.975),col="purple")
xcurve<-seq(0,13,0.1)
ycurve<-dnorm(xcurve,mean=mean(b),sd=sd(b))
lines(xcurve,ycurve)


#q20
q20 = deframe(Cell_Phones_labels[20][,1])
summary(q20)
psych::describe(q20)
hist(q20, xlim = c(0, 500))
boxplot(q20, xlab="q20")
qqnorm(q20, main="q20")
qqline(q20)
new_q20 = na.omit(q20)
b<-numeric(10000)
for (i in 1:10000) 
{
  b[i]<-mean(sample(new_q20,length(new_q20),replace=T)) # ακόμα και χωρίς το na.rm =true βγάζει ίδιο αποτέλεσμα
}
hist(b,freq=FALSE)
quantile(b,c(0.025,0.975))
abline(v=quantile(b,0.025),col="red")
abline(v=quantile(b,0.975),col="purple")
xcurve<-seq(0,50,0.1)
ycurve<-dnorm(xcurve,mean=mean(b),sd=sd(b))
lines(xcurve,ycurve)


#q26
q26 = deframe(Cell_Phones_labels[28][,1])
summary(q26)
psych::describe(q26)
hist(q26)
boxplot(q26, xlab="q26")
qqnorm(q26, main="q26")
qqline(q26)
new_q26 = na.omit(q26)
b<-numeric(10000)
for (i in 1:10000) 
{
  b[i]<-mean(sample(new_q26,length(new_q26),replace=T)) 
}
hist(b,freq=FALSE)
quantile(b,c(0.025,0.975))
abline(v=quantile(b,0.025),col="red")
abline(v=quantile(b,0.975),col="purple")
xcurve<-seq(0,22,0.1)
ycurve<-dnorm(xcurve,mean=mean(b),sd=sd(b))
lines(xcurve,ycurve)




#age
age = deframe(Cell_Phones_labels[30][,1])
summary(age)
psych::describe(age)
hist(age)
boxplot(age, xlab="age")
qqnorm(age, main="age")
qqline(age)
new_age = na.omit(age)
b<-numeric(10000)
for (i in 1:10000) 
{
  b[i]<-mean(sample(new_age,length(new_age),replace=T)) # ακόμα και χωρίς το na.rm =true βγάζει ίδιο αποτέλεσμα
}
hist(b,freq=FALSE)
quantile(b,c(0.025,0.975))
abline(v=quantile(b,0.025),col="red")
abline(v=quantile(b,0.975),col="purple")
xcurve<-seq(0,60,0.1)
ycurve<-dnorm(xcurve,mean=mean(b),sd=sd(b))
lines(xcurve,ycurve)


#mobileprice
mobileprice = deframe(Cell_Phones_labels[35][,1])
summary(mobileprice)
psych::describe(mobileprice)
hist(mobileprice)
boxplot(mobileprice, xlab="mobileprice")
qqnorm(mobileprice, main="mobileprice")
qqline(mobileprice)
b<-numeric(10000)
for (i in 1:10000) 
{
  b[i]<-mean(sample(mobileprice,length(mobileprice),replace=T)) # ακόμα και χωρίς το na.rm =true βγάζει ίδιο αποτέλεσμα
}
hist(b,freq=FALSE)
quantile(b,c(0.025,0.975))
abline(v=quantile(b,0.025),col="red")
abline(v=quantile(b,0.975),col="purple")
xcurve<-seq(0,300,0.1)
ycurve<-dnorm(xcurve,mean=mean(b),sd=sd(b))
lines(xcurve,ycurve)
kurtosis(mobileprice)




#Categorical Variables
# usr_r, sex, q10c, q14a, q14b, q14c, q14d, q14e, q14g, q14h, q17a, q17b, q17c, q17d, q17e, q17f, q17g, q22a, q22b, q22c, q22d, q22e, q24, q25, q29, mar, educ, empl, inc  


# usr_r
freq(Cell_Phones_labels[2])
count<-prop.table(table(Cell_Phones_labels[2]))
count<-round(count,digits = 2)
barplot(count, names.arg=c("Rural", "Suburban","Urban"))
pie(count,main="usr_r",,col = rainbow(length(count)),labels=count[1:length(count)])
legend("topright", c("Rural", "Suburban","Urban"), 
       cex = 1, fill = rainbow(length(count)))

#sex
freq(Cell_Phones_labels[3])
count<-prop.table(table(Cell_Phones_labels[3]))
count<-round(count,digits = 2)
barplot(count, names.arg=c("Female", "Male"))
pie(count,main="sex",col = rainbow(length(count)),labels=count[1:length(count)])
legend("topright", c("Female", "Male"), 
       cex = 1, fill = rainbow(length(count)))

#q10c
freq(Cell_Phones_labels[4])
count<-prop.table(table(Cell_Phones_labels[4]))
count<-round(count,digits = 2)
barplot(count, names.arg=c("No","Yes"))
pie(count,main="q10c",col = rainbow(length(count)),labels=count[1:length(count)])
legend("topright", c("No","Yes"), 
       cex = 1, fill = rainbow(length(count)))


#q17b
freq(Cell_Phones_labels[13])
count<-prop.table(table(Cell_Phones_labels[13]))
count<-round(count,digits = 4)
barplot(count, names.arg=c("Cell phone can’t do this","No, do not do this","Yes, do this"))
pie(count,main="q17b",col = rainbow(length(count)),labels=count[1:length(count)])
legend("topright", c("Cell phone can’t do this","No, do not do this/Have not done this","Yes, do this"), 
       cex = 0.7, fill = rainbow(length(count)))

#q22d
freq(Cell_Phones_labels[24])
count<-prop.table(table(Cell_Phones_labels[24]))
count<-round(count,digits = 4)
barplot(count, names.arg=c("Agree","Disagree","Neither agree nor disagree"))
pie(count,main="q22d",col = rainbow(length(count)),labels=count[1:length(count)])
legend("topright", c("Agree","Disagree","Neither agree nor disagree/Agree some-Disagree some"), 
       cex = 0.6, fill = rainbow(length(count)))

#marr
freq(Cell_Phones_labels[31])
count<-prop.table(table(Cell_Phones_labels[31]))
count<-round(count,digits = 2)
barplot(count, names.arg=c("Divorced","partner","Married","NotMarr","Separate","Single","Widowed"))
pie(count,main="marr",col = rainbow(length(count)),labels=count[1:length(count)])
legend("topright", c("Divorced","Living with a partner","Married","Never been married","Separated","Single","Widowed"), 
       cex = 0.6, fill = rainbow(length(count)))

#empl
freq(Cell_Phones_labels[33])
count<-prop.table(table(Cell_Phones_labels[33]))
count<-round(count,digits = 2)
barplot(count,col = rainbow(length(count)))
legend("topright", c("Disabled","Employed full-time","Employed part-time","Have own business/self-employed","Not employed for pay","Retired","Student"), 
       cex = 0.8, fill = rainbow(length(count)))
pie(count,main="empl",col = rainbow(length(count)),labels=count[1:length(count)])
legend("topright", c("Disabled","Employed full-time","Employed part-time","Have own business/self-employed","Not employed for pay","Retired","Student"), 
       cex = 0.6, fill = rainbow(length(count)))


#inc
freq(Cell_Phones_labels[34])
count<-prop.table(table(Cell_Phones_labels[34]))
counts <- c(count[9],count[1],count[4],count[5],count[6],count[7],count[8],count[2],count[3])
counts<-round(counts,digits = 2)
barplot(counts,col = rainbow(length(counts)))
legend("topright", c("<$10k","<$20k","<$30k","$30,000 to under $40,000","40,000 to under $50,000","$50,000 to under $75,000","$75,000 to under $100,000","$100,000 to under $150,000","$150,000 or more"), 
       cex = 0.6, fill = rainbow(length(counts)))
pie(counts,main="inc",col = rainbow(length(counts)),labels=counts[1:length(counts)])
legend("topright", c("Less than $10,000","$10,000 to under $20,000","$20,000 to under $30,000","$30,000 to under $40,000","40,000 to under $50,000","$50,000 to under $75,000","$75,000 to under $100,000","$100,000 to under $150,000","$150,000 or more"), 
       cex = 0.55, fill = rainbow(length(counts)))



# Νumerical Variables
# q18 , q20 , q26 , age

# q18
q18 = deframe(Cell_Phones_labels[19][,1])
mobileprice = deframe(Cell_Phones_labels[35][,1])
x<-which(!is.na(q18))
plot(mobileprice[x]~q18[x],xlab = "q18",ylab = "mobile price")
plot(log(mobileprice[x])~q18[x],xlab = "q18",ylab = "log mobile price")
cor.test(mobileprice[x],q18[x],method = "spearman",exact=FALSE)
var.test(mobileprice[x],q18[x])
wilcox.test(mobileprice[x],q18[x])

# q20
q20 = deframe(Cell_Phones_labels[20][,1])
mobileprice = deframe(Cell_Phones_labels[35][,1])
x<-which(!is.na(q20))
plot(mobileprice[x]~q20[x],xlab = "q20",ylab = "mobile price")
plot(log(mobileprice[x])~q20[x],xlab = "q20",ylab = "log mobile price")
cor.test(mobileprice[x],q20[x],method = "spearman",exact=FALSE)
var.test(mobileprice[x],q20[x])
wilcox.test(mobileprice[x],q20[x])



# q26
q26 = deframe(Cell_Phones_labels[28][,1])
mobileprice = deframe(Cell_Phones_labels[35][,1])
x<-which(!is.na(q26))
plot(mobileprice[x]~q26[x],xlab = "q26",ylab = "mobile price")
plot(log(mobileprice[x])~q26[x],xlab = "q26",ylab = "log mobile price")
cor.test(mobileprice[x],q26[x],method = "spearman",exact=FALSE)
var.test(mobileprice[x],q26[x])
wilcox.test(mobileprice[x],q26[x])


# age
age = deframe(Cell_Phones_labels[30][,1])
mobileprice = deframe(Cell_Phones_labels[35][,1])
x<-which(!is.na(age))
plot(mobileprice[x]~age[x],xlab = "age",ylab = "mobile price")
plot(log(mobileprice[x])~age[x],xlab = "age",ylab = "log mobile price")
cor.test(mobileprice[x],age[x],method = "spearman",exact=FALSE)
var.test(mobileprice[x],age[x])
wilcox.test(mobileprice[x],age[x])


#Categorical Variables
#usr_r,sex,q10c,q17b,q22d,marr,empl,inc


usr_r = deframe(Cell_Phones_labels[2][,1])
mobileprice = deframe(Cell_Phones_labels[35][,1])
x<-which(!is.na(usr_r))
boxplot(mobileprice[x]~usr_r[x],xlab = "usr_r",ylab = "mobile price")
boxplot(log(mobileprice[x])~usr_r[x],xlab = "usr_r",ylab = "log mobile price")


sex = deframe(Cell_Phones_labels[3][,1])
mobileprice = deframe(Cell_Phones_labels[35][,1])
x<-which(!is.na(sex))
boxplot(mobileprice[x]~sex[x],xlab = "sex",ylab = "mobile price")
boxplot(log(mobileprice[x])~sex[x],xlab = "sex",ylab = "log mobile price")


q10c = deframe(Cell_Phones_labels[4][,1])
mobileprice = deframe(Cell_Phones_labels[35][,1])
x<-which(!is.na(q10c))
boxplot(mobileprice[x]~q10c[x],xlab = "q10c",ylab = "mobile price")
boxplot(log(mobileprice[x])~q10c[x],xlab = "q10c",ylab = "log mobile price")


q17b = deframe(Cell_Phones_labels[13][,1])
mobileprice = deframe(Cell_Phones_labels[35][,1])
x<-which(!is.na(q17b))
boxplot(mobileprice[x]~q17b[x],xlab = "q17b",ylab = "mobile price")
boxplot(log(mobileprice[x])~q17b[x],xlab = "q17b",ylab = "log mobile price")


q22d = deframe(Cell_Phones_labels[24][,1])
mobileprice = deframe(Cell_Phones_labels[35][,1])
x<-which(!is.na(q22d))
boxplot(mobileprice[x]~q22d[x],xlab = "q22d",ylab = "mobile price")
boxplot(log(mobileprice[x])~q22d[x],xlab = "q22d",ylab = "log mobile price")


marr = deframe(Cell_Phones_labels[31][,1])
mobileprice = deframe(Cell_Phones_labels[35][,1])
x<-which(!is.na(marr))
boxplot(mobileprice[x]~marr[x],xlab = "marr",ylab = "mobile price")
boxplot(log(mobileprice[x])~marr[x],xlab = "marr",ylab = "log mobile price")


empl = deframe(Cell_Phones_labels[33][,1])
mobileprice = deframe(Cell_Phones_labels[35][,1])
x<-which(!is.na(empl))
boxplot(mobileprice[x]~empl[x],xlab = "empl",ylab = "mobile price")
boxplot(log(mobileprice[x])~empl[x],xlab = "empl",ylab = "log mobile price")


inc = deframe(Cell_Phones_labels[34][,1])
mobileprice = deframe(Cell_Phones_labels[35][,1])
x<-which(!is.na(inc))
boxplot(mobileprice[x]~inc[x],xlab = "inc",ylab = "mobile price")
boxplot(log(mobileprice[x])~inc[x],xlab = "inc",ylab = "log mobile price")


#prop test sex-q22a  q22a=Agree
sex = deframe(Cell_Phones_labels[3][,1])
q22a = deframe(Cell_Phones_labels[21][,1])
x_male<-which(sex == "Male")
x_female<-which(sex == "Female")
prop.test(c(freq(q22a[x_female])[1],freq(q22a[x_male])[1]),c(freq(sex)[1],freq(sex)[2]))


#ks test 
sex = deframe(Cell_Phones_labels[3][,1])
mobileprice = deframe(Cell_Phones_labels[35][,1])
male_mobileprice = mobileprice[sex == "Male"]
female_mobileprice = mobileprice[sex == "Female"]
ks.test(male_mobileprice,female_mobileprice)



#linear model mobileprice & age
age = deframe(Cell_Phones_labels[30][,1])
model_lm<-lm(mobileprice~age)
summary(model_lm)
plot(model_lm)

#linear model  mobileprice & empl
mobileprice = deframe(Cell_Phones_labels[35][,1])
empl = deframe(Cell_Phones_labels[33][,1])
model_lm<-lm(mobileprice~empl)
summary(model_lm)
plot(model_lm)

#linear model mobileprice & inc
mobileprice = deframe(Cell_Phones_labels[35][,1])
inc = deframe(Cell_Phones_labels[34][,1])
model_lm<-lm(mobileprice~inc)
summary(model_lm)

#polynomial model
q18 = deframe(Cell_Phones_labels[19][,1])
x<-which(!is.na(q18))
model_quadratic<-lm(mobileprice[x]~q18[x]+q18[x]^2)
summary(model_quadratic)
plot(model_quadratic)

#exponential model age & q18 & q26
age = deframe(Cell_Phones_labels[30][,1])
mobileprice = deframe(Cell_Phones_labels[35][,1])
exponential <- lm(log(mobileprice)~age)
plot(age,mobileprice,pch=16)
amount.pr.exp<-exp(predict(exponential,list(age=age.values)))
lines(age.values,mobileprice.pr.exp)
summary(exponential)


q18 = deframe(Cell_Phones_labels[19][,1])
x<-which(!is.na(q18))
mobileprice = deframe(Cell_Phones_labels[35][,1])
exponential <- lm(log(mobileprice[x])~q18[x])
summary(exponential)


q26 = deframe(Cell_Phones_labels[28][,1])
x<-which(!is.na(q26))
mobileprice = deframe(Cell_Phones_labels[35][,1])
exponential <- lm(log(mobileprice[x])~q26[x])
summary(exponential)

#gam model
age = deframe(Cell_Phones_labels[30][,1])
mobileprice = deframe(Cell_Phones_labels[35][,1])
model<-gam(mobileprice~s(age))
plot(model)
points(age,mobileprice-mean(mobileprice))
summary(model)



q18 = deframe(Cell_Phones_labels[19][,1])
x<-which(!is.na(q18))
mobileprice = deframe(Cell_Phones_labels[35][,1])
model<-gam(mobileprice[x]~s(q18[x]))
summary(model)



#anova
#mobile price & sex
sex = deframe(Cell_Phones_labels[3][,1])
mobileprice = deframe(Cell_Phones_labels[35][,1])
model_an<-(aov(mobileprice~sex))
summary(model_an)
summary.lm(model_an)


#mobile price & inc
inc = deframe(Cell_Phones_labels[34][,1])
mobileprice = deframe(Cell_Phones_labels[35][,1])
summary(aov(mobileprice~inc))

#mobileprice & sex
empl = deframe(Cell_Phones_labels[33][,1])
mobileprice = deframe(Cell_Phones_labels[35][,1])
mar = deframe(Cell_Phones_labels[31][,1])
summary.lm(aov(mobileprice~empl*mar))


#anova mobileprice & empl με παραγοντες
empl = deframe(Cell_Phones_labels[33][,1])
mobileprice = deframe(Cell_Phones_labels[35][,1])
model <-(aov(mobileprice~empl))
PostHocTest(model,method="hsd")

#anova mobileprice & inc με παραγοντες
inc = deframe(Cell_Phones_labels[34][,1])
mobileprice = deframe(Cell_Phones_labels[35][,1])
model <-(aov(mobileprice~inc))
summary(model)
PostHocTest(model,method="hsd")

#composite models

mobileprice = deframe(Cell_Phones_labels[35][,1])
q18 = deframe(Cell_Phones_labels[19][,1])
log_q18 = replace(log(q18), log(q18) ==-Inf, 0)
model1<-lm(log(mobileprice)~log_q18*log(age))
summary(model1)



inc = deframe(Cell_Phones_labels[34][,1])
inc_factor = factor(inc,levels=c("Less than $10,000","$10,000 to under $20,000","$20,000 to under $30,000","$30,000 to under $40,000","$40,000 to under $50,000","$50,000 to under $75,000","$75,000 to under $100,000","$100,000 to under $150,000","$150,000 or more"), ordered=T)
mobileprice = deframe(Cell_Phones_labels[35][,1])
model <-aov(mobileprice~inc_factor)
PostHocTest(model,method="hsd")
inc_factor_2<-factor(inc_factor, ordered=T)
levels(inc_factor_2)[c(1,2,3)]= "Less than $30,000"
levels(inc_factor_2)[c(2,3,4)]= "$30,000 to under $75,000"
levels(inc_factor_2)[c(3,4,5)]= "$75,000 or more"
model <-aov(mobileprice~inc_factor_2)
summary(model)


q18 = deframe(Cell_Phones_labels[19][,1])
q20 = deframe(Cell_Phones_labels[20][,1])
q26 = deframe(Cell_Phones_labels[28][,1])
age = deframe(Cell_Phones_labels[30][,1])
model1<-lm(mobileprice~inc_factor_2:age+log(age)+q18+age+inc_factor_2)
summary(model1)





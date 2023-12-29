library(readxl)
Digital_Transactions_and_Its_effects <- read_excel("C:/Users/Shivam M/OneDrive/Desktop/My Folder/Digital Transactions and Its effects.xlsx")
View(Digital_Transactions_and_Its_effects)
Data=Digital_Transactions_and_Its_effects
View(Data)
library(psych)
e1<-Data$`4g REVOLUTION`
e2<-Data$UPIfavorable
e3<-Data$`online payments is growing`
e4<-Data$Covid
e5<-Data$Awareness
e6<-Data$`Government Policies`
e7<-Data$`Expansion of Smartphone and Internet`
e8<-Data$Year
e9<-Data$`Online Payment made Economy Stable`
E<-data.frame(e1,e2,e3,e4,e5,e6,e7,e9)
E
alpha(E)

# Time Series 
library(readxl)
Book1 <- read_excel("C:/Users/Shivam M/OneDrive/Desktop/RJC MSc/Book1.xlsx")
View(Book1)
Book1
class(Book1)
# converting in the time series data
Book1ts <- ts(Book1$user, start = 2016, end = 2023,frequency = 1)
Book1ts
View(Book1ts)
class(Book1ts)
summary(Book1ts)
plot(Book1ts)
line(Book1ts) 
boxplot(Book1ts)

#two-way Anova
attach(Data)
myfact=as.factor(Data$Life.easy)
myfact=factor(`Life.easy`,labels=c(1,2,3,4,5))

myfact1=as.factor(Data$`Rating to online payments`)
myfact1=factor(`Rating to online payments`,labels = c(1,2,3,4,5))

myfact2=as.factor(Data$`Usage of debit and credit cards`)
myfact2=factor(`Usage of debit and credit cards`, labels = c(1,2,3))

myfact3=as.factor(Data$`Government Policies`)
myfact3=factor(`Government Policies`,labels=c(1,2,3,4,5))

myfact4=as.factor(Data$`UPIfavorable`)
myfact4=factor(`UPIfavorable`,labels=c(1,2,3,4,5))

summary(myfact)
summary(myfact1)
summary(myfact2)
summary(myfact3)
summary(myfact4)

fit<-aov(Gender~ `myfact`*`myfact1`*`myfact2`*`myfact3`*`myfact4`)
summary(fit)
fit1<-aov(Age~ `myfact`*`myfact1`*`myfact2`*`myfact3`*`myfact4`)
summary(fit1)
fit2<-aov(`ResidentialArea`~ `myfact`*`myfact1`*`myfact2`*`myfact3`*`myfact4`)
summary(fit2)

attach(Data)
TT=as.factor(Data$Transparency)
myfact=factor(`Transparency`,labels=c(1,2,3,4,5))

ESI=as.factor(Data$`Rating to online payments`)
myfact1=factor(`Expansion of Smartphone and Internet`,labels = c(1,2,3,4,5))

AWS=as.factor(Data$`Usage of debit and credit cards`)
myfact2=factor(`Usage of debit and credit cards`, labels = c(1,2,3))

DE=as.factor(Data$`Government Policies`)
myfact3=factor(`Government Policies`,labels=c(1,2,3,4,5))

UPI=as.factor(Data$`UPIfavorable`)
myfact4=factor(`UPIfavorable`,labels=c(1,2,3,4,5))

summary(TT)
summary(ESI)
summary(AWS)
summary(DE)
summary(UPI)

fit3<-aov(`Cashless Economy` ~ `TT`*`ESI`*`AWS`*`DE`*`UPI`)
summary(fit3)

#Chi_Sq
attach(Data)
a<-data.frame(Data$`Reason to use that app`,Data$`Platform using for online payments`)
a
contingency_table<-table(Data$`Reason to use that app`,Data$`Platform using for online payments`)
contingency_table_with_total<-addmargins(contingency_table)
result<-chisq.test(contingency_table_with_total)
print(contingency_table_with_total)
print(result)

b<-data.frame(Data$Age,Data$`Usage of debit and credit cards`)
b
contingency_table1<-table(Data$Age,Data$`Usage of debit and credit cards`)
contingency_table_with_total1<-addmargins(contingency_table1)
print(contingency_table_with_total1)
result1<-chisq.test(contingency_table_with_total1)
print(result1)

c<-data.frame(Data$`Platform using for online payments`,Data$`Reason to use that app`)
c
contingency_table2<-table(Data$`Platform using for online payments`,Data$`Reason to use that app`)
contingency_table_with_total2<-addmargins(contingency_table2)
print(contingency_table_with_total2)
result2<-chisq.test(contingency_table_with_total2)
print(result2)

d<-data.frame(Data$`Platform using for online payments`,Data$`Reason to use that app`)
d
contingency_table3<-table(Data$`Platform using for online payments`,Data$`Reason to use that app`)
contingency_table_with_total3<-addmargins(contingency_table3)
print(contingency_table_with_total3)
result3<-chisq.test(contingency_table_with_total3)
posthocc <- pairwise.prop.test(contingency_table_with_total3,p.adjust.method = "bonferroni")
print(posthocc)
test <- chisq.test(Data$`Platform using for online payments`,Data$`Reason to use that app`)


e<-data.frame(Data$`why you use online payments`,Data$`Platform using for online payments`)
e
contingency_table<-table(Data$`why you use online payments`,Data$`Platform using for online payments`)
contingency_table_with_total<-addmargins(contingency_table)
result<-chisq.test(contingency_table_with_total)
print(contingency_table_with_total)
print(result)

f<-data.frame(Data$Life.easy,Data$UPIfavorable)
f
contingency_table<-table(Data$Life.easy,Data$UPIfavorable)
contingency_table_with_total<-addmargins(contingency_table)
result<-chisq.test(contingency_table_with_total)
print(contingency_table_with_total)
print(result)


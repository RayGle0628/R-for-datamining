##################第六章 统计推断基础##############################################

####################统计学概念-----------------------------------------------------
house_price_gr<-read.csv("house_price_gr.csv")
attach(house_price_gr)
hist(rate,prob=T,main="Distrabution of 住宅价格增长率")
lines(density(rate))
qqnorm(rate);qqline(rate)
##计算置信区间
mean(rate)
se<-sd(rate)/sqrt(length(rate))
se
LB<-mean(rate)-1.98*se
UB<-mean(rate)+1.98*se
c(LB,UB)
##改变显著性水平，定义函数
confint<-function(x,sigma=-1,alpha=0.05)
{
    n<-length(x)
    xb<-mean(x)
    tmp<-(sd(x)/sqrt(n))*qt(1-alpha/2,n-1);df<- n-1
    data.frame(mean=xb,df=df,LB=xb-tmp,UB=xb+tmp)
}
confint(rate,0.01)

####################假设检验与单样本T检验------------------------------------------
##单样本t检验
t.test(rate,mu=0.1)
detach(house_price_gr)

##
##数据说明：creditcard_exp.csv 本数据是一份汽车贷款违约数据
##名称	中文含义
#id	id
#Acc	是否开卡(1=已开通)
#avg_exp	月均信用卡支出（元）
#avg_exp_ln	月均信用卡支出的自然对数
#gender	性别(男=1)
#Age	年龄
#Income	年收入（万元）
#Ownrent	是否自有住房（有=1；无=0)
#Selfempl	是否自谋职业(1=yes, 0=no)
#dist_home_val	所住小区房屋均价(万元)
#dist_avg_income	当地人均收入
#high_avg	高出当地平均收入
#edu_class	教育等级：小学及以下开通=0，中学=1，本科=2，研究生=3

##两样本T检验
creditcard_exp<-read.csv("creditcard_exp.csv")
creditcard_exp<-na.omit(creditcard_exp)
attach(creditcard_exp)
tapply(avg_exp, gender, summary)
var.test(avg_exp~gender)
t.test(avg_exp~gender,var.equal=T)
detach(creditcard_exp)

####################方差分析-------------------------------------------------------
##单因素方差分析
creditcard_exp$edu_class<-as.factor(creditcard_exp$edu_class)
creditcard_exp$gender<-as.factor(creditcard_exp$gender)
attach(creditcard_exp)
oneway.test(avg_exp~edu_class,var.equal=F)
##使用lm()函数进行方差分析
ana<-lm(avg_exp~edu_class+gender)
summary(ana)
##交互项的方差分析
ana<-lm(avg_exp~edu_class+gender+edu_class*gender)
summary(ana)
detach(creditcard_exp)

####################相关分析-------------------------------------------------------
##系数检测
plot(creditcard_exp$Income,creditcard_exp$avg_exp)
cor.test(creditcard_exp$Income,creditcard_exp$avg_exp,method="pearson")
##散点图矩阵
pairs(~avg_exp+Age+Income+dist_home_val+dist_avg_income,data=creditcard_exp,
      main="贷款违约数据散点图矩阵")
library(car)
scatterplotMatrix(~avg_exp+Age+Income+dist_home_val+dist_avg_income|gender
                  ,data=creditcard_exp,main="贷款违约数据散点图矩阵")
##相关矩阵图
library(corrplot)
##使用不同的method绘制矩阵图
meths<-c("circle","square","ellipse","number","pie","color")
credit<-creditcard_exp[,c("avg_exp","Age","Income","dist_home_val","dist_avg_income")]
par(mfrow=c(2,3))
temp<-mapply(function(x){corrplot(cor(credit),method=x)},meths)
library(corrgram)
corrgram(mtcars,order=T,lower.panel=panel.pie,upper.panel=panel.shade)

####################卡方检验-------------------------------------------------------
accepts<-read.csv("accepts.csv")
attach(accepts)
table(bankruptcy_ind,bad_ind)
chisq.test(x=bankruptcy_ind,y=bad_ind)
detach(accepts)






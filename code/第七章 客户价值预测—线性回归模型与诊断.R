##################第七章 客户价值预测—线性回归模型与诊断###########################

####################线性回归-------------------------------------------------------
creditcard_exp<-read.csv("creditcard_exp.csv")
creditcard_exp<-na.omit(creditcard_exp)
creditcard_exp$gender<-as.factor(creditcard_exp$gender)
attach(creditcard_exp)
##简单线性回归
lm_s<-lm(avg_exp~Income)
summary(lm_s)
##使用predict或fit产生预测值，使用resid产生残差
creditcard_exp$Pred1_avg_exp<-predict(lm_s)
creditcard_exp$Pred2_avg_exp<-fitted(lm_s)
creditcard_exp$resid_avg_exp<-resid(lm_s)
##多元线性回归
lm_m<-lm(avg_exp~Age+Income+dist_home_val+dist_avg_income,data=creditcard_exp)
summary(lm_m)
##变量筛选
lm_m<-lm(avg_exp~Age+Income+dist_home_val+dist_avg_income,data= creditcard_exp)
##向前法
lm_forward<-step(lm_m,direction = "forward")
##向后法
lm_backward=step(lm_m,direction = "backward")
##逐步法
lm_both<-step(lm_m,direction = "both")
summary(lm_both)
detach(creditcard_exp)

####################线性回归的诊断-------------------------------------------------
exp <- read.csv("creditcard_exp.csv")
names(exp)
exp <-na.omit(exp)
##构建线性模型
ana1<-lm(avg_exp~Income,data=exp)
summary(ana1)
##生成预测值和残差
exp$pred<-predict(ana1)
exp$res<-resid(ana1)
##绘制散点图
plot(exp$res~exp$pred)
abline(h=0)
##遇到异方差情况，实际上最常用的是对被解释变量取对数
ana2<-lm(avg_exp_ln~Income,data=exp)
exp$pred<-predict(ana2)
exp$res<-resid(ana2)
plot(exp$res~exp$pred)
abline(h=0)

library(car)
qqPlot(ana2,id.method="identify",simulate=TRUE)
qqnorm(exp$res);qqline(exp$res)
##强影响点分析
ana3<-lm(avg_exp_ln~Income,data=exp)
exp$rstu=rstudent(ana2)
plot(exp$pred,exp$rstu,xlab='预测值',ylab='学生化残差',ylim=c(-4,4))
abline(h=c(0,2,3,-2,-3))
exp_outlier=exp[abs(exp$rstu)>2,]
exp_new=exp[abs(exp$rstu)<=2,]
##多重共线性
##方差膨胀系数
ana3<-lm(avg_exp_ln~Income+Age+dist_home_val+dist_avg_income,data=exp)
vif(ana3)
##保留方差膨胀系数较高的两个变量中的一个进行回归
ana3<-lm(avg_exp_ln~Income+Age+dist_home_val,data=exp)
vif(ana3)

####################正则化方法-----------------------------------------------------
attach(exp)
library(MASS)
library(glmnet)
#####alpha = 0表示岭回归#####
r1 <- glmnet(y=avg_exp_ln, x=cbind(Income,Age,dist_home_val,dist_avg_income), 
             family = "gaussian",alpha=0)
plot(r1, xvar = "lambda")
##交叉验证
r2<-cv.glmnet(y=avg_exp_ln, x=cbind(Income,Age,dist_home_val,dist_avg_income), 
                family="gaussian",alpha=0,nfolds=3)
plot(r2, xvar = "lambda")
r2$lambda.min
r2$lambda.1se
##设置lambda=0.0，ana3为普通最小二乘回归，可以看到两者系数估计值基本一样
ana3<-lm(avg_exp_ln~Income+Age+dist_home_val+dist_avg_income,data=exp)
ana3
r1=glmnet(y=avg_exp_ln,lambda=0.0,x=cbind(Income,Age,dist_home_val,dist_avg_income),
          family = "gaussian",alpha=0)
coef(r1)
##设置Lambda为0.16720，进行建模
r1=glmnet(y=avg_exp_ln,lambda=0.1672,x=cbind(Income,Age,dist_home_val,dist_avg_income),
          family = "gaussian",alpha=0)
coef(r1)
#####alpha=1表示Lasso回归#####
r1 <- glmnet(y=avg_exp_ln,x=cbind(Income,Age,dist_home_val,dist_avg_income),
             family = "gaussian",alpha=1)
plot(r1,xvar = "lambda")
set.seed(1234)
r2 <- cv.glmnet(y=avg_exp_ln,x=cbind(Income,Age,dist_home_val,dist_avg_income),
                family = "gaussian",alpha=1,nfolds=3)
plot(r2)
r2$lambda.min
r2$lambda.1se
##挑选lambda.min作为lambda值，建立Lasso回归模型
r3 <- glmnet(y=avg_exp_ln,x=cbind(Income,Age,dist_home_val,dist_avg_income),
             family = "gaussian",alpha=1, lambda = r2$lambda.min)
coef(r3)
detach(exp)

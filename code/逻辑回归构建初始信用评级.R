##################第八章 逻辑回归构建初始信用评级######################################

####################相关关系与逻辑回归-------------------------------------------------
##加载数据集
accepts<-read.csv("accepts.csv",stringsAsFactors = F)
accepts$bad_ind<-as.factor(accepts$bad_ind)
accepts$bankruptcy_ind<-as.factor(accepts$bankruptcy_ind)
##卡方检验
library(prettyR)
xtab(~ bankruptcy_ind + bad_ind, data=accepts, chisq = TRUE)
#####实现logistic回归#####
##建立训练集与测试集
accepts<-na.omit(accepts)
attach(accepts)
set.seed(100)
select<-sample(1:nrow(accepts),length(accepts$application_id)*0.7)
train=accepts[select,]
test=accepts[-select,]
##logistic回归建模
lg<-glm(bad_ind ~fico_score,family=binomial(link='logit'))
summary(lg)
##多元logistic回归
lg<-glm(bad_ind ~fico_score+bankruptcy_ind+tot_derog+age_oldest_tr+rev_util + 
            ltv+ veh_mileage,family=binomial(link='logit'))
summary(lg)
##进行逐步logsitic回归
lg_ms<-step(lg,direction = "both")
summary(lg_ms)
lg<-glm(bad_ind ~fico_score+bankruptcy_ind+age_oldest_tr+rev_util+ltv,
        family=binomial(link='logit'))
summary(lg)
library(car)
vif(lg)
##预测
train$p<-predict(lg_ms, train,type='response') 
test$p<-predict(lg_ms, test,type='response')

####################模型评估-------------------------------------------------------------
library(ROCR)
pred_Te <- prediction(test$p, test$bad_ind)
perf_Te <- performance(pred_Te,"tpr","fpr")
pred_Tr <- prediction(train$p, train$bad_ind)
perf_Tr <- performance(pred_Tr,"tpr","fpr")
plot(perf_Te, col='blue',lty=1);
plot(perf_Tr, col='black',lty=2,add=TRUE);
abline(0,1,lty=2,col='red')
lr_m_auc<-round(as.numeric(performance(pred_Tr,'auc')@y.values),3)
lr_m_str<-paste("Mode_Train-AUC:",lr_m_auc,sep="")
legend(0.3,0.4,c(lr_m_str),2:8)
lr_m_auc<-round(as.numeric(performance(pred_Te,'auc')@y.values),3)
lr_m_str<-paste("Mode_Test-AUC:",lr_m_auc,sep="")
legend(0.3,0.2,c(lr_m_str),2:8)
##pOCR包也可以实现ROC曲线
library(pROC)
plot.roc(bad_ind~p,train,col="1")->r1
rocobjtr<- roc(train$bad_ind, train$p)
legend(0.7,0.5,paste("Mode_Train-AUC:",round(auc(rocobjtr),3),sep=""))
lines.roc(bad_ind~p,test,col='2')->r2
rocobjte <- roc(test$bad_ind, test$p)
legend(0.7,0.3,paste("Mode_Test-AUC:",round(auc(rocobjte),3),sep=""))
##检验这两条ROC曲线是否有显著差异
roc.test(r1,r2)

detach(accepts)

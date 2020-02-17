###################################
###1.描述性统计
###################################
###数据集描述与属性说明###：
##数据说明：本数据是一份汽车贷款违约数据,因变量为是否违约(bad_ind)
##[T]因变量,[N]数值变量,[C]分类变量,[S]序数变量,[D]时间
##application_id	申请者ID
##account_number	帐户号
##[T]bad_ind	是否违约
##[D]vehicle_year	汽车购买时间
##[C]vehicle_make	汽车制造商
##[C]bankruptcy_ind	曾经破产标识
##[N]tot_derog	五年内信用不良事件数量(比如手机欠费消号)
##[N]tot_tr	全部帐户数量
##[N]age_oldest_tr	最久账号存续时间(月)
##[N]tot_open_tr	在使用帐户数量
##[N]tot_rev_tr	在使用可循环贷款帐户数量(比如信用卡)
##[N]tot_rev_debt	在使用可循环贷款帐户余额(比如信用卡欠款)
##[N]tot_rev_line	可循环贷款帐户限额(信用卡授权额度)
##[N]rev_util	可循环贷款帐户使用比例(余额/限额)
##[N]fico_score	FICO打分
##[N]purch_price	汽车购买金额(元)
##[N]msrp	建议售价
##[N]down_pyt	分期付款的首次交款
##[N]loan_term	贷款期限(月)
##[N]loan_amt	贷款金额
##[N]ltv	贷款金额/建议售价*100
##[N]tot_income	月均收入(元)
##[N]veh_mileage	行使历程(Mile)
##[C]used_ind	是否使用
##[N]weight	样本权重

#################################################################
#1 读取数据
setwd("D:/R_edu/data")
accepts=read.csv("accepts.csv")
##查看数据整体情况，进行数据预处理
sapply(accepts,class)#查看数据类型
summary(accepts)
accepts$bad_ind=as.factor(accepts$bad_ind)
accepts$bankruptcy_ind=as.factor(accepts$bankruptcy_ind)
accepts$used_ind=as.factor(accepts$used_ind)
accepts<-accepts[,c(-1,-2)]
#################################################################
#2 变量粗筛：祛除与被解释变量相关性不大的解释变量，降低后续工作量
library(party)
set.seed(42)
crf<-cforest(bad_ind~.,control = cforest_unbiased(mtry = 2, ntree = 20), data=accepts)
varimpt<-data.frame(varimp(crf))

#得到的前5个重要变量为:fico_score、tot_derog、tot_rev_line、rev_util、age_oldest_tr
#################################################################
#3 数据清洗：实际上是和上一步同时进行的，此处只是列出需要处理的四类问题
##3.1 数据去重
accepts<-unique(accepts)

##3.2 错误值：如果发现问题，一般使用缺失值替代
###比如发现年龄为-2000，这有可能是输入错误，可以让负责数据的同时复查，也可以替换为缺失值


##3.3 缺失值：连续变量使用均值或中位数，分类变量使用众数或单独作为一类
#vmean<-mean(accepts$tot_derog,na.rm=TRUE)
#accepts$tot_derog_empflag<-is.na(accepts$tot_derog)
#accepts[is.na(accepts$tot_derog),]$tot_derog<-vmean
#或者简单粗暴的用多重插补的方法
library(mice)
bad_ind<-accepts[,c("bad_ind")]
x<-accepts[,c("fico_score","tot_derog","tot_rev_line","rev_util","age_oldest_tr")]
imp<-mice(x,met="cart",m=1)#该方法只对数值变量进行插补，分类变量的缺失值保留
x_imp<-complete(imp)
data<-cbind(bad_ind,x_imp)
##3.4 异常值
###盖帽法
#整行去掉数据框里99%以上和1%以下的点
data1<-data
summary(data1)
r=data1$rev_util#以rev_util里的数据进行盖帽处理
r.center <- (r > quantile(r, 0.01)) & (r < quantile(r, 0.99))#选取字段中位于1%-99%的数据
data1 <- data1[r.center, ]
rm(r)
summary(data1)
#################################################################
#4 建立模型
###4.1 首先构建训练和验证数据集
set.seed(10)
select<-sample(1:nrow(data1),nrow(data1)*0.7)
train=data1[select,]
test=data1[-select,]

###4.2 使用训练数据建模
#detach(train)
#attach(train)
lg<-glm(bad_ind ~fico_score+tot_derog+tot_rev_line+rev_util+age_oldest_tr,
        family=binomial(link='logit'),data=train)
summary(lg)
#lg_ms<-lg
lg_ms<-step(lg,direction = "both")
summary(lg_ms)

#################################################################
#5 模型检验
###对分类数据预测需要加上bad_ind参数 
test$y<-predict(lg_ms, test) 
test$out<-(1/(1+exp(-1*test$y)))
summary(test$out)
test[test$out<0.5,]$out<-0
test[test$out!=0,]$out<-1
table(test$bad_ind,test$out)
###计算准确率
rate2<-sum(test$out==test$bad_ind)/length(test$bad_ind)

###做出概率预测
train$lg_p<-predict(lg_ms, train) 
test$lg_p<-predict(lg_ms, test) 

###绘制ROC曲线
library(ROCR)
pred_Tr <- prediction(train$lg_p, train$bad_ind)
perf_Tr <- performance(pred_Tr,"tpr","fpr")

pred_Te <- prediction(test$lg_p, test$bad_ind)
perf_Te <- performance(pred_Te,"tpr","fpr")

plot(perf_Tr,col='green',main="ROC of Models")
plot(perf_Te, col='black',lty=2,add=TRUE);
abline(0,1,lty=2,col='red')


lr_m_auc<-round(as.numeric(performance(pred_Tr,'auc')@y.values),3)
lr_m_str<-paste("Mode_Tran-AUC:",lr_m_auc,sep="")
legend(0.3,0.45,c(lr_m_str),2:8)

lr_m_auc<-round(as.numeric(performance(pred_Te,'auc')@y.values),3)
lr_m_ste<-paste("Mode_Test-AUC:",lr_m_auc,sep="")
legend(0.3,0.25,c(lr_m_ste),2:8)

















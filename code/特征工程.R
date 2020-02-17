############################第十五章 特征工程######################################

####################数据预处理-----------------------------------------------------
##读取数据
raw<-read.csv("telecom_bill.csv")
head(raw)
######错误值处理#########
head(raw[raw$traffic>=5000,])
data <- raw [ raw$traffic < 5000 , ]
##对state列汇总作图
statesCount <- as.data.frame(table(data$state))
statesCount
attach(statesCount)
pct <- round(Freq/sum(Freq)*100,1)
lbls <- paste0(Var1,": ",pct,"%")
pie(Freq, labels = lbls, col=rainbow(length(lbls)),cex=1.2)
detach(statesCount)
##删除state列不应当出现的状态
new_state<-data
new_state[(new_state$state==2|new_state$state==4)&(!is.na(new_state$state)),"state"]<-NA
######异常值处理##########
##消费额箱线图
boxplot(data$expenditure)
##盖帽法处理异常值
tmp <- data$exp
mu <- mean(tmp)
sigma <- sd(tmp)
lb <- mu - 3 * sigma
hb <- mu + 3 * sigma
tmp[tmp<lb] <- lb
tmp[tmp>hb] <- hb
boxplot(tmp)
##月消费额数据绘制直方图
hist(data$exp, breaks = 40, main="histogram of expenditure", xlab="expenditure")
##对数转换，绘制直方图
data$log_expd <- log(tmp+1)
hist(data$log_expd, breaks=20, main="histogram of log expenditure", xlab="log expenditure")
##多个字段转换
data <- transform(data, log_call=log(call+1), log_traffic=log(traffic+1))
head(data)
#######缺失值填补#########
##每列非缺失值数量
colSums(!is.na(data))
##按照缺失值数量删除观测，并且观察非缺失值数量
colSums(!is.na(data[!rowSums(is.na(data))>1,]))
##0值填补
data$IMEI[is.na(data$IMEI)]<-0
##定义众数函数
Mode <- function(v){
    uv <- unique(v)
    vtab<-tabulate(match(v,uv))
    uv[vtab==max(vtab)]
}
##众数填补
data$state[is.na(data$state)]<-Mode(data$state)[1]
##观察非缺失值数量
colSums(!is.na(data))
########二值化###########
##通过类型转换二值化
data$has_IMEI<-as.numeric(as.logical(data$IMEI))
head(data)
##通过函数二值化
library(biclust,quietly = T)
head(binarize(data$IMEI,threshold=0),n=40)
########哑编码###########
##生成哑变量
state.f<-factor(data$state)
state.d<-model.matrix(~state.f)
data0<-cbind(data,state.d)
head(data0)
########标准化###########
##对经过对数转换的变量标准化
data1 <- transform(data, s_expd=scale(log_expd), s_call=scale(log_call),
                   s_traffic=scale(log_traffic))
head(data1,2)
##极值标准化
minmaxscale<-function(x) {(x-min(x))/(max(x)-min(x))}
cbind(minmaxscale(data$log_expd),minmaxscale(data$log_call),
      minmaxscale(data$log_traffic))[1:5,]
########规范化###########
##自定义函数进行规范化
normalizer<-function(x){x/sqrt(sum(x^2))}
head(t(apply(data1[,15:17],1,normalizer)))
####################特征构造-----------------------------------------------------
##构造新特征：用网时长
data1$duration <- as.numeric(as.Date("2015-01-01")-as.Date(data1$join_time))/30
##构造新特征：实际流量单价
data1$unit_price <- data1$expenditure/data1$traffic
##构造新特征：TAC号
data1$TAC <- substr(data1$IMEI,1,6)
head(data1,2)
####################特征抽取-----------------------------------------------------
##主成分分析
pca<-princomp(~s_expd+s_call+s_traffic,data=data1,cor=T)
head(pca$scores)
##主成分能解释的数据变异率
summary(pca,loadings=T)
##查看主成分解释的变异量
pca$sdev^2
##用于投影的方向
t(pca$loadings[1:3,])
##有两条记录的测试集进行pca转换
predict(pca,newdata=data.frame(s_expd=c(-1,1),s_call=c(0,2),s_traffic=c(1,3)))
##LDA将数据投影到一维
lda0<-lda(churn~s_expd+s_call+s_traffic,data=data1)
plda<-predict(lda0,newdata=data1[,c("s_expd","s_call","s_traffic")])
head(plda$x)
##计算投影方向
t(lda0$scaling)
##转换新数据
predict(lda0,newdata=data.frame(s_expd=c(-1,1),s_call=c(0,2),s_traffic=c(1,3)))$x
##预测正确率
mean( plda$class == data1$churn )
####################特征选择-----------------------------------------------------
########过滤法###########
library(caret,quietly = T)
##方差过滤
nearZeroVar(data1[,c("vip","has_IMEI")],freqCut=90/10 ,saveMetrics=T)
##各字段方差
c(var(data1$vip),var(data1$has_IMEI))
##基于相关性过滤，按照数据类型划分变量
x_categorical<-data1[,c("vip","state","has_IMEI")]
x_continuous<-data1[,c("s_expd","s_call","s_traffic","duration")]
y_categorical<-data1[,c("churn")]
y_continuous<-data1[,c("s_expd")]
##卡方检验
lapply(x_categorical,function(x){
    ch<-chisq.test(x,y_categorical, simulate.p.value = TRUE)
    c(ch$statistic,ch$p.value)
})
##ANOVA
lapply(x_continuous,function(x){
    an<-anova(lm(x~y_categorical))
    c(an[1,4],an[1,5])
})
##相关系数
lapply(x_continuous,function(x){
    cor<-cor.test(x,y_continuous,method="pearson")
    c(cor$statistic,cor$estimate,cor$p.value)
})
########包装法###########
##后向法筛选特征
glmdata<-cbind(y_categorical,x_continuous)
lg<-glm(y_categorical~.,data=glmdata)
glm_back<-step(lg,direction = "backward")
########集成法###########
library(randomForest,quietly=T, warn.conflicts=F)
set.seed(21)
rf<-randomForest(y_categorical~.,data=glmdata,importance=T,ntree=10)
rf$importance

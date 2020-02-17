##################第十一章	分类器入门：最近邻域与贝叶斯网络#######################

####################最近邻域-------------------------------------------------------
##加载数据集
orgData<-read.csv("date_data2.csv")
y<-orgData[,c("Dated")]
x<-orgData[,c(1,2,3,4)]

###定义极差标准化函数标准化数据
normalize <- function(x) { 
    return((x - min(x)) / (max(x) - min(x)))
}
x<- as.data.frame(lapply(x, normalize))

data<-cbind(y,x)
data$y<-as.factor(data$y)
summary(data)

#构建训练集和测试集
set.seed(110)
select<-sample(1:nrow(data),length(data$y)*0.7)
train=data[select,-1]
test=data[-select,-1]
train.y=data[select,1]
test.y=data[-select,1]

#使用KNN算法，设定k=10
library(class)
y_hat<-knn(train = train,test = test,cl=train.y,k=10)

#模型验证，将预测的类别与实际类别对比。
accuracy.knn<-sum(y_hat==test.y)/length(test.y)
accuracy.knn
agreement_KNN<- y_hat==test.y
table(agreement_KNN)

#召回率和精确度
require(gmodels)
t<-CrossTable(x =test.y, y = y_hat,prop.chisq=FALSE)
t$prop.row[2,2]#召回率
t$prop.col[2,2]#精确度

ROC<-data.frame()
for (i in seq(from =1,to =15,by =1)){
    y_hat<-knn(train = train,test = test,cl=train.y,k=i)
    require(gmodels)
    t<-CrossTable(x =test.y, y = y_hat,prop.chisq=FALSE)
    accuracy.knn<-sum(y_hat==test.y)/length(test.y)#准确率
    #t$prop.row[2,2]#召回率
    #t$prop.col[2,2]#精确度、命中率
    out<-data.frame(i,accuracy.knn,t$prop.row[2,2],t$prop.col[2,2])
    ROC<-rbind(ROC,out)
}
names(ROC)<-c("n","accuracy","Recall","Precision")

####################朴素贝叶斯-----------------------------------------------------
library(dfexplore)
library(e1071)
#数据集用之前加载的orgData
#提取如下字段进行建模
orgData1<-orgData[,c("income_rank","attractive_rank","assets_rank","Dated")]
orgData1<-as.data.frame(lapply(orgData1, as.factor))

#构建训练集和测试集
set.seed(110)
select<-sample(1:nrow(orgData1),nrow(orgData1)*0.6)
train=orgData1[select,]
test=orgData1[-select,]

#使用navie.bayes函数，建立朴素贝叶斯模型
bn = naiveBayes(Dated~income_rank+attractive_rank+assets_rank, train)

#使用数据进行预测
pred = predict(bn, test)
table(pred,test$Dated)
accuracy.bn<-sum(pred==test$Dated)/length(test$Dated)
accuracy.bn


####################TAN贝叶斯-----------------------------------------------------

library(bnlearn)
train=orgData1[select,]
test=orgData1[-select,]

##通过tree.bayes函数构建TAN分类器，属性变量集root节点设置为attractive_rank。
tan = tree.bayes(x=train, training="Dated",root="attractive_rank")
pre=predict(tan,test,prob = FALSE)
table(pre,test$Dated)

##将数据集中属性变量(自变量)的其他两个变量设置为根节点进行预测
tan1 = tree.bayes(x=train, training="Dated",root="income_rank")
pre1=predict(tan1,test,prob = FALSE)
table(pre1,test$Dated)

tan2 = tree.bayes(x=train, training="Dated",root="assets_rank")
pre2=predict(tan2,test,prob = FALSE)
table(pre2,test$Dated)

##画出树形网络图
plot(tan1,main='income_rank为根节点')
plot(tan2,main='assets_rank为根节点')

















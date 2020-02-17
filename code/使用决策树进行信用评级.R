##################第九章 使用决策树进行信用评级###############################################

##加载数据集
accepts<-read.csv("accepts.csv")
accepts$bad_ind<-as.factor(accepts$bad_ind)
names(accepts)
accepts=accepts[,c(3,7:24)]
##生成训练集测试集
set.seed(10)
select<-sample(1:nrow(accepts),length(accepts$bad_ind)*0.7)
train=accepts[select,]
test=accepts[-select,]
summary(train$bad_ind)
train<-na.omit(train)
#############用包C50建立C5.0决策树------------------------------------------------------------
library(C50)
##C50的参数设定
tc<-C5.0Control(CF=0.25,winnow=F,noGlobalPruning=F,minCases =20)
##C5.0函数进行建模
model <- C5.0(bad_ind ~.,data=train,rules=F,control =tc)
plot( model )
##将rules参数设定位T，输出决策树的规则
model <- C5.0(bad_ind~.,data=train,rules=T,control =tc)
summary(model)

#############用rpart包构建CART决策树----------------------------------------------------------
library(rpart)
##CART算法的参数设定
tc <- rpart.control(minsplit=20,maxdepth=10,xval=10,cp=0)
##建模
rpart.mod=rpart(bad_ind~.,data=train,method="class",parms=list(split="gini"),control=tc)
##查看复杂度与代价的关系
rpart.mod$cp
##函数绘制代价/复杂度参数图
plotcp(rpart.mod)
##为简化树结构，设定分割次数为7(根节点为8个)，相应的CP值为0.0094991364
rpart.mod.pru<-prune(rpart.mod,cp=0.0094991364)
##绘制树状图
library(rpart.plot)
rpart.plot(rpart.mod.pru,branch=1, extra=106, under=TRUE, faclen=0,cex=0.8, main="决策树")
##产生预测值
##type=‘class’为输出类别
preType<-predict(rpart.mod.pru,test,type="class")
##type=’prob’为输出概率
preProb<-predict(rpart.mod.pru,test,type="prob")

#############组合算法--------------------------------------------------------------------------
##加载并查看鸢尾花数据集
data("iris")
head(iris[,-5])
head(iris[,5],n=10)
#######袋装法#######
library(ipred)
bgc_model<-ipred::bagging(Species~.,data=iris,coob=T)
bgc_predict<-predict(bgc_model,iris)
table(bgc_predict,iris$Species)
##通过交叉验证计算模型准确率
errorest(Species~.,data=iris,model=ipred::bagging,estimator="cv")
#######随机森林#####
library(randomForest)
set.seed(21)
rf<-randomForest(Species~.,data=iris,importance=T,mtry=1)
rf$confusion
#######AdaBoost#####
library(adabag)
boost<-adabag::boosting(Species~.,data=iris)
table(iris[,5],predict(boost,iris)$class)
#####提升树与GBDT###
library(gbm)
##建模
gbrt<-gbm(Species~.,data=iris,distribution="multinomial",cv.folds=10,
          n.trees=300,shrinkage=0.01,interaction.depth=5)
best.iter<-gbm.perf(gbrt,method="cv")
##预测
pre<-matrix(predict(gbrt,iris,n.trees=best.iter), ncol=3)
##混淆矩阵
table(max.col(pre),iris$Species)
##错误率
sum(max.col(pre)!=as.numeric(iris$Species))/nrow(iris)
##自变量重要性
summary(gbrt,n.trees=best.iter)
##绘制边际图
par(mfrow=c(2,2))
plot(gbrt,1,best.iter)
plot(gbrt,2,best.iter)
plot(gbrt,3,best.iter)
plot(gbrt,4,best.iter)
par(mfrow=c(1,1))
#####Xgboosting简介###
library(xgboost)
lb <- as.numeric(iris$Species) - 1
set.seed(1)
bst<-xgboost(data=as.matrix(iris[,-5]),label=lb,max_depth=4,eta=0.5,nthread=2,nrounds=10,
             subsample=0.5,objective="multi:softprob",num_class=3)
pred <- matrix(predict(bst,as.matrix(iris[,-5])), ncol=3, byrow=TRUE)
table(max.col(pred),iris$Species)



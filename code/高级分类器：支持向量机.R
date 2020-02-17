##################第十二章	*高级分类器：支持向量机################################

##线性可分支持向量机---------------------------------------------------------------

##构造数据
x1=c(0,1,0,1)
x2=c(0,0,1,1)
y=c(1,1,0,0)
data1=as.data.frame(cbind(x1,x2,y))
data1$y=as.factor(data1$y)
##线性可分支持向量机建模
library(e1071)
svm.mod=svm(y~.,kernel="linear",data=data1)
##plot输出图示
plot(svm.mod,data1, x2~x1,xlim=c(-0.2,1.2),ylim=c(-0.2,1.2))

##线性支持向量机-------------------------------------------------------------------
##构造数据
x1=c(0,1,0,1,0.5)
x2=c(0,0,1,1,0)
y=c(1,1,0,0,0)
data2=as.data.frame(cbind(x1,x2,y))
data2$y=as.factor(data2$y)
##线性支持向量机建模
library(e1071)
svm.mod=svm(y~.,kernel="linear",cost=4,data=data2)
##plot输出图示
plot(svm.mod,data2, x2~x1,xlim=c(-0.2,1.2),ylim=c(-0.2,1.2))

##非线性支持向量机-----------------------------------------------------------------
x1=c(0,1,0,1)
x2=c(0,0,1,1)
y=c(0,1,1,0)
data3=as.data.frame(cbind(x1,x2,y))
data3$y=as.factor(data3$y)
##线性可分支持向量机建模，选择惩罚参数为3，二阶多项式degree为2，
##系数为1，gamma为1进行建模
svm.mod=svm(y~.,kernel="polynomial",data=data3,cost=1,gamma=1,coef0=1,degree=2)
##plot输出图示
plot(svm.mod,data2, x2~x1,xlim=c(-0.2,1.2),ylim=c(-0.2,1.2))

##支持向量机案例-------------------------------------------------------------------

##加载数据集
data_svm<-read.csv("date_data2.csv")
data_svm[,4:5]<-as.data.frame(lapply(data_svm[,4:5], as.factor))
#构建训练集和测试集
set.seed(110)
select<-sample(1:nrow(data_svm),length(data_svm$Dated)*0.6)
train=data_svm[select,]
test=data_svm[-select,]
train.y=data_svm[select,5]
test.y=data_svm[-select,5]
##支持向量机建模,选择径向基核函数，惩罚项C设定为0.1，gamma值设定为0.4，
##考虑到数据集比较小，交叉验证数设定为3。
svm.mod<-svm(Dated~income+attractive+assets+edueduclass,kernel="radial",
             data=train,probability=TRUE,cost=0.1,gamma=0.4,cross=3)
##测试数据集进行预测，输出精准率和召回率
require(gmodels)
y_hat<-predict(svm.mod,test[,1:4],probability=F)
t<-CrossTable(x =test$Dated, y = y_hat, prop.chisq=FALSE)
#召回率
t$prop.row[2,2]
#准确度
t$prop.col[2,2]

##支持向量机参数调整---------------------------------------------------------------
require(gmodels)
ROC<-data.frame()
for (i in seq(from =0.1,to =10,by =0.5)){
    for (j in seq(from =0.1,to =1,by=0.1)){
        svm.mod<-svm(Dated~income+attractive+assets+edueduclass,kernel="radial",
                     data=train,probability=TRUE,cost=i,gamma=j,cross=3)
        y_hat=predict(svm.mod,test,probability=F)
        t<-CrossTable(x =test.y, y = y_hat,prop.chisq=FALSE)
        accuracy<-sum(y_hat==test.y)/length(test.y)#准确率
        out<-data.frame(i,j,accuracy,t$prop.row[2,2],t$prop.col[2,2])
        ROC<-rbind(ROC,out)
    }  }
names(ROC)<-c("cost","gamma","accuracy","Recall","Precision")

##以召回率为首要指标，精准率为次要指标，输出最优参数
max_recall=ROC[ROC$Recall==max(ROC$Recall),]
max_rp=max_recall[max_recall$Precision==max(max_recall$Precision),]
max_rp
##选取cost=6.1，gamma=1为最优参数重新建模
svm.mod<-svm(Dated~income+attractive+assets+edueduclass,kernel="radial",
             data=train,cost=6.1,gamma=1,cross=3)




























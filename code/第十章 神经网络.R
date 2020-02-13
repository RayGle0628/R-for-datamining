###################第十章 神经网络#####################################

################nnet包的单隐层BP网络-----------------------------------
##----------------------加载数据和数据清洗-----------------------------
#清理环境变量，加载Sonar, Mines vs. Rocks数据
rm(list=ls())
library(mlbench)
data(Sonar)
##重新定义因子级别为0,1，其中岩石是级别1，金属是级别0。
levels(Sonar$Class)<-c(0,1)
##随机抽样，建立训练集和测试集,抽样比例是7:3
set.seed(1221)
select<-sample(1:nrow(Sonar),nrow(Sonar)*0.7)
train<-Sonar[select,]
test<-Sonar[-select,]

##对数据进行中心标准化
train[,1:60]=scale(train[,1:60])
test[,1:60]=scale(test[,1:60])

##---------------------使用nnet包实现BP神经网络------------------------
library(nnet)
mynnet<-nnet(Class~., linout =F,size=14, decay=0.0076, maxit=200, 
             data = train)

##---------------------结果评估----------------------------------------
##使用测试集预测
out<-predict(mynnet, test) 
out[out<0.5]=0
out[out>=0.5]=1
##计算准确率
rate<-sum(out==test$Class)/length(test$Class)

##分别在训练集和测试集上预测，并且绘制ROC曲线
##这里我们构建绘制ROC曲线函数方便日后调用
##注意此函数会在调用的数据框里增加列
ROC<-function(model,train,test,objcolname,ifplot=TRUE){
    library(ROCR,quietly = T)
    train$p<-predict(model, train) 
    test$p<-predict(model, test) 
    
    predTr <- prediction(train$p, train[,objcolname])
    perfTr <- performance(predTr,"tpr","fpr")
    
    predTe <- prediction(test$p, test[,objcolname])
    perfTe <- performance(predTe,"tpr","fpr")
    
    tr_auc<-round(as.numeric(performance(predTr,'auc')@y.values),3)
    te_auc<-round(as.numeric(performance(predTe,'auc')@y.values),3)
    
    if(ifplot==T){
        plot(perfTr,col='green',main="ROC of Models")
        plot(perfTe, col='black',lty=2,add=TRUE);
        abline(0,1,lty=2,col='red')
        
        tr_str<-paste("Tran-AUC:",tr_auc,sep="")
        legend(0.3,0.45,c(tr_str),2:8)
        te_str<-paste("Test-AUC:",te_auc,sep="")
        legend(0.3,0.25,c(te_str),2:8)
    }
    auc<-data.frame(tr_auc,te_auc)
    return(auc)
}
ROC(model=mynnet,train=train,test=test,objcolname="Class",ifplot=T)

##--------------------------调参-----------------------------------------

##输入数据的预测变量必须是二分类的。且所有变量只包含模型输入输出变量。
##调参时如果变量过多size不宜过大。
##构建调参函数network()。
network<-function(formula,data,size,adjust,decay=0,maxit=200,scale=TRUE,
                  samplerate=0.7,seed=1,linout=FALSE,ifplot=TRUE){
    library(nnet)
    ##规范输出变量为0,1
    yvar<-colnames(data)==(all.vars(formula)[1])
    levels(data[,yvar])<-c(0,1)
    ##抽样建立训练集和测试集
    set.seed(seed)
    select<-sample(1:nrow(data),nrow(data)*samplerate)
    train=data[select,]
    test=data[-select,]
    ##根据给定判断进行标准化
    if(scale==T){
        xvar<-colnames(data)!=(all.vars(formula)[1])
        train[,xvar]=scale(train[,xvar])
        test[,xvar]=scale(test[,xvar])
    }
    ##循环使用nnet训练调参
    obj<-eval(parse(text = adjust))
    auc<-data.frame()
    for(i in obj){
        if(adjust=="size"){
            mynnet<-nnet(formula,size=i,linout=linout,decay=decay,
                         maxit=maxit,trace=FALSE,data=train)
        }
        else if(adjust=="decay"){
            mynnet<-nnet(formula,size=size,linout=linout,decay=i,
                         maxit=maxit,trace=FALSE,data=train)
        }
        ##调用之前的ROC()得到对应参数的AUC值
        objcolname<-all.vars(formula)[1]
        auc0<-ROC(model=mynnet,train=train,test=test,
                  objcolname=objcolname,ifplot=F)
        ##输出指定参数不同值对应的数据框
        out<-data.frame(i,auc0)
        auc<-rbind(auc,out)
    }
    
    names(auc)<-c(adjust,"Train_auc","Test_auc")
    if(ifplot==T){
        library(plotrix)
        twoord.plot(auc[,1],auc$Train_auc,auc[,1],auc$Test_auc,lcol=4,
                    rcol=2,xlab=adjust,ylab="Train_auc",
                    rylab="Test_auc",type=c("l","b"),lab=c(15,5,10))
    }
    return(auc)
}
auc<-network(Class~.,data=Sonar,size=1:16,adjust="size",
             decay=0.0001,maxit=200,scale=T)
auc<-network(Class~.,data=Sonar,size=14,adjust="decay",
             decay=c(0,seq(0.0001,0.01,0.0003)),maxit=200)

################RSNNS包的BP网络和RBF网络-------------------------------

##-----------------------BP网络----------------------------------------
rm(list=ls())
library(mlbench)
library(RSNNS)
library(ROCR)
data(Sonar)
##乱序
Sonar<-Sonar[sample(1:nrow(Sonar),nrow(Sonar)),]
##定义输入输出
SonarValues<-Sonar[,1:60]
SonarTargets<-as.numeric(Sonar[,61])-1
##数据分割为训练集和测试集
Sonar<-splitForTrainingAndTest(SonarValues,SonarTargets,ratio=0.3)
##标准化
Sonar<-normTrainingAndTestSet(Sonar)

##模型训练
mymlp<-mlp(Sonar$inputsTrain,Sonar$targetsTrain,size=c(4,2),
           learnFuncParams=0.2,maxit=500)
##使用测试集预测
out<-predict(mymlp, Sonar$inputsTest) 
out[out<0.5]=0
out[out>=0.5]=1
##计算准确率
rate<-sum(out==Sonar$targetsTest)/length(Sonar$targetsTest)
##分别在训练集和测试集上预测
tr_mlp<-predict(mymlp,Sonar$inputsTrain)
te_mlp<-predict(mymlp,Sonar$inputsTest) 
##绘制ROC曲线
tr_pred<-prediction(tr_mlp,Sonar$targetsTrain)
tr_perf<-performance(tr_pred,"tpr","fpr")
te_pred<-prediction(te_mlp,Sonar$targetsTest)
te_perf<-performance(te_pred,"tpr","fpr")

plot(tr_perf,col='green',main="ROC of Models")
plot(te_perf, col='black',lty=2,add=TRUE);
abline(0,1,lty=2,col='red')


tr_auc<-round(as.numeric(performance(tr_pred,'auc')@y.values),3)
tr_str<-paste("Train-AUC:",tr_auc,sep="")
legend(0.3,0.45,c(tr_str),2:8)

te_auc<-round(as.numeric(performance(te_pred,'auc')@y.values),3)
te_ste<-paste("Test-AUC:",te_auc,sep="")
legend(0.3,0.25,c(te_ste),2:8)

##------------------------------RBF网络---------------------------------
myrbf<- rbf(Sonar$inputsTrain, Sonar$targetsTrain, size=100, 
            maxit=1000,linOut=F)

##使用测试集预测
out<-predict(myrbf, Sonar$inputsTest) 
out[out<0.5]=0
out[out>=0.5]=1
##计算准确率
rate<-sum(out==Sonar$targetsTest)/length(Sonar$targetsTest)
##分别在训练集和测试集上预测
tr_rbf<-predict(myrbf,Sonar$inputsTrain)
te_rbf<-predict(myrbf,Sonar$inputsTest) 
##绘制ROC曲线
tr_pred<-prediction(tr_rbf,Sonar$targetsTrain)
tr_perf<-performance(tr_pred,"tpr","fpr")
te_pred<-prediction(te_rbf,Sonar$targetsTest)
te_perf<-performance(te_pred,"tpr","fpr")

plot(tr_perf,col='green',main="ROC of Models")
plot(te_perf, col='black',lty=2,add=TRUE);
abline(0,1,lty=2,col='red')


tr_auc<-round(as.numeric(performance(tr_pred,'auc')@y.values),3)
tr_str<-paste("Train-AUC:",tr_auc,sep="")
legend(0.3,0.45,c(tr_str),2:8)

te_auc<-round(as.numeric(performance(te_pred,'auc')@y.values),3)
te_ste<-paste("Test-AUC:",te_auc,sep="")
legend(0.3,0.25,c(te_ste),2:8)




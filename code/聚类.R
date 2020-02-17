##################第十四章 聚类################################################################

####################层次聚类-------------------------------------------------------------------
##加载数据集
Data<-read.csv("cities_10.csv")
CluData<-Data[,2:10]
##生成距离矩阵
DisMatrix<-dist(CluData,method = "euclidean")
##进行层次聚类，method参数选择瓦德法
CluR<-hclust(d=DisMatrix,method="ward.D")
##使用plot函数绘制聚类结果的树形图
plot(CluR,labels=Data[,1])

###################KMEANS聚类-------------------------------------------------------------------
##先进行因子分析
Data<-read.csv("profile_telecom.csv")
library(psych)
fc1<-principal(Data[,-1],nfactors=3,rotate="varimax",covar=F,score=TRUE)
fc1
##用三个因子的因子得分进行聚类
new=as.data.frame(fc1$scores)
names(new)=c("wei_web","msg","call")
##需要指定类簇个数
library(fpc)
pamk.result=pamk(new,krange=2:5)
pamk.result$nc
##进行KMEANS聚类，参数iter.max=100表示最大迭代次数100，centers即最优值2类
kmd=kmeans(new,centers=pamk.result$nc,iter.max=100)
##使用cluster命令输出聚类的划分结果
new$cluster<-kmd$cluster
##使用center命令输出聚类的类簇中心数据作为解释的依据
kmd$center
##使用ggplot2包进行绘制
new$type = kmd$cluster
library(ggplot2)
p <- ggplot(new,aes(wei_web))
p + geom_histogram(position = 'identity',alpha=0.5,aes(y = ..density.., fill = factor(type))) + 
    stat_density(geom = 'line',position = 'identity',aes(colour = factor(type)))
p <- ggplot(new,aes(msg))
p + geom_histogram(position = 'identity',alpha=0.5,aes(y = ..density.., fill = factor(type))) + 
    stat_density(geom = 'line',position = 'identity',aes(colour = factor(type)))
p <- ggplot(new,aes(call))
p + geom_histogram(position = 'identity',alpha=0.5,aes(y = ..density.., fill = factor(type))) + 
    stat_density(geom = 'line',position = 'identity',aes(colour = factor(type)))

#####################密度聚类-------------------------------------------------------------------
ds<-dbscan(new,eps=1.2,MinPts=40,scale=F,showplot=TRUE)
ds$cluster

#####################聚类模型评估---------------------------------------------------------------
##轮廓系数
library(cluster)
result=list()
for (i in 2:5){
    kmd=kmeans(new,centers=i)
    sil=silhouette(kmd$cluster,dist(new))
    result[[paste('k=',i,sep='')]]=mean(sil[,'sil_width'])
}
result
##RMSSTD
rmsstd=list()
for (k in 2:5){
    kmd=kmeans(new,centers=k)
    new$type=kmd$cluster
    sdv=NULL
    for(i in 1:(length(new)-1))
    {
        for(j in 1:k)
        {
            sdv=c(sdv,sd(new[new$type==j,i]))
        }
    }
    variables=1:(length(new)-1)
    sdv.matrix=matrix(sdv,nrow=length(variables),byrow=T)
    rmsstd[[paste('k=',k,sep='')]]=sqrt(sum(apply(sdv.matrix,1,sum)^2)/length(variables))
}
rmsstd

##################GMM模型--------------------------------------------------------------------------
##读取数据并且降维
data(iris)
library(psych)
pr<-principal(iris[,-5],nfactors=2,rotate='none')
pr$loadings
##通过散点图展现数据分布
x<-pr$scores
class<-iris[,5]
clPairs(x,class,verInd=2,horInd=1)
# plot(pr$scores[,1],pr$scores[,2],pch=(as.numeric(as.factor(iris[,5]))+1),
#      col=as.factor(iris[,5]))
##GMM建模
library(mclust,quietly = T)
Bic<-mclustBIC(x)
plot(Bic)
summary(Bic)
##选择VVV3建模
mod1<-Mclust(x,G=3,modelNames='VVV')
summary(mod1)
plot(mod1,what='classification')
##EDDA方法
mod2<-MclustDA(x,class,modelType='EDDA')
summary(mod2)
plot(mod2,what='classification')
##MclustDA方法
mod3<-MclustDA(x,class)
summary(mod3)
plot(mod3,what='classification')
##交叉验证
unlist(cvMclustDA(mod2, nfold = 10)[2:3])
##维度降低
y<-iris[,1:4]
mod4<-MclustDA(y,class,modelType = 'EDDA')
mod4dr<-MclustDR(mod4)
summary(mod4dr)
plot(mod4dr,what="boundaries",ngrid=200)

#####################客户分群--------------------------------------------------------------------

setwd("D:\\R_edu\\data")
vdata<-read.csv("profile_bank.csv")
#数据属性说明：
#########################################################################
#方法一：发现异常个体
#加载包，查看数据缺失情况
#library(dfexplore)
#dfplot(vdata)
summary(vdata)
hist(vdata$CNT_ATM)
#进行kmeans聚类前，需要进行标准化处理，这里使用scale函数进行标准化
data=scale(vdata[,c(2:5)])
##进行变量压缩
library(psych)
fc1<-principal(data,nfactors=4,rotate="none",covar=F,score=TRUE)
fc1$Vaccounted
fc1<-principal(data,nfactors=3,rotate="varimax",covar=F,score=TRUE)
fc1
factos<-(fc1$scores)
names(factos)<-c("ATM_POS","TBM","CSC")

kmd=kmeans(factos,centers=4,iter.max=100)
#kmeans输出详解
#cluster:样本归属群号的向量
#centers:聚类中心的矩阵，每一条记录，代表相应聚类的中心点
#totss:所有数据的平方和
#withinss:群内样本点进行scale(x,scale=F)后的平方和
#tot.withinss:对所有群withinss的汇总
#betweenss:totss与tot.withinss的差
#size:每个群中的样本个数
#iter:迭代的次数
#ifault:指示可能的算法问题（专家使用），比如当一些点非常靠近的时候，算法也许不会收敛，就会返回ifault=4
pie(table(kmd$cluster))

#########################################################################
#方法二：客户分群
#############步骤1:标准化
data=scale(vdata[,c(2:5)])
#############步骤2:进行基于秩分析的正态分布转换
hist(data[,4])
library(GenABEL)
data<-apply(data,2, rntransform)
hist(data[,1])
#############步骤3:进行变量压缩
library(psych)
fc1<-principal(data,nfactors=3,rotate="varimax",covar=F,score=TRUE) 
fc1
factos<-data.frame((fc1$scores))
names(factos)<-c("ATM_POS","TBM","CSC")
#############步骤4:kmeans聚类
kmd=kmeans(factos,centers=5,iter.max=100)
pie(table(kmd$cluster))

#为什么聚类个数为4个？
#使用fpc包中的pamk函数，使用轮廓系数(Silhouette Coefficient)确定聚类个数
library(fpc)
set.seed(100)
select<-sample(1:nrow(factos),500)#必须抽样，因为pamk函数很耗时
test=factos[select,]
pamk.result=pamk(test)
pamk.result$nc
#这里使用stats包中的kmeans函数
kmd=kmeans(factos,centers=pamk.result$nc,iter.max=100)
pie(table(kmd$cluster))
########################################################################
#使用描述性方法了解每组的特征
#kmd=kmeans(factos,centers=4,iter.max=100)
anaData<-data.frame(cbind(factos,type=kmd$cluster))

tapply(anaData$ATM_POS, anaData$type, mean)
tapply(anaData$TBM, anaData$type, mean)
tapply(anaData$CSC, anaData$type, mean)

######################################################
#12.3.2使用决策树做聚类后客户画像
#方法1：C50
anaData<-data.frame(cbind(factos,type=kmd$cluster))
set.seed(10)
select<-sample(1:nrow(anaData),1000)
sample<-anaData[select,]

library(C50)
#请注意，R中的C50包比较新，存在一些问题，比如遇到缺失值、字符类型变量会报错“c50 code called exit with value 1”
##建模
sample$type<-as.factor(sample$type)
tc<-C5.0Control(subset =F,CF=0.25,winnow=F,noGlobalPruning=F,minCases =floor(nrow(sample)*0.05))
#形式1
#model <- C5.0(bad_ind ~fico_score,data=train,rules=F,control =tc)
#形式2
x=sample[,1:3]
y=sample[,4]
names(x)<-c("ATM_POS","TBM","CSC")
model <- C5.0(x=x,y=y,rules=F,control =tc )
summary( model )
#图形展示
plot(model)
C5imp(model)
#生成规则
rule <- C5.0(x=x,y=y,
             rules=T,control =tc ,trials=1)
summary( rule )


#方法2：CART
library(rpart)
tc <- rpart.control(minsplit=1000,minbucket=1000,maxdepth=10,xval=10,cp=0.005)
rpart.mod=rpart(type ~ATM_POS+TBM+CSC,data=anaData, 
                parms = list(split = "gini"),method="class",control=tc)


library(rpart.plot)
rpart.plot(rpart.mod,branch=1, extra=106, under=TRUE, faclen=0,
           cex=0.8, main="聚类描述")








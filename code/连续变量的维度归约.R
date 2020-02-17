##################第十三章 连续变量的维度归约######################################

####################主成分分析-----------------------------------------------------
##加载数据集
Data<-read.csv("profile_telecom.csv")
##使用协方差矩阵进行主成分分析
pr1<-princomp(Data[,-1],cor=TRUE)
summary(pr1,loadings=TRUE)
##输出碎石图
screeplot(pr1,type="lines")
##主成分得分
pr1$scores

##使用包psych的principal函数也可以进行主成份分析
library(psych)
pr2<-principal(Data[,-1],nfactors=3,rotate="none",covar=F,score=TRUE)
pr2
##主成分得分
pr2$scores

###################因子分析---------------------------------------------------------
##使用主成分分析时加载过的数据集和包
fc1<-principal(Data[,-1],nfactors=3,rotate="varimax",covar=F,score=TRUE)
fc1
##因子得分
head(fc1$scores)
##使用plot绘制双标图以可视化因子分析结果
plot(fc1$scores)
abline(v=0,h=0)

###################奇异值分解-------------------------------------------------------
##生成原始数据的矩阵A
A<-matrix(c(5,5,0,5,
            5,0,3,4,
            3,4,0,3,
            0,0,5,3,
            5,4,4,5,
            5,4,5,5),nrow=6,byrow=T)
##进行分解，使用str函数查看分解的结果，d表示奇异值，u表示左奇异向量，v表示右奇异向量。
A.svd<-svd(A)
str(A.svd)
##类似主成份分析中的碎石图
plot(A.svd$d,type='b')
##选取K=2，即提取前两个较大奇异值d和所对应的左右奇异向量u和v
d<-A.svd$d[1:2]
u<-A.svd$u[,1:2]
v<-A.svd$v[,1:2]
##对于一个新用户bob来说，其评分向量为
bob<-c(5,5,0,0,0,5)
##计算bob在右奇异矩阵(用户矩阵)中的得分
bob.v<-bob%*%u%*%solve(diag(d))
bob.v

###################对应分析-------------------------------------------------------------
#加载需要的
library(mice)
library(MASS)
setwd("D:/R_edu/data")
#加载数据集，使用商场客户调研数据
orgData<-read.csv("shopping.csv")
#属性说明：

#对应分析的基本思想是将一个联列表的行和列中各元素的比例结构以点的形式在较低维的空间中表示出来

##不同学历对消费金额的影响
#1.建立列联表
ctb<-table(orgData$edu,orgData$average)
#2.检验两个维度的相关性
chisq.test(ctb)
#可以看到，拒绝原假设，说明两个维度之间相关
#将列联表转换成data.frame
df0<-matrix(ctb,nrow=4,byrow=F)
rownames(df0)<-dimnames(ctb)[[1]]
colnames(df0)<-dimnames(ctb)[[2]]
#3.进行对应分析
cal<-corresp(df0,nf=2)
#Row scores和Column scores分别为行和列对应的主成份
#4.画出对应关系
biplot(cal)
abline(v=0,h=0,lty=3)
#结论:
#最高消费群体为硕士学历,其次为大专学历群体。商场可以通过引进这两类群体偏好的商品以提高客单价。

##客户购物目的不同对消费金额的影响，过程同上
ctb<-table(orgData$purpose,orgData$average)
chisq.test(ctb)
df0<-matrix(ctb,nrow=5,byrow=F)
rownames(df0)<-dimnames(ctb)[[1]]
colnames(df0)<-dimnames(ctb)[[2]]
cal<-corresp(df0,nf=2)
biplot(cal)
abline(v=0,h=0,lty=3)

###################多维尺度分析---------------------------------------------------------

##使用MDS对城市的经济情况进行演示(常见数据框)
setwd("D:\\R_edu\\data")
#数据属性说明：
#AREA	省份
#X1	GDP
#X2	人均GDP
#X3	工业增加值
#X4	第三产业增加值
#X5	固定资产投资
#X6	基本建设投资
#X7	社会消费品零售总额
#X8	海关出口总额
#X9	地方财政收入
#####################################################################
#实践方法一：
vdata<-read.csv("cities_10.csv")
#############步骤1：要预先处理变量
#加载包，查看数据缺失情况
summary(vdata)
library(dfexplore)
dfplot(vdata)
CluData<-vdata[,2:10]
#############步骤2：对数据矩阵做中心化和标准化变换
CluData<-scale(CluData, center = TRUE, scale = TRUE) 
#############步骤3：降维做主成分分析
library(psych)
fc1<-principal(CluData,nfactors=2,rotate="varimax",covar=F,score=TRUE) 
fc1
factos<-(fc1$scores)
#############步骤4：MDS
DisMatrix<-dist(factos,method = "euclidean")
#method：聚类的方法
#（1）euclidean
#（2）maximum
#（3）manhattan
#（4）canberra
#（5）binary
#（6）minkowski
city3<-as.matrix(DisMatrix) #转化为矩阵
rownames(city3)<-vdata[,1]
#names(city3)<-vdata[,1]
citys<-cmdscale(city3,k=2) #计算MDS，为可视化，取前两个主坐标plot(citys[,1],citys[,2],type='n') #绘图
plot(citys[,1],citys[,2],type='n') 
text(citys[,1],citys[,2],vdata[,1],cex=.7)#标上城市名字


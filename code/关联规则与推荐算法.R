##################第十六章 关联规则与推荐算法######################################

####################关联规则-------------------------------------------------------
##加载数据集
library(arules)
##read.transaction函数是arules包中特有的载入关联规则数据的读取函数
tr <- read.transactions("bank.csv", format = "single", cols = c(1,2),sep=",")
##使用inspect命令查看读取的数据
inspect(tr)
##使用apriori函数进行关联规则建模
rules=apriori(tr,parameter=list(supp=0.1,conf=0.1,minlen=2,maxlen=10))
inspect(rules)
##将规则转换为数据框形式
rulesData=as(rules,"data.frame")
##规则排序
rules.st=sort(rules,by="lift",decreasing=T)
##进行可视化以辅助查看模型的效果
library(arulesViz)
plot(rules, shading="lift")
plot(rules, method="grouped")

####################序贯模型-------------------------------------------------------
##加载数据集
library(arulesSequences)
data=read.csv("bankS.csv")
##通过原数据物品列生成交易型对象数据
data1=data.frame(item=factor(data$ITEM))
tran=as(data1,"transactions")
##通过transactionInfo将事件得到ID和序列ID的值
transactionInfo(tran)$sequenceID=data$SID
transactionInfo(tran)$eventID=data$EID
##使用inspect命令查看生成的数据
inspect(tran)
##使用cspade函数进行序贯模型建模
##参数support表示最小支持度，maxlen表示序贯模式的最大个数限制
rule.seq = cspade(tran, parameter = list(support = 0.2,maxlen = 10))
inspect(rule.seq)
##支持度筛选大于0.5的模式
rules=rule.seq[which(rule.seq@quality$support>0.5)]
inspect(rules)
##按照支持度排序
rule.seq=sort(rule.seq,by="support",decreasing=T)
inspect(rule.seq)

####################序贯模型-------------------------------------------------------
##同现矩阵自定义函数
CooccurrenceM<-function(data=data,item=item,user=user){
    with(data,{
        item_name=unique(item)
        item_name=item_name[order(item_name)]
        n=length(item_name)
        temp=matrix(rep(0,n**2),ncol=n) 
        colnames(temp)=item_name;row.names(temp)=item_name
        list_temp=split(item,user)#按照用户分割数据
        #产生同现矩阵的上对角元素
        for (k in list_temp){
            for (i in k){
                for (j in k){
                    k=setdiff(k,i)
                    temp[i,j]=temp[i,j]+1
                }
            }
        }
        #补全下对角线矩阵
        final=temp+t(temp)
        diag(final)<-diag(final)/2
        return(final)
    })
}
##分母矩阵自定义函数
JaccardM<-function(data=data,item=item,user=user){
    with(data,{
        item_name=unique(item)
        item_name=item_name[order(item_name)]
        n=length(item_name)
        temp=matrix(rep(0,n**2),ncol=n) 
        colnames(temp)=item_name;row.names(temp)=item_name
        list_temp=split(user,item)#按照用户分割数据
        #产生item交集个数矩阵的上对角元素
        for (i in item_name){
            for (j in item_name){
                item_name=setdiff(item_name,i)
                temp[i,j]=length(union(list_temp[[i]],list_temp[[j]]))
            }
        }
        #补全下对角线矩阵
        final=temp+t(temp)
        diag(final)<-diag(final)/2
        return(final)
    })
}
##推荐函数
Recommedation<-function(user_vector,data,item=item,user=user,Jaccad=F)
{ 
    with(data,{
        CoocM=CooccurrenceM(data,item=item,user=user)
        if (Jaccad==F){
            result=as.vector(CoocM%*%user_vector)
            names(result)=names(user_vector)
            Recommed=result[as.logical(user_vector)!=as.logical(result)]
            item_list=Recommed[order(Recommed,decreasing=T)]
            return(item_list) 
        }
        if (Jaccad==T){
            JaccM=JaccardM(data,item=item,user=user)
            JaccSM=CoocM/JaccM
            result=as.vector(JaccSM%*%user_vector)
            names(result)=names(user_vector)
            Recommed=result[as.logical(U3)!=as.logical(result)]
            item_list=Recommed[order(Recommed,decreasing=T)]
            return(item_list) 
        }
    })
}
##创建并处理数据集
test=matrix(c(1,'101',5,
              1,'102',3,
              1,'103',2.5,
              2,'101',2,
              2,'102',2.5,
              2,'103',5,
              2,'104',2,
              3,'101',2,
              3,'104',4,
              3,'105',4.5,
              3,'107',5,
              4,'101',5,
              4,'103',3,
              4,'104',4.5,
              4,'106',4,
              5,'101',4,
              5,'102',3,
              5,'103',2,
              5,'104',4,
              5,'105',3.5,
              5,'106',4
),ncol=3,byrow=T
)
colnames(test)=c('CID','MID','score')
test=as.data.frame(test,stringsAsFactors =T)
test$score=as.numeric(test$score)
##建立物品的同现矩阵
CooccurrenceM(test,test$MID,test$CID)
##分母矩阵
JaccardM(test,test$MID,test$CID)
##用同现矩阵除以分母矩阵可以得到Jaccaed相似度矩阵
CooccurrenceM(test,test$MID,test$CID)/JaccardM(test,test$MID,test$CID)
##建立用户对物品的评分矩阵
user_item=reshape2::acast(test,CID~MID,value='score')
user_item[is.na(user_item)==T]=0
##矩阵计算推荐结果
U3=c(2,0,0,4,4.5,0,5)
names(U3)=c(101,102,103,104,105,106,107)
Recommedation(U3,test,item=test$MID,user=test$CID,Jaccad=F)
Recommedation(U3,test,item=test$MID,user=test$CID,Jaccad=T)












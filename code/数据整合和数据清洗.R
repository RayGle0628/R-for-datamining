##################第五章 数据整合和数据清洗#########################################

####################数据整合--------------------------------------------------------
#####SQL语句#####
library(sqldf)
sale <- read.csv("sale.csv")
##选择字段
sqldf("select year,market,sale,profit from sale")
##选择一列数据
sqldf("select DISTINCT year from sale")
##筛选数据
sqldf("select * from sale where year=2012")
##排序
sqldf("select year,market,sale,profit from sale order by year")
#####R数据整合#####
##选择字段
sale[,c('year','market','sale','profit')]
sale$year
##去除重复值
unique( sale$year)
##选择行
sale[c(1,3),]
sale[1:3,]
##选择行和列
sale[c(1,2,3),c('year','profit')]
sale[1:3,c(1,4)]
##筛选数据
sale[sale$year==2012,c("year","profit")]
sale[sale$year==2012&sale$profit>2500,]
##排序
sale[order(sale$profit),]
sale[order(sale$year,sale$profit,decreasing=T),]
##数据纵向合并
one <- read.csv("one.csv")
two <- read.csv("two.csv")
two1 <- read.csv("two1.csv")
rbind(one, two1) 
sqldf("select * from one union select * from two")
sqldf("select * from one union all select * from two")
##差集
sqldf("select * from one EXCEPT select * from two")
##交集
sqldf("select * from one INTERSECT select * from two")
##数据横向合并
library(dplyr)
table1 <- read.csv("table1.csv")
table2 <- read.csv("table2.csv")
##笛卡尔积连接
sqldf("select * from table1,table2")
##内连接
inner1<- merge(table1,table2, by = "id", all = FALSE)
inner2<-inner_join(table1,table2, by = "id")
inner3<-sqldf("select * from table1 as a inner join table2 as b on a.id=b.id")
inner4<-sqldf("select * from table1 as a,table2 as b where a.id=b.id")
##左连接
left1<- merge(table1, table2, by = "id", all.x = TRUE)
left2<-left_join(table1, table2, by = "id")
left3<-sqldf("select * from table1 as a left join table2 as b on a.id=b.id")
##右连接
right1<- merge(table1, table2, by = "id", all.y = TRUE)
right2<-right_join(table1, table2, by = "id")
##全连接
full1<- merge(table1, table2, by = "id", all = TRUE)
full2<-full_join(table1, table2, by = "id")

####################高级数据整合----------------------------------------------------
library(reshape2)
rfm_trad_flow<-read.csv("rfm_trad_flow.csv")
##FRM提取行为变量
rfm<-sqldf("select  cust_id,type,
             max(time) as Recency,count(*) as freq,sum(amount) as Monetary
           from  rfm_trad_flow
           where type='Special_offer' or type='Normal'
           group by cust_id,type")
##拆分列
rfm_w<-dcast(rfm,cust_id~type,value.var='Monetary')
rfm_w1<-dcast(rfm,cust_id~type,fun.aggregate=length, value.var='Monetary')
##堆叠列
rfm_l=melt(rfm_w, id.vars="cust_id", 
           measure.vars=c('Normal','Special_offer'),
           variable.name = "type",
           na.rm = F, value.name = "Monetray")
##分割列
data<-data.frame(id=1:3,info=c("张三-26-高中","李四-33-大学","王五-23-"))
library(tidyr)
data1=separate(data,col='info', sep='-',remove=F,into=c('name','age','edu'))

####################抽样--------------------------------------------------------------
clients<-read.csv("clients.csv")
##简单随机抽样
set.seed(100)
select<-sample(1:nrow(clients),100)
sample_client<-clients[select,]
##生成训练集和测试集
set.seed(100)
select<-sample(1:nrow(clients), length(clients$client_id)*0.1)
sample_client<-clients[select,]
other_client<-clients[-select,]
##分层抽样
library(sampling)
sample_client_stra=strata(clients,stratanames="district_id",
                          size=rep(5,times=77),method="srswor")

####################数据清洗-----------------------------------------------------------
accepts<-read.csv("accepts.csv")
##通过直方图与汇总函数summary查看purchase_price变量的错误值
hist(accepts$purch_price)
summary(accepts$purch_price)
##去掉重复行
accepts_nodup1<-unique(accepts)
accepts_nodup2<-accepts[!duplicated(accepts),]
##缺失值处理
##使用均值或中位数填补
vmean<-mean(accepts$tot_derog,na.rm=TRUE)
accepts$tot_derog_empflag<-is.na(accepts$tot_derog)
accepts[is.na(accepts$tot_derog),]$tot_derog<-vmean
##多重插补
library(mice)
library(mitools)
imp<-mice(accepts[,-1],method="cart",m=1)
result<-complete(imp)
summary(result)
##噪声值处理
q1<-quantile(result$tot_derog, 0.01)
q99<-quantile(result$tot_derog, 0.99)
result[result$tot_derog<=q1,]$tot_derog<-q1
result[result$tot_derog>q99,]$tot_derog<-q99
##分箱法
##等宽分箱
ewtd<-cut(result$age_oldest_tr,4)#这里以age_oldest_tr字段等宽分为4段
table(ewtd)
levels(ewtd)<-paste("L",0:3,sep="")#将连续变量转化成定序变量
result$age_oldest_tr_1<-ewtd
table(result$age_oldest_tr_1)
##等深分箱
parts<-5
eRat<-quantile(result$age_oldest_tr, probs=seq(0,1,1/parts))
table(eRat)
eRat<-unique(eRat)
eRat[1]<-eRat[1]-0.001
veRat<-cut(result$age_oldest_tr,eRat)
table(veRat)
result$age_oldest_tr_3<-paste("L",as.numeric(veRat)-1,sep="")
table(result$age_oldest_tr_3)


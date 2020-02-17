##################第四章 R描述性统计分析与绘图######################################

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
##读取数据
accepts=read.csv("accepts.csv")
##数据预处理
sapply(accepts,class)#查看数据类型
summary(accepts)
accepts$bad_ind=as.factor(accepts$bad_ind)
accepts$used_ind=as.factor(accepts$used_ind)

####################描述性统计与探索型数据分析--------------------------------------
##连续变量分析
fs=accepts$fico_score
mean(fs,na.rm=T)#求fs的均值
median(fs,na.rm=T)#求fs的中位数
quantile(fs,probs=c(0.25,0.5,0.75),na.rm=T)#求a的上下四分位数与中位数
hist(fs,nclass=20)#绘制fs的直方图
##数据的离散程度
max(fs,na.rm=T)-min(fs,na.rm=T)#极差
var(fs,na.rm=T)#求方差
sd(fs,na.rm=T)#求标准差
mad=function(x){
    mean(abs(x-mean(x,na.rm=T)),na.rm=T)
}#定义平均绝对偏差函数mad()
mad(fs)#平均绝对偏
##汽车违约贷款数据例子-描述
str(accepts)
table(accepts$bad_ind)
barplot(table(accepts$bad_ind))#使用barplot函数输出条形频数图
hist(fs,nclass=15)#绘制fs的直方图
library(GLDEX)
fs_s=skewness(fs,na.rm=T)
fs_k=kurtosis(fs,na.rm=T)
p=accepts$purch_price
hist(p,nclass=20)
p_s=skewness(p,na.rm=T)#偏度
p_k=kurtosis(p,na.rm=T)#峰度
m=matrix(c(fs_s,fs_k,p_s,p_k),nrow= 2)
colnames(m)=c('信用得分正态','购买价格右偏')
rownames(m)=c('偏度','峰度')
print(m)
##双变量分析：分类变量与连续变量
pdf(file="pair.pdf")#将输出图保存到本地工作目录下
tapply(accepts$fico_score,accepts$bad_ind,hist)
tapply(accepts$purch_price,accepts$bad_ind,hist)
dev.off()
##均值上的差异
tapply(accepts$fico_score,accepts$bad_ind,mean,na.rm=T)

####################R基础绘图包---------------------------------------------------
plot(x=pressure$temperature,y=pressure$pressure)
plot(pressure,type="l")#线图
plot(pressure,type="b")#点线图
plot(pressure,type="p")#点图
plot(pressure,type="h")#直方图
##饼图
pie.sales <- c(10,20,15,30)
names(pie.sales)=c('类别1','类别2','类别3','类别4')
pie(pie.sales, labels=names(pie.sales),
    col=c('green','orange','white','grey'),
    main='主标题',
    sub='副标题')
##直方图
x=accepts$fico_score
hist(x, freq = TRUE,main="fico_score", 
       sub ="source:汽车贷款数据", 
       xlab="fico_score打分", 
       ylab="频数",
       nclass=20)
##箱线图
boxplot(accepts$purch_price, 
        col = "orange",
        main = "箱线图",
        xlab = "车辆价格",
        ylab = "变量值",
        outline = TRUE,
        width=0.1)
#####分析案例：汽车违约贷款数据的例子-制图#####
x=accepts$purch_price
y=accepts$loan_amt
plot(x=x,y=y,type="p",
       main='汽车价格vs贷款金额',
       sub='来源:汽车贷款数据集',
       xlab='汽车价格',ylab='贷款金额'
)#散点图
text(30000,80000,"汽车价格vs贷款金额")
##变量是否违约
paint.sales=table(accepts$bad_ind)#违约变量
names(paint.sales)=c("不违约", "违约")
pie(paint.sales, labels=names(paint.sales),
      col=c('green','orange'),
      main='汽车贷款违约占比饼图',
      sub='来源：汽车贷款数据')
##破产标识变量
paint.sales=table(accepts$bankruptcy_ind)
names(paint.sales)=c("破产", "未破产")
pie(paint.sales, 
      col=c('green','orange'),
      main='客户破产占比饼图',
      sub='来源：汽车贷款数据')
##信用评分
x=accepts$fico_score
hist(x, freq = TRUE,main="fico_score", sub ="source:汽车贷款数据", 
      xlab="fico_score打分",  ylab="频数",nclass=20)
boxplot(accepts$fico_score,data=accepts, boxwex = 0.5,col="yellow", 
        main = "FICO信用评分箱线图", xlab = "FICO信用评分", ylab = "分值")
##探索违约与不违约状态下信用评分的差异
boxplot(accepts$fico_score~accepts$bad_ind,
        data=accepts,
        boxwex = 0.5, 
        at = 1:2 - 0.2,
        col = c("yellow","orange"),
        ylim = c(400, 900),
        main = "违约与不违约下信用评分箱线图",
        xlab = "0不违约/1违约",
        ylab = "信用得分")

####################GGPLOT2绘图--------------------------------------------------
library(ggplot2)
ggplot(data=pressure,aes(x=temperature, y=pressure)) + 
    geom_point(size=5)+
    geom_smooth(size=2)#图基数据(指定轴)+类型(点+平滑曲线)
ggplot(data=diamonds,aes(price)) +
    geom_histogram(fill="blue")#图基数据(指定轴)+类型(直方图)
ggplot(data=diamonds,aes(price)) +
    geom_density(fill="blue")#图基数据(指定轴)+类型(概率密度图)
##使用qplot()
qplot(accepts$bad_ind,
      main="违约分布",
      xlab="0不违约/1违约",
      ylab="频数")  
x=accepts$purch_price
y=accepts$loan_amt
qplot(x=x,y=y,
        main='汽车价格vs贷款金额',
        xlab='汽车价格',ylab='贷款金额',
        geom="point"
)#散点图
qplot(accepts$fico_score,
      main='信用得分直方图',
      xlab='信用得分',ylab='频数',
      colour=I("red"),
      geom='histogram',
      binwidth=15
)









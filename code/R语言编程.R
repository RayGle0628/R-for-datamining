##################第三章 R语言编程#################################################

####################R的基本对象----------------------------------------------------
##字符
c<-'abc'
c
mode(c)
##数字
e=123.2
mode(e)
##复数
f=-3i+1
mode(f)
##逻辑
a=TRUE
mode(a)
##缺失值
i<-NA
3+i
4*i
##空值
a <-NULL
a
##无穷值(Inf)与非数值(NaN)
1/0
-1/0
0/0

####################R的基本数据类型------------------------------------------------
##创建向量
a<-c(1,2,3)
a
class(a)
b<-c(1,'a',3)
b
class(b)
##数值向量的运算
x1 <- c(1,2,3)
x2 <- c(3,2,1)
x3 <- c(1,2,3,4)
x1+x2+1
x1+x3
x1 * x2
x1 / x2
x1 %*% x2
sum((x1-mean(x1))^2)/sqrt(length(x1)-1)
##产生序列
1:4
seq(from=-2,to=2,by=0.5)
rep(1,times=5)
##向量的索引
x1 <- c(1,2,NA,4,5)
x1[3]
x1[1:3]	
x1[c(1,2,3)]
x1[-1]
fruit <- c(5, 10, 1, 20)
names(fruit) <- c("orange", "banana", "apple", "peach")
fruit
fruit['orange']
fruit[1]
x1 <- 1:5
x1 >= 2
x1[x1>=2]
##矩阵的创建
matrix1<-matrix(c(1,2,3,4,5,6),nrow=2,ncol=3)
matrix1
matrix1<-matrix(c(1,2,3,4,5,6),nrow=2,ncol=3,byrow=TRUE)
matrix1
##数值矩阵的运算
matrix1 <- matrix(1:4,nrow=2)
matrix1
t(matrix1)
matrix1 %*% t(matrix1)
solve(matrix1)
eigen(matrix1)$values
eigen(matrix1)$vector
##矩阵的索引
matrix1 <- matrix(1:6,nrow=2)
matrix1
matrix1[1,]
matrix1[,1]
matrix1[1,1]
matrix1[1:2,c(1,2)]
matrix_named <- matrix(c(1,2,3,4),nrow=2,dimnames=list(c('row1','row2'),c('col1','col2')))
matrix_named
matrix_named[1,'col1']
##数组
array1<-array(1:18,c(3,3,2))
array1
##因子
performance<-c("bad","good","good","bad", "excellent","bad")
class(performance)
f1<-factor(performance)
f1
levels(f1)
mode(performance)
mode(f1)
##有序因子向量
age <- c('old','median','young','median','median','young')
age_f <- factor(age)
age_f
age_f_ordered <- factor(age,levels=c('young','median','old'),ordered=T)
age_f_ordered
table(age_f) 
table(age_f_ordered)
##列表
v1<-c(2:8)
v1
v2<-c("aa","bb","cc")
v2
m1<-matrix(c(1:9),nrow=3)
m1
f5<-factor(c("high","low","low","high"))
mylist<-list(v1,v2,m1,f5)
mylist
mylist[1]
list2<-list(num=v1,cha=v2,matrix=m1,factor=f5)
list2
list2[[1]]
list2[[3]]
list2[["matrix"]]
##数据框
##数据框与列表
list0 <- list(name = c('Bob','Lindy','Mark','Yago'),
              age = c(24,35,23,45),
              math = c(92,100,78,67))
list0
data.frame(list0)
##数据框的创建
name<-c("Jane","Bob","Elena","Lily","Max")
English<-c(84,86,78,90,88)
Math<-c(80,85,90,87,85)
Art<-c(78,80,80,85,86)
Score<-data.frame(name,English,Math,Art)
Score
##数据框的索引
Score[1,]
Score[,1]
Score[,'name'] 
Score[1:2,c('name','Art')]

####################R的程序控制---------------------------------------------------
##顺承结构
a=c(1,2,3)
print (a[1]*a[1])
print (a[2]*a[2])
print (a[3]*a[3])
##分支结构
a=-1
if (a>0){
    print('a大于0')
    }else{print('a小于等于0')}
a=1
if (a>0){
    print('a大于0')
}else if(a==0){print('a等于0')
}else{print('a小于0')}
##循环语句
x=c("a", "b", "c", "d")
for(i in 1:4) {
    print(x[i])
    }
x=c("a", "b", "c", "d")
for(letter in x) {
    print(letter)
    }
i=0
while(i<=3) {
    i<-i+1
    print(i)
}
##APPLY函数族
x=list(a = 1:5, b = rnorm(10))#生成两组向量
x
lapply(x, mean)
sapply(x, mean)

x=matrix(rnorm(6), 2, 3)
x
apply(x, 2, mean)#对数据框中的列进行循环
apply(x, 1, sum)#对数据框中的行进行循环

data<-data.frame(x=c(-0.6234743,-0.4719858,-1.871619,0.545811,0.4839098,
                     0.7882512,0.4481959,0.8458964,0.8737372,0.05652524,
                     -0.3543139,1.641247,0.3337766,1.425203,0.1899111),
                 group=rep(1:3,each=5))
tapply(data$x, data$group, mean)#分组求均值
tapply(data$x, data$group, mean, simplify = FALSE)#不简化结果
tapply(data$x, data$group, range)#分组求范围
split(data$x, data$group)#类型为列表
sapply(split(data$x, data$group),mean)

####################R的函数-------------------------------------------------------
args(lm)
?lm
mydata<-rnorm(100)
sd(mydata)
sd(x = mydata)
sd(x = mydata, na.rm = FALSE)
sd(na.rm = FALSE, x = mydata)
sd(na.rm = FALSE, mydata)

####################R的日期与时间类型---------------------------------------------
x <-as.Date("1970-01-01")
x
unclass(x)
unclass(as.Date("1970-01-02"))
x <-Sys.time()
x
p <-as.POSIXlt(x)
names(unclass(p))
p$sec
x <-as.POSIXct("2012-10-25 01:00:00")
y<-as.POSIXct("2012-10-25 02:00:00", tz = "GMT")
y-x
x <-as.Date("2012-01-01")
y <-as.Date("2006-01-08")
x-y
x <-as.POSIXlt("2012-01-01")
y <-strptime("2006-01-08 10:07:52", "%Y-%m-%d %H:%M:%S")
x-y

####################R中读写数据----------------------------------------------------
##csv读取
read.csv("ADS.csv",header=TRUE,encoding='UTF-8',stringsAsFactors=TRUE)
##写入文件
data$NAvalue<-NA
data$letter<-letters[1:nrow(data)]
write.table(data,'test.txt',sep=',',na='缺失',
            quote=TRUE,row.names=TRUE,fileEncoding='UTF-8')




















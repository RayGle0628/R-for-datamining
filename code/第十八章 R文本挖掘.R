##编码
readLines('a.txt',encoding = 'GBK')
readLines('a.txt',encoding = 'UTF-8')
###################正则表达式###########################################
library(stringr)
##提取字母
eg_extract<-' T文本Mi挖掘1234 '
str_extract_all(eg_extract,'[A-z]')
##选择非邮箱
eg<-c('asd@sina.com','asd@pinggu','asd@163.cn')
pattern<-'[A-z0-9]+@[A-z0-9]+\\.(com|cn)'
gsub(pattern,'*',eg)
##常用的正则表达式例子
eg='1.文本挖掘(Test Mining),可以发掘海量文本中有价值的信息'
##匹配数字
gsub('[0-9]','*',eg)
##匹配英文字符
gsub('[A-z]','*',eg)
##匹配汉字
gsub('[\u4E00-\u9FA5]','*',eg)
##多条件匹配
gsub('[A-z0-9\u4E00-\u9FA5]','*',eg)
##轮流匹配
gsub('([A-z]|[0-9]|[\u4E00-\u9FA5])','*',eg)
##非的使用
gsub('[^\u4E00-\u9FA5]','*',eg)

###################分词#################################################
library("jiebaR")
##分词示例
engine=worker()
engine<="宋仲基真是太帅了"
egvector=c('天气真好','我好像没带钱包')
engine<=egvector
##词性标注
cutter=worker(type = "tag")
cutter_words<-cutter<="我爱北京天安门"
cutter_words
##停用词处理
stop=c('r','p')
cutter=worker(type = "tag")
cutter_words<-cutter<="这几天生活与工作貌似都不太顺利" 
cutter_words[!(names(cutter_words) %in% stop)]
##停用词
cutter=worker(stop_word='stop.txt')
cutter_words<-cutter<="这几天生活与工作貌似都不太顺利"
cutter_words
##自定义分词词典
system_dict=worker(type = "mix")
system_dict<="段玉波和李红军在红岭大厦玩杀人游戏呢"
user_dict=worker(type = "mix",user="user.txt")
user_dict<="段玉波和李红军在红岭大厦玩杀人游戏呢"

###################生成词文档矩阵及相关性度量############################
library(tm)
##创建语料库
A='三星中国公司宣布召回在中国大陆地区销售的全部Note7手机'
B='三星宣布全球停售Note7，全部销毁处理三星Note7的起火召回事件已经告一段落。'
library(jiebaR)
engine<-worker()
##分词
vector=list(engine<=A,engine<=B)
##构建语料库
words_corpus=VCorpus(VectorSource(vector))
words_corpus
##查看语料库中的文档情况
lapply(words_corpus[1:2],as.character)
##生成词文档矩阵
jieba_tokenizer=function(d){unlist(segment(d[[1]],engine))}
control<-list(stopwords='国土资源部',removePunctuation=F,removeNumbers=F,
              wordLengths=c(3, Inf),tokenize=jieba_tokenizer)
inspect(DocumentTermMatrix(words_corpus,control=control))
##依据TFIDF权重生成词文档矩阵
control<-list(stopwords='国土资源部',removePunctuation=F,removeNumbers=F,
              wordLengths=c(3, Inf),tokenize=jieba_tokenizer,
              weighting = function(x)weightTfIdf(x))
inspect(DocumentTermMatrix(words_corpus,control=control))
###相关性度量
##自定义余弦距离矩阵函数
cosmatrix=function(data){
    name=colnames(data);temp=NULL
    for (j in name){
        x=data[,j];cosin=NULL
        for (i in name){
            y=data[,i]
            cosin[i]=(x%*%y)/(sqrt(x%*%x)*sqrt(y%*%y))
        }
        temp=cbind(temp,cosin);
    }
    colnames(temp)=name
    return(temp)
}
tdm<-TermDocumentMatrix(words_corpus,control=list(wordLengths=c(1, Inf),
                                                  tokenize=jieba_tokenizer))
tdm_m<-as.matrix(tdm)
cosmatrix(tdm_m)

###################综合案例##############################################
#2.文本预处理#
library(topicmodels)
###分词:
##分词之前需要注意:先去除数据中可能存在的数字和一些特殊符号等,然后分词,如:
##去停词
files_dir=list.files("./Sample-Sougou",full.names=T)
length(files_dir)
files_dir #看一下文件路径
filedirs=files_dir[1:10]
filedirs #根据实际情况删去最后的说明文档
filedirs[1]
##遍历子文件
readsubfiles=function(x){
    list.files(filedirs[x],full.names=T)
}
fullfiles=lapply(1:length(filedirs),readsubfiles)
fullfiles_final=unlist(fullfiles)
length(fullfiles_final) #最终的全部txt文件路径
head(fullfiles_final)

##读取文件
readdata=function(y){
    readLines(fullfiles_final[y],encoding="ANSI")
}
datafinal=lapply(1:length(fullfiles_final),readdata)
length(datafinal)
class(datafinal) #list类型
#随机查看任意一篇文章第一段前25个字符
substr(unlist(datafinal[runif(1,1,100)])[1],1,25) 

#####初步清理及jiebaR分词#####
##去除数字及nbsp
datafinal1=gsub("[0-9 nbsp a-z A-Z]","",datafinal) 
##利用jiebaR包分词
engine=worker()
wordsall=list()
##分词
for(i in 1:100){wordsall[[i]]=engine<=datafinal1[[i]]}
##随机查看某个文章的前八个词
head(wordsall[[runif(1,1,100)]],8) 

#3.文本分类#
#清理数据
#去除 0和1 长度的词
wordsfinal=list()
for(j in 1:20){
    wordsfinal[[j]]=unlist(wordsall[[j]])
    wordsfinal[[j]]=wordsfinal[[j]][nchar(wordsfinal[[j]])!=1 & nchar(wordsfinal[[j]])!=0]
}
#随机查看某个文章的前八个词
head(wordsall[[runif(1,1,20)]],8) 
class(wordsfinal) #查看类型
#将文本转换为语料库
words_corpus=Corpus(VectorSource(wordsfinal))
#将语料库转化为文档-词条矩阵
jieba_tokenizer=function(d){unlist(segment(d[[1]],engine))}
control<-list(wordLengths=c(2, Inf),tokenize=jieba_tokenizer)
words_dtm=DocumentTermMatrix(words_corpus,control=control)
#将文档-词条矩阵转换为普通矩阵
words_matrix=as.matrix(words_dtm)
dim(words_matrix)
words_matrix<-data.frame(words_matrix)
#创建标签
labels=list(rep("汽车",10),rep("财经",10))
labels1=unlist(labels)
words_matrix$Class<-as.factor(labels1)
write.csv(words_matrix,"words_matrix.csv")

#构造训练集&测试集
set.seed(10)
select<-sample(1:nrow(words_matrix),nrow(words_matrix)*0.6)
train=words_matrix[select,]
test=words_matrix[-select,]

##svm算法
library(e1071)
head(names(train))
SVM<-svm(Class~.,data=train,kernel="sigmoid",cost=4,gamma=0.01)
Predictions<-predict(SVM,test)
Predictions
#结果对比
pred=as.data.frame(cbind(test$Class,Predictions))
library(mlearning)
mats=confusion(x=pred, vars = c("V1", "Predictions"))
summary(mats, type = c("Fscore", "Recall", "Precision"))

##KNN算法
library(class)
head(names(train))
Predictions=knn(train=train[,1:3571],test=test[,1:3571],cl=train$Class,k=12)
Predictions
#结果对比
pred=as.data.frame(cbind(test$Class,Predictions))
library(mlearning)
mats=confusion(x=pred, vars = c("V1", "Predictions"))
summary(mats, type = c("Fscore", "Recall", "Precision"))

########LDA文本主题模型########
#仅分析汽车栏目的新闻
#清理数据
#去除 0和1 长度的词
wordsfinal<-list()
for(j in 1:10){
    wordsfinal[[j]]=unlist(wordsall[[j]])
    wordsfinal[[j]]=wordsfinal[[j]][nchar(wordsfinal[[j]])!=1 & nchar(wordsfinal[[j]])!=0]
}
#随机查看
head(wordsall[[runif(1,1,10)]],8) 
##利用tm包将文本数据转换为数据矩阵
#将文本转换为语料库
words_corpus=Corpus(VectorSource(wordsfinal)) 
#将语料库转化为文档-词条矩阵
jieba_tokenizer=function(d){unlist(segment(d[[1]],engine))}
control<-list(wordLengths=c(2, Inf),tokenize=jieba_tokenizer)
words_dtm=DocumentTermMatrix(words_corpus,control=control)
#将文档-词条矩阵转换为普通矩阵
words_matrix=as.matrix(words_dtm)
dim(words_matrix)
words_matrix<-data.frame(words_matrix)
write.csv(words_matrix,"words_matrix.csv",row.names=F)

##开始建立LDA模型，输入为词频-文档矩阵
test=words_matrix
train.lda=LDA(test,3,control=list(alpha=0.1))
#返回后验概率/文档-主题分布/文档-主题分布
test.topics=posterior(train.lda,test)$topics
names(posterior(train.lda,test))
head(test.topics,10) #文档-主题分布 
terms(train.lda,5) #词-主题

##################第十六章 时间序列建模################################################

####################简单时间序列分析---------------------------------------------------
library(TTR)
orgData=read.csv("popgrow.csv")
##ts函数将数据转换为时间序列型对象的数据
tsdata=ts(orgData$PopGrow,start=c(1949),end=c(2004))
##SMA(简单移动平均)和EMA(指数平均)
##n表示平均多少期的数据,ratio值表示指数平滑中的平滑系数为0.3
tsdata_sma<-SMA(tsdata,n=3)
tsdata_ema<-EMA(tsdata,n=3,ratio=0.3)
##将tsdata画出来
plot.ts(tsdata_sma)
plot.ts(tsdata_ema)
##样本外预测
tsdata_p_sma<-predict(tsdata_sma,3)
tsdata_p_ema<-predict(tsdata_ema,3)
plot(tsdata_p_sma)
plot(tsdata_p_ema)

##霍尔特指数平滑
##霍尔特指数平滑法——有增长或下降趋势的，但没有季节性因素的时间序列:HoltWinters(gamma=F)
skirts<-read.csv("skirts.csv")
skirtsseries <- ts(skirts$x,start=c(1866))
plot.ts(skirtsseries)
##样本内预测
skirtsseriesforecasts <- HoltWinters(skirtsseries, gamma=F)
plot(skirtsseriesforecasts)
##样本外预测
skirtsseriesforecasts2 <- forecast.HoltWinters(skirtsseriesforecasts, h=6)
plot.forecast(skirtsseriesforecasts2)

##霍尔特-温特指数平滑法
##有增长或下降趋势，且有季节性变动趋势的时间序列:HoltWinters(gamma=T)或decompose()
orgData=read.csv("air.csv")
orgData$DATE=1:nrow(orgData)
##转换成时间序列数据
tsdata=ts(orgData$AIR,frequency = 12, start = c(1949, 1))
##将tsdata画出来
plot.ts(tsdata)
##将tsdata进行简单指数平滑
##累加效应
tsdata_a<-HoltWinters(tsdata,seasonal = c( "additive"))
plot(tsdata_a,main='HW-additive Air dataset')
##累积效应
tsdata_m<-HoltWinters(tsdata,seasonal = c( "multiplicative"))
plot(tsdata_m,main='HW-multiplicative Air dataset')
##样本外预测
tsdata_p<- predict(tsdata_a, n.ahead=36, prediction.interval = TRUE)
plot(tsdata_a, tsdata_p,main='HW-additive Air dataset Prediction')

####################AR模型------------------------------------------------------------
##读取时间序列数据集
orgData<-read.csv("ts_simu.csv")
##将orgData转换成时间序列数据
tsdata<-ts(orgData$AR1_a)
##1 将tsdata画出来，初步判断是否为平稳数据，如图
plot(tsdata)
##2 查看自相关函数图以及偏自相关函数图，对平稳的序列模型进行识别
##绘制自相关函数图
acf(tsdata)
##从图可以得知，自相关函数是多阶拖尾
##绘制偏自相关函数图
pacf(tsdata)
##从图可以得知，偏自相关函数是1阶截尾
##3 构建ARIMA模型：ARIMA(1,0,0)，并检验模型的残差
arima.mod<-arima(tsdata,order=c(1,0,0))
##4 提取模型的残差向量，检验是否还有相关信息可以提取
##提取模型的残差向量
resid<-arima.mod$residuals
##使用Box.test函数进行白噪声检验
Box.test(resid)
##检验的p-value远大于0.01，说明残差为白噪声序列，模型最终确定为ARIMA(1,0,0)
##残差的自相关与偏自相关系数图，已经没有信息再提取
acf(resid)
pacf(resid)
#从QQ图上看，中间部分还是近接近正态分布的，只是开头和结尾的部分离期望正态分布较远
qqnorm(resid) 
qqline(resid)
##5 进行预测
library(forecast)
arima.pred<-forecast(arima.mod,h=10)
plot.forecast(arima.pred)

####################MA模型------------------------------------------------------------
##读取时间序列数据集
orgData<-read.csv("ts_simu.csv")
##将orgData转换成时间序列数据
tsdata<-ts(orgData$MA1_a)
##1 将tsdata画出来，初步判断是否为平稳数据
plot(tsdata)
##2 查看自相关函数图以及偏自相关函数图，对平稳的序列模型进行识别
#绘制自相关函数图
acf(tsdata)
#从图可以得知，自相关函数是1阶截尾
#绘制偏自相关函数图
pacf(tsdata)
#从图可以得知，偏自相关函数是多阶拖尾
##3 构建ARIMA模型：ARIMA(0,0,1)，并检验模型的残差
arima.mod=arima(tsdata,order=c(0,0,1))
##4 提取模型的残差向量，检验是否还有相关信息可以提取
resid=arima.mod$residuals 
#对残差进行白噪声检验
Box.test(resid) 
##检验的p-value远大于0.01，说明残差为白噪声序列，模型最终确定为ARIMA(0,0,1)
##再次查看自相关函数图和偏自相关函数图，已经没有信息再提取
acf(resid)
pacf(resid)
##从QQ图上看，中间部分还是近接近正态分布的，只是开头和结尾的部分离期望正态分布较远
qqnorm(resid)
qqline(resid)
##5 进行预测
library(forecast)
arima.pred<-forecast(arima.mod,h=10)
plot.forecast(arima.pred)

####################ARMA模型----------------------------------------------------------
##读取时间序列数据集
orgData<-read.csv("ts_simu.csv")
##将orgData转换成时间序列数据
tsdata<-ts(orgData$ARMA_11_b)
##1 将tsdata画出来，初步判断是否为平稳数据，如图
plot(tsdata)
##2 查看自相关函数图以及偏自相关函数图，对平稳的序列模型进行识别
#绘制自相关函数图
acf(tsdata)
#从图可以得知，自相关函数是多阶拖尾
#绘制偏自相关函数图
pacf(tsdata)
#从图可以得知，偏自相关函数是多阶拖尾
#均为多阶拖尾，只好通过循环测试不同参数的AIC指标
##确定p和q的阶数
lagL=0:3
aicV=NULL
pV=NULL
qV=NULL
for(i in lagL)
{
    for(j in lagL)
    {
        tmp=arima(tsdata,order=c(i,0,j))
        pV=c(pV,i)
        qV=c(qV,j)
        aicV=c(aicV,tmp$aic)
    }
}
chdata=data.frame(pV,qV,aicV)
chdata=chdata[order(chdata$aicV,decreasing=F),]
head(chdata,3)
##根据chdata的aicV属性，可以知道，当p=1,q=1时，ARMA模型的AIC最小
##3 构建ARIMA模型：ARIMA(1,0,1)，并检验模型的残差
arima.mod=arima(tsdata,order=c(1,0,1))
##4 提取模型的残差向量，检验是否还有相关信息可以提取
resid=arima.mod$residuals 
#对残差进行白噪声检验
Box.test(resid) 
#检验的p-value远大于0.01，说明残差为白噪声序列，模型最终确定为ARIMA(1,0,1)
#再次查看自相关函数图和偏自相关函数图，已经没有信息再提取
acf(resid)
pacf(resid)
#从QQ图上看，中间部分还是近接近正态分布的，只是开头和结尾的部分离期望正态分布较远
qqnorm(resid)
qqline(resid)
##5 进行预测
arima.pred<-forecast(arima.mod,h=10)
plot.forecast(arima.pred)

####################ARIMA模型---------------------------------------------------------
##读取时间序列数据集
orgData<-read.csv("ts_simu.csv")
##将orgData转换成时间序列数据
tsdata<-ts(orgData$ARIMA_110)
##1 将tsdata画出来，初步判断是否为平稳数据，如图
plot(tsdata)
##通过plot(tsdata)，可以看出来，图形有明显的趋势，为非平衡的序列
##调用R的包tseries中的函数adf.test进行平稳性检验
tseries::adf.test(tsdata)
#可以看到p-value=0.4017，不显著，不能拒绝非平稳的假设
##使用函数diff进行差分
tsdata.diff11<-diff(tsdata,lag=1,differences=1)
##将经过一阶差分运算的序列图出来，如图
plot(tsdata.diff11)
##从差分结果可以知道，图中已经没有明显的趋势变化特征，需要进一步检验序列的平稳性
tseries:: adf.test(tsdata.diff11)
##2 定阶
##查看自相关函数图以及偏自相关函数图，对平稳的序列模型进行识别
##绘制自相关函数图
acf(tsdata.diff11)
##从图可以得知，自相关函数是3阶拖尾
##绘制偏自相关函数图
pacf(tsdata.diff11)
##从图可以得知，偏自相关函数是1阶截尾,可以判断应使用ARIMA(1,1,0)模型
##3 构建ARIMA模型：ARIMA(1,1,0)，并检验模型的残差
arima.mod=arima(tsdata,order=c(1,1,0))
##4 提取模型的残差向量，检验是否还有相关信息可以提取
resid=arima.mod$residuals 
##对残差进行白噪声检验
Box.test(resid) 
##检验的p-value远大于0.01，说明残差为白噪声序列
##再次查看自相关函数图和偏自相关函数图，已经没有信息再提取
acf(resid)
pacf(resid)
##从QQ图上看，中间部分还是近接近正态分布的
qqnorm(resid)
qqline(resid)
##5 进行预测
library(forecast)
arima.pred<-forecast(arima.mod,h=20)
plot.forecast(arima.pred)

####################真实数据: ARIMA模型----------------------------------------------
##有季节性变动趋势的时间序列
library(tseries)
library(forecast)
air=read.csv("air.csv")
##将air转换成时间序列数据
tsdata=ts(air$AIR,frequency = 12, start = c(1949, 1))
##将tsdata画出来，如图
plot.ts(tsdata)
##波动有明显的增加情况，因此取对数
log_tsdata<-log(tsdata)
plot.ts(log_tsdata)
##存在趋势性与季节性。这里需要进行季节差分
log_tsdata.diff1=diff(log_tsdata,lag=1,differences=1)
log_tsdata.diff1_12=diff(log_tsdata.diff1,lag=12,differences=1)
plot.ts(log_tsdata.diff1_12)
adf.test(log_tsdata.diff1_12)
##可以看到差分后的序列变得平稳且通过了平稳性检验。
##2 定阶
##查看自相关函数图以及偏自相关函数图，对平稳的序列模型进行识别
##这里60代表12*5=60即展示以12为周期5期的数据
acf(log_tsdata.diff1_12,60)
pacf(log_tsdata.diff1_12,60)
##由于不好确定准确的阶数，故可采用遍历的方法根据最小AIC值准则确定阶数
lagL=0:2
aicV=NULL
pV=NULL
qV=NULL
for (i in lagL)
{
    for(j in lagL)
    {
        if(i==2&&j==2){next}
        else {
            tmp=arima(log_tsdata.diff1_12,order=c(i,1,j),seasonal=list(order=c(i, 1,j),period = 12))
            pV=c(pV,i)
            qV=c(qV,j)
            aicV=c(aicV,tmp$aic)
        }
        }
}  
chdata=data.frame(pV,qV,aicV)
chdata=chdata[order(chdata$aicV,decreasing=F),]
head(chdata,3)
##根据结果，p取0，q取2时模型AIC最小。故应使用ARIMA(0,1,2)建模
##3 构建ARIMA模型并检验模型的残差
arima.mod=arima(log(tsdata), c(0, 1, 2),seasonal = list(order = c(0, 1, 2), period = 12))
##提取模型的残差向量
resid<-arima.mod$residuals
##对残差进行白噪声检验
Box.test(resid) 
##检验的p-value远大于0.01，说明残差为白噪声序列
##再次查看自相关函数图和偏自相关函数图，已经没有信息再提取
acf(resid)
pacf(resid)
##从QQ图上看，中间部分还是近接近正态分布的
qqnorm(resid)
qqline(resid)
##5 进行预测
library(forecast)
arima.pred<-forecast(arima.mod,h=5*12)
##查看原始数据的预测情况，注意原数据预测需要进行指数转换
##lty =c(1,2)表示预测期的线条样式与原序列数据不一
ts.plot(tsdata,exp(1)^arima.pred$mean, lty = c(1,2))














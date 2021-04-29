library(neuralnet)
library(forecast)

data=read.csv("dataskripsifix.csv", sep = ";")
data2=data$rr

date=seq(as.Date("2018-05-01"), as.Date("2020-09-29"), by = "day")
CHL=ts(data2,start = c(2018, as.numeric(format(date[1],"%j"))), frequency = 365)
autoplot(CHL)
pacf(CHL, lag.max = 50)
#function untuk membuat dataframe target dan lag-lag nya
makeinput = function(x, lags = NULL){
  n = length(x)
  a = matrix(0, nrow = length(x)-lags, ncol = 1+lags)
  a[,1] = x[-c(1:lags)]
  a[,1+lags] = x[-c((n-lags+1):n)]
  for (i in 1:(lags-1)) {
    a[,i+1] = x[-c(1:(lags-i),(n+1-i):n)]
  }
  Ytarget = a[,1]
  Xinput = a[,(2:(lags+1))]
  a = data.frame(Ytarget,Xinput)
  return(a)
}
data = makeinput(CHL, lags = 26)

##Normalisasi
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
denormalize <- function(xtrans, data = x) {
  return(xtrans*(max(data)-min(data))+min(data))
}

dtransform = normalize(data)

#split data untuk data transformasi
datatraining = head(dtransform, round(0.8*nrow(dtransform)))
datatesting = tail(dtransform, round(0.2*nrow(dtransform)))

#membuat forecast function
forecast.nn = function(fit, df, rep = 1, h = 1) {
  frc = c()
  na = data.frame(NA)
  a = df[nrow(df),-ncol(df)]
  aa = cbind(na,a)
  colnames(aa) = as.vector(names(df))
  frc[1] = predict(fit, aa, n.ahead = 1, rep = rep)
  if (h > 1) {
    for (i in 2:h) {
      aa[1,1] = frc[(i-1)]
      abaru = aa[nrow(aa),-(ncol(aa)-1)]
      aa = cbind(na,abaru)
      colnames(aa) = as.vector(names(df))
      frc[i] = predict(fit, aa, n.ahead = 1, rep = rep)
    }
  }
  return(frc)
}

#function k cross validation on rolling forecasting origin
ts.kcv = function(Trainset, Testset, lr = 0.01){
  ntrain = nrow(Trainset)
  ntest = nrow(Testset)
  frc.h5 = c()
  frc.h30 = c()
  for (i in 1:ntest) {
    if (i == 1) {
      nnCHL <- neuralnet(Ytarget ~ ., data = Trainset, hidden = 5,
                         linear.output = F, algorithm = 'backprop',
                         learningrate = lr, err.fct = 'sse',
                         act.fct = 'logistic', startweights = NULL,
                         stepmax = 100000)
      frc.h5[i] = forecast.nn(nnCHL, Trainset, h=1)
    }
    if (i > 1) {
      Trainset[(ntrain+i-1),] = Testset[(i-1),]
      nnCHL <- neuralnet(Ytarget ~ ., data = Trainset, hidden = 5,
                         linear.output = F, algorithm = 'backprop',
                         learningrate = lr, err.fct = 'sse',
                         act.fct = 'logistic', startweights = NULL,
                         stepmax = 100000)
      frc.h5[i] = forecast.nn(nnCHL, Trainset, h=1)
    }
  }
  for (i in 1:ntest) {
    if (i == 1) {
      nnCHL <- neuralnet(Ytarget ~ ., data = Trainset, hidden = 30,
                         linear.output = F, algorithm = 'backprop',
                         learningrate = lr, err.fct = 'sse',
                         act.fct = 'logistic', startweights = NULL,
                         stepmax = 100000)
      frc.h30[i] = forecast.nn(nnCHL, Trainset, h=1)
    }
    if (i > 1) {
      Trainset[(ntrain+i-1),] = Testset[(i-1),]
      nnCHL <- neuralnet(Ytarget ~ ., data = Trainset, hidden = 30,
                         linear.output = F, algorithm = 'backprop',
                         learningrate = lr, err.fct = 'sse',
                         act.fct = 'logistic', startweights = NULL,
                         stepmax = 100000)
      frc.h30[i] = forecast.nn(nnCHL, Trainset, h=1)
    }
  }
  e.h5 = frc.h5 - Testset$Ytarget
  mse.h5 = mean(e.h5^2)
  e.h30 = frc.h30 - Testset$Ytarget
  mse.h30 = mean(e.h30^2)
  return(list(frc.h5=frc.h5, frc.h30=frc.h30, e.h5=e.h5, e.h30=e.h30,
              mse.h5=mse.h5, mse.h30=mse.h30))
}
fitnn26lr01 = ts.kcv(datatraining, datatesting, lr=0.01)

h5 = readRDS("fitnn26h5.rds") ;h6 = readRDS("fitnn26h6.rds") 
h7 = readRDS("fitnn26h7.rds") ;h8 = readRDS("fitnn26h8.rds") 
h9 = readRDS("fitnn26h9.rds") ;h10 = readRDS("fitnn26h10.rds")
h11 = readRDS("fitnn26h11.rds") ;h12 = readRDS("fitnn26h12.rds") 
h13 = readRDS("fitnn26h13.rds") ;h14 = readRDS("fitnn26h14.rds") 
h15 = readRDS("fitnn26h15.rds") ;h16 = readRDS("fitnn26h16.rds")
h17 = readRDS("fitnn26h17.rds") ;h18 = readRDS("fitnn26h18.rds") 
h19 = readRDS("fitnn26h19.rds") ;h20 = readRDS("fitnn26h20.rds") 
h21 = readRDS("fitnn26h21.rds") ;h22 = readRDS("fitnn26h22.rds") 
h23 = readRDS("fitnn26h23.rds") ;h24 = readRDS("fitnn26h24.rds") 
h25 = readRDS("fitnn26h25.rds") ;h26 = readRDS("fitnn26h26.rds") 
h27 = readRDS("fitnn26h27.rds") ;h28 = readRDS("fitnn26h28.rds")
h29 = readRDS("fitnn26h29.rds") ;h30 = readRDS("fitnn26h30.rds")

listnn = list(h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15,h16,h17,
              h18,h19,h20,h21,h22,h23,h24,h25,h26,h27,h28,h29,h30)
hidden = c()
mse.nn26 = c()
for (i in 1:26) {
  hidden[i] = i+4
  mse.nn26[i] = listnn[[i]]$mse
}
hasil.nn26 = data.frame(hidden, mse.nn26)
hasil.nn26[which(hasil.nn26$mse==min(hasil.nn26$mse.nn26)),]

y.test = as.ts(denormalize(datatesting$Ytarget, data = data));
frc.test = as.ts(denormalize(h5$frc, data=data))
ts.plot(y.test,frc.test,lty=c(1,3),col=c(4,10),
        main="Plot Hasil Ramalan Data Training Terhadap Data Testing")
legend("topright",c("aktual", "prediksi"), bty = "n",
       lty=c(1,3), col=c(4,10),lwd=c(1,1), cex = 0.8)

#memodelkan full data dari model nn terbaik, best hidden = 5, best learning rate = 0.01
nnCHL <- neuralnet(Ytarget ~ ., data = dtransform, hidden = 5,
                   linear.output = F, algorithm = 'backprop',
                   learningrate = 0.01, err.fct = 'sse', rep = 20,
                   act.fct = 'logistic', startweights = NULL,
                   stepmax = 100000)

nnCHL = readRDS("nnfixRep20Mse44Rsq75.rds")
input = subset(dtransform, select = -Ytarget)
rep = c()
mse = c()
Rsq = c()
AdjRsq = c()
for (i in 1:20) {
  comp.nnCHL = compute(nnCHL, input, rep = i)
  y = as.ts(denormalize(dtransform$Ytarget, data = data))
  yhat = as.ts(denormalize(comp.nnCHL$net.result, data=data))
  T = nrow(data)
  k = ncol(data) - 1
  rep[i] = i
  mse[i] = mean((y-yhat)^2) #ini mse model fix
  Rsq[i] = sum((yhat-mean(y))^2)/sum((y-mean(y))^2)
  AdjRsq[i] = 1-(((1-Rsq[i])*(T-1))/(T-k-1))
}
hasil = data.frame(rep=rep, mse=mse, Rsq=Rsq, AdjRsq=AdjRsq)

comp.nnCHL = compute(nnCHL, input, rep = 9)
y = as.ts(denormalize(dtransform$Ytarget, data = data))
yhat = as.ts(denormalize(comp.nnCHL$net.result, data=data))
e = ts(y-yhat)
library(FitAR)
LjungBoxTest(e, SquaredQ = F)
ts.plot(y,yhat,lty=c(1,3),col=c(4,10),
        main="Plot Hasil Prediksi Data Curah Hujan Harian Luwu Utara")
legend("topleft",c("aktual", "prediksi"), bty = "n",
       lty=c(1,3), col=c(4,10),lwd=c(1,1), cex = 0.8)


nn.for = forecast.nn(nnCHL,dtransform, h = 30, rep = 9)
forecast = ts(denormalize(nn.for, data = data), start = 920, end = 949) #end nya janlup diubah
autoplot(y) + autolayer(forecast)

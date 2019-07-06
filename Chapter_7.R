library(fpp2)
#Prob. 7-1
fc <- ses(pigs, h=4)
autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") + ylab("number of pigs") + xlab("Year")
round(accuracy(fc),2)
checkresiduals(fc)
# sigma of the residual error
sqrt(var(resid(fc)))
# PI given by R
summary(fc)

#Prob. 7-2
exp.smoothing <- function(alpha, l_0, y) {
  y.smooth <- seq(from=1, to=length(y))
  y.smooth[1] <- alpha * y[1]+ (1-alpha) * l_0
  for(i in 2:length(y)) {
    y.smooth[i] <- (alpha * y[i])+ ((1-alpha) * y.smooth[i-1])
  }
  return(y.smooth)
}
alpha = 0.2971
l_0 = 77260.0561
y<- pigs
x <- exp.smoothing(alpha, l_0, y)
# They are different
x-fitted(fc)

# Problem 7-3
min.RSS <- function(data, par) {
  y.fc <- seq(from=1, to=length(data))
  y.fc[1] <- par[1] * data[1]+ (1-par[1]) * par[2]
  for(i in 2:length(data)) {
    y.fc[i] <- (par[1] * data[i])+ ((1-par[1]) * y.fc[i-1])
  }
  return(sum((y.fc-data)^2))
}

y <- pigs
(result <- optim(par = c(0.2, y[1]), fn = min.RSS, data = y))

# Problem 7-4
y <- pigs
result <- optim(par = c(0.2, y[1]), fn = min.RSS, data = y)
exp.smoothing(result$par[1], result$par[2], y)

#Problem 7-5
autoplot(books)
fcast.paperback <- ses(books[,'Paperback'], h=4)
fcast.hardcover <- ses(books[,'Hardcover'], h=4)
autoplot(fcast.paperback)+autolayer(fitted(fcast.paperback))
autoplot(fcast.hardcover)
accuracy(fcast.paperback)
accuracy(fcast.hardcover)

#Problem 7-6
holt.paperback <- holt(books[, "Paperback"], h=4)
holt.hardcover <- holt(books[, "Hardcover"], h=4)
accuracy(holt.paperback)
accuracy(holt.hardcover)

#Problem 7-7
fc <- holt(eggs, h=100, lambda = "auto")
fc2 <- holt(eggs, damped=TRUE, phi = 0.95, lambda = "auto", h=100)
autoplot(eggs) +
  autolayer(fc, series="Holt's method", PI=FALSE) +
  autolayer(fc2, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Price of dozen eggs in US") +
  guides(colour=guide_legend(title="Forecast"))

#Problem 7-8
retaildata <- readxl::read_excel("C:/Users/vahid/Documents/retail.xlsx", skip=1)
myts <- ts(retaildata[,"A3349873A"],
           frequency=12, start=c(1982,4))
autoplot(myts)
fit1 <- hw(myts,seasonal="multiplicative")
fit2 <- hw(myts,damped=TRUE, seasonal="multiplicative")
autoplot(myts) +
  autolayer(fit1, series="HW multiplicative forecasts", PI=FALSE)+
  autolayer(fit2, series="damped HW multiplicative forecasts", PI=FALSE)
accuracy(fit1)
accuracy(fit2)
checkresiduals(fit1)
checkresiduals(fit2)
train.data <- window(myts, end=2011)
test.data <- window(myts, start=2011, end=2012)
fit1.train <- hw(train.data, seasonal="multiplicative", h=11)
fit2.train <- hw(train.data, damped=TRUE, seasonal="multiplicative", h=11)
fit3.train <- naive(train.data, h=11)
ts_test.data <- ts(test.data[2:12], start = c(2011,2), freq=12)
# calcualte RMSE for different forecast method
sum((fit1.train$mean-ts_test.data)^2)
sum((fit2.train$mean-ts_test.data)^2)
sum((fit3.train$mean-ts_test.data)^2)

#problem 7-9
myts %>% autoplot()
myts %>%
  mstl( s.window="periodic", robust=TRUE) -> mstl.myts
autoplot(mstl.myts)
seasadj(mstl.myts) %>% autoplot()
ets.myts <- ets(seasadj(mstl.myts), model="ZZZ", damped=NULL, alpha=NULL, beta=NULL,
                gamma=NULL, phi=NULL, lambda=NULL, biasadj=FALSE,
                additive.only=FALSE, restrict=TRUE,
                allow.multiplicative.trend=FALSE)
autoplot(ets.myts)

#Problem 7-10
ukcars %>% autoplot()
ukcars %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) -> ukcars.stl

holt.fcst <- holt(seasadj(ukcars.stl), damped = F, h=8)
ets.seasadj.ukcars <- ets(seasadj(ukcars.stl))
forecast(ets.seasadj.ukcars) %>% accuracy()
summary(holt.fcst)
checkresiduals(ets.seasadj.ukcars)

#Problem 7-11
visitors %>% autoplot()
train.data.visitor <- window(visitors, end=2003)
hw(train.data.visitor, seasonal = "multiplicative", h=23)
ets.visitor <- ets(train.data.visitor, model="ZZZ", damped=NULL, alpha=NULL, beta=NULL,
                   gamma=NULL, phi=NULL, lambda=NULL, biasadj=FALSE,
                   additive.only=FALSE, restrict=TRUE,
                   allow.multiplicative.trend=FALSE)
ets.visitor.additive <- ets(train.data.visitor, model="ZZZ", damped=NULL, alpha=NULL, beta=NULL,
                            gamma=NULL, phi=NULL, lambda=TRUE, biasadj=FALSE,
                            additive.only=T, restrict=TRUE,
                            allow.multiplicative.trend=FALSE)
snaive.visitor <- snaive(train.data.visitor, h=23)
train.data.visitor %>%
  stl( s.window="periodic", robust=TRUE)%>% seasadj()%>%ets(, model="ZZZ", damped=NULL, alpha=NULL, beta=NULL,
                                                            gamma=NULL, phi=NULL, lambda=TRUE, biasadj=FALSE,                                                                        additive.only=T, restrict=TRUE,allow.multiplicative.trend=FALSE)
#Problem 7-12
fcast <- stlf(elecequip, method='naive')
fets <- function(y, h) {
  forecast(ets(y), h = h)
}
fets.snaive <- function(y, h) {
  forecast(snaive(y), h = h)
}
e1 <- tsCV(qcement, fets, h=4)
e2 <- tsCV(qcement, fets.snaive, h=4)

mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)


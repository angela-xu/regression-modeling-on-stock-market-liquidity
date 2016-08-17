####Environment Setup####
install.packages('Rcmdr')
stock=read.csv(choose.files(), header=T)
attach(stock)
library(abind)


#####proposal part#####
    ####Summary of variables####
    numSummary(stock[,c("AVGT", "DEBEQ", "NTRAN", "PRICE", "SHARE", "VALUE", "VOLUME")], statistics=c("mean", "sd",'skewness', "quantiles"), quantiles=c(0,.25,.75,1))

    ####Summary of correlation####
    cor(stock[,c("AVGT","DEBEQ","NTRAN","PRICE","SHARE","VALUE","VOLUME")], use="complete.obs")

    ####Correlation Scatter Plot####
    pairs(stock[,c("AVGT", "DEBEQ", "NTRAN", "PRICE", "SHARE", "VALUE", "VOLUME")],upper.panel=NULL,xaxt="n",yaxt="n",gap=0,cex.labels=1, main='Scatter plots of variables')


####Model 1####
    model1=lm(VOLUME~NTRAN)
    summary(model1)
    plot(model1$residuals,xlab = '')    ####In this figure, the residuals are plotted against the observation
    plot(NTRAN,model1$residuals)    ####Here I made a second plot that may have different implications. This plot and the plot above should give us same judgement when used to see whether variances of the residuals are equal.
    qqnorm(model1$residuals, main ='Normal Q Q Plot(Model1)')
    
    ####histogram of model1####
    hist(model1$residuals,freq= F,breaks=20)
    a1=seq(min(model1$residuals),max(model1$residuals),0.001)
    b1=dnorm(a1,mean(model1$residuals),sd(model1$residuals))
    lines(a1,b1,col='blue',lwd=2)
    
    ####table 5.4，P159 Only use the first line of the output####
    cor(stock[,c("residual.model1","AVGT","DEBEQ","PRICE","SHARE","VALUE")], use="complete.obs")
    
    ####Figure5.5,#### 
    plot(stock$residual.model1, AVGT, main='Scatter plot of Residuals(model1) and AVGT')

    
####Model 2####
    model2=lm(VOLUME~NTRAN+AVGT)
    summary(model2)
    plot(model2$residual,xlab ='')
    qqnorm(model2$residuals, main ='Normal Q Q Plot(Model2)')
    
    ####histogram of model2####
    hist(model2$residuals,freq= F,breaks=20)
    a2=seq(min(model2$residuals),max(model2$residuals),0.001)
    b2=dnorm(a2,mean(model2$residuals),sd(model2$residuals))
    lines(a2,b2,col='blue',lwd=2)
    
    
####Model 3####
    model3=lm(VOLUME~NTRAN+AVGT+PRICE+DEBEQ+SHARE+VALUE)
    summary(model3)
    plot(model3$residuals,xlab = '')
    qqnorm(model1$residuals, main ='Normal Q Q Plot(Model3)')
    
    ####histogram of model3####
    hist(model3$residuals,freq= F,breaks=20)
    a3=seq(min(model3$residuals),max(model3$residuals),0.001)
    b3=dnorm(a3,mean(model3$residuals),sd(model3$residuals))
    lines(a3,b3,col='blue',lwd=2)
####Model 4####
    model4=lm(logvol~logntr+logavgt)
    plot(model4$residuals,xlab = '')
    qqnorm(model4$residuals, main ='Normal Q Q Plot(Model4)')

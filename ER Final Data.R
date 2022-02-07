library(readxl)
ER_Final_Data <- read_excel("Documents/Accunique/9-Dickey Fuller test1200/ER Final Data.xlsx")
View(ER_Final_Data)                                                                                                     
data <- as.data.frame(ER_Final_Data)
head(data)

Rt <- data$`stock return`
EPU <- data$`Economic Policy Uncertainty Index`
CCI <- data$`Investor Sentiment (CCI)`
CPI <- data$CPI
Rf <- data$non_risk

model <- lm(Rt ~ EPU + CCI + CPI + Rf, data = data)
summary(model)

#Call:
#  lm(formula = Rt ~ EPU + CCI + CPI + Rf, data = data)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-0.5361 -0.1371  0.0277  0.1345  0.4182 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -4.722845   0.733061  -6.443 2.78e-09 ***
#  EPU          0.042667   0.005339   7.992 1.09e-12 ***
#  CCI          0.631954   0.342929   1.843   0.0679 .  
#  CPI         -0.005706   0.002660  -2.145   0.0340 *  
#  Rf           0.032966   0.004595   7.175 7.34e-11 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.191 on 116 degrees of freedom
#Multiple R-squared:  0.5197,	Adjusted R-squared:  0.5032 
#F-statistic: 31.38 on 4 and 116 DF,  p-value: < 2.2e-16


library(ggplot2)

ggplot(model, aes(y=Rt, x=EPU), title = "ggplot for linear regression between EPU and Rt") +
  + geom_point()+
  + geom_smooth(method = lm) +
  + ggtitle("ggplot for linear regression between EPU and Rt")

ggplot(model, aes(y=Rt, x=Rf), title = "ggplot for linear regression between Rf and Rt") +
  + geom_point()+
  + geom_smooth(method = lm) +
  + ggtitle("ggplot for linear regression between Rf and Rt")

dat <- read.table('Demographic.txt')
head(dat)
n = nrow(dat)
n

# Y = (column 10) total number of serious crimes in 1990
Y = dat[,10]
Y
boxplot(Y)

# 17 Geographic region: choose 3 = W
datW = dat[dat[,17]==3,]
nW = nrow(datW)

# no need to consider non-numeric variables: 
# 1. ID, 2. county, 3.state

#For this project we will be choosing column 9 and 16 as our predictors

pairs(datW[, c(10,9,16)])
# Y appears to have positive trend with 
# 9. total population and 16


cor(datW[,c(10, 9, 16)])
# 9,16 are highly correlated. 
# V9 has r = 0.907 with Y
# V16 has r = 0.919 with Y


Y = datW[,10]
X9 = datW[,9]
X16 = datW[,16]

anova(lm(Y ~ X9 + X16))

xy.lm = lm(Y ~ X9 + X16)
summary(xy.lm)
anova(xy.lm)

# (a) conf. int. for \beta_j
p = 3
b = xy.lm$coefficients
anv = anova(xy.lm)
MSE = anv$`Mean Sq`[p]

X = model.matrix(xy.lm)
XtX = t(X)%*%X

VarCov.b = MSE * solve(XtX)
SE.b = sqrt(diag(VarCov.b))

out = summary(xy.lm)
coef = out$coefficients
SE.b = coef[,2]

alpha = 0.10
b + qt(1-alpha/2, df = nW - p) * SE.b
b - qt(1-alpha/2, df = nW - p) * SE.b

#(b) P value

##B1
#Ho: B1 = 0, Ha: B1 != 0
#Decision Rule if p value is less than a = 0.01 we reject Ho

pval.b1 = 2 * pt(q = abs(b[2]/SE.b[2]), df = nW - p, lower.tail = F)
pval.b1

#Conclusion: 2.001e-16 < 0.01 so we reject Ho under significance level a = 0.01.

##B2
#Ho: B2 = 0, Ha: B2 != 0
#Decision Rule if p value is less than a = 0.01 we reject Ho

pval.b2 = 2 * pt(q = abs(b[3]/SE.b[3]), df = nW - p, lower.tail = F)
pval.b2

#Conclusion: 7.02e-21 < 0.01 so we reject Ho under significance level a = 0.01.

# test for H0: \beta_1 = \beta_2 = 0
SSR = sum(anv$`Sum Sq`[1:2])
SSE = anv$`Sum Sq`[3]
F.stat = (SSR/(p-1)) / (SSE/(nW-p))
F.stat

pval = pf(F.stat, df1 = p-1, df2 = nW-p, lower.tail = F)
pval

#Decision Rule: If p value is less than 0.05, we reject Ho(F stat has been converted to p value)
#Conclusion: 7.71e-76 < 0.05 so we reject Ho under significance level a = 0.05

adj.R2 = 1 - (SSE/(nW-p)) / ((SSR+SSE)/(nW-1))
adj.R2

#R^2 is 0.9 which is a closer value to 1 so it indicates that this model is a good fit.

# diagnostic plots
res = xy.lm$residuals

##Residuals vs Leverage
#Cook and Harris has relatively high leverage point but we don't need to take it out

##Residuals vs Fitted
#Many outliers
#Not all plots are equally spread around 0 and we cannot conclude a non linear relationship

##Normal Q-Q plot
#Not all residuals align with the straight dashed line especially in both tail ends
#From the outliers, we cannot conclude that it is normally distributed

##Scale Location
#Not all residuals spread out without pattern and not equal variance implying that it is not a good model

plot(xy.lm)

# We can see multiple outliers in both graphs



# PS I - Regression Analysis ----------------------------------------------


# \\ Problem 1 \\ ---------------------------------------------------------

rm(list=ls())
library(RColorBrewer)
c = brewer.pal(7,"Reds")

# The given data pertains to the information on 'death due 
# to lung cancer (millions)' and 'per capita cigarette 
# consumption (annual)' across 11 different countries.

# \\ Let Y - denote the # deaths due to Lung Cancer observed
# in millions and,
# let X - denote the per capita cigarette consumption measured
# annually.\\

# \\ deaths due to lung cancer (millions) \\
y = c(58,115,165,190,250,465,90,150,170,245,350)

# \\ per capita cigarette consumption (annual) \\
x = c(220,310,380,1280,530,1145,250,510,455,460,415)

# \\ A Scatterplot of 'deaths due to lung cancer' on
# per capita cigarette consumption \\

plot(x,y,main="Deaths due to Lung Cancer (millions)
     vs Per Capita cigarette consumption (annual)",
     xlab="Per Capita Cigarette Consumption (annual)",
     ylab="Deaths due to Lung Cancer (millions)",
     pch=16)

# \\ From the scatterplot above, it can be clearly observed
# that, with the increase in the per capita cigarette 
# consumption, there is a prominent increase in the number
# of deaths due to Lung Cancer, which is quite natural. Besides,
# this 'increasing trend', it also to be noted that, outliers
# with respect to both x and y axes are present in the given
# data.

# Also, the strength of linear relationship or association
# between the # deaths due to Lung Cancer and Cigarette 
# consumption is moderate, i.e., r_xy = 0.5724. \\


# \\ (a) Appropriate Regression Model \\ -------------------------------------

# \\ In reference to the Scatterplot, the number of deaths
# due to lung cancer seems almost 'linear increasing' with
# respect to the per capita cigarette consumption. This
# relationship, i.e., the degree of 'non-linearity' is 
# further validated and studied in the following lines 
# through an appropriate regression fit. \\

# \\ In particular, we consider, 
# y = f(x) + u, where the degree of f(.) is to be determined
# for an appropriate regression fit. \\

# \\ Simple Linear Regression: y = beta_0 + beta_1*x + u,
# where R^2 = 0.3277. \\

m1 = lm(y~x)

# \\ Quadratic Regression: y =  beta_0 + beta_1*x + beta_2*x^2 + u,
# where R^2 = 0.5487. \\

m2 = lm(y~x+I(x^2))

# \\ Regression of order 3: y =  beta_0 + beta_1*x + beta_2*x^2 +
# beta_3*x^3 + u, where R^2 = 0.6750. \\

m3 = lm(y~x+I(x^2)+I(x^3))

# \\ Regression of order 4: y =  beta_0 + beta_1*x + beta_2*x^2 +
# beta_3*x^3 + beta_4*x^4 + u, where R^2 = 0.7747. \\

m4 = lm(y~x+I(x^2)+I(x^3)+I(x^4))

# \\ Regression of order 5: y =  beta_0 + beta_1*x + beta_2*x^2 +
# beta_3*x^3 + beta_4*x^4 + beta_5*x^5 + u, where R^2 = 0.7986. \\

m5 = lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5))

# \\ Regression of order 6: y =  beta_0 + beta_1*x + beta_2*x^2 +
# beta_3*x^3 + beta_4*x^4 + beta_5*x^5 + beta_6*x^6 + u, 
# where R^2 = 0.8073. \\

m6 = lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6))

# \\ Regression of order 7: y =  beta_0 + beta_1*x + beta_2*x^2 +
# beta_3*x^3 + beta_4*x^4 + beta_5*x^5 + beta_6*x^6 + beta_7*x^7+ u, 
# where R^2 = 0.8811. \\

m7 = lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7))


# \\ NOTE: 

# 1. After the regression fit of order 7, there is 
# no significant change observed in the value of R^2, apart 
# from the usual increase in the magnitude with the increase
# in the degree of the regression.

# 2. Also for the regression fits of order 4, 5 and 6, the
# change in the magnitude of R^2 was very 'close' unlike the
# change in the case of the regression fit of order 7.

# 3. Even though the R^2 value for the regression fit of
# order 7 is significantly higher, the regression coefficients
# associated to x^3 on-wards are observed to be very close to
# zero (0). 

# 4. The points (1145,465), (1280,190) and (415,350) seems
# to be the possible candidates for 'outlying observations'.
# Therefore, we remove these pairs of observations from the 
# set up and refit the simple linear regression. //

x_new = x[-c(4,6,11)]
y_new = y[-c(4,6,11)]

m1_new = lm(y_new~x_new)
summary(m1_new)

plot(x,y,main="Linear Regression Fit of deaths due to Lung Cancer
     on cigarette consumption after removing
     unusual observations",xlab="Per Capita Cigarette Consumption (annual)",
     ylab="Deaths due to Lung Cancer (millions)",pch=16)
abline(m1_new)
abline(m1,col="red",lty=2)
points(c(x[4],x[6],x[11]),c(y[4],y[6],y[11]),pch=16,col="red")

# \\ The magnitude of R^2 increases from 0.5487 to 0.7550
# when the possible unusual observations were removed. But,
# we need to validate the above points as unusual, i.e.,
# classifying them as outliers or influential. \\

# \\ For detection of the outliers, influential points and 
# leverages, further diagnostics for instance, 'Standardized',
# 'Studentized' Residuals, 'DFBETA', 'DFFIT' and 'Cook's Distance'
# are presented. \\

# \\ Here we work with the 'simple linear regression model' of
# the deaths due to lung cancer on cigarette consumption. \\

# \\ (b) Further Diagnostics - Standardized, Studentized Residuals \\ --------

par(mfrow=c(2,2))

# \\ The Residual Plot \\

plot(resid(m1),main="Residual Plot of the Linear Regression Fit",
     xlab="Observation #",ylab=bquote("Residuals---"~e[i]),
     pch=16,type="h")
abline(h=0,lty=2)

# \\ The Standardized Residual Plot \\

plot(abs(rstandard(m1)),main="Standardized Residual Plot of the 
     Linear Regression Fit",
     xlab="Observation #",ylab=bquote("Standardized Residuals---"~"|"~t[i]~"|"),
     pch=16,col="blue",type="h")
abline(h=2,lty=2)

# \\ The Studentized Residual Plot \\

plot(abs(rstudent(m1)),main="Studentized Residual Plot of the 
     Linear Regression Fit",
     xlab="Observation #",ylab=bquote("Studentized Residuals---"~"|"~t[i]~"*|"),
     pch=16,col="red",type="h")
abline(h=2,lty=2)
which(rstudent(m1)>2)

# \\ The Leverage - hii \\

plot(hatvalues(m1),main="Leverages (hii) of the Linear Regression Fit",
     xlab="Observation #",ylab=bquote("Leverages---"~h[ii]),
     pch=16,col="brown",type="h")
abline(h=(4/11),lty=2)
which(hatvalues(m1)>(4/11))

# \\ NOTE: 
# 1. According to the thumb rule in case of identifying 
# outlying observations on the basis of studentized and 
# standardized residuals, we have, |t_i| and |t_i*|>2,
# which implies that observations 4, 6 and 11 as marked 
# out earlier are potential outlying observations based on
# studentized residual.

# 2. Where as if we observe the leverages (hii > 2p/n), it is to be noted
# that observations 4 and 6 are potential leverage points, influencing
# the regression line.

# 3. Observations 4 and 6 can be clearly noted to be outliers
# in the direction of the x-axis, i.e., 'regressor outliers'/
# 'leverages', which greatly/dramatically influence the regression
# line, but observation 11 is an outlier in the direction of y-axis,
# i.e., 'response outlier', which does not seem to affect the regression
# much. \\

# \\ Therefore, we validate in the following lines, whether
# observations 4 and 6 are 'influential' . \\

# \\ (c) DFBETA, DFFIT, Cook's Distance \\ --------------------------------

# \\ Belsley, Kuh, and Welsch recommend 2 as a general cutoff value to 
# indicate influential observations and 2/root(n) as a 
# size-adjusted cutoff. \\

DFBETA = dfbetas(m1)
ols_plot_dfbetas(m1)

# \\ An observation is deemed influential if the absolute value of its 
# DFFITS value is greater than:
# 2*sqrt(p+1)/(n-p-1)
# where n is the number of observations and p is the number
# of predictors including the intercept. \\

DFFIT = dffits(m1)
ols_plot_dffits(m1)

# \\ Here we consider the threshold value or the cut off to
# be 1, i.e., D_i > 1, would denote that the particular observation
# is an influential point. \\

CD = cooks.distance(m1)
ols_plot_cooksd_chart(m1)


# \\ NOTE:

# 1. In addition to the diagnostics of the fitted regression
# model presented on the basis of studentized residuals and
# leverages, the DFBETAs and DFFITs also suggests that, 
# observations 4 and 6 are respectively outliers in the direction 
# of x-axis, i.e., leverage as well as influential observations, thereby,
# drastically affecting the simple linear regression fit of the deaths
# due to lung cancer on the cigarette consumption.

# 2. Therefore, we eliminate the influential points as well as the
# response outlier and re-fit the linear regression model to the modified data,
# as illustrated in Section 1. \\



# -------------------------------------------------------------------------


# \\ Problem II \\ --------------------------------------------------------
rm(list=ls())
# \\ The given data pertains to the 'light intensities' and
# the 'surface temperature' (both in logarithm) of 47 stars.
# It is known that light intensity of a star depends on its
# surface temperature. 

# Therefore, let Y: denote the 'light intensity' of the star,
# and X: denote the 'surface temperature' of the star.

# In particular, y = f(x) + u, where f(.) - the degree of
# relationship or dependency of light intensity on the surface
# temperature is to be determined for an appropriate regression
# fit. \\

# \\ light intensity of the stars (in logarithm) \\
y = c(5.23,5.74,4.93,5.74,5.19,5.46,4.65,5.27,5.57,5.12,
      5.73,5.45,5.42,4.05,4.26,4.58,3.94,4.18,5.89,4.38,
      4.22,4.42,4.85,5.02,4.66,4.66,4.90,4.90,4.39,6.05,
      4.42,5.10,5.22,6.29,4.34,5.62,5.10,5.22,5.18,5.57,
      4.62,5.06,5.34,5.34,5.54,4.98,4.50)

# \\ surface temperature of the stars (in logarithms) \\
x = c(4.37,4.56,4.26,4.56,4.30,4.46,3.84,4.57,4.26,4.37,
      3.49,4.43,4.48,4.01,4.29,4.42,4.23,4.42,4.23,3.49,
      4.29,4.29,4.42,4.49,4.38,4.42,4.29,4.38,4.22,3.48,
      4.38,4.56,4.45,3.49,4.23,4.62,4.53,4.45,4.53,4.43,
      4.38,4.45,4.50,4.45,4.55,4.45,4.42)

# \\ A Scatterplot of 'light intensity' on
# 'surface temperature' \\

par(mfrow=c(1,2))
plot(x,y,main="Light Intensity vs Surface Temperature
     (in logarithms)",
     xlab="Surface Temperature (in logarithm)",
     ylab="Light Intensity (in logarithm)",
     pch=16)

plot(exp(x),exp(y),main="Light Intensity vs Surface Temperature
     (taking e)",
     xlab="Surface Temperature",
     ylab="Light Intensity",
     pch=16)

# \\ Note that, both the above scatterplots, shows a clustering
# of observations towards the higher index of surface temperature
# around the light intensity value 4.0 to 5.5 (in logarithms).
# Besides that, light intensity seems to be 'non-linearly' related
# to the surface temperature with a roughly decreasing trend,
# along with some unusual observations
# at considerable lower values of the x (surface temperature).
# Hence, we validate the relationship between the two variables
# after studying and analyzing the outliers, leverages and influential
# observations in the given data. \\


# \\ A Regression Fit of Light Intensity on Surface Temperature \\ --------

# \\ We have considered a 'quadratic' regression of the light
# intensity on the surface temperature, as after the second degree
# the change in the R^2 value is nominal. Also, the p-values
# corresponding to the regression coefficients comes out to be
# > 0.05 after degree 2 of the regression fit, implying the 
# insignificance of the coefficients. \\

m = lm(y~x+I(x^2))
scatterplot(x,y,boxplots=F,pch=16,main="Quadratic Regression Fit of Light Intensity
     on Surface Temperature",xlab="Surface Temperature (in logartihm)",
            ylab="Light Intensity (in logartihm)")
points(x[c(11,9,19,20,30,34,36)],y[c(11,9,19,20,30,34,36)],
       pch=16,col="red") # \\ suspected unusualities \\

# \\ In order to detect the unusual observations including
# outliers, leverages and influential points, the quadratic
# regression fit will be considered and later modified accounting
# for such observations. \\



# \\ Further Diagnosis -  Standardized & Studentized Residuals, Le --------

par(mfrow=c(2,2))

# \\ The Residual Plot \\

plot(resid(m),main="Residual Plot of the Quadratic Regression Fit",
     xlab="Observation #",ylab=bquote("Residuals---"~e[i]),
     pch=16,type="h")
abline(h=0,lty=2)

# \\ The Standardized Residual Plot \\

plot(abs(rstandard(m)),main="Standardized Residual Plot of the 
     Quadratic Regression Fit",
     xlab="Observation #",ylab=bquote("Standardized Residuals---"~"|"~t[i]~"|"),
     pch=16,col="blue",type="h")
abline(h=2,lty=2)

# \\ The Studentized Residual Plot \\

plot(abs(rstudent(m)),main="Studentized Residual Plot of the 
     Quadratic Regression Fit",
     xlab="Observation #",ylab=bquote("Studentized Residuals---"~"|"~t[i]~"*|"),
     pch=16,col="red",type="h")
abline(h=2,lty=2)
which(rstudent(m)>2)

# \\ The Leverage - hii \\

plot(hatvalues(m),main="Leverages (hii) of the Quadratic Regression Fit",
     xlab="Observation #",ylab=bquote("Leverages---"~h[ii]),
     pch=16,col="brown",type="h")
abline(h=(6/47),lty=2)
which(hatvalues(m)>(6/47))


# \\ NOTE:

# 1. In reference to the studentized residuals, observations
# 9, 19 and 20 are potential outlying observations, i.e., 
# according to the thumb rule |t_i*|>2, for i = 9,19,20.

# 2. Where as on the basis of the hat values, observations
# 11,20,30,34 and 36 are identified as potential leverages,
# with hii>2p/n = 6/47. \\



# \\ Diagnosis of Influential Observations - DFBETA, DFFIT and Cook's D \\ --------

# \\ Belsley, Kuh, and Welsch recommend 2 as a general cutoff value to 
# indicate influential observations and 2/root(n) as a 
# size-adjusted cutoff. \\

DFBETA = dfbetas(m)
ols_plot_dfbetas(m)

# \\ An observation is deemed influential if the absolute value of its 
# DFFITS value is greater than:
# 2*sqrt(p+1)/(n-p-1)
# where n is the number of observations and p is the number
# of predictors including the intercept. \\

DFFIT = dffits(m)
ols_plot_dffits(m)

# \\ Here we consider the threshold value or the cut off to
# be 1, i.e., D_i > 1, would denote that the particular observation
# is an influential point. \\

CD = cooks.distance(m)
ols_plot_cooksd_chart(m)
plot(as.vector(CD),type="h",ylim=c(0,1))
abline(h=1)


# \\ NOTE:

# 1. In addition to the diagnosis of the fitted quadratic
# regression line of light intensity of the stars on the 
# surface temperature provided by the residuals and leverages,
# the other measures such as DFBETAs and DFFITs,
# identifies observations 9,19,20,30 and 34 as potential
# influential observations, which affects the regression line.

# 2. Where as, the Cooks D value for the observations 19,20,
# 30 and 34 are comparatively large, and D_20 is almost close
# to 1. 

# 3. Therefore we consider the observations 9,19,20,30 and 34 as
# influential observations, implying that they are outliers too.
# Observe that observations 9 and 19 are outliers in the direction
# of y-axis, i.e., response outliers, where as observations 20,30 and
# 34 are regressor outliers, drastically influencing the regression fit. \\



# \\ Removing the Outliers and Influential Observations from the set up \\ --------

x_new = x[-c(9,19,20,30,34)]
y_new = y[-c(9,19,20,30,34)]

m_new = lm(y_new~x_new+I(x_new^2))
summary(m_new)

scatterplot(x_new,y_new,boxplots=F,pch=16,main="Quadratic Regression Fit of Light Intensity
     on Surface Temperature after removal of
            influential and outlying observations",xlab="Surface Temperature (in logartihm)",
           ylab="Light Intensity (in logartihm)",ylim=c(3,7))
points(x[c(9,19,20,30,34)],y[c(9,19,20,30,34)],pch=16,col="red")


# \\ NOTE:

# Therefore on removing the outliers and influential 
# (leverages) observations, the R^2 value increases to 0.5745
# from 0.3538, and hence we get an appropriate quadratic 
# regression fit of light intensity of the stars on the surface
# temperature. \\

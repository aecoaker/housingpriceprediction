library(GGally)
library(dplyr)
library(olsrr)
library(car)

housing <- read.csv('housing.csv')
#running some inital exploratory analysis
ggpairs(housing, lower = list(continuous = wrap("smooth", alpha = 0.3, color = "blue")),
        diag = list(continuous = wrap("barDiag", colour = "blue")),
        upper = list(continuous = wrap("cor", size = 5),binwidth=15))

#exploring the data to test for the outlier(s) that appear
par(mfrow=c(1,3))
boxplot(housing$price)
boxplot(housing$sqft)
boxplot(housing$bath)
housing[housing$bath>60&housing$price>10000000&housing$sqft>10000,]
#removing what is clearly human error
housing.new <- housing %>%
  filter(!housing$bath>60) 

#Merging 'Not Provided' and 'No Parking' into 'Parking not available'
housing.new$parking[housing.new$parking=='Not Provided'] <- 'Parking not available'
housing.new$parking[housing.new$parking=='No Parking'] <- 'Parking not available'
#confirming there is no natural order to this categorical variable
ggplot(housing.new) + aes(x=parking,y=price) +
  geom_boxplot() + ggtitle('Price by Parking Type') +
  theme(plot.title = element_text(hjust = 0.5)) + xlab('Parking Type') +
  ylab('House Price')

#and now running our exploratory plot again
ggpairs(housing.new, lower = list(continuous = wrap("smooth", alpha = 0.3, color = "blue")),
        diag = list(continuous = wrap("barDiag", colour = "blue")),
        upper = list(continuous = wrap("cor", size = 5),binwidth=15))
#there is multicollinearity between dist_am1 and dist_am3
#so we do not use dist_am3 in our model

#fitting an initial linear model
housing.new.lm <- lm(price ~ elevation + dist_am1 + dist_am2 + bath + sqft + parking
   + precip, data=housing.new)
summary(housing.new.lm)
#the R^2(adj) value is 0.86, good but could be better

#as an aside, building a lm of the original data set quickly
housing.lm <-  lm(price ~ elevation + dist_am1 + dist_am2 + bath + sqft + parking
                  + precip, data=housing)
outlierTest(housing.lm) #formally shows that the row we removed was an outlier

#improving the model by intuition
#from the p-values the (by far) most significant variable is bath
housing.bath.lm <- lm(price~bath, data=housing.new)
summary(housing.bath.lm)
#by it's R^2 value this is likely a far better fit
#the only other p-values less than 0.1 were for parking
housing.bath.parking.lm <- lm(price~bath+parking, data=housing)
summary(housing.bath.parking.lm)
#this gives a slightly lower R^2(adj) 
#so by inituition we say that price ~ bath provides the best fit

#checking this model fit using stepwise regression
pricing.model.selection<-ols_step_all_possible(housing.new.lm)
pricing.model.selection
plot(pricing.model.selection)
pricing.model.selection[c(1,8,29,64,99,120,127), ]
#adding any variables beyond bath provides only very marginal improvement
#as predicted housing.bath.lm is the best fit

#producing a CI for this model
confint(housing.bath.lm)
#visualising this & the rest of the model
ggplot(housing.new) + aes(x=bath,y=price) +
  geom_point() + geom_smooth(method="lm") + 
  ggtitle('Number of Bathrooms versus House Price') +
  xlab('Number of Bathrooms') + ylab('House Price')
#there is clearly a strong fit

#checking assumptions using residual plots
ggplot(data=housing.new, aes(x=housing.bath.lm$residuals)) +
  geom_histogram() + ggtitle('Histogram of Residuals') +
  xlab('Residuals') + ylab('Frequency')
par(mfrow=c(2,2))
plot(housing.bath.lm)
#the histogram of residuals
#the residuals vs fitted plot does not indicate a linear regression may be inappropriate
#the QQ plot does seem to have lighter tails
#so more data than we would expect for the normal distribution is concentrated in the centre

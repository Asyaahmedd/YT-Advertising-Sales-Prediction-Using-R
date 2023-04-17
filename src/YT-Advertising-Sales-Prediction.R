#Predict future sales on the basis of advertising budget spent on youtube.
#------------------------------------------------------------------------
# Load required R packages
#------------------------------------------------------------------------
library(tidyverse) # for data manipulation and visualization
library(ggpubr) # creates easily a publication ready-plot
library(devtools) # for installing packages from GitHub
#------------------------------------------------------------------------
# Install and load the datarium package
#------------------------------------------------------------------------
install_github("kassambara/datarium")
library(datarium)
#------------------------------------------------------------------------
# Load the marketing dataset from datarium package
data("marketing")
head(marketing, 4)
#the data contains the impact of three advertising media (youtube, Facebook and newspaper) on sales.
#Data are the advertising budget in thousands of dollars along with the sale
# I want to predict future sales on the basis of advertising budget spent on youtube.
#------------------------------------------------------------------------
#Visualization
#------------------------------------------------------------------------
#Create a scatter plot displaying the sales units versus youtube advertising budget.
ggplot(marketing, aes(x = youtube, y = sales)) +
  geom_point() +
  stat_smooth()
# The graph above suggests a linearly increasing relationship between the sales and the youtube variables.
#---------------------------------------------------------------------------
# compute the correlation coefficient 
cor(marketing$sales, marketing$youtube)
# the correlation coefficient is large enough, so I can continue by building a linear model of y as a function of x.
#--------------------------------------------------------------------------
#Computation
#--------------------------------------------------------------------------
model <- lm(sales ~ youtube, data = marketing)
model
#---------------------------------------------------------------------------
#Interpretation
#---------------------------------------------------------------------------
#From the output above:
  
# 1- the estimated regression line equation can be written as follow: sales = 8.44 + 0.048*youtube

# 2- the intercept (b0) is 8.44. It can be interpreted as the predicted sales unit for a zero youtube advertising budget. Recall that, we are operating in units of thousand dollars. This means that, for a youtube advertising budget equal zero, we can expect a sale of 8.44 *1000 = 8440 dollars.

# 3- the regression beta coefficient for the variable youtube (b1), also known as the slope, is 0.048. 
#This means that, for a YouTube advertising budget equal to 1000 dollars, we can expect an increase of 48 units (0.048*1000) in sales. That is, sales = 8.44 + 0.048*1000 = 56.44 units. As we are operating in units of thousand dollars, this represents a sale of 56440 dollars.
#-------------------------------------------------------------------------
#Regression line
#--------------------------------------------------------------------------
ggplot(marketing, aes(youtube, sales)) +
  geom_point() +
  stat_smooth(method = lm)
#-------------------------------------------------------------------------
#Model assessment
#-------------------------------------------------------------------------
#In the previous section, I built a linear model of sales as a function of YouTube advertising budget:
#sales = 8.44 + 0.048*YouTube.
#Before using this formula to predict future sales, I should make sure that this model is statistically significant, that is:
  
# 1- There is a statistically significant relationship between the predictor and the outcome variables
# 2- The model that we built fits very well the data in our hand.
#-------------------------------------------------------------------------
#Model summary
#-------------------------------------------------------------------------
summary(model)
#The summary outputs shows 6 components, including:

# 1. Call. Shows the function call used to compute the regression model.
# 2. Residuals. Provide a quick view of the distribution of the residuals, which by definition have a mean zero. Therefore, the median should not be far from zero, and the minimum and maximum should be roughly equal in absolute value.
# 3. Coefficients. Shows the regression beta coefficients and their statistical significance. Predictor variables, that are significantly associated to the outcome variable, are marked by stars.
# 4. Residual standard error (RSE), R-squared (R2) and the F-statistic are metrics that are used to check how well the model fits to our data.
#------------------------------------------------------------------------
#Coefficients significance
#------------------------------------------------------------------------
# Both the p-values for the intercept and the predictor variable are highly significant,
#so we can reject the null hypothesis and accept the alternative hypothesis,
#which means that there is a significant association between the predictor and the outcome variables.

#-------------------------------------------------------------------------
#Standard errors and confidence intervals
#--------------------------------------------------------------------------
confint(model)
# That is, there is approximately a 95% chance that the interval [0.042, 0.052] will contain the true value of b1.
#Similarly the 95% confidence interval for b0 can be computed as b0 +/- 2*SE(b0).
#--------------------------------------------------------------------------
#Model assumptions
#--------------------------------------------------------------------------
#Residual standard error (RSE).
sigma(model)*100/mean(marketing$sales)
#-------------------------------------------------------------------------

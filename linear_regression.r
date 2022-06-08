#### libraries ---------

library(tidyverse)
library(report)
library(broom)
library(gvlma)
library(papaja)

#### setup ----------

set.seed(42)

#### regression ---------
 
view(mtcars)


?lm

lm_model_1 <- lm(vs ~ mpg, data = mtcars)

summary(lm_model_1)

view(lm_model_1)

report::report(lm_model_1)

apa_lm <- papaja::apa_print(lm_model_1)

apa_table(
  apa_lm$table
  , caption = "A full regression table."
)

broom::tidy(lm_model_1)

broom::augment(lm_model_1)

broom::glance(lm_model_1)




# https://www.scribbr.com/statistics/linear-regression-in-r/


#### assumptions ------------

# Linearity: The relationship between X and the mean of Y is linear.
# Homoscedasticity: The variance of residual is the same for any value of X.
# Independence: Observations are independent of each other.
# Normality: For any fixed value of X, Y is normally distributed.

plot(lm_model_1) 

lm_model_1_gvlma <- gvlma::gvlma(lm_model_1)

plot(lm_model_1_gvlma)

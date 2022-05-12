require(tidyverse)
require(knitr)

data <- read.csv("bio239_spring2022.csv")
data

knitr::kable(head(data[, 1:5]), "pipe")

data2 <-
  data %>%
  rename("Fall East" = "east_fall",
         "Spring East" = "east_spring",
         "Fall North" = "north_fall",
         "Spring North" = "north_spring")

# The starting model contains, as its predictors, the main effect of the response
# factor (in our study, the species of insect at a site, or mound) and the main effects
# and and mutual interactions of th explanatory factors. The response variable in this model
# is the counts. We set up the null model (with a Poisson distribution and an implicit log 
# link function) as follows:

data3 <- read.csv("bio239_spring2022b.csv")
data3

data3 <- as_tibble(data3)
data3

glm.x0 <- glm( count ~ species + season * mound, data = data3, family = poisson)

# We compare this model with alternative models, each extended by one of the two
# interactions between the response factor (species) and an explanatory factor:

add1 ( glm.x0, scope = ~species * season * mound, test = "Chisq")

# We use the `scope` parameter to specify the target model with a full interaction among all 
# three factors for the `add1` function, but the function has its own rules regarding the 
# sequence of testing interactions of different orders. Consequently, the test of the second-order
# interaction (i.e., the )

# Both tested interactions have a significant effect (p < 0.001) and explain a relatively large 
# fraction of the variability that was not explained by the null model.
# (See the Deviance column: e.g., species:season term explains (100 x (322.99 - 25.78)/322.99) = XX.XX%) 
# of the variation unexplained by the glm.x0 model.)

# The tests are based on X^2 (Chi-Square) statistics (due to a specific test parameter in the add1 function call)
# but in the add1 output this statistic is labelled LRT (likelihood ratio test), which it indeed represents.

# We also not that the `add1` function scores the 3 models under comparison with the 
# information-theoretic statistic (AIC) value and this comparison  confirms the appropriateness of adding both 
# interactions to the model. The effect of the mound type seems to be larger than the season, which is in agreement
# with the likelihood ratio test.

# So we now extend our model by both first-order interactions and call `add1` again with the extended 
# model to test the remaining second-order interactions:

glm.x1 <- update ( glm.x0, . ~ . + species:season + species:mound )
add1( glm.x1, scope = ~ species*season*mound, test = "Chisq")

# This interaction term, however, is not significant (and the AIC value even increases with addition),
# so we conclude that the season and the mound type within the neighborhood affect the presence of 
# grasshopper species and their size independently.

# To provide students with of this research project with a more specific ecological interpretation, we must
# work out the direction of the effects we have just revealed. For this, we start from the estimated values 
# of our regression coefficients. These coefficients can be obtained with extracting function `coef`. 

coef ( glm.x1 )



52.593-35.315
52.593-23.126

322.99-25.78 #season 297.21
322.99-279.51 #mound 43.48




























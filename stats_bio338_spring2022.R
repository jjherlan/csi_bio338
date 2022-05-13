require(tidyverse)
require(knitr)
require(report)

data <- read.csv("bio338_main.csv")
data

#knitr::kable(head(data[, 1:5]), "pipe")

#data2 <-
#  data %>%
#  rename("Fall East" = "east_fall",
#         "Spring East" = "east_spring",
#         "Fall North" = "north_fall",
#         "Spring North" = "north_spring")

# The starting model contains, as its predictors, the main effect of the response
# factor (in our study, the species of insect at a site, or mound) and the main effects
# and and mutual interactions of th explanatory factors. The response variable in this model
# is the counts. We set up the null model (with a Poisson distribution and an implicit log 
# link function) as follows:

data2 <- as_tibble(data)
data2

data2_long <-
  data2 %>%
    pivot_longer(
      cols = everything(),
      names_to = "bird_behav",
      values_to = "count"
    )

data3_long <-
  data2_long %>% 
    group_by(bird_behav) %>%
      tally(count)

#data4_long <-
#  data3_long %>% 
#    pull(bird_behav) %>% 
#      str_split("_")

#data5_long <-
#  data3_long %>% 
#    pull() %>% 
#      str_dup(bird_behav, times = 2)

data4_long <-
  data3_long %>% 
  #    pull() %>% 
  separate(bird_behav, c("bird", "behavior")
          ) %>%
  rename( count = n )

#library(stringr)
#str_split_fixed(before$type, "_and_", 2)

glm.x0 <- glm( count ~ bird + behavior, data = data4_long, family = poisson)

report(glm.x0)

glm.x1 <- glm( count ~ bird * behavior, data = data4_long, family = poisson)

report(glm.x1)

# We compare this model with alternative models, each extended by one of the two
# interactions between the response factor (species) and an explanatory factor:

add1 ( glm.x0, scope = ~bird * behavior, test = "Chisq")

# We use the `scope` parameter to specify the target model with a full interaction among the two 
# factors for the `add1` function, but the function has its own rules regarding the 
# sequence of testing interactions of different orders.

# The tested interaction has a significant effect (p < 0.001) and explains a relatively large 
# fraction of the variability that was not explained by the null model.
# (See the Deviance column: e.g., species:season term explains (100 x (162.03 - 0.00)/162.03) = XX.XX%) 
# of the variation unexplained by the glm.x0 model.)

# The tests are based on X^2 (Chi-Square) statistics (due to a specific test parameter in the `add1` function call)
# but in the `add1` output this statistic is labelled LRT (likelihood ratio test), which it indeed represents.

# We also not that the `add1` function scores the 2 models under comparison with the 
# information-theoretic statistic (AIC) value and this comparison  confirms the appropriateness of adding both 
# interactions to the model. The effect of the mound type seems to be larger than the season, which is in agreement
# with the likelihood ratio test.

# To provide students with of this research project with a more specific ecological interpretation, we must
# work out the direction of the effects we have just revealed. For this, we start from the estimated values 
# of our regression coefficients. These coefficients can be obtained with extracting function `coef`. 

coef ( glm.x1 )

# The number of behaviors was significantly affected by the interaction of species of water fowl and 
# the behavior type (X^2,3 = 162.03, p < 0.001). 

52.593-35.315 #17.278
52.593-23.126 #

322.99-25.78 #season 297.21
322.99-279.51 #mound 43.48




























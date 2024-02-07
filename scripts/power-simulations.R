#> -------------------------------------------
#> AUTONOMY SUPPORTIVE INSTRUCTIONAL LANGUAGE
#> -- St. Germain, McKay, Tandon, Seedu, Barbera,
#>    Carrillo, Brown, and Carter
#>
#> Power simulations
#>
#> Authors:
#>   Laura St. Germain
#>   Brad McKay
#>
#> Website: https://www.cartermaclab.org
#> -------------------------------------------


#> SCRIPT SETUP ----
#>
#> REQUIRED LIBRARIES
library(effsize)
library(rstatix)
library(rogme)
library(tidyverse)

#> Set seed for reproducibility
set.seed(123)

#> CUSTOM FUNCTION
#>
#> Simulate two skewed distributions to represent two independent
#> groups in an experiment
skew_data <- function(){
  g1 <- rlnorm(78, 0, 0.5)              # group 1
  g2 <- rlnorm(78, 0.22, 0.5)           # group 2
  dat <- as.data.frame(cbind(g1, g2))   # combine data from both groups

  return(dat)
}


#> SIMULATION
#>
#> Number of simulations. Can change to a smaller value to reduce the time
#> required to run the simulations if you do not have a very powerful machine
nsims <- 10000

#> create empty vectors to store outcome of simulations
ttest_p <- rep(NA, nsims)   # p-values from t-test
ttest_d <- rep(NA, nsims)   # cohen's d from t-test
shift_sig <- rep(NA, nsims) # store significant findings from shift function

#> run nsims number of simulations for both t-test and shift function
for (i in 1:nsims) {

  #> create simulated data using skew_data() function
  DATA <- skew_data()

  #> make simulated data tidy
  DATA_long <- DATA %>% pivot_longer(everything(),
                                     names_to = "group",
                                     values_to = "outcome")

  #> T-TEST SIMULATEION
  #> run one way t-test on simulated data
  ret_ttest <- rstatix::t_test(outcome ~ group,
                        data = DATA_long,
                        alternative = "less")

  #> store p-value from simulated t-test
  ttest_p[i] <- ret_ttest[[8]]

  #> calculate cohen's d of simulated t-test
  d <- effsize::cohen.d(outcome ~ group,
                        data = DATA_long,
                        within = FALSE)

  #> store cohen's d of simulated t-test
  ttest_d[i] <- abs(d[[3]]) # extract cohen's d


  #> SHIFT FUNCTION SIMULATION
  #> extract data from each group
  g1 <- na.omit(DATA$g2)
  g2 <- na.omit(DATA$g1)

  #> create tibble of data from each group
  shift_tib <- rogme::mkt2(g1,
                           g2,
                           group_labels = c("g1", "g2"))

  #> make factors
  as.factor(shift_tib$gr)

  #> compute shift function
  shift <- rogme::shifthd_pbci(data = shift_tib,
                               formula = obs ~ gr,
                               q = seq(0.1, 0.9, 0.1),
                               nboot = 200,
                               alpha = 0.1)

  #> extract lowerbound of 95% CI
  lb <- as.vector(shift[[1]][[8]])

  #> check if any lowerbounds of the 95% CI cross zero
  #> if no, return a 0.04 (a significant result),
  #> if yes return 1 (non-significant result)
  #shift_sig[i] <- if (any(lb > 0))
  #{0.04}
  #else {1}
}

#> determine power and average effect size of t-test simulations
ttest_power <- mean(ttest_p <= 0.05)
ttest_effect <- mean(ttest_d)
#> RESULT: T-TEST HAS 80% POWER FOR AN EFFECT SIZE OF D = 0.4

shift_power <- mean(lb <= 0.05)

test_psf <- rogme::plot_sf(
  shift,
  plot_theme = 2,
  symb_fill = c("#73a1be", "#c8746d"),
  diffint_col = c("#73a1be", "#c8746d"),
  theme2_alpha = c(1, 1)
)
test_psf

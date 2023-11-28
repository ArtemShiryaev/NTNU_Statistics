############################
# Artem Angelchev Shiryaev #
#      NTNU Case Script    #
############################

# Libraries
library(pwr)
library(pwrss)
library(ggplot2)
library(scales)

# Proportions
Prop_DrugA <- 0.5 
Prop_DrugB <- 0.7

Alpha <- 0.05 # Significance Level
Beta <- 0.8   # Power Level

# Difference between Two Proportions (z Test)
z_prop <- pwrss.z.2props(p1 = Prop_DrugA,
                         p2 = Prop_DrugB,
                         alternative = "greater",
                         power = Beta,
                         alpha = Alpha
                         
)


# Effect Size using Binomial Transform
h_prop <- 2*asin(sqrt(Prop_DrugA)) - 2*asin(sqrt(Prop_DrugB))

# Power calculation for two proportions (same sample sizes)
two_test <-pwr.2p.test(h=(-1)*round(h_prop, digits = 2), 
                       sig.level = Alpha,
                       power = Beta,
                       alternative = "greater")


# Power Calculation for two proportions (different sizes)
diff_size <- pwr.2p2n.test(h=(-1)*round(h_prop, digits = 2),
                           n1 = 140,
                           power =  Beta,
                           sig.level = Alpha,
                           alternative="greater")

chi2_tst <- pwr.chisq.test(w = (-1)*round(h_prop, digits = 2),
                           N = NULL,
                           df = (72-1)*(2-1),#(r-1)(c-1),
                           sig.level = 0.05,
                           power = 0.8
                           
)



# Power calculations for t-tests of means (one sample, two samples and paired samples)
t_test_2means <- pwr.t.test(d = (-1)*round(h_prop, digits = 2),
                            sig.level = Alpha,
                            power = Beta,
                            type = "two.sample",
                            alternative = "greater") 



# Power calculations for two samples (different sizes) t-tests of means
t.test_diff_means <- pwr.t2n.test(n1 = 140,
                                  d = (-1)*round(h_prop, digits = 2),
                                  sig.level = Alpha,
                                  power = Beta,
                                  alternative = "greater")


# Difference between Two Groups (Non-parametric Tests for Independent and Paired Samples)
np_prop <- pwrss.np.2groups(mu1 = 0,
                            mu2 = round(h_prop, digits = 2),
                            alternative = "greater",
                            power = Beta,
                            alpha = Alpha,
                            distribution = "normal"
)

# Paired samples

paired_t <- pwrss.t.2means(mu1 = 0,
                           mu2 = round(h_prop, digits = 2),
                           alternative = "greater",
                           paired = TRUE,
                           paired.r = 0.5,
                           alpha = Alpha,
                           power = Beta
                           
)


paired_np_2groups <- pwrss.np.2groups(mu2 = round(h_prop, digits = 2),
                                      mu1 = 0,
                                      paired = TRUE,
                                      paired.r = 0.5,
                                      alternative = "greater",
                                      power = Beta,
                                      alpha = Alpha,
                                      distribution = "normal"
)

# Graphical Results
plot(z_prop)
plot.power.htest(two_test, main = "Two proportions (same sample sizes)")
plot.power.htest(diff_size, main = "Two proportions (different sample sizes)")
plot.power.htest(chi2_tst, main = "Chi^2 Power Calculation")
paired_np_2groups
paired_t

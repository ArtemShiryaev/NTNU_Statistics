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
                         alternative = "not equal",
                         power = Beta,
                         alpha = Alpha
                         
)


# Effect Size using Binomial Transform
h_prop <- 2*asin(sqrt(Prop_DrugA)) - 2*asin(sqrt(Prop_DrugB))

# Power calculation for two proportions (same sample sizes)
two_test <-pwr.2p.test(h=round(h_prop, digits = 2), 
                       sig.level = Alpha,
                       power = Beta,
                       alternative = "two.sided")


# Power Calculation for two proportions (different sizes)
diff_size <- pwr.2p2n.test(h=round(h_prop, digits = 2),
                           n1 = 180,
                           power =  Beta,
                           sig.level = Alpha,
                           alternative="two.sided")

# Chi2 power calculations
chi2_tst <- pwr.chisq.test(w = (-1)*round(h_prop, digits = 2),
                           N = NULL,
                           df = (91-1)*(2-1),#(r-1)(c-1),
                           sig.level = 0.05,
                           power = 0.8
                           
)



# Difference between Two Groups (Non-parametric Tests for Independent and Paired Samples)
np_prop <- pwrss.np.2groups(mu1 = round(h_prop, digits = 2),
                            mu2 = 0,
                            alternative = "not equal",
                            power = Beta,
                            alpha = Alpha,
                            distribution = "normal"
)

paired_t <- pwrss.t.2means(mu1 = round(h_prop, digits = 2),
                           alternative = "not equal",
                           paired = TRUE,
                           paired.r = 0.2,
                           alpha = Alpha,
                           power = Beta
                           
)


paired_np_2groups <- pwrss.np.2groups(mu1 = round(h_prop, digits = 2),
                                      mu2 = 0,
                                      paired = TRUE,
                                      paired.r = 0.5,
                                      alternative = "not equal",
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

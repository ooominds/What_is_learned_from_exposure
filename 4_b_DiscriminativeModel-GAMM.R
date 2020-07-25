
require(itsadug) # for GAMMs: mgcv package and auxiliary functions

options(show.signif.stars=FALSE)

# -

load('discr_data.rda')

# numeric, boolean if a word has u- ending
discr_data$isU = as.numeric(discr_data$ending == 'u')

# removing influential data points (extreme values and outlyers)
discr_data2 = discr_data[discr_data$G2F.WordEndDiversity > -2.2 &
        discr_data$G2F.WordEndDiversity < 2.7,]

# with this we have lost 83 data points
(nrow(discr_data) - nrow(discr_data2))

############
### GAMM ###
############

### NOTE: Comparing models with less number of knots always give
###       significantly poorer fit. So, we are keeping it maximal.

summary(discr_data2.gam.FINAL <- bam(isU ~
    s(G2F.WordEndDiversity) +
    te(C2F.Prior, C2F.Typicality),
    method='ML',
    data=discr_data2, family='binomial'))
# Parametric coefficients:
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept)  1.38423    0.05382   25.72   <2e-16
# 
# Approximate significance of smooth terms:
#                                edf Ref.df Chi.sq p-value
# s(G2F.WordEndDiversity)      7.959  8.674  291.5  <2e-16
# te(C2F.Prior,C2F.Typicality) 3.002  3.003  100.6  <2e-16
# 
# R-sq.(adj) =  0.146   Deviance explained = 14.7%
# -ML = 4897.5  Scale est. = 1         n = 3487

# trimming extreme residuals (cf., Baayen & Milin, 2010)
summary(discr_data2.gam.FINAL.t <- bam(isU ~
    s(G2F.WordEndDiversity) +
    te(C2F.Prior, C2F.Typicality),
    method='ML',
    data=discr_data2, subset=abs(scale(resid(discr_data2.gam.FINAL)))<2.5,
    family='binomial'))
# Parametric coefficients:
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept)  1.64998    0.07637    21.6   <2e-16
# 
# Approximate significance of smooth terms:
#                                edf Ref.df Chi.sq p-value
# s(G2F.WordEndDiversity)      8.137  8.744  247.8  <2e-16
# te(C2F.Prior,C2F.Typicality) 4.105  4.835  105.8  <2e-16
# 
# R-sq.(adj) =  0.167   Deviance explained = 18.3%
# -ML = 4689.1  Scale est. = 1         n = 3460



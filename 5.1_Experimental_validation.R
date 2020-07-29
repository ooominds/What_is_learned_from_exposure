
### Lead analyst: Adnane Ez-Zizi

###################
# Preliminary steps
###################

# Load necessary packages
library(itsadug) 
library(caret)
library(e1071)

options(show.signif.stars=FALSE)

# Load the data
load('exp_data.rda')

#####################
# Model fit: GAMM
#####################

### We used used 3 knots to make the partial effect curve smoother
summary(exp_gamm <- bam(Response ~
    s(G2F.WordEndDiversity, k=3) +
    s(ParticipantNo, bs='re') +
    s(nonce, bs='re'),
    data=exp_data, family='binomial'))
# Parametric coefficients:
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept)  0.09963    0.06394   1.558    0.119
# 
# Approximate significance of smooth terms:
#                            edf Ref.df  Chi.sq  p-value    
# s(G2F.WordEndDiversity)   1.00      1   12.34 0.000444 ***
# s(ParticipantNo)        170.10    215  925.43  < 2e-16 ***
# s(nonce)                426.92    561 1809.49  < 2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# R-sq.(adj) =  0.232   Deviance explained = 22.2%
# fREML =  14616  Scale est. = 1         n = 10287

### Evaluation of the performance of the GAMM model 
# predicted probability of a-ending
prob.gamm = predict(exp_gamm, newdata = exp_data, type = 'response')		
# predicted responses
r.gamm = rep('0', nrow(exp_data))
r.gamm[prob.gamm>.5] = '1'
# Transform the resulting vector into a factor to be able to use confusionMatrix
r.gamm = as.factor(r.gamm)
r.gamm = relevel(r.gamm, ref = '0') 
# Evaluation results
CM.gamm = confusionMatrix(r.gamm, exp_data$Response)
# Overall accuracy
CM.gamm$overall[1] 
# Accuracy 
# 0.7322835

#############################################
# Control for the effect of prize
#############################################

# First check whether there is an interaction with Prize
summary(gamm_prize0 <- bam(Response ~
    Prize +
    s(G2F.WordEndDiversity) +
    s(G2F.WordEndDiversity, by = Prize) +
    s(ParticipantNo, bs='re') +
    s(nonce, bs='re'),
    data=exp_data, family='binomial'))
# Parametric coefficients:
#              Estimate Std. Error z value Pr(>|z|)
# (Intercept)  0.155041   0.113419   1.367   0.1716
# Prizeyes    -0.070989   0.120739  -0.588   0.5566
# 
# Approximate significance of smooth terms:
#                                         edf     Ref.df    Chi.sq   p-value
# s(G2F.WordEndDiversity)          4.5734e+00 4.8379e+00   23.0956 0.0004102
# s(G2F.WordEndDiversity):Prizeno  1.0002e+00 1.0004e+00    3.5051 0.0612252
# s(G2F.WordEndDiversity):Prizeyes 3.3097e-05 6.4078e-05    0.0000 0.9997318
# s(ParticipantNo)                 1.6957e+02 2.1400e+02  918.5016 < 2.2e-16
# s(nonce)                         4.2064e+02 5.6100e+02 1740.1576 < 2.2e-16
# 
# Rank: 807/808
# R-sq.(adj) =  0.233   Deviance explained = 22.2%
# fREML =  14616  Scale est. = 1         n = 10287

# => NO INTERACTION

# Final model
summary(gamm_prize1 <- bam(Response ~
    Prize +
    s(G2F.WordEndDiversity) +
    s(ParticipantNo, bs='re') +
    s(nonce, bs='re'),
    data=exp_data, family='binomial'))
# Parametric coefficients:
#              Estimate Std. Error z value Pr(>|z|)
# (Intercept)  0.154546   0.113296  1.3641   0.1725
# Prizeyes    -0.070836   0.120611 -0.5873   0.5570
# 
# Approximate significance of smooth terms:
#                              edf   Ref.df   Chi.sq   p-value
# s(G2F.WordEndDiversity)   4.5737   4.8378   26.877 7.909e-05
# s(ParticipantNo)        169.5396 214.0000  917.681 < 2.2e-16
# s(nonce)                420.6961 561.0000 1740.394 < 2.2e-16
# 
# R-sq.(adj) =  0.232   Deviance explained = 22.2%
# fREML =  14615  Scale est. = 1         n = 10287

# => NO EFFECT OF PRIZE


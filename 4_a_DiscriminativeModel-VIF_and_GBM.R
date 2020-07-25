
require(dplyr) # for inner_join()
require(usdm)  # for VIF analysis
require(gbm)   # for Gradient Boosting Machines -- gbm()

options(show.signif.stars=FALSE)
set.seed(1828)

# -

load('discr_data.rda')

# numeric, boolean if a word has a genitive u- ending
discr_data$isU = as.numeric(discr_data$ending == 'u')

####################
### VIF Analysis ###
####################

m = round(cor(discr_data[,7:13]), 2)
m[upper.tri(m, diag=TRUE)] = NA
print(m, na.print='')
#                      G2F.Activation G2F.Prior G2F.aDiversity
# G2F.Activation                                              
# G2F.Prior                      0.89                         
# G2F.aDiversity                -0.13     -0.33               
# G2F.WordEndDiversity           0.09      0.00           0.45
# G2F.TriDiversity               0.18      0.05           0.44
# C2F.Prior                      0.86      0.99          -0.35
# C2F.Typicality                 0.64      0.71          -0.22
#                      G2F.WordEndDiversity G2F.TriDiversity C2F.Prior
# G2F.Activation                                                      
# G2F.Prior                                                           
# G2F.aDiversity                                                      
# G2F.WordEndDiversity                                                
# G2F.TriDiversity                     0.38                           
# C2F.Prior                            0.01             0.04          
# C2F.Typicality                       0.02             0.03      0.73

# G2F.Activation and G2F.Prior seem to be causing multicollinearity.
# When we remove them, the problem is no longer severe.

vifcor(discr_data[,9:13], th=0.75)
# No variable from the 5 input variables has a collinearity problem. 
# 
# The linear correlation coefficients range between: 
# min correlation ( C2F.Prior ~ G2F.WordEndDiversity ):  0.01119378 
# max correlation ( C2F.Typicality ~ C2F.Prior ):  0.7303349 
# 
# ---------- VIFs of the remained variables -------- 
#              Variables      VIF
# 1       G2F.aDiversity 1.739576
# 2 G2F.WordEndDiversity 1.356392
# 3     G2F.TriDiversity 1.366756
# 4            C2F.Prior 2.439588
# 5       C2F.Typicality 2.150959

###########
### GBM ###
###########

### NOTE: the cv.folds parameter in gbm() causes error. This is often
###       the case, especially when a new version of the package is
###       deposited on CRAN. I have turned it off but please give
###       it a try by uncommenting: remove ) and #.

gbmdat = discr_data[,c(14,3,9,10,11,12,13)]

gbm1 = gbm(isU ~
    G2F.aDiversity +
    G2F.WordEndDiversity +
    G2F.TriDiversity +
    C2F.Prior +
    C2F.Typicality,
    data=gbmdat,
    n.trees=5000, interaction.depth=5, shrinkage=0.001) # , cv.folds=5)
print(sum.gbm1 <- summary(gbm1, plotit=FALSE), row.names=FALSE)
 #                  var   rel.inf
 # G2F.WordEndDiversity 46.424142
 #     G2F.TriDiversity 36.061447
 #       C2F.Typicality  7.757041
 #       G2F.aDiversity  5.851770
 #            C2F.Prior  3.905599

gbm2 = gbm(isU ~
    type +
    G2F.aDiversity +
    G2F.WordEndDiversity +
    G2F.TriDiversity +
    C2F.Prior +
    C2F.Typicality,
    data=gbmdat,
    n.trees=5000, interaction.depth=5, shrinkage=0.001) # , cv.folds=5)
print(sum.gbm2 <- summary(gbm2, plotit=FALSE), row.names=FALSE)
 #                  var   rel.inf
 #                 type 31.564389
 # G2F.WordEndDiversity 31.318658
 #     G2F.TriDiversity 23.944715
 #       C2F.Typicality  5.808666
 #       G2F.aDiversity  4.677876
 #            C2F.Prior  2.685696

# -----

# Adding the Phonologically Typical variable and removing all 'atypical'
# instances, which will cause significant data loss.
gbmdat2 = droplevels(discr_data[discr_data$phon_typical %in% c('a', 'u'),
    c(14,3,5,9,10,11,12,13)])
# dim(gbmdat2)
# [1] 1269    8

gbm3 = gbm(isU ~
    type +
    phon_typical +
    G2F.aDiversity +
    G2F.WordEndDiversity +
    G2F.TriDiversity +
    C2F.Prior +
    C2F.Typicality,
    data=gbmdat2,
    n.trees=5000, interaction.depth=5, shrinkage=0.001) #, cv.folds=5)
print(sum.gbm3 <- summary(gbm3, plotit=FALSE), row.names=FALSE)
 #                  var   rel.inf
 #                 type 32.610410
 #     G2F.TriDiversity 22.978116
 # G2F.WordEndDiversity 16.352478
 #         phon_typical 13.095602
 #            C2F.Prior  5.294134
 #       C2F.Typicality  5.014430
 #       G2F.aDiversity  4.654829



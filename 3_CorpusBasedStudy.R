
### Lead analyst: Petar Milin

options(show.signif.stars=FALSE)

load('corpus_data.rda')

#####################################
### 3.2.1 Phonological typicality ###
#####################################

# NOTE: Roughly 36.5% of data is available for analysis
#       due to zero-cells.

tmpdat = droplevels(corpus_data[corpus_data$phon_typical %in% 
    c('a','u'), c(1,2)])
tmpdat = lapply(tmpdat, relevel, ref='u')

phondat = data.frame(tab2 <- xtabs(~
    ending +
    phon_typical,
    data=tmpdat))
ftable(tab2)
#        phon_typical   u   a
# ending                     
# u                   461 636
# a                    26 654

pois.phondat0 <- glm(Freq ~
    1,
    data=phondat, family=poisson)
pois.phondat1a <- glm(Freq ~
    ending,
    data=phondat, family=poisson)
pois.phondat1b <- glm(Freq ~
    phon_typical,
    data=phondat, family=poisson)
pois.phondat2 <- glm(Freq ~
    ending + phon_typical,
    data=phondat, family=poisson)

anova(pois.phondat0, pois.phondat1a, test='Chi')
#   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)
# 1         3     848.77                      
# 2         2     750.00  1   98.774 < 2.2e-16

anova(pois.phondat0, pois.phondat1b, test='Chi')
#   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)
# 1         3     848.77                      
# 2         2     472.42  1   376.35 < 2.2e-16

anova(pois.phondat1a, pois.phondat2, test='Chi')
#   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)
# 1         2     750.00                      
# 2         1     373.65  1   376.35 < 2.2e-16

anova(pois.phondat1b, pois.phondat2, test='Chi')
#   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)
# 1         2     472.42                      
# 2         1     373.65  1   98.774 < 2.2e-16

ll.phondat2 = loglin(tab2, list(c(1), c(2)), param=TRUE, fit=TRUE)
1 - pchisq(ll.phondat2$lrt, ll.phondat2$df)

phondat = data.frame(cbind(phondat, 'Poisson'=pois.phondat2$fitted))
phondat
#   ending phon_typical Freq Poisson
# 1      u            u  461 300.641
# 2      a            u   26 186.359
# 3      u            a  636 796.359
# 4      a            a  654 493.641

######################################
### 3.2.2 Morphological typicality ###
######################################

# NOTE: Roughly 23% of data is available for analysis
#       due to zero-cells.

tmpdat = droplevels(corpus_data[corpus_data$morph_typical %in%
        c('a','u'), c(1,3)])
tmpdat = lapply(tmpdat, relevel, ref='u')

morphdat = data.frame(tab2 <- xtabs(~
    ending +
    morph_typical,
    data=tmpdat))
ftable(tab2)
#        morph_typical   u   a
# ending                      
# u                    347 190
# a                     14 558

pois.morphdat0 <- glm(Freq ~
    1,
    data=morphdat, family=poisson)
pois.morphdat1a <- glm(Freq ~
    ending,
    data=morphdat, family=poisson)
pois.morphdat1b <- glm(Freq ~
    morph_typical,
    data=morphdat, family=poisson)
pois.morphdat2 <- glm(Freq ~
    ending + morph_typical,
    data=morphdat, family=poisson)

anova(pois.morphdat0, pois.morphdat1a, test='Chi')
#   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
# 1         3     709.11                     
# 2         2     708.00  1   1.1048   0.2932

anova(pois.morphdat0, pois.morphdat1b, test='Chi')
#   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)
# 1         3     709.11                      
# 2         2     571.17  1   137.93 < 2.2e-16

anova(pois.morphdat1a, pois.morphdat2, test='Chi')
#   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)
# 1         2     708.00                      
# 2         1     570.07  1   137.93 < 2.2e-16

anova(pois.morphdat1b, pois.morphdat2, test='Chi')
#   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
# 1         2     571.17                     
# 2         1     570.07  1   1.1048   0.2932

morphdat = data.frame(cbind(morphdat, 'Poisson'=pois.morphdat1b$fitted))
morphdat
#   ending morph_typical Freq Poisson
# 1      u             u  347   180.5
# 2      a             u   14   180.5
# 3      u             a  190   374.0
# 4      a             a  558   374.0

##################
### 3.2.3 Type ###
##################

typedat = data.frame(tab2 <- xtabs(~
    ending +
    type,
    data=corpus_data))
typedat$ending = relevel(typedat$ending, ref='u')
typedat$type = relevel(typedat$type, ref='abstract')
ftable(tab2)
#        type abstract collective count mass name place
# ending                                               
# a                362         15  1000  125   39   120
# u               1815         82   670  507   39    98

pois.typedat0 <- glm(Freq ~
    1,
    data=typedat, family=poisson)
pois.typedat1a <- glm(Freq ~
    ending,
    data=typedat, family=poisson)
pois.typedat1b <- glm(Freq ~
    type,
    data=typedat, family=poisson)
pois.typedat2 <- glm(Freq ~
    ending + type,
    data=typedat, family=poisson)

anova(pois.typedat0, pois.typedat1a, test='Chi')
#   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)
# 1        11     6459.7                      
# 2        10     5958.0  1    501.8 < 2.2e-16

anova(pois.typedat0, pois.typedat1b, test='Chi')
#   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)
# 1        11     6459.7                      
# 2         6     1425.2  5   5034.5 < 2.2e-16

anova(pois.typedat1a, pois.typedat2, test='Chi')
#   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)
# 1        10     5958.0                      
# 2         5      923.4  5   5034.5 < 2.2e-16

anova(pois.typedat1b, pois.typedat2, test='Chi')
#   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)
# 1         6    1425.22                      
# 2         5     923.42  1    501.8 < 2.2e-16

typedat = data.frame(cbind(typedat, 'Poisson'=pois.typedat2$fitted))
typedat
#    ending       type Freq    Poisson
# 1       a   abstract  362  742.19971
# 2       u   abstract 1815 1434.80029
# 3       a collective   15   33.06999
# 4       u collective   82   63.93001
# 5       a      count 1000  569.34934
# 6       u      count  670 1100.65066
# 7       a       mass  125  215.46634
# 8       u       mass  507  416.53366
# 9       a       name   39   26.59236
# 10      u       name   39   51.40764
# 11      a      place  120   74.32225
# 12      u      place   98  143.67775

#########################
### 3.2.4 Entity size ###
#########################

# NOTE: Roughly 25% of data is available for analysis
#       due to zero-cells.

tmpdat = droplevels(corpus_data[corpus_data$entity_size %in%
        c('immovable','manipulable'), c(1,5)])
tmpdat$ending = relevel(tmpdat$ending, ref='u')

sizedat = data.frame(tab2 <- xtabs(~
    ending +
    entity_size,
    data=tmpdat))
ftable(tab2)
#        entity_size immovable manipulable
# ending                                  
# u                        285         188
# a                        150         589

pois.sizedat0 <- glm(Freq ~
    1,
    data=sizedat, family=poisson)
pois.sizedat1a <- glm(Freq ~
    ending,
    data=sizedat, family=poisson)
pois.sizedat1b <- glm(Freq ~
    entity_size,
    data=sizedat, family=poisson)
pois.sizedat2 <- glm(Freq ~
    ending + entity_size,
    data=sizedat, family=poisson)

anova(pois.sizedat0, pois.sizedat1a, test='Chi')
#   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)
# 1         3     357.71                      
# 2         2     298.85  1   58.857 1.695e-14

anova(pois.sizedat0, pois.sizedat1b, test='Chi')
#   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)
# 1         3     357.71                      
# 2         2     259.88  1   97.828 < 2.2e-16

anova(pois.sizedat1a, pois.sizedat2, test='Chi')
#   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)
# 1         2     298.85                      
# 2         1     201.02  1   97.828 < 2.2e-16

anova(pois.sizedat1b, pois.sizedat2, test='Chi')
#   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)
# 1         2     259.88                      
# 2         1     201.02  1   58.857 1.695e-14

sizedat = data.frame(cbind(sizedat, 'Poisson'=pois.sizedat2$fitted))
sizedat
#   ending entity_size Freq  Poisson
# 1      u   immovable  285 169.7649
# 2      a   immovable  150 265.2351
# 3      u manipulable  188 303.2351
# 4      a manipulable  589 473.7649



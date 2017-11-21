library(ggplot2)
library(grid)
library(reshape2)
library(plyr)
library(psych)
library(rms)
library(languageR)
library(lme4)
library(effects)
library(Rmisc)
library(cowplot)
library(multcomp)

#########################
#the following code is used in the analysis of:
#Brand, J., Monaghan, P., & Walker, P. (2017). The changing role of sound symbolism for small versus large vocabularies. Cognitive Science.
#all data available at - https://git.io/v5BXJ
#corresponding author - James Brand - j.brand [at] lancaster.ac.uk
###########################

#########################
#Set up data
#########################

R.version

# platform       x86_64-apple-darwin15.6.0
# arch           x86_64
# os             darwin15.6.0
# system         x86_64, darwin15.6.0
# status
# major          3
# minor          4.2
# year           2017
# month          09
# day            28
# svn rev        73368
# language       R
# version.string R version 3.4.2 (2017-09-28)
# nickname       Short Summer

#set the working directory, this should be where the data file is located
setwd("~/Desktop/Lancaster/Experiments/Experiment_1/Analysis")
data1 <- read.csv(file = "Final_data1.csv", head=TRUE,sep=",",na.strings= "-999")

#make vocab size variable ordered
data1$Category <- ordered(data1$Category, levels = c("Small", "Medium", "Large"))

#questionnaire data for spiky/round preference of non-words
questionnaire_data <- read.csv(file = "Questionnaire_data_2.csv", head=TRUE,sep=",",na.strings= "-999")

#remove data with responses that differ across the 2 trials by >= 5, n=62, ~3% of data
questionnaire_data <- subset(questionnaire_data, Difference_3 %in% c(0, 1, 2, 3, 4))

#subset the data to leave only the stimuli used in Monaghan et al (2012)
questionnaire_data <- subset(questionnaire_data, Category %in% c("p", "r"))

questionnaire_aggregate <- summarySE(questionnaire_data, measurevar="Response_score_2", groupvars=c("Category"))


#---------------------------------
#glmer anlayses
#---------------------------------

############################
#glmer for all data
############################

#the following code is for a series of general linear mixed-effects models (using the lme4 package)
#the models are built from a null model (glmer_all1), then adding in extra fixed effects at each subsequent step
#the anova function is used to make model comparisons at each step
#if the anova comes out significant, the added fixed effect improves model fit, the effect is then included in the next model step
#if the anova comes out non-significant, the added fixed effect does not improve model fit and is removed from the next model step
#the best performing model is glmer_all9, where the intereaction between Vocab size*congruency*same/different category is significant

glmer_all1 <- glmer(Slide3.ACC ~ 
                      #Block2 +
                      #Category +
                      #con_inc +
                      #same_diff +
                      #Category*con_inc*same_diff +
                      (1|Subject2) + (1|Sound),
                    data = data1,
                    family = binomial,
                    glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_all1)

glmer_all2 <- glmer(Slide3.ACC ~ 
                      #Block2 +
                      Category +
                      #con_inc +
                      #same_diff +
                      #Category*con_inc*same_diff +
                      (1|Subject2) + (1|Sound),
                    data = data1,
                    family = binomial,
                    glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_all2)

glmer_all3 <- glmer(Slide3.ACC ~ 
                      Block2 +
                      #Category +
                      #con_inc +
                      #same_diff +
                      #Category*con_inc*same_diff +
                      (1|Subject2) + (1|Sound),
                    data = data1,
                    family = binomial,
                    glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_all3)

glmer_all4 <- glmer(Slide3.ACC ~ 
                      Block2 +
                      #Category +
                      con_inc +
                      #same_diff +
                      #Category*con_inc*same_diff +
                      (1|Subject2) + (1|Sound),
                    data = data1,
                    family = binomial,
                    glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_all4)

glmer_all5 <- glmer(Slide3.ACC ~ 
                      Block2 +
                      #Category +
                      con_inc +
                      same_diff +
                      #Category*con_inc +
                      (1|Subject2) + (1|Sound),
                    data = data1,
                    family = binomial,
                    glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_all5)

glmer_all6 <- glmer(Slide3.ACC ~ 
                      Block2 +
                      #Category +
                      con_inc +
                      #same_diff +
                      Category*con_inc +
                      #Category*con_inc*same_diff +
                      (1|Subject2) + (1|Sound),
                    data = data1,
                    family = binomial,
                    glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_all6)

glmer_all7 <- glmer(Slide3.ACC ~ 
                      Block2 +
                      #Category +
                      con_inc +
                      #same_diff +
                      #Category*con_inc +
                      Category*same_diff +
                      #Category*con_inc*same_diff +
                      (1|Subject2) + (1|Sound),
                    data = data1,
                    family = binomial,
                    glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_all7)

glmer_all8 <- glmer(Slide3.ACC ~ 
                      Block2 +
                      #Category +
                      con_inc +
                      #same_diff +
                      #Category*con_inc +
                      #Category*same_diff +
                      con_inc*same_diff +
                      #Category*con_inc*same_diff +
                      (1|Subject2) + (1|Sound),
                    data = data1,
                    family = binomial,
                    glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_all8)

glmer_all9 <- glmer(Slide3.ACC ~ 
                      Block2 +
                      #Category +
                      con_inc +
                      #same_diff +
                      #Category*con_inc +
                      #Category*same_diff +
                      con_inc*same_diff +
                      Category*con_inc*same_diff +
                      (1|Subject2) + (1|Sound),
                    data = data1,
                    family = binomial,
                    glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_all9)

# AIC      BIC   logLik deviance df.resid 
# 17946.7  18059.7  -8958.3  17916.7    13809 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.5729 -1.1062  0.6143  0.7970  1.5625 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# Subject2 (Intercept) 0.119109 0.34512 
# Sound    (Intercept) 0.007895 0.08885 
# Number of obs: 13824, groups:  Subject2, 72; Sound, 16
# 
# Fixed effects:
#                                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                     0.23880    0.07196   3.318 0.000905 ***
#   Block2                          0.19827    0.01608  12.328  < 2e-16 ***
#   con_inci                       -0.47361    0.05436  -8.713  < 2e-16 ***
#   same_diffs                     -0.20884    0.05358  -3.898 9.70e-05 ***
#   Category.L                      0.16186    0.09733   1.663 0.096315 .  
# Category.Q                     -0.15211    0.09638  -1.578 0.114487    
# con_inci:same_diffs             0.36940    0.07458   4.953 7.31e-07 ***
#   con_inci:Category.L            -0.16721    0.09359  -1.787 0.073996 .  
# con_inci:Category.Q             0.42596    0.09019   4.723 2.32e-06 ***
#   same_diffs:Category.L          -0.25432    0.09423  -2.699 0.006958 ** 
#   same_diffs:Category.Q           0.23839    0.09113   2.616 0.008901 ** 
#   con_inci:same_diffs:Category.L  0.39183    0.13156   2.978 0.002898 ** 
#   con_inci:same_diffs:Category.Q -0.51708    0.12660  -4.084 4.42e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

glmer_all10 <- glmer(Slide3.ACC ~ 
                      Block2 +
                      #Category +
                      con_inc +
                      #same_diff +
                      #Category*con_inc +
                      #Category*same_diff +
                      con_inc*same_diff +
                      Category*con_inc*same_diff +
                      Block2*Category*con_inc*same_diff +
                      (1|Subject2) + (1|Sound),
                    data = data1,
                    family = binomial,
                    glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_all10)

conf_interval_glmer_all9 <- confint(glmer_all9,parm="beta_",method="Wald")
ctab <- cbind(est=fixef(glmer_all9), conf_interval_glmer_all9)
print(ctab,digits=3)


#Vocab size
anova(glmer_all1, glmer_all2)   #n.s
#Block
anova(glmer_all1, glmer_all3)   #p< 2.2e-16 ***
#Congruency
anova(glmer_all3, glmer_all4)   #p=3.159e-14 ***
#Same/different category
anova(glmer_all4, glmer_all5)   #n.s
#Vocab size*congruency
anova(glmer_all4, glmer_all6)   #n.s 0.06638 .
#Vocab size*same/different
anova(glmer_all4, glmer_all7)   #n.s
#Congruency*same/different
anova(glmer_all4, glmer_all8)   #p=1.045e-08 ***
#Vocab size*congruency*same/different
anova(glmer_all8, glmer_all9)   #p=0.0001139 ***
#Block*vocab size*congruency*same/different
anova(glmer_all9, glmer_all10)  #n.s

########################
#random effects justification
########################

#justify the use of subject and word sound random effects
#this follows the same model building process as before

glmer_all9 <- glmer(Slide3.ACC ~ 
                      Block2 +
                      #Category +
                      con_inc +
                      #same_diff +
                      #Category*con_inc +
                      #Category*same_diff +
                      con_inc*same_diff +
                      Category*con_inc*same_diff +
                      (1|Subject2) + (1|Sound),
                    data = data1,
                    family = binomial,
                    glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)),
                    REML = TRUE)
summary(glmer_all9)

print(glmer_all9, corr = FALSE)

glmer_all9.1 <- glmer(Slide3.ACC ~ 
                      Block2 +
                      #Category +
                      con_inc +
                      #same_diff +
                      #Category*con_inc +
                      #Category*same_diff +
                      con_inc*same_diff +
                      Category*con_inc*same_diff +
                      (1|Subject2),
                    data = data1,
                    family = binomial,
                    glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)),
                    REML = TRUE)
summary(glmer_all9.1)

glmer_all9.2 <- glmer(Slide3.ACC ~ 
                      Block2 +
                      #Category +
                      con_inc +
                      #same_diff +
                      #Category*con_inc +
                      #Category*same_diff +
                      con_inc*same_diff +
                      Category*con_inc*same_diff +
                      (1|Sound),
                    data = data1,
                    family = binomial,
                    glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)),
                    REML = TRUE)
summary(glmer_all9.2)

anova(glmer_all9, glmer_all9.1) #p=0.002233 **
anova(glmer_all9, glmer_all9.2) #p< 2.2e-16 ***

############################
#glmer for same/diff trials
############################

glmer_diff1 <- glmer(Slide3.ACC ~ 
                       #Block2 +
                       #Category +
                       #con_inc +
                       #Category*con_inc +
                       (1|Subject2) + (1|Sound),
                     data = subset(data1, same_diff == "d"),
                     family = binomial,
                     glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_diff1)

glmer_diff2 <- glmer(Slide3.ACC ~ 
                       Block2 +
                       #Category +
                       #con_inc +
                       #Category*con_inc +
                       (1|Subject2) + (1|Sound),
                     data = subset(data1, same_diff == "d"),
                     family = binomial,
                     glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_diff2)

glmer_diff3 <- glmer(Slide3.ACC ~ 
                       Block2 +
                       Category +
                       #con_inc +
                       #Category*con_inc +
                       (1|Subject2) + (1|Sound),
                     data = subset(data1, same_diff == "d"),
                     family = binomial,
                     glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_diff3)

glmer_diff4 <- glmer(Slide3.ACC ~ 
                       Block2 +
                       #Category +
                       con_inc +
                       #Category*con_inc +
                       (1|Subject2) + (1|Sound),
                     data = subset(data1, same_diff == "d"),
                     family = binomial,
                     glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_diff4)

glmer_diff5 <- glmer(Slide3.ACC ~ 
                       Block2 +
                       #Category +
                       con_inc +
                       Category*con_inc +
                       (1|Subject2) + (1|Sound),
                     data = subset(data1, same_diff == "d"),
                     family = binomial,
                     glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_diff5)

# Fixed effects:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          0.16816    0.09312   1.806    0.071 .  
# Block2               0.23802    0.02320  10.258  < 2e-16 ***
# con_inci            -0.49814    0.05683  -8.766  < 2e-16 ***
# Category.L          -0.15891    0.11789  -1.348    0.178    
# Category.Q          -0.14797    0.11683  -1.266    0.205    
# con_inci:Category.L  0.16875    0.09542   1.768    0.077 .  
# con_inci:Category.Q  0.43168    0.09145   4.720 2.36e-06 ***

anova(glmer_diff1, glmer_diff2)
anova(glmer_diff2, glmer_diff3)#n.s
anova(glmer_diff2, glmer_diff4)
anova(glmer_diff4, glmer_diff5)

# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# glmer_diff4  5 8815.6 8849.8 -4402.8   8805.6                             
# glmer_diff5  9 8799.5 8861.1 -4390.7   8781.5 24.156      4  7.432e-05 ***

glmer_same1 <- glmer(Slide3.ACC ~ 
                       #Block2 +
                       #Category +
                       #con_inc +
                       #Category*con_inc +
                        (1|Subject2) + (1|Sound),
                      data = subset(data1, same_diff == "s"),
                      family = binomial,
                      glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_same1)

glmer_same2 <- glmer(Slide3.ACC ~ 
                       Block2 +
                       #Category +
                       #con_inc +
                       #Category*con_inc +
                       (1|Subject2) + (1|Sound),
                     data = subset(data1, same_diff == "s"),
                     family = binomial,
                     glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_same2)

glmer_same3 <- glmer(Slide3.ACC ~ 
                       Block2 +
                       Category +
                       #con_inc +
                       #Category*con_inc +
                       (1|Subject2) + (1|Sound),
                     data = subset(data1, same_diff == "s"),
                     family = binomial,
                     glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_same3)

glmer_same4 <- glmer(Slide3.ACC ~ 
                       Block2 +
                       #Category +
                       con_inc +
                       #Category*con_inc +
                     (1|Subject2) + (1|Sound),
                     data = subset(data1, same_diff == "s"),
                     family = binomial,
                     glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_same4)

glmer_same5 <- glmer(Slide3.ACC ~ 
                       Block2 +
                       #Category +
                       #con_inc +
                       Category*con_inc +
                       (1|Subject2) + (1|Sound),
                     data = subset(data1, same_diff == "s"),
                     family = binomial,
                     glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_same5)

# Fixed effects:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          0.10272    0.07980   1.287   0.1980    
# Block2               0.16269    0.02244   7.249 4.19e-13 ***
# Category.L           0.08711    0.08408   1.036   0.3002    
# Category.Q           0.08201    0.08155   1.006   0.3146    
# con_inci            -0.09036    0.05471  -1.652   0.0986 .  
# Category.L:con_inci -0.22163    0.09281  -2.388   0.0169 *  
# Category.Q:con_inci -0.08970    0.08841  -1.015   0.3103 

anova(glmer_same1, glmer_same2)
anova(glmer_same2, glmer_same3)#n.s
anova(glmer_same2, glmer_same4)#n.s
anova(glmer_same2, glmer_same5)

#######################
#glmer for small vocabulary size
#######################

#glmer for all small vocabulary size data

glmer_small1 <- glmer(Slide3.ACC ~ 
                        #Block2 +
                        #con_inc +
                        #same_diff +
                        #con_inc*same_diff +
                        (1|Subject2) + (1|Sound),
                      data = subset(data1, Category == "Small"),
                      family = binomial,
                      glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_small1)

glmer_small2 <- glmer(Slide3.ACC ~ 
                        Block2 +
                        #con_inc +
                        #same_diff +
                        #con_inc*same_diff +
                        (1|Subject2) + (1|Sound),
                      data = subset(data1, Category == "Small"),
                      family = binomial,
                      glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_small2)

glmer_small3 <- glmer(Slide3.ACC ~ 
                        Block2 +
                        con_inc +
                        #same_diff +
                        #con_inc*same_diff +
                        (1|Subject2) + (1|Sound),
                      data = subset(data1, Category == "Small"),
                      family = binomial,
                      glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_small3)

glmer_small4 <- glmer(Slide3.ACC ~ 
                        Block2 +
                        con_inc +
                        same_diff +
                        #con_inc*same_diff +
                        (1|Subject2) + (1|Sound),
                      data = subset(data1, Category == "Small"),
                      family = binomial,
                      glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_small4)

glmer_small5 <- glmer(Slide3.ACC ~ 
                        Block2 +
                        con_inc +
                        same_diff +
                        con_inc*same_diff +
                        (1|Subject2) + (1|Sound),
                      data = subset(data1, Category == "Small"),
                      family = binomial,
                      glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_small5)

# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          0.19214    0.14416   1.333    0.183    
# Block2               0.14461    0.03392   4.263 2.02e-05 ***
#   con_inci            -0.17148    0.11195  -1.532    0.126    
# same_diffs           0.05737    0.10936   0.525    0.600    
# con_inci:same_diffs -0.10782    0.15219  -0.708    0.479

anova(glmer_small1, glmer_small2)
anova(glmer_small2, glmer_small3)
anova(glmer_small3, glmer_small4)#n.s
anova(glmer_small3, glmer_small5)#n.s

#glmer for small vocabulary size data - categorical trials

glmer_small_diff1 <- glmer(Slide3.ACC ~ 
                        #Block2 +
                        #con_inc +
                        #same_diff +
                        #con_inc*same_diff +
                        (1|Subject2) + (1|Sound),
                      data = subset(data1, Category == 'Small' & same_diff == "d"),
                      family = binomial,
                      glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_small_diff1)

glmer_small_diff2 <- glmer(Slide3.ACC ~ 
                             Block2 +
                             #con_inc +
                             #same_diff +
                             #con_inc*same_diff +
                             (1|Subject2) + (1|Sound),
                           data = subset(data1, Category == 'Small' & same_diff == "d"),
                           family = binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_small_diff2)

glmer_small_diff3 <- glmer(Slide3.ACC ~ 
                             Block2 +
                             con_inc +
                             #Block2*con_inc
                             #same_diff +
                             #con_inc*same_diff +
                             (1|Subject2) + (1|Sound),
                           data = subset(data1, Category == 'Small' & same_diff == "d"),
                           family = binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_small_diff3)

anova(glmer_small_diff1, glmer_small_diff2)
anova(glmer_small_diff2, glmer_small_diff3)#n.s

#Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#glmer_small_diff2  4 1970.2 1991.5 -981.09   1962.2                         
#glmer_small_diff3  5 1969.9 1996.6 -979.94   1959.9 2.3061      1     0.1289

glmer_small_diff4 <- glmer(Slide3.ACC ~ 
                             Block2 +
                             con_inc +
                             #same_diff +
                             #con_inc*same_diff +
                             Block2*con_inc +
                             #Block2*same_diff +
                             #same_diff*con_inc +
                             (1|Subject2) + (1|Sound),
                           data = subset(data1, Category == 'Small' & same_diff == "d"),
                           family = binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_small_diff4)

anova(glmer_small_diff2, glmer_small_diff4)

#glmer for small vocabulary size data - individual trials

glmer_small_same1 <- glmer(Slide3.ACC ~ 
                             #Block2 +
                             #con_inc +
                             #same_diff +
                             #con_inc*same_diff +
                             #Block2*con_inc*same_diff +
                             (1|Subject2) + (1|Sound),
                           data = subset(data1, Category == 'Small' & same_diff == "s"),
                           family = binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_small_same1)

glmer_small_same2 <- glmer(Slide3.ACC ~ 
                             Block2 +
                             #con_inc +
                             #same_diff +
                             #con_inc*same_diff +
                             #Block2*con_inc*same_diff +
                             (1|Subject2) + (1|Sound),
                           data = subset(data1, Category == 'Small' & same_diff == "s"),
                           family = binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_small_same2)

glmer_small_same3 <- glmer(Slide3.ACC ~ 
                             Block2 +
                             con_inc +
                             #same_diff +
                             #con_inc*same_diff +
                             #Block2*con_inc*same_diff +
                             (1|Subject2) + (1|Sound),
                           data = subset(data1, Category == 'Small' & same_diff == "s"),
                           family = binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_small_same3)

anova(glmer_small_same1, glmer_small_same2)
anova(glmer_small_same2, glmer_small_same3)

#Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
#glmer_small_same2  4 2046.7 2068.0 -1019.3   2038.7                           
#glmer_small_same3  5 2042.1 2068.8 -1016.0   2032.1 6.5879      1    0.01027 *
  
glmer_small_same4 <- glmer(Slide3.ACC ~ 
                             Block2 +
                             con_inc +
                             #same_diff +
                             #con_inc*same_diff +
                             Block2*con_inc +
                             #Block2*same_diff +
                             #Block2*same_diff*con_inc +
                             (1|Subject2) + (1|Sound),
                           data = subset(data1, Category == "Small" & same_diff == "s"),
                           family = binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_small_same4)

anova(glmer_small_same3, glmer_small_same4)

#######################
#glmer for medium vocabulary size
#######################

#glmer for all medium vocabulary size data

glmer_medium1 <- glmer(Slide3.ACC ~ 
                         #Block2 +
                         #con_inc +
                         #same_diff +
                         #con_inc*same_diff +
                         #Block2*con_inc*same_diff +
                         (1|Subject2) + (1|Sound),
                       data = subset(data1, Category == "Medium"),
                       family = binomial,
                       glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_medium1)

glmer_medium2 <- glmer(Slide3.ACC ~ 
                         Block2 +
                         #con_inc +
                         #same_diff +
                         #con_inc*same_diff +
                         #Block2*con_inc*same_diff +
                         (1|Subject2) + (1|Sound),
                       data = subset(data1, Category == "Medium"),
                       family = binomial,
                       glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_medium2)

glmer_medium3 <- glmer(Slide3.ACC ~ 
                         Block2 +
                         con_inc +
                         #same_diff +
                         #con_inc*same_diff +
                         #Block2*con_inc*same_diff +
                         (1|Subject2) + (1|Sound),
                       data = subset(data1, Category == "Medium"),
                       family = binomial,
                       glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_medium3)

glmer_medium4 <- glmer(Slide3.ACC ~ 
                         Block2 +
                         con_inc +
                         same_diff +
                         #con_inc*same_diff +
                         #Block2*con_inc*same_diff +
                         (1|Subject2) + (1|Sound),
                       data = subset(data1, Category == "Medium"),
                       family = binomial,
                       glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_medium4)

glmer_medium5 <- glmer(Slide3.ACC ~ 
                         Block2 +
                         con_inc +
                         same_diff +
                         con_inc*same_diff +
                         #Block2*con_inc*same_diff +
                         (1|Subject2) + (1|Sound),
                       data = subset(data1, Category == "Medium"),
                       family = binomial,
                       glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_medium5)

# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          0.40435    0.11905   3.397 0.000682 ***
#   Block2               0.18064    0.02776   6.506 7.70e-11 ***
#   con_inci            -0.81448    0.09044  -9.005  < 2e-16 ***
#   same_diffs          -0.41745    0.09002  -4.637 3.53e-06 ***
#   con_inci:same_diffs  0.80547    0.12444   6.472 9.64e-11 ***

anova(glmer_medium1, glmer_medium2)
anova(glmer_medium2, glmer_medium3)
anova(glmer_medium3, glmer_medium4)#n.s
anova(glmer_medium3, glmer_medium5)

#glmer for medium vocabulary size data - categorical trials

glmer_medium_diff1 <- glmer(Slide3.ACC ~ 
                             #Block2 +
                             #con_inc +
                             #same_diff +
                             #con_inc*same_diff +
                             #Block2*con_inc*same_diff +
                             (1|Subject2) + (1|Sound),
                           data = subset(data1, Category == 'Medium' & same_diff == "d"),
                           family = binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_medium_diff1)

glmer_medium_diff2 <- glmer(Slide3.ACC ~ 
                             Block2 +
                             #con_inc +
                             #same_diff +
                             #con_inc*same_diff +
                             #Block2*con_inc*same_diff +
                             (1|Subject2) + (1|Sound),
                           data = subset(data1, Category == 'Medium' & same_diff == "d"),
                           family = binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_medium_diff2)

glmer_medium_diff3 <- glmer(Slide3.ACC ~ 
                             Block2 +
                             con_inc +
                             #Block2*con_inc
                             #same_diff +
                             #con_inc*same_diff +
                             #Block2*con_inc*same_diff +
                             (1|Subject2) + (1|Sound),
                           data = subset(data1, Category == 'Medium' & same_diff == "d"),
                           family = binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_medium_diff3)

anova(glmer_medium_diff1, glmer_medium_diff2)
anova(glmer_medium_diff2, glmer_medium_diff3)

#Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#glmer_medium_diff2  4 3034.3 3057.3 -1513.2   3026.3                             
#glmer_medium_diff3  5 2949.9 2978.6 -1470.0   2939.9 86.399      1  < 2.2e-16 ***

#glmer for medium vocabulary size data - individual trials

glmer_medium_same1 <- glmer(Slide3.ACC ~ 
                             #Block2 +
                             #con_inc +
                             #same_diff +
                             #con_inc*same_diff +
                             #Block2*con_inc*same_diff +
                             (1|Subject2) + (1|Sound),
                           data = subset(data1, Category == 'Medium' & same_diff == "s"),
                           family = binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_medium_same1)

glmer_medium_same2 <- glmer(Slide3.ACC ~ 
                             Block2 +
                             #con_inc +
                             #same_diff +
                             #con_inc*same_diff +
                             #Block2*con_inc*same_diff +
                             (1|Subject2) + (1|Sound),
                           data = subset(data1, Category == 'Medium' & same_diff == "s"),
                           family = binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_medium_same2)

glmer_medium_same3 <- glmer(Slide3.ACC ~ 
                             Block2 +
                             con_inc +
                             #same_diff +
                             #con_inc*same_diff +
                             #Block2*con_inc*same_diff +
                             (1|Subject2) + (1|Sound),
                           data = subset(data1, Category == 'Medium' & same_diff == "s"),
                           family = binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_medium_same3)

anova(glmer_medium_same1, glmer_medium_same2)
anova(glmer_medium_same2, glmer_medium_same3)#n.s

#Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
#glmer_medium_same2  4 3077.0 3099.9 -1534.5   3069.0                        
#glmer_medium_same3  5 3078.9 3107.7 -1534.5   3068.9 0.012      1     0.9127

#######################
#glmer for large vocabulary size
#######################

#glmer for all large vocabulary size data

glmer_large1 <- glmer(Slide3.ACC ~ 
                        #Block2 +
                        #con_inc +
                        #same_diff +
                        #con_inc*same_diff +
                        #Block2*con_inc*same_diff +
                        (1|Subject2) + (1|Sound),
                      data = subset(data1, Category == "Large"),
                      family = binomial,
                      glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_large1)

glmer_large2 <- glmer(Slide3.ACC ~ 
                        Block2 +
                        #con_inc +
                        #same_diff +
                        #con_inc*same_diff +
                        #Block2*con_inc*same_diff +
                        (1|Subject2) + (1|Sound),
                      data = subset(data1, Category == "Large"),
                      family = binomial,
                      glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_large2)

glmer_large3 <- glmer(Slide3.ACC ~ 
                        Block2 +
                        con_inc +
                        #same_diff +
                        #con_inc*same_diff +
                        #Block2*con_inc*same_diff +
                        (1|Subject2) + (1|Sound),
                      data = subset(data1, Category == "Large"),
                      family = binomial,
                      glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_large3)

glmer_large4 <- glmer(Slide3.ACC ~ 
                        Block2 +
                        con_inc +
                        same_diff +
                        #con_inc*same_diff +
                        #Block2*con_inc*same_diff +
                        (1|Subject2) + (1|Sound),
                      data = subset(data1, Category == "Large"),
                      family = binomial,
                      glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_large4)

glmer_large5 <- glmer(Slide3.ACC ~ 
                        Block2 +
                        con_inc +
                        same_diff +
                        con_inc*same_diff +
                        #Block2*con_inc*same_diff +
                        (1|Subject2) + (1|Sound),
                      data = subset(data1, Category == "Large"),
                      family = binomial,
                      glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_large5)

# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          0.19376    0.10972   1.766 0.077409 .  
# Block2               0.23974    0.02432   9.857  < 2e-16 ***
#   con_inci            -0.42437    0.07797  -5.443 5.25e-08 ***
#   same_diffs          -0.29277    0.07744  -3.781 0.000156 ***
#   con_inci:same_diffs  0.43763    0.10808   4.049 5.14e-05 ***

anova(glmer_large1, glmer_large2)
anova(glmer_large2, glmer_large3)
anova(glmer_large3, glmer_large4)#n.s
anova(glmer_large3, glmer_large5)

#glmer for large vocabulary size data - categorical trials

glmer_large_diff1 <- glmer(Slide3.ACC ~ 
                             #Block2 +
                             #con_inc +
                             #same_diff +
                             #con_inc*same_diff +
                             #Block2*con_inc*same_diff +
                             (1|Subject2) + (1|Sound),
                           data = subset(data1, Category == 'Large' & same_diff == "d"),
                           family = binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_large_diff1)

glmer_large_diff2 <- glmer(Slide3.ACC ~ 
                             Block2 +
                             #con_inc +
                             #same_diff +
                             #con_inc*same_diff +
                             #Block2*con_inc*same_diff +
                             (1|Subject2) + (1|Sound),
                           data = subset(data1, Category == 'Large' & same_diff == "d"),
                           family = binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_large_diff2)

glmer_large_diff3 <- glmer(Slide3.ACC ~ 
                             Block2 +
                             con_inc +
                             #Block2*con_inc
                             #same_diff +
                             #con_inc*same_diff +
                             #Block2*con_inc*same_diff +
                             (1|Subject2) + (1|Sound),
                           data = subset(data1, Category == 'Large' & same_diff == "d"),
                           family = binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_large_diff3)

anova(glmer_large_diff1, glmer_large_diff2)
anova(glmer_large_diff2, glmer_large_diff3)

#Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#glmer_large_diff2  4 3907.0 3931.2 -1949.5   3899.0                             
#glmer_large_diff3  5 3878.6 3908.8 -1934.3   3868.6 30.437      1  3.448e-08 ***

#glmer for large vocabulary size data - individual trials

glmer_large_same1 <- glmer(Slide3.ACC ~ 
                             #Block2 +
                             #con_inc +
                             #same_diff +
                             #con_inc*same_diff +
                             #Block2*con_inc*same_diff +
                             (1|Subject2) + (1|Sound),
                           data = subset(data1, Category == 'Large' & same_diff == "s"),
                           family = binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_large_same1)

glmer_large_same2 <- glmer(Slide3.ACC ~ 
                             Block2 +
                             #con_inc +
                             #same_diff +
                             #con_inc*same_diff +
                             #Block2*con_inc*same_diff +
                             (1|Subject2) + (1|Sound),
                           data = subset(data1, Category == 'Large' & same_diff == "s"),
                           family = binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_large_same2)

glmer_large_same3 <- glmer(Slide3.ACC ~ 
                             Block2 +
                             con_inc +
                             #same_diff +
                             #con_inc*same_diff +
                             #Block2*con_inc*same_diff +
                             (1|Subject2) + (1|Sound),
                           data = subset(data1, Category == 'Large' & same_diff == "s"),
                           family = binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000000)))
summary(glmer_large_same3)

anova(glmer_large_same1, glmer_large_same2)
anova(glmer_large_same2, glmer_large_same3)#n.s

#Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#glmer_large_same2  4 4032.8 4056.9 -2012.4   4024.8                         
#glmer_large_same3  5 4034.8 4064.9 -2012.4   4024.8 0.0561      1     0.8128




#----------------------------
#Graphs
#----------------------------

####################
#figure 3
####################

#produce values by subject for block*vocab size*congruency*same/different
exp1_block_aggregate <- summarySE(data = data1, measurevar = "Slide3.ACC", groupvars = c("Category", "con_inc", "same_diff", "Subject2", "Block2"))
exp1_block_aggregate$same_diff <- revalue(exp1_block_aggregate$same_diff, c("s"="Individual", "d"="Categorical"))
exp1_block_aggregate$Block2 <- as.factor(exp1_block_aggregate$Block2)

#plot these values using boxplots in the ggplot2 package
exp1_boxplot_block_plot <- ggplot(exp1_block_aggregate, aes(x=Block2, y=Slide3.ACC, fill=con_inc)) +
  geom_boxplot(outlier.alpha = 0, position = position_dodge()) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), alpha=0.2) +
  xlab("Block") +
  ylab("Proportion correct") +
  scale_fill_manual(name="Presentation trial",
                    breaks=c("c", "i"),
                    labels=c("Congruent", "Incongruent"),
                    values=c("gray65", "gray90")) +
  coord_cartesian(ylim=c(0, 1.05)) +
  scale_y_continuous(breaks=seq(0, 1.05, 0.1)) +
  annotate("segment", x = 0.5, xend = 4.5, y = 0.5, yend = 0.5,
           colour = "black", size = 0.5, linetype = "dotted") +
  facet_grid(Category~same_diff) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.05), legend.box.background = element_rect(),axis.text=element_text(size=14), axis.title=element_text(size=14),
        legend.text=element_text(size=17), legend.title = element_text(face = "bold", size=17), legend.direction="vertical", strip.text.x = element_text(size = 14), strip.text.y = element_text(size = 14))

#view
exp1_boxplot_block_plot

#save
ggsave(plot = exp1_boxplot_block_plot, filename = "exp1_boxplot_block_plot.png", dpi=600, width = 10, height = 12)

####################
#figure 4
####################

#produce values by subject for vocab size*congruency*same/different
exp1_aggregate <- summarySE(data = data1, measurevar = "Slide3.ACC", groupvars = c("Category", "con_inc", "same_diff", "Subject2"))
exp1_aggregate$same_diff <- revalue(exp1_aggregate$same_diff, c("s"="Individual", "d"="Categorical"))

#plot the different category trials
exp1_diff_boxplot_plot <- ggplot(subset(exp1_aggregate, same_diff=="Categorical"), aes(x=Category, y=Slide3.ACC, fill=con_inc)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), alpha=0.2) +
  xlab("Vocabulary size condition") +
  ylab("Proportion correct") +
  scale_fill_manual(name="Presentation trial",
                    breaks=c("c", "i"),
                    labels=c("Congruent", "Incongruent"),
                    values=c("gray65", "gray90")) +
  coord_cartesian(ylim=c(0, 1.05)) +
  scale_y_continuous(breaks=seq(0, 1.05, 0.1)) +
  annotate("text", x = 2, y = 1.02, label = "***", size=8) +
  annotate("text", x = 3, y = 1.02, label = "***", size=8) +
  annotate("segment", x = 0.5, xend = 3.5, y = 0.5, yend = 0.5,
           colour = "black", size = 0.5, linetype = "dotted") +
  facet_grid(~same_diff) +
  theme_bw() +
  theme(legend.position="none", strip.text.x = element_text(size = 14), axis.text=element_text(size=14), axis.title=element_text(size=14))

#view
exp1_diff_boxplot_plot

#plot the same category trials
exp1_same_boxplot_plot <- ggplot(subset(exp1_aggregate, same_diff=="Individual"), aes(x=Category, y=Slide3.ACC, fill=con_inc)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1), alpha=0.2) +
  xlab("Vocabulary size condition") +
  ylab("") +
  scale_fill_manual(name="Presentation trial",
                    breaks=c("c", "i"),
                    labels=c("Congruent", "Incongruent"),
                    values=c("gray65", "gray90")) +
  coord_cartesian(ylim=c(0, 1.05)) +
  scale_y_continuous(breaks=seq(0, 1.05, 0.1)) +
  annotate("text", x = 1, y = 1.02, label = "*", size=8) +
  annotate("segment", x = 0.5, xend = 3.5, y = 0.5, yend = 0.5,
           colour = "black", size = 0.5, linetype = "dotted") +
  facet_grid(~same_diff) +
  theme_bw() +
  theme(legend.position = c(0.77, 0.1), legend.box.background = element_rect(), axis.text=element_text(size=14), axis.title=element_text(size=14),
        legend.text=element_text(size=17), legend.title = element_text(face = "bold", size=17), legend.direction="vertical", strip.text.x = element_text(size = 14))

#view
exp1_same_boxplot_plot

#use the cowplot package to plot both different and same category trial plots together
exp1_boxplot_plot <- plot_grid(exp1_diff_boxplot_plot, exp1_same_boxplot_plot, labels = c("A", "B"), ncol = 2, nrow = 1, rel_widths = c(1, 1))
exp1_boxplot_plot

#save
ggsave(plot = exp1_boxplot_plot, filename = "exp1_boxplot_plot2.png", dpi=600, width = 14, height = 10)



#------------------------
#Questionnaire data
#------------------------

#######################
#lmer for questionnaire data
#######################

lmer_questionnaire1 <- lmer(Response_score_2 ~
                              #Category +
                              (1|Subject) + (1|Sound),
                            data=questionnaire_data, REML = FALSE)
summary(lmer_questionnaire1)

lmer_questionnaire2 <- lmer(Response_score_2 ~
                              Category +
                              (1|Subject) + (1|Sound),
                            data=questionnaire_data, REML = FALSE)
summary(lmer_questionnaire2)

anova(lmer_questionnaire1, lmer_questionnaire2)


#######################
#questionnaire data plot
#######################

sr_data <- ddply(questionnaire_data, c("Category"),
                 summarise,
                 N = length(Response_score_2),
                 mean = mean(Response_score_2),
                 sd = sd(Response_score_2),
                 se = sd / sqrt(N))

sr_plot <- ggplot(sr_data, aes(x=Category, y=mean)) +
  geom_bar(aes(),
           position=position_dodge(),
           stat="identity",
           colour="black", # Use black outlines,
           size=.3) +     # Thinner lines
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  coord_cartesian(ylim=c(-1.0,1.0)) +
  scale_y_continuous(breaks=seq(-1, 1, 0.25)) +
  ylab("Mean") +
  xlab("Consonant sound") +
  scale_x_discrete(breaks=c("p", "r"),
                   labels=c("Plosives", "Continuants")) +
  theme_bw()

sr_plot



#--------------------------
#additional analysis for reviewers comments
#--------------------------

################################
#overall chance t-tests
################################

#small same inc
t.test(subset(data1, Category == 'Small' & same_diff == "s" & con_inc == "i")$Slide3.ACC, mu = 0.5) #t = 4.3811, df = 767, p-value = 1.345e-05 mean of x 0.578125 
#small same con
t.test(subset(data1, Category == 'Small' & same_diff == "s" & con_inc == "c")$Slide3.ACC, mu = 0.5) #t = 8.2804, df = 767, p-value = 5.443e-16 mean of x 0.6432292
#small diff inc
t.test(subset(data1, Category == 'Small' & same_diff == "d" & con_inc == "i")$Slide3.ACC, mu = 0.5) #t = 5.0587, df = 767, p-value = 5.285e-07 mean of x 0.5898438
#small diff con
t.test(subset(data1, Category == 'Small' & same_diff == "d" & con_inc == "c")$Slide3.ACC, mu = 0.5) #t = 7.31, df = 767, p-value = 6.724e-13 mean of x 0.6276042 

#medium same inc
t.test(subset(data1, Category == 'Medium' & same_diff == "s" & con_inc == "i")$Slide3.ACC, mu = 0.5) #t = 7.038, df = 1151, p-value = 3.343e-12 mean of x 0.6015625 
#medium same con
t.test(subset(data1, Category == 'Medium' & same_diff == "s" & con_inc == "c")$Slide3.ACC, mu = 0.5) #t = 7.2896, df = 1151, p-value = 5.766e-13 mean of x 0.6050347 
#medium diff inc
t.test(subset(data1, Category == 'Medium' & same_diff == "d" & con_inc == "i")$Slide3.ACC, mu = 0.5) #t = 0.64802, df = 1151, p-value = 0.5171 mean of x 0.5095486 
#medium diff con
t.test(subset(data1, Category == 'Medium' & same_diff == "d" & con_inc == "c")$Slide3.ACC, mu = 0.5) #t = 14.396, df = 1151, p-value < 2.2e-16 mean of x 0.6953125 

#large same inc
t.test(subset(data1, Category == 'Large' & same_diff == "s" & con_inc == "i")$Slide3.ACC, mu = 0.5) #t = 9.724, df = 1535, p-value < 2.2e-16 mean of x 0.6204427 
#large same con
t.test(subset(data1, Category == 'Large' & same_diff == "s" & con_inc == "c")$Slide3.ACC, mu = 0.5) #t = 9.3902, df = 1535, p-value < 2.2e-16 mean of x 0.6165365 
#large diff inc
t.test(subset(data1, Category == 'Large' & same_diff == "d" & con_inc == "i")$Slide3.ACC, mu = 0.5) #t = 6.9424, df = 1535, p-value = 5.67e-12 mean of x 0.5872396 
#large diff con
t.test(subset(data1, Category == 'Large' & same_diff == "d" & con_inc == "c")$Slide3.ACC, mu = 0.5) #t = 15.151, df = 1535, p-value < 2.2e-16 mean of x 0.6803385 

######################
#block 4 chance t-tests
######################

#small same inc
t.test(subset(data1, Category == 'Small' & same_diff == "s" & con_inc == "i" & Block2 == 4)$Slide3.ACC, mu = 0.5) #t = 3.0982, df = 191, p-value = 0.002241
#small same con
t.test(subset(data1, Category == 'Small' & same_diff == "s" & con_inc == "c" & Block2 == 4)$Slide3.ACC, mu = 0.5) #t = 4.5465, df = 191, p-value = 9.666e-06
#small diff inc
t.test(subset(data1, Category == 'Small' & same_diff == "d" & con_inc == "i" & Block2 == 4)$Slide3.ACC, mu = 0.5) #t = 4.8862, df = 191, p-value = 2.17e-06
#small diff con
t.test(subset(data1, Category == 'Small' & same_diff == "d" & con_inc == "c" & Block2 == 4)$Slide3.ACC, mu = 0.5) #t = 6.5277, df = 191, p-value = 5.867e-10

#medium same inc
t.test(subset(data1, Category == 'Medium' & same_diff == "s" & con_inc == "i" & Block2 == 4)$Slide3.ACC, mu = 0.5) #t = 5.0318, df = 287, p-value = 8.587e-07
#medium same con
t.test(subset(data1, Category == 'Medium' & same_diff == "s" & con_inc == "c" & Block2 == 4)$Slide3.ACC, mu = 0.5) #t = 5.4364, df = 287, p-value = 1.163e-07
#medium diff inc
t.test(subset(data1, Category == 'Medium' & same_diff == "d" & con_inc == "i" & Block2 == 4)$Slide3.ACC, mu = 0.5) #t = 3.6086, df = 287, p-value = 0.0003631
#medium diff con
t.test(subset(data1, Category == 'Medium' & same_diff == "d" & con_inc == "c" & Block2 == 4)$Slide3.ACC, mu = 0.5) #t = 9.2485, df = 287, p-value < 2.2e-16

#large same inc
t.test(subset(data1, Category == 'Large' & same_diff == "s" & con_inc == "i" & Block2 == 4)$Slide3.ACC, mu = 0.5) #t = 6.5574, df = 383, p-value = 1.775e-10
#large same con
t.test(subset(data1, Category == 'Large' & same_diff == "s" & con_inc == "c" & Block2 == 4)$Slide3.ACC, mu = 0.5) #t = 8.7008, df = 383, p-value < 2.2e-16
#large diff inc
t.test(subset(data1, Category == 'Large' & same_diff == "d" & con_inc == "i" & Block2 == 4)$Slide3.ACC, mu = 0.5) #t = 6.0843, df = 383, p-value = 2.839e-09
#large diff con
t.test(subset(data1, Category == 'Large' & same_diff == "d" & con_inc == "c" & Block2 == 4)$Slide3.ACC, mu = 0.5) #t = 11.616, df = 383, p-value < 2.2e-16



library("checkpoint")
checkpoint("2019-04-23")
library(ggplot2)

library(dplyr)
library(gridExtra)
library(mgcv) #gam

#setwd('~/Box/RESEARCH/WomenInScience/newdata/')
dat <- read.csv('output/OpenSci3Discipline.csv', stringsAsFactors = FALSE)
nrow(dat) #2926

#number of papers in each literature
dat %>% group_by(Tag) %>% summarise(count=n())
# 1 OpenScience       879
# 2 Reproducibility  2047

#number of Journal Articles and Conference Proceedings 
dat %>% group_by(DocType) %>% summarise(count=n())

#########################################################      
#########################################################
### Female participation analysis
#########################################################
#########################################################

# variables X1st, X2nd, etc. = 1 if female, 0 if male, NA if unknown

#Female First or Last Author
dat$femaleLead <- (dat$X1st==1) | (dat$last==1)
table(dat$femaleLead, dat$Tag)
# OpenScience Reproducibility
# FALSE         336             613
# TRUE          364             652

#percent of multi-author papers with female lead (of those where it is observable)
dat %>% filter(authorCount > 1) %>% group_by(Tag) %>% 
  summarise(count_known=sum(!is.na(femaleLead)), 
            pct_femLead = sum(femaleLead, na.rm=TRUE)/count_known)
# 1 OpenScience             482       0.606
# 2 Reproducibility         995       0.579

#number of single-authored papers in each literature
dat %>% filter(authorCount==1) %>% group_by(Tag) %>% summarise(count=n())
# 1 OpenScience       255
# 2 Reproducibility   342

#number of multi-authored papers in each literature
dat %>% filter(authorCount > 1) %>% group_by(Tag) %>% summarise(count=n())
# 1 OpenScience       624
# 2 Reproducibility  1705

#number of single-authored papers with female, male or unknown author gender
dat %>% filter(authorCount==1) %>% group_by(Tag, X1st) %>% summarise(count=n())
# Tag              X1st count
# <chr>           <int> <int>
#   1 OpenScience       0   146
# 2 OpenScience         1    72
# 3 OpenScience        NA    37
# 4 Reproducibility     0   194
# 5 Reproducibility     1    76
# 6 Reproducibility    NA    72

#distribution of team size
hist(dat$authorCount, breaks=seq(0,94,2), main='Distribution of Team Size', xlab='Number of Authors')
abline(v=15, col='red')

#sample size for regression
dat %>% filter(authorCount > 1, authorCount <= 12, !is.na(femaleLead)) %>% group_by(Tag) %>% summarise(count=n())
# 1 OpenScience       454
# 2 Reproducibility   955

#number of papers with more than 12 authors (excluded from regression)
dat %>% filter(authorCount > 12, !is.na(femaleLead)) %>% group_by(Tag) %>% summarise(count=n())
# 1 OpenScience        28
# 2 Reproducibility    40



### Test for differences in rates of female vs. male authorship in single-author papers

tmp <- dplyr::filter(dat, authorCount==1)
tab <- table(tmp$femaleLead, tmp$Tag, useNA = 'ifany')
rownames(tab) <- c("Male","Female","Unknown")
tab
# OpenScience Reproducibility
# Male            146             194
# Female           72              76
# Unknown          37              72
round(tab/matrix(colSums(tab), nrow=3,ncol=2,byrow=TRUE),3)
# OpenScience Reproducibility
# Male          0.573           0.567
# Female        0.282           0.222
# Unknown       0.145           0.211

### COMPARE RATES OF WOMEN IN SINGLE-AUTHOR PAPERS

# % of single-author papers written by women, among those with known gender
tab <- tab[1:2,] #exclude papers with unknown author gender 
n_OS <- sum(tab[,1])
n_RR <- sum(tab[,2])
n_fem_OS <- tab[2,1]
n_fem_RR <- tab[2,2]
pct_fem_OS <- n_fem_OS/n_OS #33% 
pct_fem_RR <- n_fem_RR/n_RR #28%

#H0: p = 0.5

#Normal approximation
t_OS <- (pct_fem_OS-0.5)/sqrt(0.5*0.5/n_OS)
t_RR <- (pct_fem_RR-0.5)/sqrt(0.5*0.5/n_RR)
pnorm(t_OS) #2.694587e-07
pnorm(t_RR) #4.888296e-13

#Exact test
pbinom(n_fem_OS, size = n_OS, prob = 0.5) #3.012727e-07
pbinom(n_fem_RR, size = n_RR, prob = 0.5) #2.279883e-13

#H0: p_OS = p_RR
pct_fem = (n_fem_OS+n_fem_RR)/(n_OS+n_RR)
t_compare <- (pct_fem_OS - pct_fem_RR)/sqrt(pct_fem*(1-pct_fem)*(1/n_OS + 1/n_RR))
pnorm(t_compare, lower.tail = FALSE) #p=0.1218532

#########################################################
# Exploratory Analysis
#########################################################

### Number of papers in each field over Time

pdf('figures/TimeHist.pdf', width=8, height=5)
par(mfrow=c(1,2))
par(cex.axis=0.7)
hist(dat$Year[dat$Tag=='OpenScience'], col=rgb(0,0,1,0.5), breaks=2010:2017, xlab='Year', xlim=c(2010,2017), main='Open Science', ylab='Number of Papers Published')
hist(dat$Year[dat$Tag=='Reproducibility'], col=rgb(0,0,1,0.5), breaks=2010:2017, xlab='Year', xlim=c(2010,2017), main='Reproducibility', ylab='Number of Papers Published')
dev.off()

### Number of papers in each field by Team Size

pdf('figures/TeamSizeHist.pdf', width=8, height=5)
par(mfrow=c(1,2))
par(cex.axis=0.7)
hist(dat$authorCount[dat$Tag=='OpenScience'], col=rgb(0,0,1,0.5), xlab='Number of Authors', xlim=c(0,20), breaks=seq(0,100), main='Open Science', ylab='Number of Papers Published')
hist(dat$authorCount[dat$Tag=='Reproducibility'], col=rgb(0,0,1,0.5), xlab='Number of Authors', xlim=c(0,20), breaks=seq(0,100), main='Reproducibility', ylab='Number of Papers Published')
dev.off()


### Team Size Distribution (**** FIGURE 3 ****)

#how many papers then excluded due to high number of authors?
dat %>% filter(authorCount > 1, !is.na(femaleLead), authorCount > 15) %>% group_by(Tag) %>% summarize(count=n())
# 1 OpenScience        21
# 2 Reproducibility    25

dat %>% filter(authorCount > 1, !is.na(femaleLead), authorCount > 20) %>% group_by(Tag) %>% summarize(count=n())
# 1 OpenScience        14
# 2 Reproducibility    15

pdf('figures/TeamSizeHist_fem.pdf', width=8, height=5) 
par(mfrow=c(1,2))
angle <- c(45,-45)

#Open Science
datOS <- filter(dat, Tag=='OpenScience', authorCount <= 20, authorCount > 1, !is.na(femaleLead))
hist(datOS$authorCount[datOS$femaleLead==TRUE], border='black', breaks=seq(1,20,1), main='Open Science', xlab='Team Size', ylim=c(0,150), xlim=c(0,20))
hist(datOS$authorCount[datOS$femaleLead==FALSE], border='gray', breaks=seq(1,20,1), add=TRUE)
legend('topright', legend=c('Female with High Status', 'No Female with High Status'), fill='white', border=c('black','gray'))
# hist(datOS$authorCount[datOS$femaleLead==TRUE], angle=angle[1], density=30, col='magenta', breaks=seq(1,20,1), main='Open Science', xlab='Team Size', ylim=c(0,200), xlim=c(0,20))
# hist(datOS$authorCount[datOS$femaleLead==FALSE], angle=angle[2], density=30, col='blue', breaks=seq(1,20,1), add=TRUE)
# legend('topright', legend=c('Female with High Status', 'No Female with High Status'), fill=c('magenta','blue'),angle=angle,density=30)

#Reproducibility
datRP <- filter(dat, Tag=='Reproducibility', authorCount <= 20, authorCount > 1, !is.na(femaleLead))
hist(datRP$authorCount[datRP$femaleLead==TRUE], border='black', breaks=seq(1,20,1), main='Reproducibility', xlab='Team Size', ylim=c(0,150), xlim=c(0,20))
hist(datRP$authorCount[datRP$femaleLead==FALSE], border='gray', breaks=seq(1,20,1), add=TRUE)
legend('topright', legend=c('Female with High Status', 'No Female with High Status'), fill='white', border=c('black','gray'))
# hist(datRP$authorCount[datRP$femaleLead==TRUE], angle=angle[1], density=30, col='magenta', breaks=seq(1,20,1), main='Reproducibility', xlab='Team Size', ylim=c(0,200), xlim=c(0,20))
# hist(datRP$authorCount[datRP$femaleLead==FALSE], angle=angle[2], density=30, col='blue', breaks=seq(1,20,1), add=TRUE)
# legend('topright', legend=c('Female with High Status', 'No Female with High Status'), fill=c('magenta','blue'),angle=angle,density=30)

dev.off()


### Time Trends: Female lead authorship is frowing in Open Science but declining in Reproducibility

pdf('figures/TimeTrends_fem.pdf', width=8, height=5) 
#multi-author papers
dat_summ1 <- dat %>% filter(Year >= 2010, Year < 2018, authorCount <= 15, authorCount > 1) %>% 
  group_by(Tag, Year) %>% 
  summarize(count=sum(!is.na(femaleLead)), pctFemaleLead = mean(femaleLead, na.rm=TRUE), logitFemaleLead = log(pctFemaleLead/(1-pctFemaleLead)))
p1 <- ggplot(dat_summ1, aes(x=Year, y=logitFemaleLead, color=Tag, group=Tag)) + 
  geom_point() + geom_smooth() + ylab('Logit of Pr(Female Lead)') +
  ggtitle('Multi-Author Papers') + theme(legend.position='bottom')
#single-author papers
dat_summ2 <- dat %>% filter(Year >= 2010, Year < 2018, authorCount == 1) %>% 
  group_by(Tag, Year) %>% 
  summarize(count=sum(!is.na(femaleLead)), pctFemaleLead = mean(femaleLead, na.rm=TRUE), logitFemaleLead = log(pctFemaleLead/(1-pctFemaleLead)))
p2 <- ggplot(dat_summ2, aes(x=Year, y=logitFemaleLead, color=Tag, group=Tag)) + 
  geom_point() + geom_smooth() + ylab('Logit of Pr(Female Lead)') +
  ggtitle('Single-Author Papers') + theme(legend.position='bottom')
grid.arrange(p1, p2, nrow=1)
dev.off()

#########################################################
#Models for Female Lead Authorship
#########################################################

#how many papers excluded due to unknown female lead authorship?
dat %>% filter(authorCount > 1, is.na(femaleLead)) %>% group_by(Tag) %>% summarize(count=n())
# 1 OpenScience       142
# 2 Reproducibility   710

#how many papers then excluded due to high number of authors?
dat %>% filter(authorCount > 1, !is.na(femaleLead), authorCount > 12) %>% group_by(Tag) %>% summarize(count=n())
# 1 OpenScience        28
# 2 Reproducibility    40

dat3 <- filter(dat, authorCount <= 12) #very few papers with over 12 authors in either literature (numbers above)
dat3 <- filter(dat3, authorCount > 1) #exclude single-author papers
dat3 %>% group_by(Tag) %>% summarise(count=sum(!is.na(femaleLead)))
# 1 OpenScience       454
# 2 Reproducibility  955

### Model 1: All multi-author papers with known female lead authorship

dat3a <- filter(dat3, !is.na(femaleLead))
dat3a$Conference <- (dat3a$DocType == 'Conference')
dat3a %>% group_by(Tag) %>% summarise(count=n())
# 1 OpenScience       454
# 2 Reproducibility   955

# fit3a <- glm(formula = femaleLead*1 ~ Tag*(Year2 + authorCount_m2 + authorCount_m2_sq) + Conference, family = 'binomial', data=dat3a)

dat3a$RR <- (dat3a$Tag=='Reproducibility')
dat3a$authorCount_m2 <- (dat3a$authorCount - 2) #center at the smallest team size (2)
dat3a$Year2 <- (dat3a$Year - 2017) #center at the most recent publication year 
dat3a$RR_AuthorCount <- (dat3a$RR)*(dat3a$authorCount_m2)
fit3a <- gam(formula = femaleLead*1 ~ RR*Year2 + s(authorCount_m2) + s(RR_AuthorCount) + Conference, family = 'binomial', data=dat3a)
summary(fit3a)
# Parametric coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     1.02429    0.16149   6.343 2.26e-10 ***
#   RRTRUE         -0.93331    0.21275  -4.387 1.15e-05 ***
#   Year2           0.14482    0.05129   2.823  0.00475 ** 
#   ConferenceTRUE -0.38474    0.18140  -2.121  0.03393 *  
#   RRTRUE:Year2   -0.18137    0.05897  -3.076  0.00210 ** 
  
#coefficients on odd scale
(coef_odds <- exp(summary(fit3a)$p.coeff))
# (Intercept)         RRTRUE          Year2 ConferenceTRUE   RRTRUE:Year2 
# 2.7851284      0.3932506      1.1558262      0.6806292      0.8341235 

#coefficients on probability scale
coef_odds/(1+coef_odds)
# (Intercept)         RRTRUE          Year2 ConferenceTRUE   RRTRUE:Year2 
# 0.7358082      0.2822540      0.5361407      0.4049848      0.4547804 

#time trends
exp(summary(fit3a)$p.coeff[3]) #OS: 1.1558262

#get p-value for time trend in RR
dat3a$OS <- (dat3a$Tag=='OpenScience')
dat3a$OS_AuthorCount <- (dat3a$OS)*(dat3a$authorCount_m2)
fit3a_alt <- gam(formula = femaleLead*1 ~ OS*Year2 + s(authorCount_m2) + s(OS_AuthorCount) + Conference, family = 'binomial', data=dat3a)
summary(fit3a_alt)
1-exp(summary(fit3a_alt)$p.coeff[3]) #RR: -0.03601971
exp(summary(fit3a_alt)$p.coeff[5]) #OS: 1.196883

#make confidence intervals on odds scale
table <- as.data.frame(summary(fit3a)$p.table[,1:2])
table$LB_logodds <- table$Estimate - 1.96*table$`Std. Error`
table$UB_logodds <- table$Estimate + 1.96*table$`Std. Error`
table <- table[,c(1,3,4)]
table_odds <- exp(table)
round(table_odds, 3)


# Plot predictions

#function to convert log odds to probability scale (expit function)
logodds_to_prob <- function(logodds){ 
  odds <- exp(logodds)
  return(odds/(1 + odds))
}

dat.pred3a <- expand.grid(Tag=c('Reproducibility','OpenScience'),
                          Conference = c(FALSE,TRUE),
                          Year2 = (2010:2017) - 2017,
                          authorCount_m2 = (2:12)-2)
# dat.pred3a$authorCount_m2_sq <- (dat.pred3a$authorCount_m2)^2
dat.pred3a$authorCount <- dat.pred3a$authorCount_m2 + 2
dat.pred3a$RR <- (dat.pred3a$Tag=='Reproducibility')
dat.pred3a$RR_AuthorCount <- (dat.pred3a$RR)*(dat.pred3a$authorCount_m2)
pred3a <- predict(fit3a, newdata=dat.pred3a, type = "link", se.fit = TRUE) #on log-odds scale
dat.pred3a$predict <- pred3a$fit
dat.pred3a$SE <- pred3a$se.fit
dat.pred3a$LB <- (dat.pred3a$predict - 1.96*dat.pred3a$SE)
dat.pred3a$UB <- (dat.pred3a$predict + 1.96*dat.pred3a$SE)
dat.pred3a$predict_p <- logodds_to_prob(dat.pred3a$predict)
dat.pred3a$LB_p <- logodds_to_prob(dat.pred3a$LB)
dat.pred3a$UB_p <- logodds_to_prob(dat.pred3a$UB)

#average team size?
dat3 %>% summarize(avg_team_size = mean(authorCount)) #4.798661

dat.pred3a.journals2017 <- filter(dat.pred3a, !Conference, Year2==0) #fix year and document type, vary team size
dat.pred3a.journals4auth <- filter(dat.pred3a, !Conference, authorCount == 4) #fix team size and document type, vary year of publication

#Team Size
pdf('figures/PredictedProbs_multiauthor_spline2.pdf', width=8.5, height=5)
p1 <- ggplot(dat.pred3a.journals2017, aes(x=authorCount, group=Tag)) + 
  geom_ribbon(aes(ymin=LB_p, ymax=UB_p), alpha=0.2) + 
  geom_line(aes(y=predict_p, color=Tag), size=2) + 
  ylim(0.35,0.8) + xlim(2,12) + theme_bw() + theme(legend.position='bottom', panel.grid=element_blank()) +
  scale_color_manual(name=NULL, values=c('darkgray','turquoise')) +
  xlab('Number of Authors') + ylab('Probability of Female in a High Status Position') +
  labs(title='Female Lead Authorship versus Team Size', subtitle='Journal Articles Published in 2017')
p2 <- ggplot(dat.pred3a.journals4auth, aes(x=Year2+2017, group=interaction(Tag,authorCount))) + 
  geom_ribbon(aes(ymin=LB_p, ymax=UB_p), alpha=0.2) + 
  geom_line(aes(y=predict_p, color=Tag), size=2) + 
  ylim(0.25,0.8) + theme_bw() + theme(legend.position='bottom', panel.grid=element_blank()) +
  scale_x_continuous(breaks=2010:2017) +
  scale_color_manual(name=NULL, values=c('darkgray','turquoise')) +
  xlab('Year of Publication') + ylab('Probability of Female in a High Status Position') +
  labs(title='Female Lead Authorship vs Publication Year', subtitle='Journal Articles with 4 Authors (Average Size)')
grid.arrange(p1, p2, nrow=1)
dev.off()

#Team Size
#pdf('PredictedProbs_multiauthor_teamsize_spline.pdf', width=6, height=5)
#print(p1)
#dev.off()

#Time
#pdf('PredictedProbs_multiauthor_year_spline.pdf', width=6, height=5)
#print(p2)
#dev.off()

##### Model 1B: Control for Field of Study ######

#each paper has several FOSs listed ... how many unique FOSs?
FoS <- paste(dat3a$FoSNames, collapse='; ')
FoS <- trimws((strsplit(FoS, ';', fixed=TRUE))[[1]])
FoS_unique <- unique(FoS)
length(FoS_unique) #2328

#consider the most frequently appearing FOSs
FOScount <- sort(table(FoS))
FOScount_pct <- cumsum(rev(FOScount))/sum(FOScount)
howmany_50pct <- min(which(FOScount_pct >= .5))
topFOScount_pct <- FOScount_pct[1:howmany_50pct]
pdf('figures/FoS_appearances.pdf', width=8, height=4)
qplot(y=topFOScount_pct, x=factor(names(topFOScount_pct), levels=names(topFOScount_pct))) + xlab('Field of Study') + ylab('Cumulative % of Appearances') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('Fields of Study Explaining 50% of All FoS Appearances')
dev.off()      

topFOScount <- tail(FOScount,30)
topFOS <- names(topFOScount)
#combine medical fields into a single field (below)
medicineFOS <- c("anesthesiology","cardiology","diabetes mellitus","internal medicine","surgery","alternative medicine","physical therapy","pathology","radiology","medicine")
#exclude FOSs that do not represent traditional fields of study
notFOS <- c("publishing","workflow","data sharing","reproducibility","open science","repeatability","open data", "data mining","intraclass correlation")
topFOS <- setdiff(topFOS, notFOS)

sum(topFOScount) #4562
sum(table(FoS)) #10335

#create indicator columns for each of the FOSs
dat3a$anyFOS <- 0 #for each paper, determine if any of the FOSs on the short-list are listed 
for(i in 1:length(topFOS)){
  print(i)
  FOS_i = topFOS[i]
  dat3a$newcol = grepl(pattern = FOS_i, x = dat3a$FoSNames, fixed = F)
  dat3a$anyFOS = dat3a$anyFOS + dat3a$newcol
  names(dat3a)[names(dat3a)=="newcol"] <- FOS_i
}
dat3a$anyFOS <- (dat3a$anyFOS > 0)
table(dat3a$anyFOS)
# FALSE  TRUE 
# 98  1311
mean(dat3a$anyFOS) #0.9304471

#combine medicine-related FOSs into a single FOS
dat3a$medicine <- rowSums(dat3a[,medicineFOS])>0
topFOS2 <- c("medicine", setdiff(topFOS, medicineFOS))

#remove spaces in column names
names(dat3a) <- gsub(pattern = " ", replacement = "", x = names(dat3a))
topFOS2 <- gsub(pattern = " ", replacement = "", x = topFOS2)

#sample size for new model
dat3a %>% filter(anyFOS) %>% group_by(Tag) %>% summarise(count=n())
# 1 OpenScience       401
# 2 Reproducibility   910

#fit model with new FOS indicator variables
formula <- as.formula(paste0("femaleLead*1 ~ RR*Year2 + s(authorCount_m2) + s(RR_AuthorCount) + Conference +", paste(topFOS2,collapse = " + ")))
fit3b <- gam(formula = formula, family = 'binomial', data= dat3a[dat3a$anyFOS,])
pdf('figures/spline_estimates.pdf', width=6, height=4)
plot(fit3b, pages=1)
dev.off()

summary(fit3b)
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                 0.88032    0.25183   3.496 0.000473 ***
#   RRTRUE                     -0.71550    0.23061  -3.103 0.001918 ** 
#   Year2                       0.11674    0.05727   2.038 0.041515 *  
#   ConferenceTRUE             -0.22197    0.21010  -1.056 0.290758    
# medicineTRUE                0.05638    0.18158   0.310 0.756186    
# artificialintelligenceTRUE -0.01666    0.32403  -0.051 0.958988    
# managementscienceTRUE       0.12087    0.30066   0.402 0.687665    
# analyticalchemistryTRUE    -0.78940    0.29509  -2.675 0.007470 ** 
#   bioinformaticsTRUE         -0.56847    0.26228  -2.167 0.030202 *  
#   engineeringTRUE            -0.04795    0.19742  -0.243 0.808096    
# knowledgemanagementTRUE     0.48385    0.27072   1.787 0.073892 .  
# psychologyTRUE             -0.11178    0.23939  -0.467 0.640533    
# softwareTRUE               -0.09736    0.22395  -0.435 0.663740    
# biologyTRUE                 0.32164    0.22955   1.401 0.161155    
# statisticsTRUE              0.35846    0.21051   1.703 0.088610 .  
# computerscienceTRUE        -0.23221    0.19732  -1.177 0.239264    
# RRTRUE:Year2               -0.15497    0.06512  -2.380 0.017329 *

exp(summary(fit3b)$p.coeff)
# (Intercept)                     RRTRUE                      Year2 
# 2.0809466                  0.5057489                  1.1208422 
# ConferenceTRUE               medicineTRUE artificialintelligenceTRUE 
# 0.8170876                  1.2803541                  0.9982283 
# anesthesiologyTRUE            datasharingTRUE               workflowTRUE 
# 1.1008653                  0.7204109                  0.9378115 
# cardiologyTRUE      managementscienceTRUE             publishingTRUE 
# 0.7858934                  1.1804293                  1.2730461 
# analyticalchemistryTRUE         bioinformaticsTRUE            engineeringTRUE 
# 0.4808829                  0.6106435                  1.0064758 
# knowledgemanagementTRUE             psychologyTRUE               softwareTRUE 
# 1.7490100                  0.9580213                  0.9111430 
# biologyTRUE             statisticsTRUE        computerscienceTRUE 
# 1.4690358                  1.4875839                  0.8707008 
# RRTRUE:Year2 
# 0.8600996 

table <- as.data.frame(summary(fit3b)$p.table[,1:2])
table$LB_logodds <- table$Estimate - 1.96*table$`Std. Error`
table$UB_logodds <- table$Estimate + 1.96*table$`Std. Error`
table <- table[,c(1,3,4)]
table_odds <- exp(table)
round(table_odds, 3)



# Plot predictions

dat.pred3b <- expand.grid(Tag=c('Reproducibility','OpenScience'),
                          Conference = c(FALSE,TRUE),
                          Year2 = (2010:2017) - 2017,
                          authorCount_m2 = (2:12)-2)
falses = as.data.frame(matrix(FALSE, nrow=nrow(dat.pred3b), ncol=length(topFOS2)))
names(falses) = topFOS2
dat.pred3b <- cbind(dat.pred3b, falses)

# dat.pred3b$authorCount_m2_sq <- (dat.pred3b$authorCount_m2)^2
dat.pred3b$authorCount <- dat.pred3b$authorCount_m2 + 2
dat.pred3b$RR <- (dat.pred3b$Tag=='Reproducibility')
dat.pred3b$RR_AuthorCount <- (dat.pred3b$RR)*(dat.pred3b$authorCount_m2)
pred3b <- predict(fit3b, newdata=dat.pred3b, type = "link", se.fit = TRUE) #on log-odds scale
dat.pred3b$predict <- pred3b$fit
dat.pred3b$SE <- pred3b$se.fit
dat.pred3b$LB <- (dat.pred3b$predict - 1.96*dat.pred3b$SE)
dat.pred3b$UB <- (dat.pred3b$predict + 1.96*dat.pred3b$SE)
dat.pred3b$predict_p <- logodds_to_prob(dat.pred3b$predict)
dat.pred3b$LB_p <- logodds_to_prob(dat.pred3b$LB)
dat.pred3b$UB_p <- logodds_to_prob(dat.pred3b$UB)


dat.pred3b.journals2017 <- filter(dat.pred3b, !Conference, Year2==0) #fix year and document type, vary team size
dat.pred3b.journals4auth <- filter(dat.pred3b, !Conference, authorCount == 4) #fix team size and document type, vary year of publication

#Team Size
pdf('figures/PredictedProbs_multiauthor_spline2_FOS.pdf', width=8.5, height=5)
p1 <- ggplot(dat.pred3b.journals2017, aes(x=authorCount, group=Tag)) + 
  geom_ribbon(aes(ymin=LB_p, ymax=UB_p), alpha=0.2) + 
  geom_line(aes(y=predict_p, color=Tag), size=2) + 
  ylim(0.35,0.8) + xlim(2,12) + theme_bw() + theme(legend.position='bottom', panel.grid=element_blank()) +
  scale_color_manual(name=NULL, values=c('darkgray','turquoise')) +
  xlab('Number of Authors') + ylab('Probability of Female in a High Status Position') +
  labs(title='Female Lead Authorship versus Team Size', subtitle='Journal Articles Published in 2017')
p2 <- ggplot(dat.pred3b.journals4auth, aes(x=Year2+2017, group=interaction(Tag,authorCount))) + 
  geom_ribbon(aes(ymin=LB_p, ymax=UB_p), alpha=0.2) + 
  geom_line(aes(y=predict_p, color=Tag), size=2) + 
  ylim(0.25,0.8) + theme_bw() + theme(legend.position='bottom', panel.grid=element_blank()) +
  scale_x_continuous(breaks=2010:2017) +
  scale_color_manual(name=NULL, values=c('darkgray','turquoise')) +
  xlab('Year of Publication') + ylab('Probability of Female in a High Status Position') +
  labs(title='Female Lead Authorship vs Publication Year', subtitle='Journal Articles with 4 Authors (Average Size)')
grid.arrange(p1, p2, nrow=1)
dev.off()






### Model 2: All single-author papers

# dat3b <- filter(dat3, singleAuthor, !is.na(femaleLead))
# dat3b %>% group_by(Tag, femaleLead) %>% summarise(count=n())
# fit3b <- glm(formula = femaleLead*1 ~ Tag*Year2 + DocType, family = 'binomial', data=dat3b)
# summary(fit3b)
# 
# dat.pred3b <- expand.grid(Tag=c('Reproducibility','OpenScience'),
#                           DocType = c('Journal','Conference'),
#                           Year2 = (2010:2017) - 2017,
#                           authorCount = 2:15)
# dat.pred3b$authorCount2 <- (dat.pred3b$authorCount)^2
# pred3b <- predict(fit3b, newdata=dat.pred3b, type = "link", se.fit = TRUE)
# dat.pred3b$predict <- pred3b$fit
# dat.pred3b$SE <- pred3b$se.fit
# dat.pred3b$LB <- (dat.pred3b$predict - 1.96*dat.pred3b$SE)
# dat.pred3b$UB <- (dat.pred3b$predict + 1.96*dat.pred3b$SE)
# dat.pred3b$predict_p <- logodds_to_prob(dat.pred3b$predict)
# dat.pred3b$LB_p <- logodds_to_prob(dat.pred3b$LB)
# dat.pred3b$UB_p <- logodds_to_prob(dat.pred3b$UB)
# 


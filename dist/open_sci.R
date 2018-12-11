setwd('~/Box Sync/Research/WomenInScience')

library(ggplot2)
library(dplyr)
library(gridExtra)
library(textcat)
library(Hmisc)
# library(betareg)
# library(gamlss)
# library(gamlss.dist) # gamlss.dist package, contains inflated

#########################################################
### Select subset of articles to obtain abstract for semantic analysis
#########################################################

### Open Science Dataset

#read data and assign ID
dat <- read.csv('OpenSci.csv')
dat$ID <- 1:nrow(dat)
nvars <- ncol(dat)
dat <- dat[,c(nvars,1:(nvars-1))] #put ID first
write.csv(dat, 'OpenSci.csv', row.names = FALSE)

#exclude articles with no URL
no_link <- (dat$ArticleURL=='')
dat_links <- dat[!no_link,]

#select 1000 articles
set.seed(43278174)
samp <- sort(sample(1:nrow(dat_links),1000,replace = FALSE))
dat_samp <- dat_links[samp,]
dat_samp <- dat_samp[,c('ID','ArticleURL')]
write.csv(dat_samp, 'OpenSci_samp.csv', row.names = FALSE)

### Reproducibility Dataset

#read data and assign ID
dat <- read.csv('Reproducibility.csv')
dat$ID <- 1:nrow(dat)
nvars <- ncol(dat)
dat <- dat[,c(nvars,1:(nvars-1))] #put ID first
write.csv(dat, 'Reproducibility.csv', row.names = FALSE)

#exclude articles with no URL
no_link <- (dat$ArticleURL=='')
dat_links <- dat[!no_link,]

#select 1000 articles
set.seed(4752389)
samp <- sort(sample(1:nrow(dat_links),1000,replace = FALSE))
dat_samp <- dat_links[samp,]
dat_samp <- dat_samp[,c('ID','ArticleURL')]
write.csv(dat_samp, 'Reproducibility_samp.csv', row.names = FALSE)

#########################################################
### Match author names across articles for network analysis
#########################################################

# 1. Reshape dataset to have authors in rows rather than articles

dat_gender_open <- read.csv('OpenSci-gender.csv')
dat_gender_repr <- read.csv('Reproducibility.csv')
dat_gender_open$field <- "Open Science"
dat_gender_repr$field <- "Reproducibility"
dat_gender_open$ID <- paste0('O',1:nrow(dat_gender_open))
dat_gender_repr$ID <- paste0('R',1:nrow(dat_gender_repr))

dat_gender <- data.frame(Field = c(dat_gender_open$field, dat_gender_repr$field),
                         Authors = c(as.character(dat_gender_open$Authors), as.character(dat_gender_repr$Authors)),
                         ID = c(dat_gender_open$ID, dat_gender_repr$ID))

dat_gender$Authors <- as.character(dat_gender$Authors)
authors_split <- strsplit(dat_gender$Authors,', ')
  
N <- nrow(dat_gender)
dat_authors <- NULL
for(i in 1:N){
  print(i)
  ID_i <- dat_gender$ID[i]
  authors_i <- authors_split[[i]]
  authors_i_lastname <- gsub('.+ ','',authors_i) #last name = everything after last space
  nchar_full <- nchar(authors_i)
  nchar_last <- nchar(authors_i_lastname)
  authors_i_firstmiddle <- substr(authors_i, 1, (nchar_full-nchar_last-1))
  
  # grab gender fields
  num_authors <- length(authors_i)
  # if(!all.equal(num_authors, dat_gender$authorCount[i])) print('mismatch author count')
  # gender_i <- rep(NA, num_authors)
  # if(num_authors >= 5) gender_i[1:5] <- t(dat_gender[i,gender_cols])
  # if(num_authors > 5) gender_i[num_authors] <- dat_gender$last[i]
  # if(num_authors < 5) gender_i[1:num_authors] <- t(dat_gender[i,gender_cols[1:num_authors]])
  
  dat_authors_i <- data.frame(ArticleID = ID_i, LastName = authors_i_lastname, FirstMiddle = authors_i_firstmiddle, Order = 1:num_authors, Total = num_authors)#, Female = gender_i)
  dat_authors <- rbind(dat_authors, dat_authors_i)
}

dat_authors$LastName <- as.character(dat_authors$LastName)
dat_authors$FirstMiddle <- as.character(dat_authors$FirstMiddle)

write.csv(dat_authors, file = 'Authors.csv', row.names = FALSE)


# 2. Match authors 

is.initial <- function(name){ return(nchar(name)==1) }
get.initial <- function(name) { return(substr(name, 1, 1)) }

dat_authors$ID <- NA
lastnames <- c()
firstnames <- c()
M <- nrow(dat_authors)
IDcounter <- 1
for(i in 25056:M){
  print(i)
  lastname_i <- dat_authors$LastName[i]
  firstmiddle_i <- dat_authors$FirstMiddle[i]
  first_i <- gsub(' .+','',firstmiddle_i) #drop middle
  if(substr(lastname_i,1,1)=='?') next()
  if(!(lastname_i %in% lastnames)){
    
    # NO LAST NAME MATCH
    new <- TRUE

  } else {
    
    # LAST NAME MATCHES, CONSIDER FIRST NAMES
    match_last <- which(lastnames==lastname_i)
    candidates <- firstnames[match_last]
    candidates2 <- gsub(' .+','',candidates) #drop middle
    
    if(first_i %in% candidates2){
      
      # FIRST NAME MATCHES
      match_ind <- match_last[which(candidates2==first_i)]
      new <- FALSE
      
    } else {
      
      # FIRST NAME DOESN'T MATCH EXACTLY, CONSIDER INITIALS
      if(is.initial(first_i)){ 
        
        # FIRST NAME IS INITIAL, CHECK FOR MATCH
        match_init <- which(first_i == get.initial(candidates2))
        
      } else {
          
        # FIRST NAME IS NOT INITIAL, CHECK FOR MATCH
        candidates_init <- ifelse(is.initial(candidates2),candidates2,NA)
        match_init <- which(get.initial(first_i) == candidates_init)
          
      }
      
      if(length(match_init) > 1) { new <- TRUE } #this only happens a few times
      if(length(match_init) == 1) { new <- FALSE; match_ind <- match_last[match_init] }
      if(length(match_init) == 0) { new <- TRUE }
      
    }
  }
      
  if(new){    
    lastnames <- c(lastnames, lastname_i)
    firstnames <- c(firstnames, firstmiddle_i)
    dat_authors$ID[i] <- IDcounter
    IDcounter <- IDcounter + 1
  } else {
    dat_authors$ID[i] <- match_ind
    if(nchar(firstmiddle_i) > nchar(firstnames[match_ind])) firstnames[match_ind] <- firstmiddle_i
  }
  new <- NULL
}


# visual inspection of results
tab <- table(dat_authors$ID)
tab <- tab[tab > 1]
for(i in 1:length(tab)){
  ID_i <- names(tab)[i]
  print(dat_authors[dat_authors$ID==ID_i,])
  Sys.sleep(1)
}

# corrections
dat_authors$ID[dat_authors$LastName=='Kelly' & dat_authors$FirstMiddle=='Ann H'] <- IDcounter
dat_authors$ID[dat_authors$LastName=='Williams' & dat_authors$FirstMiddle=='Jason'] <- IDcounter + 1
lastnames <- c(lastnames, c('Kelly','Williams'))
firstnames <- c(firstnames, c('Ann H','Jason'))

write.csv(dat_authors, file = 'Authors.csv', row.names = FALSE)
save(lastnames, firstnames, file = 'Authors.RData')

#########################################################      
#########################################################
### Female participation analysis
#########################################################
#########################################################

dat <- read.csv('OpenSci2-update.csv', stringsAsFactors = FALSE)

#Exclude General Science
dat <- filter(dat, dataset != 0)
dat$dataset <- factor(dat$dataset, levels=c(2,1), labels=c('Open Science','Reproducibility'))
dat %>% group_by(dataset) %>% summarise(count=n())

#Include only 2010-2017
dat <- filter(dat, Year >= 2010, Year <= 2017)
dat %>% group_by(dataset) %>% summarise(count=n())

#Female First or Last Author
dat$femaleLead <- (dat$X1st==1) | (dat$last==1)
table(dat$femaleLead, dat$dataset)
dat %>% group_by(dataset) %>% summarise(count=sum(!is.na(femaleLead)))
dat %>% filter(authorCount > 1) %>% group_by(dataset) %>% summarise(count_known=sum(!is.na(femaleLead)), 
                                                                    pct_fem = sum(femaleLead, na.rm=TRUE)/count_known, 
                                                                    pct_fem2 = sum(femaleLead, na.rm=TRUE)/n())

dat %>% filter(authorCount==1) %>% group_by(dataset) %>% summarise(count=n())
dat %>% filter(authorCount==1) %>% group_by(dataset, X1st) %>% summarise(count=n())
dat %>% filter(authorCount > 1) %>% group_by(dataset) %>% summarise(count=n())

#sample size for regression
dat %>% filter(authorCount > 1, authorCount <= 15, !is.na(femaleLead)) %>% group_by(dataset) %>% summarise(count=n())


#Test for differences in rates of female vs. male authorship in single-author papers
tmp <- dplyr::filter(dat, authorCount==1)
tab <- table(tmp$femaleLead, tmp$dataset, useNA = 'a')
tab <- tab[,1:2]
rownames(tab) <- c("Male","Female","Unknown")
tab
round(tab/matrix(colSums(tab), nrow=3,ncol=2,byrow=TRUE),3)

### COMPARE RATES OF WOMEN IN SINGLE-AUTHOR PAPERS

# % of single-author papers written by women, among those with known gender
n_OS <- (tab[2,1]+tab[1,1])
n_RR <- (tab[2,2]+tab[1,2])
n_fem_OS <- tab[2,1]
n_fem_RR <- tab[2,2]
pct_fem_OS <- n_fem_OS/n_OS #34% 
pct_fem_RR <- n_fem_RR/n_RR #32%

#H0: p = 0.5

#Normal approximation
t_OS <- (pct_fem_OS-0.5)/sqrt(0.5*0.5/n_OS)
t_RR <- (pct_fem_RR-0.5)/sqrt(0.5*0.5/n_RR)
pnorm(t_OS)
pnorm(t_RR)

#Exact test
pbinom(n_fem_OS, size = n_OS, prob = 0.5)
pbinom(n_fem_RR, size = n_RR, prob = 0.5)

#H0: p_OS = p_RR
pct_fem = (n_fem_OS+n_fem_RR)/(n_OS+n_RR)
t_compare <- (pct_fem_OS - pct_fem_RR)/sqrt(pct_fem*(1-pct_fem)*(1/n_OS + 1/n_RR))
pnorm(t_compare, lower.tail = FALSE) #0.3, not significant

#########################################################
# Exploratory Analysis
#########################################################

### Histogram over Time

png('TimeHist.png', width=400, height=400)
hist(dat$Year[dat$dataset=='Reproducibility'], col=rgb(0,0,1,0.5), breaks=1999:2018, xlab='Year', ylim=c(0,500), xlim=c(2000,2018), main='Number of Papers over Time')
hist(dat$Year[dat$dataset=='Open Science'], col=rgb(1,0,0,0.8), breaks=1999:2018, xlab='Year', add=TRUE)
legend('topleft', legend=c('Reproducibility', 'Open Science'), fill=c(rgb(0,0,1,0.5), rgb(1,0,0,0.5)))
dev.off()

### Histogram of Team Size

png('TeamSizeHist.png', width=400, height=400)
hist(dat$authorCount[dat$dataset=='Reproducibility'], col=rgb(0,0,1,0.5), breaks=seq(0,50,5), xlab='Number of Authors', main='Number of Papers by Team Size')
hist(dat$authorCount[dat$dataset=='Open Science'], col=rgb(1,0,0,0.8), breaks=seq(0,50,5), add=TRUE)
legend('topright', legend=c('Reproducibility', 'Open Science'), fill=c(rgb(0,0,1,0.5), rgb(1,0,0,0.5)))
dev.off()


### Team Size Distribution (**** FIGURE 4 ****)

pdf('TeamSizeHist_fem.pdf', width=8, height=5) 

par(mfrow=c(1,2))
angle <- c(45,-45)

#Open Science
datOS <- filter(dat, dataset=='Open Science', authorCount <= 15, authorCount > 1, !is.na(femaleLead))
hist(datOS$authorCount[datOS$femaleLead==TRUE], border='black', breaks=seq(1,20,1), main='Open Science', xlab='Team Size', ylim=c(0,200), xlim=c(0,20))
hist(datOS$authorCount[datOS$femaleLead==FALSE], border='gray', breaks=seq(1,20,1), add=TRUE)
legend('topright', legend=c('Female with High Status', 'No Female with High Status'), fill='white', border=c('black','gray'))
# hist(datOS$authorCount[datOS$femaleLead==TRUE], angle=angle[1], density=30, col='magenta', breaks=seq(1,20,1), main='Open Science', xlab='Team Size', ylim=c(0,200), xlim=c(0,20))
# hist(datOS$authorCount[datOS$femaleLead==FALSE], angle=angle[2], density=30, col='blue', breaks=seq(1,20,1), add=TRUE)
# legend('topright', legend=c('Female with High Status', 'No Female with High Status'), fill=c('magenta','blue'),angle=angle,density=30)

#Reproducibility
datRP <- filter(dat, dataset=='Reproducibility', authorCount <= 15, authorCount > 1, !is.na(femaleLead))
hist(datRP$authorCount[datRP$femaleLead==TRUE], border='black', breaks=seq(1,20,1), main='Reproducibility', xlab='Team Size', ylim=c(0,200), xlim=c(0,20))
hist(datRP$authorCount[datRP$femaleLead==FALSE], border='gray', breaks=seq(1,20,1), add=TRUE)
legend('topright', legend=c('Female with High Status', 'No Female with High Status'), fill='white', border=c('black','gray'))
# hist(datRP$authorCount[datRP$femaleLead==TRUE], angle=angle[1], density=30, col='magenta', breaks=seq(1,20,1), main='Reproducibility', xlab='Team Size', ylim=c(0,200), xlim=c(0,20))
# hist(datRP$authorCount[datRP$femaleLead==FALSE], angle=angle[2], density=30, col='blue', breaks=seq(1,20,1), add=TRUE)
# legend('topright', legend=c('Female with High Status', 'No Female with High Status'), fill=c('magenta','blue'),angle=angle,density=30)

dev.off()


pdf('TeamSizeLine.pdf', width=8, height=6)

par(mfrow=c(1,2))

#years 2010-2017
dat2 <- filter(dat, authorCount > 1 & authorCount <= 10, Year >= 2010, Year < 2018)
dat_summ <- dat2 %>%
  filter(!is.na(femaleLead)) %>%
  group_by(dataset, authorCount) %>% 
  summarise(count_fem = sum(femaleLead), 
            count = n(), 
            pct_fem = count_fem/count)

dat_summ <- dplyr::mutate(dat_summ, LB = pct_fem - 1.96*sqrt(pct_fem*(1-pct_fem)/count),
                   UB = pct_fem + 1.96*sqrt(pct_fem*(1-pct_fem)/count))
dat_summ_RR <- filter(dat_summ, dataset=='Reproducibility')
dat_summ_OS <- filter(dat_summ, dataset=='Open Science')

plot(dat_summ_RR$authorCount, dat_summ_RR$pct_fem, type='l',col='gray',lwd=3, 
     ylab='Proportion of Papers', xlab='Number of Authors', ylim=c(0,0.8), 
     main='Proportion of Papers with\nFemales in High Status Positions\n(2010-2017)', xlim=c(1,10))
points(dat_summ_RR$authorCount, dat_summ_RR$pct_fem, col='gray',pch=19,cex=2)
lines(dat_summ_OS$authorCount, dat_summ_OS$pct_fem, col='turquoise',lwd=3)
points(dat_summ_OS$authorCount, dat_summ_OS$pct_fem, col='turquoise',pch=19,cex=2)
legend('topright',legend=c('Open Science','Reproducibility'), lwd=2, pch=19,col=c('turquoise','gray'))

#add histograms
angle <- c(45,-45)
hist(dat2$authorCount[dat2$dataset=='Reproducibility'], freq=FALSE, add=TRUE, angle=angle[1], density=30, col='gray', breaks=seq(1,10,1),border='black')
hist(dat2$authorCount[dat2$dataset=='Open Science'], freq=FALSE, add=TRUE, angle=angle[2], density=20, col='turquoise', breaks=seq(1,10,1),border='black')

#year 2017
dat2 <- filter(dat, authorCount > 1 & authorCount <= 10, Year == 2017)
dat_summ <- dat2 %>%
  filter(!is.na(femaleLead)) %>%
  group_by(dataset, authorCount) %>% 
  summarise(count_fem = sum(femaleLead), 
            count = n(), 
            pct_fem = count_fem/count)

dat_summ <- dplyr::mutate(dat_summ, LB = pct_fem - 1.96*sqrt(pct_fem*(1-pct_fem)/count),
                          UB = pct_fem + 1.96*sqrt(pct_fem*(1-pct_fem)/count))
dat_summ_RR <- filter(dat_summ, dataset=='Reproducibility')
dat_summ_OS <- filter(dat_summ, dataset=='Open Science')

plot(dat_summ_RR$authorCount, dat_summ_RR$pct_fem, type='l',col='gray',lwd=3, 
     ylab='Proportion of Papers', xlab='Number of Authors', ylim=c(0,0.8), 
     main='Proportion of Papers with\nFemales in High Status Positions\n(2017)', xlim=c(1,10))
points(dat_summ_RR$authorCount, dat_summ_RR$pct_fem, col='gray',pch=19,cex=2)
lines(dat_summ_OS$authorCount, dat_summ_OS$pct_fem, col='turquoise',lwd=3)
points(dat_summ_OS$authorCount, dat_summ_OS$pct_fem, col='turquoise',pch=19,cex=2)
legend('topright',legend=c('Open Science','Reproducibility'), lwd=2, pch=19,col=c('turquoise','gray'))

#add histograms
angle <- c(45,-45)
hist(dat2$authorCount[dat2$dataset=='Reproducibility'], freq=FALSE, add=TRUE, angle=angle[1], density=30, col='gray', breaks=seq(1,10,1),border='black')
hist(dat2$authorCount[dat2$dataset=='Open Science'], freq=FALSE, add=TRUE, angle=angle[2], density=20, col='turquoise', breaks=seq(1,10,1),border='black')

dev.off()


# hist(dat2$authorCount[dat2$dataset=='Open Science'], col=rgb(0,0,0,0.2), breaks=seq(0,20,2), freq=FALSE, main="Distribution of Team Size", xlab='Team Size')
# hist(dat2$authorCount[dat2$dataset=='Reproducibility'], col=rgb(0,0,1,0.2), breaks=seq(0,20,2), freq=FALSE, add=TRUE)
# ggplot(dat2, aes(x=authorCount)) + geom_density(aes(group=dataset, fill=dataset), alpha=0.2) + theme_bw() + xlab('Team Size')
# ggplot(dat2, aes(y=authorCount, x=dataset)) + geom_violin(aes(group=dataset, fill=dataset)) + theme_bw() + ylab('Team Size')


# ggplot(dat2, aes(x=femalePct)) + geom_density(aes(group=dataset, fill=dataset), alpha=0.2) + theme_bw() + xlab('Percent Female')
# ggplot(dat2, aes(x=authorCount, y=femaleLast, group=dataset, color=dataset)) + geom_smooth()

### Time Trends

#multi-author papers
dat_summ1 <- dat %>% filter(Year >= 2010, Year < 2018, authorCount <= 15, authorCount > 1) %>% 
  group_by(dataset, Year) %>% 
  summarize(count=sum(!is.na(femaleLead)), pctFemaleLead = mean(femaleLead, na.rm=TRUE), logitFemaleLead = log(pctFemaleLead/(1-pctFemaleLead)))
p1 <- ggplot(dat_summ1, aes(x=Year, y=logitFemaleLead, color=dataset, group=dataset)) + 
  geom_point() + geom_smooth() + ylab('Logit of Pr(Female Lead)') +
  ggtitle('Multi-Author Papers')
#single-author papers
dat_summ2 <- dat %>% filter(Year >= 2010, Year < 2018, authorCount == 1) %>% 
  group_by(dataset, Year) %>% 
  summarize(count=sum(!is.na(femaleLead)), pctFemaleLead = mean(femaleLead, na.rm=TRUE), logitFemaleLead = log(pctFemaleLead/(1-pctFemaleLead)))
p2 <- ggplot(dat_summ2, aes(x=Year, y=logitFemaleLead, color=dataset, group=dataset)) + 
  geom_point() + geom_smooth() + ylab('Logit of Pr(Female Lead)') +
  ggtitle('Single-Author Papers')
grid.arrange(p1, p2, nrow=1)

### Effect of Team Size

dat_summ <- dat %>% 
  filter(Year >= 2010, Year < 2018, authorCount <= 15, authorCount > 1) %>% 
  group_by(dataset, authorCount) %>% summarize(count=sum(!is.na(femaleLead)), pctFemaleLead = mean(femaleLead, na.rm=TRUE), logitFemaleLead = log(pctFemaleLead/(1-pctFemaleLead)))
ggplot(dat_summ, aes(x=authorCount, y=logitFemaleLead, color=dataset, group=dataset)) + 
  geom_point() + geom_smooth() + ylab('Logit of Pr(Female Lead)') +
  ggtitle('Proportion of Papers with Female Lead Authors by Team Size and Field')


#########################################################
#Models for Female Lead Authorship
#########################################################

dat3 <- filter(dat, authorCount <= 15)
dat3 %>% group_by(dataset) %>% summarise(count=sum(!is.na(femaleLead)))
dat3$singleAuthor <- (dat3$authorCount == 1)
dat3$authorCount_m2 <- (dat3$authorCount-2)
dat3$authorCount_m2_sq <- (dat3$authorCount_m2)^2
dat3$Year2 <- (dat3$Year - 2017)

### Model 1: All multi-author papers

dat3a <- filter(dat3, !singleAuthor, !is.na(femaleLead))
dat3a$Conference <- (dat3a$Type == 'Conference')
dat3a %>% group_by(dataset) %>% summarise(count=n())
fit3a <- glm(formula = femaleLead*1 ~ dataset*(Year2 + authorCount_m2 + authorCount_m2_sq) + Conference, family = 'binomial', data=dat3a)
summary(fit3a)

#reformulate to get RR time slope directly
dat3_alt <- dat3a
dat3_alt$dataset <- factor(dat3_alt$dataset, levels=c('Reproducibility','Open Science'))
fit3a_alt <- glm(formula = femaleLead*1 ~ dataset*(Year2 + authorCount_m2 + authorCount_m2_sq) + Conference, family = 'binomial', data=dat3_alt)
summary(fit3a_alt)

CI <- as.data.frame(exp(confint(fit3a_alt)))
names(CI) <- c('LB','UB')
CI$est <- exp(coefficients(fit3a_alt))
CI

# Plot coefficients and CIs

variables <- c('Intercept','Reproducibility','YearOfPublication', 
               'TeamSize','TeamSizeSquared','ConferencePaper',
               'Reproducibility X YearOfPublication','Reproducibility X TeamSize',
               'Reproducibility X TeamSizeSquared')

CI <- as.data.frame(exp(confint(fit3a)))
names(CI) <- c('LB','UB')
CI$est <- exp(coefficients(fit3a))
CI$var <- variables
row.names(CI) <- NULL

exp(CI[3,]+CI[7,])

CI <- CI[,c('var','est','LB','UB')]
CI[,2:4] <- round(CI[,2:4],3)
write.csv(CI, file='model1_estimates.csv', row.names=FALSE)

# ggplot(CI, aes(y=var)) + geom_point(aes(x=est)) + 
#   geom_errorbarh(aes(x=est, xmin=LB, xmax=UB), height=0) +
#   geom_vline(xintercept = 1, col='red',linetype=2) + 
#   xlab('Multiplicative Effect on Odds of Female in High-Status Position')



# Plot predictions

dat.pred3a <- expand.grid(dataset=c('Reproducibility','Open Science'),
                          Conference = c(FALSE,TRUE),
                          Year2 = (2010:2017) - 2017,
                          authorCount_m2 = (2:15)-2)
dat.pred3a$authorCount_m2_sq <- (dat.pred3a$authorCount_m2)^2
dat.pred3a$authorCount <- dat.pred3a$authorCount_m2 + 2
pred3a <- predict(fit3a, newdata=dat.pred3a, type = "link", se.fit = TRUE)
dat.pred3a$predict <- pred3a$fit
dat.pred3a$SE <- pred3a$se.fit
dat.pred3a$LB <- (dat.pred3a$predict - 1.96*dat.pred3a$SE)
dat.pred3a$UB <- (dat.pred3a$predict + 1.96*dat.pred3a$SE)
dat.pred3a$predict_p <- logodds_to_prob(dat.pred3a$predict)
dat.pred3a$LB_p <- logodds_to_prob(dat.pred3a$LB)
dat.pred3a$UB_p <- logodds_to_prob(dat.pred3a$UB)

logodds_to_prob <- function(logodds){ 
  odds <- exp(logodds)
  return(odds/(1 + odds))
}


#Team Size
pdf('PredictedProbs_multiauthor.pdf', width=8, height=5)
p1 <- ggplot(filter(dat.pred3a, !Conference, Year2==0), aes(x=authorCount, group=dataset)) + 
  geom_ribbon(aes(ymin=LB_p, ymax=UB_p), alpha=0.2) + 
  geom_line(aes(y=predict_p, color=dataset), size=2) + 
  ylim(0.3,0.8) + xlim(2,10) + theme_bw() + theme(legend.position='bottom', panel.grid=element_blank()) +
  scale_color_manual(name=NULL, values=c('darkgray','turquoise')) +
  xlab('Number of Authors') + ylab('Probability of Female in a High Status Position') +
  ggtitle('Journal Articles, Year = 2017')
avg_team_size <- mean(dat3$authorCount)
p2 <- ggplot(filter(dat.pred3a, !Conference, authorCount == 4), aes(x=Year2+2017, group=interaction(dataset,authorCount))) + 
  geom_ribbon(aes(ymin=LB_p, ymax=UB_p), alpha=0.2) + 
  geom_line(aes(y=predict_p, color=dataset), size=2) + 
  ylim(0.3,0.8) + theme_bw() + theme(legend.position='bottom', panel.grid=element_blank()) +
  scale_x_continuous(breaks=2010:2017) +
  scale_color_manual(name=NULL, values=c('darkgray','turquoise')) +
  xlab('Year of Publication') + ylab('Probability of Female in a High Status Position') +
  ggtitle('Journal Articles, Average Team Size')
grid.arrange(p1, p2, nrow=1)
dev.off()


#Time
pdf('PredictedProbs_multiauthor_year.pdf', width=6, height=5)
dev.off()

### Model 2: All single-author papers

dat3b <- filter(dat3, singleAuthor, !is.na(femaleLead))
dat3b %>% group_by(dataset, femaleLead) %>% summarise(count=n())
fit3b <- glm(formula = femaleLead*1 ~ dataset*Year2 + Type, family = 'binomial', data=dat3b)
summary(fit3b)

dat.pred3a <- expand.grid(dataset=c('Reproducibility','Open Science'),
                          Type = c('Journal','Conference'),
                          Year2 = (2010:2017) - 2017,
                          authorCount = 2:15)
dat.pred3a$authorCount2 <- (dat.pred3a$authorCount)^2
pred3a <- predict(fit3a, newdata=dat.pred3a, type = "link", se.fit = TRUE)
dat.pred3a$predict <- pred3a$fit
dat.pred3a$SE <- pred3a$se.fit
dat.pred3a$LB <- (dat.pred3a$predict - 1.96*dat.pred3a$SE)
dat.pred3a$UB <- (dat.pred3a$predict + 1.96*dat.pred3a$SE)
dat.pred3a$predict_p <- logodds_to_prob(dat.pred3a$predict)
dat.pred3a$LB_p <- logodds_to_prob(dat.pred3a$LB)
dat.pred3a$UB_p <- logodds_to_prob(dat.pred3a$UB)

logodds_to_prob <- function(logodds){ 
  odds <- exp(logodds)
  return(odds/(1 + odds))
}


#Team Size
pdf('PredictedProbs_multiauthor_teamsize.pdf', width=6, height=5)
ggplot(filter(dat.pred3a, Type=='Journal', Year2==0), aes(x=authorCount, group=dataset)) + 
  geom_ribbon(aes(ymin=LB_p, ymax=UB_p), alpha=0.2) + 
  geom_line(aes(y=predict_p, color=dataset), size=2) + 
  ylim(0.3,0.8) + xlim(2,10) + theme_bw() + theme(legend.position='bottom', panel.grid=element_blank()) +
  scale_color_manual(name=NULL, values=c('darkgray','turquoise')) +
  xlab('Number of Authors') + ylab('Probability of Female in a High Status Position') +
  ggtitle('Predicted Probabilities for Journal Articles in 2017')
dev.off()


#Time
pdf('PredictedProbs_multiauthor_year.pdf', width=6, height=5)
ggplot(filter(dat.pred3a, Type=='Journal', authorCount == 2), aes(x=Year2+2017, group=interaction(dataset,authorCount))) + 
  geom_ribbon(aes(ymin=LB_p, ymax=UB_p), alpha=0.2) + 
  geom_line(aes(y=predict_p, color=dataset, linetype=factor(authorCount)), size=2) + 
  ylim(0.3,0.8) + theme_bw() + theme(legend.position='bottom', panel.grid=element_blank()) +
  scale_x_continuous(breaks=2010:2017) +
  scale_color_manual(name=NULL, values=c('darkgray','turquoise')) +
  xlab('Year of Publication') + ylab('Probability of Female in a High Status Position') +
  ggtitle('Predicted Probabilities for Journal Articles with Two Authors')
dev.off()


#########################################################
#########################################################
### Semantic analysis
#########################################################
#########################################################

#Exclude non-English titles
dat$language <- textcat(as.character(dat$Title))
dat <- filter(dat, language=='english')

#########################################################
### SENTIMENT
#########################################################

MTurk_OS <- read.csv('Mturk Results/BATCH 2 Batch_3127646_batch_results.csv', stringsAsFactors=FALSE)
sentiment_OS <- read.csv('Mturk Results/BATCH 2 as.csv', stringsAsFactors=FALSE)
sentiment_OS <- cbind(MTurk_OS, sentiment_OS)
sentiment_OS$dataset <- 'Open Science'

MTurk_RP <- read.csv('Mturk Results/Batch 3 Batch_3127939_batch_results.csv', stringsAsFactors=FALSE)
MTurk_RP$Answer.doNotRedirect <- NULL
sentiment_RP <- read.csv('Mturk Results/as_batch_3.csv', stringsAsFactors=FALSE)
sentiment_RP <- cbind(MTurk_RP, sentiment_RP)
sentiment_RP$dataset <- 'Reproducibility'

sentiment <- rbind(sentiment_RP, sentiment_OS)

#remove answers like "NA" or "no abstract available
sentiment$nchar <- nchar(sentiment$Answer.Abstract)
sentiment <- arrange(sentiment, nchar)
sentiment$Answer.Abstract[sentiment$nchar < 62] <- NA
sentiment <- filter(sentiment, !is.na(Answer.Abstract)) #2000 to 1465 rows

#merge with main dataset using URL
names(sentiment)[names(sentiment)=='Input.ArticleURL'] <- 'ArticleURL'
dat <- filter(dat, ArticleURL != '')
dat_sentiment <- merge(dat, sentiment, by='ArticleURL') #1027 rows

#check for ID match
ID1 <- dat_sentiment$id_old
ID2 <- paste0(substr(dat_sentiment$dataset.y,1,1),dat_sentiment$Input.ID)
rows.rm <- which(ID1 != ID2) #remove 5 rows
dat_sentiment <- dat_sentiment[-rows.rm,]

#remove duplicated titles
titles <- tolower(as.character(dat_sentiment$Title.x))
rows.rm <- which(duplicated(titles)) #remove 1 row
dat_sentiment <- dat_sentiment[-rows.rm,]

#identify language
dat_sentiment$language <- textcat(as.character(dat_sentiment$Title.x))
dat_sentiment <- filter(dat_sentiment, language=='english') #1021 to 939 rows

write.csv(dat_sentiment, 'Sentiment.csv', row.names = FALSE)

#look at boxplots and histograms of sentiment by field

dat_sentiment$Female_Field <- paste(dat_sentiment$dataset.y, dat_sentiment$femaleLead)
boxplot(PositivityQDAP ~ Female_Field, data = filter(dat_sentiment, !is.na(femaleLead)), 
        xlab = 'Field, Female Lead', col=c('lightblue','pink'), ylab='Positivity QDAP')

### QDAP

par(mfrow=c(1,2))

#Sentiment
tmp <- filter(dat_sentiment, SentimentQDAP >= -0.2)
hist(tmp$SentimentQDAP[tmp$dataset.y=='Open Science'], breaks=seq(-0.2,0.5,0.03), col=rgb(0,1,1,0.5), main='Sentiment', xlab=NULL, ylim=c(0,150))
hist(tmp$SentimentQDAP[tmp$dataset.y=='Reproducibility'], breaks=seq(-0.2,0.5,0.03), col=rgb(1,0,1,0.5), add=TRUE)
legend('topright', legend=c('Open Science', 'Reproducibility'), fill=c(rgb(0,1,1,0.5), rgb(1,0,1,0.5)))

#Positivity
hist(dat_sentiment$PositivityQDAP[dat_sentiment$dataset.y=='Open Science'], breaks=seq(-0.2,0.5,0.03), col=rgb(0,1,1,0.5), main='Positivity', xlab=NULL, ylim=c(0,150))
hist(dat_sentiment$PositivityQDAP[dat_sentiment$dataset.y=='Reproducibility'], breaks=seq(-0.2,0.5,0.03), col=rgb(1,0,1,0.5), add=TRUE)
legend('topright', legend=c('Open Science', 'Reproducibility'), fill=c(rgb(0,1,1,0.5), rgb(1,0,1,0.5)))


### GI

par(mfrow=c(1,2))

#Sentiment
tmp <- filter(dat_sentiment, SentimentGI >= -0.2)
hist(tmp$SentimentGI[tmp$dataset.y=='Open Science'], breaks=seq(-0.2,0.5,0.03), col=rgb(0,1,1,0.5), main='Sentiment', xlab=NULL, ylim=c(0,120))
hist(tmp$SentimentGI[tmp$dataset.y=='Reproducibility'], breaks=seq(-0.2,0.5,0.03), col=rgb(1,0,1,0.5), add=TRUE)
legend('topright', legend=c('Open Science', 'Reproducibility'), fill=c(rgb(0,1,1,0.5), rgb(1,0,1,0.5)))

#Positivity
hist(dat_sentiment$PositivityGI[dat_sentiment$dataset.y=='Open Science'], breaks=seq(-0.2,0.5,0.03), col=rgb(0,1,1,0.5), main='Positivity', xlab=NULL, ylim=c(0,120))
hist(dat_sentiment$PositivityGI[dat_sentiment$dataset.y=='Reproducibility'], breaks=seq(-0.2,0.5,0.03), col=rgb(1,0,1,0.5), add=TRUE)
legend('topright', legend=c('Open Science', 'Reproducibility'), fill=c(rgb(0,1,1,0.5), rgb(1,0,1,0.5)))


### Team Size

ggplot(dat_sentiment, aes(x=authorCount, y=SentimentQDAP, color=dataset.y, group=dataset.y)) + geom_point() + geom_smooth() + xlim(0,10)

#########################################################
### CUSTOM DICTIONARIES
#########################################################

library(reshape2)

dictionary <- read.csv('Lancet Dictionaries.csv', stringsAsFactors = FALSE)
dictionary <- dictionary[,1:2]
constructs <- unique(dictionary$IndivConstruct)
composites <- unique(dictionary$PotentialComposite)
dat_dict <- read.csv('full_custom_results.csv', stringsAsFactors = FALSE)
names(dat_dict) %in% constructs
names(dat_dict)[2] <- "ArticleURL"
names(dat_dict)[4] <- "Competiition/Achievement"
names(dat_dict)[5] <- "Feminine words"
names(dat_dict)[6] <- "Individual-focused Pronouns"
names(dat_dict)[10] <- "Masculine words"
names(dat_dict)[12] <- "Other-focused pronouns"
names(dat_dict)[14] <- "Prosocial Motives"
names(dat_dict)[16] <- "Team-focused Pronouns"
names(dat_dict)[17] <- "Team/Communality"

TeamScienceConstructs <- unique(dictionary$IndivConstruct[dictionary$PotentialComposite=='Team Science'])
MasculinityConstructs <- unique(dictionary$IndivConstruct[dictionary$PotentialComposite=='Masculinity'])
FemininityConstructs <- unique(dictionary$IndivConstruct[dictionary$PotentialComposite=='Femininity'])

dat_dict$TeamScienceComposite <- rowSums(dat_dict[,TeamScienceConstructs])
dat_dict$MasculinityComposite <- rowSums(dat_dict[,MasculinityConstructs])

dat <- filter(dat, ArticleURL != '')
dat$ArticleURL <- as.character(dat$ArticleURL)
dat_dict <- merge(dat_dict, dat, by='ArticleURL')

#Exploratory Analysis

dat_dict2 <- filter(dat_dict, Year >= 2010, Year < 2018, authorCount <= 15)

hist(dat_dict2$'Competiition/Achievement')

pdf('Construct_boxplots.pdf', width=4, height=6)
for(c in constructs){
  print(c)
  dat_dict_c <- dat_dict[dat_dict[,c] > 0,]
  names(dat_dict_c)[names(dat_dict_c)==c] <- 'var'
  print(nrow(dat_dict_c))
  print(ggplot(dat_dict_c, aes(x=dataset, y=var, group=dataset)) + geom_boxplot() + ylab(c) + ggtitle(c)) 
}
dev.off()

# Histograms for Pro-Social Construct


dat_dict_OS <- filter(dat_dict, dataset=='Open Science')
dat_dict_RR <- filter(dat_dict, dataset=='Reproducibility')

pdf('ProSocialHist.pdf', width=6, height=5)
c <- "Prosocial Motives"
angle <- c(45,-45)
hist(dat_dict_RR[,c], border='gray', main='Distribution of Pro-Social Motives Construct Score', xlab='Pro-Social Motives Construct Score', breaks=seq(0,0.2,0.01))
hist(dat_dict_OS[,c], border='black', add=TRUE, breaks=seq(0,0.2,0.01))
legend('topright',legend=c('Open Science','Reproducibility'),fill='white',border=c('black','gray'))
# hist(dat_dict_RR[,c], angle=angle[2], density=30, col='gray', main='Distribution of Pro-Social Motives Construct Score', xlab='Pro-Social Motives Construct Score', breaks=seq(0,0.2,0.01))
# hist(dat_dict_OS[,c], angle=angle[1], density=30, col='turquoise', add=TRUE, breaks=seq(0,0.2,0.01))
# legend('topright',legend=c('Open Science','Reproducibility'),fill=c('turquoise','gray'),angle=angle,density=30)
dev.off()

# Test for Differences in Pro-Social Construct Score

mean_diff <- mean(dat_dict_OS$`Prosocial Motives`) - mean(dat_dict_RR$`Prosocial Motives`)
med_diff <- median(dat_dict_OS$`Prosocial Motives`) - median(dat_dict_RR$`Prosocial Motives`)

M <- 100000
labels_true <- dat_dict$dataset
n <- length(dat_dict$dataset)
mean_diff_null <- rep(NA, M)
med_diff_null <- rep(NA, M)
for(m in 1:M){
  labels_m <- sample(labels_true, n, replace=FALSE)
  dat_dict_OS_m <- dat_dict[labels_m=='Open Science',]
  dat_dict_RR_m <- dat_dict[labels_m=='Reproducibility',]
  mean_diff_null[m] <- mean(dat_dict_OS_m$`Prosocial Motives`) - mean(dat_dict_RR_m$`Prosocial Motives`)
  med_diff_null[m] <- median(dat_dict_OS_m$`Prosocial Motives`) - median(dat_dict_RR_m$`Prosocial Motives`)
}

mean(abs(mean_diff_null) > abs(mean_diff))
mean(abs(med_diff_null) > abs(med_diff))


# Test for Differences in Use of Any Pro-Social Words

#what percentage of papers use any pro-social words?
tab <- table(dat_dict[,c] > 0, dat_dict$dataset)
tab/matrix(colSums(tab), nrow=2, ncol=2, byrow=TRUE)

# % of single-author papers written by women, among those with known gender
n_OS <- (tab[2,1]+tab[1,1])
n_RR <- (tab[2,2]+tab[1,2])
n1_OS <- tab[2,1]
n1_RR <- tab[2,2]
pct_OS <- n1_OS/n_OS #69% 
pct_RR <- n1_RR/n_RR #43%

#H0: p_OS = p_RR
pct = (n1_OS+n1_RR)/(n_OS+n_RR)
t_compare <- (pct_OS - pct_RR)/sqrt(pct*(1-pct)*(1/n_OS + 1/n_RR))
2*pnorm(t_compare, lower.tail = FALSE) 


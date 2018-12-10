######### 1. load libraries ############

library(dplyr)
library(SentimentAnalysis)

######### 2. load the abstracts ########
data3 <- read.csv("~/Downloads/WomenInScience/Abstracts.csv", header = T, stringsAsFactors = F)
glimpse(data3) #Answer.Abstract has the text for the abstracts

######### 3. load the pro-social dictionary #############
custom <- read.csv("~/Downloads/WomenInScience/ProSocialDictionary.csv", header = T, stringsAsFactors = F)
glimpse(custom)
# how many dictionaries:
levels(as.factor(custom$IndivConstruct)) #15

######### 4. Measure pro-social in the abstracts 
levels <- levels(as.factor(custom$IndivConstruct))
full_custom_results <- data.frame(articleURL = data3$ArticleURL);
for (level in levels)
{
  X <- custom %>% 
    filter(IndivConstruct %in% level)
  # create the dictionary with the word list
  Collaboration <- SentimentDictionaryWordlist(X$Words)
  summary(Collaboration)
  # score the data
  custom_dic_results <- analyzeSentiment(data3$Answer.Abstract,
                                         rules=list(x=list(ruleRatio, Collaboration)))
  full_custom_results <- cbind(full_custom_results, custom_dic_results)
}
# fix the columns name
levels_col <- gsub(pattern = " ", replacement = "", x=levels)
colnames(full_custom_results)[2:16] <- levels_col

glimpse(full_custom_results)
write.csv(x = full_custom_results, file="Downloads/WomenInScience/Abstracts_ProSocialMeasure.csv")
# ProsocialMotives has the measure of prosocial for each abstract
# adjust as appropriate
setwd("/home/everyan/Dropbox/slides/NetSciSat/womenOpenSci")
library(dplyr)
library(tidyr)
library(gender)
library(stringr)

block = read.csv("OpenSci2.csv")
myvars <- c("Authors", "Title", "Year")
data <- block[myvars]
data$Year1 <-  data$Year-35 #25-45
data$Year2 <-  data$Year-45 #35-55
data$Year3 <-  data$Year-55 #45-65
data$authorCount <- str_count(data$Authors, ',')+1
data$ID <- seq.int(nrow(data))

AuthorTable <- data %>% separate(Authors, into = sprintf('%s.%s', rep('Author',50), rep(1:50)), sep = ",")
AuthorTable <- AuthorTable %>% gather(authorOrder, name, into = sprintf('%s.%s', rep('Author',50), rep(1:50)))

#AuthorTable <- data %>% separate(Authors, c("others", "lastAuthor"), "(?=[^,]+$)", extra = "merge")
AuthorList <- data.frame(lapply(AuthorTable, trimws), stringsAsFactors = FALSE)
#nameTable <- AuthorList %>% separate(lastAuthor, into = sprintf('%s.%s', rep('namePart',4), rep(1:4)), sep = " ")
nameTable <- AuthorList %>% separate(name, into = sprintf('%s.%s', rep('namePart',4), rep(1:4)), sep = " ")
newvars <- c("ID","namePart.1", "namePart.2", "namePart.3", "namePart.4", "Title", "Year2", "authorOrder", "authorCount")

#newvars <- c("ID","namePart.1", "namePart.2", "namePart.3", "namePart.4", "Title", "Year2", "authorCount")
nameParts <- nameTable[newvars]
nameParts$namePart.1 <- gsub("\"", "", nameParts$namePart.1)
nameParts$namePart.2 <- gsub("\"", "", nameParts$namePart.2)
nameParts$namePart.3 <- gsub("\"", "", nameParts$namePart.3)
nameParts$namePart.4 <- gsub("\"", "", nameParts$namePart.4)
nameParts$min_years <- as.numeric(nameParts$Year2)-20
nameParts$max_years <- as.numeric(nameParts$Year2)+20
results <- gender_df(nameParts, name_col = "namePart.1", year_col = c("min_years", "max_years"), method = "ssa")
output <- nameParts %>% left_join(results, by = c("namePart.1" = "name", "min_years" = "year_min"))
results <- gender_df(nameParts, name_col = "namePart.2", year_col = c("min_years", "max_years"), method = "ssa")
output <- output %>% left_join(results, by = c("namePart.2" = "name", "min_years" = "year_min"))
results <- gender_df(nameParts, name_col = "namePart.3", year_col = c("min_years", "max_years"), method = "ssa")
output <- output %>% left_join(results, by = c("namePart.3" = "name", "min_years" = "year_min"))
output <- within(output, #if namepart3 is the last name, set it NA 
   temp2 <- ifelse(is.na(namePart.4),NA,proportion_female)
)
output <- within(output, #if namepart2 is the last name, set it NA 
   temp1 <- ifelse(is.na(namePart.3),NA,proportion_female.y)
)
output <- within(output, #if namepart1 is the last name, set it NA 
   temp0 <- ifelse(is.na(namePart.2),NA,proportion_female.x)
)
output <- within(output, # namepart2 merged with namepart3
   femaleProb0 <- ifelse(is.na(temp1),temp2,temp1)
)
output <- within(output, # namepart1 merged with namepart2
   femaleProb <- ifelse(is.na(temp0),temp1,temp0)
)
#write.csv(output, file = "test.csv")

newvars <- c("ID","Title", "Year2", "authorOrder", "femaleProb", "authorCount")
merged <- output[newvars]
merged <- spread(merged, authorOrder, femaleProb) 
merged$detectCount <- 50-rowSums(is.na(merged))
merged$sum <- (rowSums(merged[sapply(merged, is.numeric)], na.rm = TRUE) - merged$detectCount)/merged$detectCount
merged$sum[is.infinite(merged$sum) | is.nan(merged$sum) ] <- NA
merged$authorCount <- as.numeric(merged$authorCount)
write.csv(merged, file = "test.csv")

block = read.csv("OpenSci.csv")
myvars <- c("id", "Title", "ArticleURL")
data <- block[myvars]
data$Title <- gsub("\"", "", data$Title)
data <- data.frame(lapply(data, trimws), stringsAsFactors = FALSE)
data$id <- sub("^", "O", data$id)

block = read.csv("reproducibility.csv")
myvars <- c("id", "Title", "ArticleURL")
data0 <- block[myvars]
data0$Title <- gsub("\"", "", data0$Title)
data0 <- data.frame(lapply(data0, trimws), stringsAsFactors = FALSE)
data0$id <- sub("^", "R", data0$id)

block = read.csv("OpenSci2full.csv")
myvars <- c("Authors", "Title", "ArticleURL")
data2 <- block[myvars]
data2$id_new <- seq.int(nrow(data2))
data2$Title <- gsub("\"", "", data2$Title)
data2 <- data.frame(lapply(data2, trimws), stringsAsFactors = FALSE)

data2 <- data2 %>% left_join(data, by = c("Title" = "Title"))
#data2 <- data2 %>% left_join(data0, by = c("Title" = "Title"))
data2 <- data2[row.names(unique(data2[,c("id_new", "Authors", "Title")])),]
write.csv(data2, file = "match.csv")





block = read.csv("Authors.csv")
myvars <- c("ArticleID", "LastName", "FirstMiddle", "Order")
dataAuthor <- block[myvars]
merged <- unite(dataAuthor, name, "LastName", "FirstMiddle", sep = " ") 
merged <- spread(merged, Order, name) 
merged <- unite(merged, authors, sprintf('%s', rep(1:50)), sep = "|") 
merged$authors <- gsub("\\|NA", "", merged$authors)
write.csv(merged, file = "network.csv")





block = read.csv("OpenSci2full.csv")
myvars <- c("Authors", "Title", "Year", "dataset")
#myvars <- c("Authors", "Title", "Year", "id_O", "id_R")
data <- block[myvars]
data$ID <- seq.int(nrow(data))
#data <- data[data$dataset == 1,]
data <- data[data$Year >=2010 & data$Year <=2017,]
 
AuthorTable <- data %>% separate(Authors, into = sprintf('%s.%s', rep('Author',50), rep(1:50)), sep = ",")
AuthorTable <- AuthorTable %>% gather(authorOrder, name, into = sprintf('%s.%s', rep('Author',50), rep(1:50)))
AuthorVector <- group_by(AuthorTable, name) 
#AuthorVector <- summarize(AuthorVector, id_O = sum(!is.na(id_O)), id_R = sum(!is.na(id_R)))
AuthorVector <- summarize(AuthorVector, IDlist = paste(ID, collapse="|"))
#AuthorVector$area <- as.numeric(AuthorVector$id_O>0) + 2*as.numeric(AuthorVector$id_R>0)
#AuthorVector <- data.frame(lapply(AuthorVector, trimws), stringsAsFactors = FALSE)
write.csv(AuthorVector, file = "network.csv")

block = read.csv("nodeList.csv")
myvars <- c("Id", "Label")
nodes <- block[myvars]
nodes <- nodes %>% left_join(AuthorVector, by = c("Label" = "name"))
write.csv(nodes, file = "network.csv")





block = read.csv("OpenSci2-update.csv")
myvars <- c("Authors", "Title", "Year", "dataset", "Type", "authorCount", "X1st", "last")
data <- block[myvars]
data <- data[data$authorCount == 1,]
data <- data[data$dataset == 1,]
data <- data[data$Year >=2010 & data$Year <=2017,]


data$allmale <- as.numeric(data$X1st==0) * as.numeric(data$last==0) * as.numeric(data$authorCount>1)
data$allfemale <- as.numeric(data$X1st==1) * as.numeric(data$last==1) * as.numeric(data$authorCount>1)
data$mixed <- as.numeric(data$X1st==1) + as.numeric(data$last==1)
data$mixed <- as.numeric(data$mixed==1)
data$allUnknown <- as.numeric(is.na(data$X1st)) * as.numeric(is.na(data$last)) * as.numeric(data$authorCount>1)
data$UnknownMale <- ifelse(is.na(data$X1st),as.numeric(data$last==0),0)
data$UnknownFemale <- ifelse(is.na(data$X1st),as.numeric(data$last==1),0)
data$maleUnknown <- ifelse(is.na(data$last),as.numeric(data$X1st==0),0)
data$femaleUnknown <- ifelse(is.na(data$last),as.numeric(data$X1st==1),0)
data$singleMale <- as.numeric(data$X1st==0) * as.numeric(data$authorCount==1)
data$singleFemale <- as.numeric(data$X1st==1) * as.numeric(data$authorCount==1)
data$singleUnknown <- is.na(data$X1st) * as.numeric(data$authorCount==1)
write.csv(data, file = "pie.csv")

#R code for running Fisher exact tests for edge density comparison 
table <- cbind(c(743,1136),c(1617*1616/2-743,2189*2188/2-1136))
fisher.test(table, alternative="greater")

table <- rbind(c(743,1136),c(1617*1616/2-743,2189*2188/2-1136))
fisher.test(table, alternative="greater")


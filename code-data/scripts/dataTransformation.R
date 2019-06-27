# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .R
#       format_name: light
#       format_version: '1.4'
#       jupytext_version: 1.1.1
#   kernelspec:
#     display_name: R
#     language: R
#     name: ir
# ---
library("checkpoint")
checkpoint("2019-04-23")
library(dplyr)
library(tidyr)
library(stringr)
#install.packages('widyr',lib=.libPaths()[3])
library(widyr)
#install.packages('readr',lib=.libPaths()[3])
library(readr)
#install.packages('igraph',lib=.libPaths()[3])
library(igraph)
block = read_csv("Open.csv", col_names = TRUE, quote = "\"")
block2 = read_csv("Reproduce.csv", col_names = TRUE, quote = "\"")

# ### We start data cleaning by checking the publication years. Our original choice of 2010-2017 remains fundamentally sound here. Next, check document types.

# + {"active": "ipynb"}
# myvars <- c("AuthorNames", "FoSNames", "Year", "DocType", "DisplayName" , "Doi", "OriginalTitle", "URLs")
# data <- block[myvars]
# data <- data[data$Year >=2010 & data$Year <=2017,]
# data <- data[is.na(data$DocType),]
# #write_csv(data, "noTypeOpen.csv")
# data <- block2[myvars]
# data <- data[data$Year >=2010 & data$Year <=2017,]
# data <- data[is.na(data$DocType),]
# #write_csv(data, "noTypeRep.csv")
#
# data <- block[myvars]
# data <- data[data$Year >=2010 & data$Year <=2017,]
# data <- data[!is.na(data$DocType),]
# data2 <- data[is.na(data$URLs),]
# #write_csv(data2, "noURLOpen.csv")
# #write_csv(data, "typeOpen.csv")
# data <- block2[myvars]
# data <- data[data$Year >=2010 & data$Year <=2017,]
# data <- data[!is.na(data$DocType),]
# data2 <- data[is.na(data$URLs),]
# #write_csv(data2, "noURLRep.csv")
# #write_csv(data, "typeRep.csv")
# -

# ### The new data now contains new document types includin books and book chapters, together with Patent document type, they are all excluded. The data quality of the remaining document types (jouranl and conference) is much higher compared with original crawled data, manual cleaning is no longer necessary (check tables in depreciated folder). DOI coverage is only missing 7 in reproducbility and 1 in open science, which is good news for matching WoS records.

# +
myvars <- c("PaperId", "AuthorIds", "AuthorIdsOrder", "Year", "DocType", "OriginalTitle", "EstimatedCitation")
data <- block[myvars]
data <- data[data$Year >=2010 & data$Year <=2017,]
data <- data[!is.na(data$DocType),]
data <- data[data$DocType!="Book",]
data <- data[data$DocType!="BookChapter",]

data <- arrange(data, tolower(as.character(OriginalTitle)), EstimatedCitation, Year, DocType) #put most cited, last the version of each paper that is published later, have more authors and are journal articles (will retain these)
titles <- tolower(as.character(data$OriginalTitle))
duplicated1 <- which(duplicated(titles)) #
duplicated2 <- which(duplicated(titles, fromLast=TRUE)) #remove these (keep the last appearing one of each set of duplicates)
duplicated_all <- sort(unique(c(duplicated1,duplicated2)))
duplicated_keep <- setdiff(duplicated_all, duplicated2)
data_duplicated <- data[duplicated_all,]
data_duplicated_keep <- data[duplicated_keep,]
#write.csv(data[duplicated2,], 'duplicated_remove0.csv', row.names=FALSE)
data <- data[-(duplicated2),]

AuthorTable <- data %>% separate(AuthorIdsOrder, into = sprintf('%s.%s', rep('Author',100), rep(1:100)), sep = "; ") #max author has exceeded 90
AuthorTable <- AuthorTable %>% gather(authorOrder, AuthorIdsOrder, into = sprintf('%s.%s', rep('Author',100), rep(1:100)))
AuthorTable <- AuthorTable[!is.na(AuthorTable$AuthorIdsOrder), ]
#AuthorTable
# -

PaperCollab <- pairwise_count(AuthorTable, PaperId, AuthorIdsOrder, sort = TRUE)
openG <- graph_from_data_frame(d = PaperCollab, directed=FALSE, vertices=data)
write_graph(openG, "openRaw.graphml", format="graphml")
n1=vcount(openG)
m1=ecount(openG)/2
#n1
#m1

# +
myvars <- c("PaperId", "AuthorIds", "AuthorIdsOrder", "Year", "DocType", "OriginalTitle", "EstimatedCitation")
data2 <- block2[myvars]
data2 <- data2[data2$Year >=2010 & data2$Year <=2017,]
data2 <- data2[!is.na(data2$DocType),]
data2 <- data2[data2$DocType!="Book",]
data2 <- data2[data2$DocType!="BookChapter",]
data2 <- data2[data2$DocType!="Patent",]

data2 <- arrange(data2, tolower(as.character(OriginalTitle)), EstimatedCitation, Year, DocType) #put most cited, last the version of each paper that is published later, have more authors and are journal articles (will retain these)
titles <- tolower(as.character(data2$OriginalTitle))
duplicated1 <- which(duplicated(titles)) #
duplicated2 <- which(duplicated(titles, fromLast=TRUE)) #remove these (keep the last appearing one of each set of duplicates)
duplicated_all <- sort(unique(c(duplicated1,duplicated2)))
duplicated_keep <- setdiff(duplicated_all, duplicated2)
data2_duplicated <- data2[duplicated_all,]
data2_duplicated_keep <- data2[duplicated_keep,]
#write.csv(data2[duplicated2,], 'duplicated_remove1.csv', row.names=FALSE)
data2 <- data2[-(duplicated2),]

AuthorTable2 <- data2 %>% separate(AuthorIdsOrder, into = sprintf('%s.%s', rep('Author',100), rep(1:100)), sep = "; ") #max author has exceeded 90
AuthorTable2 <- AuthorTable2 %>% gather(authorOrder, AuthorIdsOrder, into = sprintf('%s.%s', rep('Author',100), rep(1:100)))
AuthorTable2 <- AuthorTable2[!is.na(AuthorTable2$AuthorIdsOrder), ]
#AuthorTable2
# -

PaperCollab2 <- pairwise_count(AuthorTable2, PaperId, AuthorIdsOrder, sort = TRUE)
repG <- graph_from_data_frame(d = PaperCollab2, directed=FALSE, vertices=data2)
write_graph(repG, "reproduceRaw.graphml", "graphml")
n2=vcount(repG)
m2=ecount(repG)/2
#n2
#m2

# ### After creating the networks, we proceed to conduct network analysis

# + {"active": "ipynb"}
# nrow(data)+nrow(data2)

# + {"active": "ipynb"}
# AuthorTable %>% summarize(n = n_distinct(AuthorIdsOrder))
# AuthorTable2 %>% summarize(n = n_distinct(AuthorIdsOrder))
# AuthorTable0 <- dplyr::bind_rows(list(OpenScience=AuthorTable, Reproducibility=AuthorTable2), .id = 'Tag')
# AuthorTable0 %>% summarize(n = n_distinct(AuthorIdsOrder))

# + {"active": "ipynb"}
# nrow(block)+nrow(block2)
# nrow(block)
# nrow(block2)
# nrow(block01)
# nrow(block02)
# -

# ### Network density measures and Fisher's Exact Test

# + {"active": "ipynb"}
# 2*m1/n1/(n1-1)
# 2*m2/n2/(n2-1)

# + {"active": "ipynb"}
# table <- cbind(c(m1,m2),c(n1*(n1-1)/2-m1,n2*(n2-1)/2-m2))
# fisher.test(table, alternative="greater")
#
# table <- rbind(c(m1,m2),c(n1*(n1-1)/2-m1,n2*(n2-1)/2-m2))
# fisher.test(table, alternative="greater")

# + {"active": "ipynb"}
# n1/660
# n2/1641
# -

# ### Network analysis ends with average component sizes (Other analysis is done in Gephi). Finally, we merge the two data sets into "newdataCombined.csv" for downstream gender detection. Papers of all years are kepted for plotting purposes.

# +
myvars <- c("PaperId", "AuthorIdsOrder", "AuthorNamesOrder", "FoSNames", "Year", "DocType", "DisplayName", "Publisher", "Doi", 
            "OriginalTitle", "EstimatedCitation", "URLs", "IndexedAbstract")
data01 <- block[myvars]
data02 <- block2[myvars]
data0 <- dplyr::bind_rows(list(OpenScience=data01, Reproducibility=data02), .id = 'Tag')
#data0 <- data0[data0$Year >=2010 & data0$Year <=2017,]
data0 <- data0[!is.na(data0$DocType),]
data0 <- data0[data0$DocType!="Book",]
data0 <- data0[data0$DocType!="BookChapter",]
data0 <- data0[data0$DocType!="Patent",]

data0 <- arrange(data0, tolower(as.character(OriginalTitle)), EstimatedCitation, Year, DocType) #put most cited, last the version of each paper that is published later, have more authors and are journal articles (will retain these)
titles <- tolower(as.character(data0$OriginalTitle))
duplicated1 <- which(duplicated(titles)) #
duplicated2 <- which(duplicated(titles, fromLast=TRUE)) #remove these (keep the last appearing one of each set of duplicates)
duplicated_all <- sort(unique(c(duplicated1,duplicated2)))
duplicated_keep <- setdiff(duplicated_all, duplicated2)
data0_duplicated <- data0[duplicated_all,]
data0_duplicated_keep <- data0[duplicated_keep,]
write.csv(data0[duplicated2,], 'duplicated_remove.csv', row.names=FALSE)
data0 <- data0[-(duplicated2),]

#data0$ID <- seq.int(nrow(data0))
data0 <- rename(data0, "Journal" = "DisplayName", "Title" = "OriginalTitle")
write_csv(data0, "newdataCombined.csv")
#sum(is.na(data0$IndexedAbstract)&(data0$Tag == "OpenScience"))
#sum(is.na(data0$IndexedAbstract)&(data0$Tag == "Reproducibility"))

# + {"active": "ipynb"}
# 233/n1
# 1704/n2

# + {"active": "ipynb"}
# data9 = read_csv("OpenSci3Discipline.csv", col_names = TRUE)
# #data9$disc_name
# sum((data9$DocType == "Journal")&(data9$Tag == "OpenScience"))
# sum((data9$DocType == "Journal")&(data9$Tag == "Reproducibility"))
# sum(!is.na(data9$disc_name)&(data9$Tag == "OpenScience")&(data9$DocType == "Conference"))
# sum(!is.na(data9$disc_name)&(data9$Tag == "Reproducibility")&(data9$DocType == "Conference"))
# sum(!is.na(data9$disc_name)&(data9$Tag == "OpenScience")&(data9$DocType == "Journal"))
# sum(!is.na(data9$disc_name)&(data9$Tag == "Reproducibility")&(data9$DocType == "Journal"))
# -

# ### Counting papers for supplimentary information

# + {"active": "ipynb"}
# block01 = read_tsv("Open-old.csv", col_names = TRUE, quote = "\"")
# block02 = read_tsv("Reproduce-old.csv", col_names = TRUE, quote = "\"")
#
# myvars <- c("PaperId", "AuthorIds", "AuthorNames", "FoSNames", "Year", "DocType", "DisplayName", "Publisher", "Doi", 
#             "OriginalTitle", "EstimatedCitation", "IndexedAbstract")
# data01 <- block01[myvars]
# data02 <- block02[myvars]
# AuthorTable <- data01 %>% separate(AuthorIds, into = sprintf('%s.%s', rep('Author',100), rep(1:100)), sep = "; ") #max author has exceeded 90
# AuthorTable <- AuthorTable %>% gather(authorOrder, AuthorIds, into = sprintf('%s.%s', rep('Author',100), rep(1:100)))
# AuthorTable <- AuthorTable[!is.na(AuthorTable$AuthorIds), ]
#
# AuthorTable2 <- data02 %>% separate(AuthorIds, into = sprintf('%s.%s', rep('Author',100), rep(1:100)), sep = "; ") #max author has exceeded 90
# AuthorTable2 <- AuthorTable2 %>% gather(authorOrder, AuthorIds, into = sprintf('%s.%s', rep('Author',100), rep(1:100)))
# AuthorTable2 <- AuthorTable2[!is.na(AuthorTable2$AuthorIds), ]
#
# AuthorTable %>% summarize(n = n_distinct(AuthorIds))
# AuthorTable2 %>% summarize(n = n_distinct(AuthorIds))
# AuthorTable0 <- dplyr::bind_rows(list(OpenScience=AuthorTable, Reproducibility=AuthorTable2), .id = 'Tag')
# AuthorTable0 %>% summarize(n = n_distinct(AuthorIds))
# length(intersect(AuthorTable$AuthorIds, AuthorTable2$AuthorIds))

# + {"active": "ipynb"}
# nrow(block01)-68
# nrow(block02)-68
# nrow(block02)+nrow(block01)-68-68
# -



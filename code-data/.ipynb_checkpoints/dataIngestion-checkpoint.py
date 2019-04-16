# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: light
#       format_version: '1.4'
#       jupytext_version: 1.1.1
#   kernelspec:
#     display_name: Python 3
#     language: python
#     name: python3
# ---

# + {"active": "ipynb"}
# #!pip install --upgrade pip --user
# #!pip install configparser --user
# import os, sys
# import configparser
# sys.path.append('/home/jovyan/.local/lib/python3.6/site-packages/')
# print(sys.path)
#
# os.path.abspath("AzureDownload/config.txt")
# os.getcwd()
# config = configparser.ConfigParser()
# config.read("/home/jovyan/AzureDownload/config.txt")
# config.sections()

# + {"active": "ipynb"}
# ## Required for Azure Data Lake Analytics job management
# #!pip install azure-mgmt-datalake-analytics --user
# from azure.mgmt.datalake.analytics.job import DataLakeAnalyticsJobManagementClient
# from azure.mgmt.datalake.analytics.job.models import JobInformation, JobState, USqlJobProperties
# from azure.common.credentials import ServicePrincipalCredentials
#
# credentials = ServicePrincipalCredentials(client_id =config.get("credentials","client"), secret=config.get("credentials","password"), tenant =config.get("credentials","tenant"))
#
# subid= 'Microsoft Azure Sponsorship'
# rg = 'IUNI-MAG-RG'
# location = 'eastus2'
# adla = 'iunimag'
#
# adlaJobClient = DataLakeAnalyticsJobManagementClient(credentials, 'azuredatalakeanalytics.net')
# -

# ### After seting up the environement and Azure crendentials, we submit a u-sql script to run on Azure Datalake Analytics 

# + {"active": "ipynb"}
# import logging, getpass, pprint, uuid, time
# script = open("/home/jovyan/openScience/code-data/usql-scripts/MAGgetOld.usql", "r")
#
# jobId = str(uuid.uuid4())
# jobResult = adlaJobClient.job.create(
#     adla,
#     jobId,
#     JobInformation(
#         name='MAGgetOld@jupyter',
#         type='USql',
#         degree_of_parallelism=15,
#         properties=USqlJobProperties(script=script.read() )
#     )
# )

# + {"active": "ipynb"}
# import time
# from azure.storage.blob import BlockBlobService
#
# CONTAINERNAME ='mag-2019-03-02'
# BLOBNAME= "Open-old.csv"
# LOCALFILENAME= "/home/jovyan/openScience/code-data/Open-old.csv"
#
# #download from blob
# block_blob_service=BlockBlobService(account_name=config.get("configuration","account"),account_key=config.get("configuration","password"))
# #download from blob
# t1=time.time()
# block_blob_service.get_blob_to_path(CONTAINERNAME,BLOBNAME,LOCALFILENAME)
# t2=time.time()
# print(("It takes %s seconds to download "+BLOBNAME) % (t2 - t1))

# + {"active": "ipynb"}
# BLOBNAME= "Reproduce-old.csv"
# LOCALFILENAME= "/home/jovyan/openScience/code-data/Reproduce-old.csv"
#
# #download from blob
# t1=time.time()
# block_blob_service.get_blob_to_path(CONTAINERNAME,BLOBNAME,LOCALFILENAME)
# t2=time.time()
# print(("It takes %s seconds to download "+BLOBNAME) % (t2 - t1))
# -

# ### With the output tables downloaded, we proceed to parse the inverted-indexed abstracts

# +
import pandas as pd

def parse(inputStr):
  if pd.isna(inputStr):
    return None
  s = eval(inputStr)
  List = list()
  for word in s['InvertedIndex'].keys():
    for index in s['InvertedIndex'][word]:
        List.append((word, index))
  List.sort(key=lambda x: x[1], reverse=False)
  article = [i[0] for i in List]
  article = ' '.join(article)
  return article
  pass

#eval(open.iloc[3]['IndexedAbstract'])['InvertedIndex']['Research']


# +
open = pd.read_csv('Open-old.csv', delimiter="\t")
open = open[open['AuthorIds'].notnull()]
open['IndexedAbstract'] = open['IndexedAbstract'].map(parse)

rep = pd.read_csv('Reproduce-old.csv', delimiter="\t")
rep = rep[rep['AuthorIds'].notnull()]
rep['IndexedAbstract'] = rep['IndexedAbstract'].map(parse)
#rep.count()
# -

# ### Remove duplicate records from both data sets. 

openSet = set(open.PaperId)
repSet = set(rep.PaperId)
overlap = openSet.intersection(repSet)
cover = openSet.union(repSet)
len(cover)
open = open[~open["PaperId"].isin(overlap)]
rep = rep[~rep["PaperId"].isin(overlap)]
#rep.count()

# ### Finally we sort the AuthorIds, AuthorNames according to AuthorOrders

# +
def Strip(x):
  return x.strip()

def SortAuthors(row):
  ids = list(map(Strip, row['AuthorIds'].split(";")))
  names = list(map(Strip, row['AuthorNames'].split(";")))
  orders = list(map(Strip, row['AuthorOrders'].split(";")))
  
  Tup = list()
  for i in range(0,len(ids)):
    Id, name, order  = ids[i], names[i], orders[i]
    Tup.append((Id, name, order))
  
  Tup = list(set(Tup))
  Tup.sort(key=lambda x:x[2], reverse=False)
  
  sortedIDs = ""
  flag = 1
  for Id, name, order in Tup:
    if flag == 1:
      sortedIDs += Id
      flag = 0
    else:
      sortedIDs += "; "
      sortedIDs += Id
        
  sortedNames = ""
  flag = 1
  for Id, name, order in Tup:
    if flag == 1:
      sortedNames += name
      flag = 0
    else:
      sortedNames += "; "
      sortedNames += name
  
  return pd.Series([sortedIDs,sortedNames])
  pass


# +
sorted = open.apply(SortAuthors, axis=1)
open['AuthorIdsOrder'] = sorted[0]
open['AuthorNamesOrder'] = sorted[1]

sorted = rep.apply(SortAuthors, axis=1)
rep['AuthorIdsOrder'] = sorted[0]
rep['AuthorNamesOrder'] = sorted[1]
#open.count()
# -

open.to_csv('Open.csv',index=False, encoding='utf-8')
rep.to_csv('Reproduce.csv',index=False, encoding='utf-8')

# ### check if the author count remains the same after the re-ordering.

# + {"active": "ipynb"}
# import pandas as pd
# import numpy as np
#
# openTest = pd.read_csv('Open.csv')
# compare1 = openTest.AuthorIdsOrder.str.count("; ")
# compare2 = openTest.AuthorIds.str.count("; ")
# result = pd.concat([compare1,compare2],axis=1).astype(np.float16)
# result['AuthorIdsOrder'].equals(result['AuthorIds'])

# + {"active": "ipynb"}
# openTest = pd.read_csv('Reproduce.csv')
# compare1 = openTest.AuthorIdsOrder.str.count("; ")
# compare2 = openTest.AuthorIds.str.count("; ")
# result = pd.concat([compare1,compare2],axis=1).astype(np.float16)
# result['AuthorIdsOrder'].equals(result['AuthorIds'])
# -



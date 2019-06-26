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

import os, sys
os.getcwd()
# #!pip install azure-storage-blob --user
# #!pip install storefact --user

# + {"active": "ipynb"}
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
# -

# ### Credentials setup, read the WoS jounral name mapped table from Azure

# + {"active": "ipynb"}
# import time
# from azure.storage.blob import BlockBlobService
#
# CONTAINERNAME = "mag-2019-01-25"
# BLOBNAME= "MAGwosJournalMatch/OpenSci3Journal.csv/part-00000-tid-8679026268804875386-7586e989-d017-4b12-9d5a-53fc6497ec02-1116-c000.csv"
# LOCALFILENAME= "/home/jovyan/openScience/code-data/OpenSci3Journal.csv"
#
# block_blob_service=BlockBlobService(account_name=config.get("configuration","account"),account_key=config.get("configuration","password"))
# #download from blob
# t1=time.time()
# block_blob_service.get_blob_to_path(CONTAINERNAME,BLOBNAME,LOCALFILENAME)
# t2=time.time()
# print(("It takes %s seconds to download "+BLOBNAME) % (t2 - t1))

# +
import pandas as pd

openJ = pd.read_csv('input/OpenSci3Journal.csv', escapechar='\\', encoding='utf-8')
openJ.count()
# -

# ### To verify that the Spark output is consistent, we compare the pandas dataframes before and after the WoS jounral mapping

open0 = pd.read_csv('output/OpenSci3.csv', escapechar='\\', encoding='utf-8')
open0.count()

# ### Compare matched MAG journal names and WoS journal names

openJ['Journal'] = openJ.Journal.str.lower()
openJ['WoSjournal'] = openJ.WoSjournal.str.lower()
matched = openJ[openJ['Journal'] == openJ['WoSjournal']]
matched.count()

# ### Matching with UCSD map of science journal names

journalMap = pd.read_csv('WoSmatch/journalName.csv')
journalMap['journal_name'] = journalMap.journal_name.str.lower()
JwosMap = journalMap[journalMap['source_type']=="Thomson"]
MAGmatched = pd.merge(openJ, JwosMap, left_on=['Journal'], right_on=['journal_name'], how='left')
MAGmatched.count()

WoSmatched = pd.merge(openJ, JwosMap, left_on=['WoSjournal'], right_on=['journal_name'], how='left')
WoSmatched.count()

# ### Combining matched journal names from WoS and MAG to the UCSD map of science

MAGmatched.update(WoSmatched)
MAGmatched.count()

# ### Mapping from matched jounrals to subdisciplines

JsubMap = pd.read_csv('WoSmatch/jounral-subdiscipline.csv')
JsubMap.journ_id = JsubMap.journ_id.astype('float64')
subMatched = pd.merge(MAGmatched, JsubMap, left_on=['journ_id'], right_on=['journ_id'], how='left').drop(columns='formal_name')
subMatched.count()
#subMatched.dtypes

subTable = pd.read_csv('WoSmatch/subdiscipline.csv')
subTable.subd_id = subTable.subd_id.astype('float64')
subNameMatched = pd.merge(subMatched, subTable, left_on=['subd_id'], right_on=['subd_id'], how='left').drop(columns=['size','x','y'])
subNameMatched.count()

# ### Since each journal has a distribution of corresponding disciplines, we will collect the disipline vectors into new columns

import numpy as np
majTable = pd.read_csv('WoSmatch/discipline.csv')
majTable.disc_id = majTable.disc_id.astype('float64')
discMatched = pd.merge(subNameMatched, majTable, left_on=['disc_id'], right_on=['disc_id'], how='left').drop(columns=['color','x','y'])
discMatched.jfraction = discMatched.jfraction.astype('str')
discMatched.subd_name = discMatched.subd_name.astype('str')
discMatched.disc_name = discMatched.disc_name.astype('str')
temp = pd.DataFrame() 
temp = discMatched[['PaperId','WoSID','WoSjournal','jfraction','subd_name','disc_name']]
temp['jfraction'] = discMatched.groupby(['PaperId'])['jfraction'].transform(lambda x: ';'.join(x)).replace('nan', np.nan)
temp['subd_name'] = discMatched.groupby(['PaperId'])['subd_name'].transform(lambda x: ';'.join(x)).replace('nan', np.nan)
temp['disc_name'] = discMatched.groupby(['PaperId'])['disc_name'].transform(lambda x: ';'.join(x)).replace('nan', np.nan)
temp2 = temp.drop_duplicates()
temp2.count()

# ### Groupby number matches and we merge the mapped discipline data back to the OpenSci3.csv

OpenSci3Disc = pd.merge(open0, temp2, left_on=['PaperId'], right_on=['PaperId'], how='left')
OpenSci3Disc.count()

OpenSci3Disc.to_csv('output/OpenSci3Discipline.csv',index=False, sep=',', encoding='utf-8')



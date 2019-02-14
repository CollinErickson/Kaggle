# -*- coding: utf-8 -*-
"""
Created on Wed Feb 13 12:49:12 2019

@author: cbe117
"""

#for i in range(366000000):
#    1+1
folderpath = "C://Users/cbe117/My Documents/GitHub/Kaggle/Earthquake/"
rowsperfile = 10000000

i=0
j=0
groupnum = 0
done = False
with open(folderpath + "train.csv") as f:
    firstline = f.readline() # Remove header row
    while not done:
        groupnum += 1
        j = 0
        print("Starting ", groupnum)
        with open("E://Earthquake//trainsmallb_" + str(groupnum) + ".csv",'a') as outcsv:
            while j < rowsperfile:
                i += 1
                j += 1
                line1 = f.readline()
                if not line1:
                    done = True
                    break
                outcsv.write(line1)

# -*- coding: utf-8 -*-
"""
Created on Wed Feb 13 13:19:33 2019

@author: cbe117
"""

import numpy as np

folderpath = "C://Users/cbe117/My Documents/GitHub/Kaggle/Earthquake/"
rowsperfile = 10000000

i=0
#j=0
numgroups = 100
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

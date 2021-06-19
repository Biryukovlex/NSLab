
#input – numpy ndarray with medoids coordinates (based on clustering on patent annotations embeddings 
#(for example on fasttext)) and number of cluster objects in year 1 (past) and year 2 (current year). 
#Object connections are calculated based on manhattan distance between objects (objects are connected 
#when the distance between them is below the custom border)

#AnalyticsFrame – data frame with data about number of objects for every cluster in two periods
#AnalyticsFrameYTCt – data frame with data about number of external connections of each cluster
#BigTheme – list of huge clusters (that are more than 50% of all objects)
#Ythemegraph – data frame with data about graph basis: “from cluster”, “to cluster”, “number of connections between clusters – number of connections between objects from different clusters”

import numpy as np
import pandas as pd
import math

#basic calculations
grthemes=pd.DataFrame()
growingthemes = AnalyticsFrame.iloc[1:(len(AnalyticsFrame.iloc[:,0])),0]/AnalyticsFrame.iloc[1:(len(AnalyticsFrame.iloc[:,0])),1]
connectedthemes = AnalyticsFrameYTCt[0]/AnalyticsFrameYTCt[1]
for elem in connectedthemes:
    if math.isnan(elem):
        connectedthemes[connectedthemes.index(elem)] = 1
    elif math.isinf(elem):
        connectedthemes[connectedthemes.index(elem)] = 0.99
connectedthemesA = []
i=0
im=len(connectedthemes.iloc[:,0])

while(i<=im):
    SC = 0
    SM = 0
    for elem in connectedthemes.iloc[i,:]:
        if elem > 0:
            SC += elem
            SM += 1
    i += 1

for elem in growingthemes:
    if math.isnan(elem):
        growingthemes[growingthemes.index(elem)] = 0
    elif math.isinf(elem):
        growingthemes[growingthemes.index(elem)] = 1.1
        
i=0
d=-1
while(i<=len(growingthemes)):
    if(growingthemes[i]>1):
        d += 1
        grthemes.iloc[d,0] = i
        grthemes.iloc[d,1] = growingthemes[i] 
        grthemes.iloc[d,2] = connectedthemesA[i]
        if (grthemes.iloc[d,2] > 1 or grthemes.iloc[d,2] == 0):
            grthemes.iloc[d,3]='dispersing' 
        elif (grthemes.iloc[d,2] == 1):
            grthemes.iloc[d,3]='stable'
        elif (grthemes.iloc[d,2]<1 and grthemes.iloc[d,2] != 0):
            grthemes.iloc[d,3]='concentrating'
        grthemes.iloc[d,4]=AnalyticsFrameYTC.iloc[i,1]    #в ворде не дописана строка
        grthemes.iloc[d,5] = AnalyticsFrameYTC.iloc[i,1]/sum(AnalyticsFrameYTC.iloc[:,1]) 
        grthemes.iloc[d,6] = AnalyticsFrame.iloc[i+1,1] 
        grthemes.iloc[d,7]='temp'
    i += 1

# Selecting growing and big clusters to inspect
InspectGroups = []
i = 0
im = len(grthemes.iloc[:,0])
while(i<=im):
    if(grthemes.iloc[d,3] == 'dispersing'):
        InspectGroups[len(InspectGroups)+1] = grthemes.iloc[i,0]
    i += 1
BigStag = []
if(len(BigTheme)>0):
    i = 0
    im = len(BigThemeStat.iloc[:,0])
    while(i<=im):
        if(BigThemeStat.iloc[i,1] == 'not growing'):
            BigStag[len(BigStag)+1] = BigThemeStat.iloc[i,1]
        i += 1

#analize big clusters
BigThemeStat=pd.DataFrame()
i = 0
while(i<=len(BigTheme)):
    BigThemeStat.iloc[i,0]=BigTheme[i]
    j = 0
    BTS='not growing'
    while(j<=len(grthemes.iloc[:,0])):
        if(BigTheme[i]==grthemes.iloc[j,0]):
            BTS='growing'
        j += 1
    BigThemeStat.iloc[i,1]= BTS #В R была функция "as.character(BTS)"
    BigThemeStat.iloc[i,2]='temp'
    i += 1

# merge groups to inspect
GroupedIns = InspectGroups + BigStag    

# get data on connections between inspected groups (clusters)
Connections = pd.DataFrame()
i=0
im=len(GroupedIns)
ConLen=0
while(i<=im):
    j = 0
    jm=len(Ythemegraph.iloc[:,0])
    while(j<=jm):
        if(Ythemegraph.iloc[j,2] > 0):
            lm=len(GroupedIns)
            l=0
            if(GroupedIns[i] == Ythemegraph.iloc[j,0]):
                while(l<=lm):
                    if(GroupedIns[l] == Ythemegraph.iloc[j,1]):
                        Connections.iloc[ConLen,0] = Ythemegraph.iloc[j,0]
                        Connections.iloc[ConLen,1] = Ythemegraph.iloc[j,1]
                        Connections.iloc[ConLen,2] = Ythemegraph.iloc[j,2]
                        ConLen = ConLen+1
                    l=l+1
            if(GroupedIns[i] == Ythemegraph.iloc[j,1]):
                while(l<=lm):
                    if(GroupedIns[l] == Ythemegraph.iloc[j,0]):
                        Connections.iloc[ConLen,0] = Ythemegraph.iloc[j,0]
                        Connections.iloc[ConLen,1] = Ythemegraph.iloc[j,1]
                        Connections.iloc[ConLen,2] = Ythemegraph.iloc[j,2]
                        ConLen = ConLen+1
                    l=l+1
        j=j+1
    i=i+1

#Creating RepCon – data frame with predictions: “growing to the new theme”, new product, new technology 
#or “basic restriction” – tendency to overcome current technology basic restriction 
Ythemegraph = Connections
if(len(Ythemegraph) == 0):
    BRGTflag = 1
if(BRGTflag == 0):
    Ytemptg=Ythemegraph
    Ythemegraph=np.unique(Ythemegraph) #В R функция без сортировки, тут с
    RepCon = pd.DataFrame(Ythemegraph.iloc[:,0], Ythemegraph.iloc[:,1], YTConWeight)
    RepCon=np.unique(RepCon)
    RepCon.iloc[:,2]=RepCon.iloc[:,2]/2
    i=0
    im=len(RepCon.iloc[:,0])
    while(i<=im):
        j=0
        jm=len(RepCon.iloc[:,0])
        while(j<=jm):
            if(RepCon.iloc[i,0] == RepCon.iloc[j,1] and RepCon.iloc[i,1] == RepCon.iloc[j,0]):
                RepCon.iloc[i,2] = RepCon.iloc[i,2]+RepCon.iloc[j,2]
                RepCon = RepCon.iloc[-j,:] #-j на питоне?
                jm=len(RepCon.iloc[:,0])
                im=len(RepCon.iloc[:,0])
                if(j>jm):
                    break
            else:
                j=j+1
        RepCon.iloc[i,3] = 'GrowingToNewTheme'
        l=0
        lm=len(BigStag)
        while(l<=lm):
            if(RepCon.iloc[i,0] == float(BigStag[l]) or RepCon.iloc[i,1] == float(BigStag[l])): #В оригинале было as.numeric
                RepCon.iloc[i,3] = 'BasicRestriction'
            l=l+1
        i=i+1
    RepCon = pd.DataFrame([{'ThemeID1' : (RepCon.iloc[:,0]), 'ThemeID2' : (RepCon.iloc[:,1]), 'Inverse intensive of connection' : (RepCon.iloc[:,2]), 'Expected shifts' : (RepCon.iloc[:,3]))
    RepCon = np.unique(RepCon)    
    i = 0
    im=len(RepCon.iloc[:,0])
    while(i<=im):
        if(len(BigStag[BigStag == RepCon.iloc[i,0]])>0 and len(BigStag[BigStag == RepCon.iloc[i,1]])>0): #Не уверена
            RepCon = RepCon.iloc[-i,:]
            im=len(RepCon.iloc[:,0])
            if(i>im):
                break
        else:
            i=i+1
if(BRGTflag == 1):
    RepCon = pd.DataFrame()



        





        



    

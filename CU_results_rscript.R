#################################
#Name: Christine Urbanowicz
#Date: April 9, 2018
#R version: 3.3.1
#Purpose: Code cleans up data by changing prunse cot to fraxinus,
#calculates diversity of native plant species, 
#calculates abundance of fxnl groups, 
#runs PERMANOVA, and creates NMDS plot.

#################################
library(vegan) #for adonis and nmds
library(dplyr) #for summarizing
library(reshape2) #for melting and casting

##### Open data#####
#earthworm data from Michelle, March 2018
wormData<-read.csv("earthwormData.csv")

#functional group look-up table, derived from master sheet from Erin, February 2018
fxnLUT<-read.csv("fxnlGroupByCode.csv")

#plants from Erin - christine cleaned up by removing canopy trees
allPlants<-read.csv("allCommunity_canopyRemoved_Feb2018.csv")
names(allPlants)

##### Clean data ######
#subset by sites in earthworm dataset
allPlants_just2Sites<-allPlants[allPlants$site=="HF"|
                                  allPlants$site=="MC",]
#subset by 2017 and remove sprayed treatment
allPlants_just2Sites2017<-allPlants_just2Sites[allPlants_just2Sites$year=="2017"&allPlants_just2Sites$treatment!="s",]

#get rid of plants without any presence in the 2 sites in 2017
community<-allPlants_just2Sites2017[,-c(1:4)]
community_ew<-community[,(colSums(community)!=0)]

#take out unknown seedling
community_ew$unk_seedling <- NULL

#change prunse cotelydon to fraxam!!!!!!!!!!!!! prunse cotyeldon was actually white ash (Erin identified April 2018)
community_ew$fraxam<-community_ew$fraxam+community_ew$prunse_cot

#get rid of prunse_cot
community_ew$prunse_cot <- NULL

#double check there are 36 taxa (took away unknown seedling and prunse)
ncol(community_ew) ==35 

###### Calculate native total stem and shannon diversity #######
#get rid of allipe columns and only other invasive, euonal (burning bursh)
nonNative<-c("euonal","allipe_1","allipe_2","allipe_total")
community_ew_native<-community_ew[,!(names(community_ew) %in% nonNative)] 

#calculate total stem count and shannon diversity
nativeShannon<-diversity(community_ew_native)
totalNativeStem<-rowSums(community_ew_native)

######## Separate out by functional group ######

##### Merge with site info and worms ####
communityByPlot<-cbind(allPlants_just2Sites2017[,c(1:4)],community_ew)

#melt data so that I can merge with functional group
melted<-melt(communityByPlot,id.vars=c("plotID","site","treatment","year"))
head(melted)

#remove funky double spaces in funcitonal groups look up table
fxnLUT$functionalGroup<-gsub(" ", "", fxnLUT$functionalGroup, fixed = TRUE)

#merge melted data frame with functional gorup
melted_fxn<-merge(melted,fxnLUT,by.x="variable",by.y="code",all.x=TRUE)
nrow(melted)==nrow(melted_fxn) #double check

#Reclassify tree and shrubs as woody
melted_fxn$functionalGroup[melted_fxn$functionalGroup=="Tree"|melted_fxn$functionalGroup=="Shrub"]<-"Woody"
melted_fxn$functionalGroup[melted_fxn$functionalGroup=="Non-native"]<-"NonNative"

#drop allipe 1 and 2 and just keep total allipe
melted_fxn<-melted_fxn[melted_fxn$variable!="allipe_1"&melted_fxn$variable!="allipe_2",]
unique(melted_fxn$variable) #double check

#sumamrize by functional group
fxnlGroupStemCounts<-melted_fxn %>% group_by(plotID,functionalGroup) %>% summarize(stemCount=sum(value))

#cast as columns
fxnlGroupCasted<-dcast(fxnlGroupStemCounts,plotID~functionalGroup,value.var="stemCount",fun.aggregate = sum)

###### Put all data together ######
# Bind diversity with community data 
bindNativeMetrics<-cbind(communityByPlot,nativeShannon,totalNativeStem)

#merge functional groups with bindNativeMetrics
mergeFxnl<-merge(bindNativeMetrics,fxnlGroupCasted,by="plotID")

#merge with earthworm data
allData<-merge(mergeFxnl,wormData,by="plotID")
nrow(allData) == 18
write.csv(allData, "garlicMustardEarthworm_prunseCot_to_fraxam_9April2018.csv",row.names=F)

############### Permanova ################

#get rid of nonNative
allData_native<-allData[,!(names(allData) %in% nonNative)] 

#get bray distances using abundances of native plants (subset df by these columns)
brayDist<-vegdist(allData_native[,c(5:35)]) #*will change if community data changes
myPerm<-adonis(brayDist ~treatment*mass, data = allData_native,strata=allData_native$site)
myPerm
#note that p values will change each time based on seed (random generator for permutaitons). I am not sure what seed I used in manuscript!

############### NMDS Plots ###########################
meta<-metaMDS(allData_native[,c(5:35)]) 

#set parameters for NMDS
treatment<- factor(allData$treatment)
col.gr <- c("#56B4E9", "#E69F00", "#000000") #red corresponds to invaded, gray to pulled, black to unvivdaded
size<-log(allData$mass+1)*1.5 #for setting size of points proportional to worm biomass
with(allData$treatment, legend("topright", legend = levels(Use), bty = "n",
                      col = colvec, pch = 21, pt.bg = colvec))

#output
svg(filename="nmds_withFrax.svg", 
    width=6.5, 
    height=6.5)
plot(meta, type = "n",cex.axis=1.8, cex.lab=1.8)
points(meta, display = "sites", cex = size, pch=19, col = col.gr[treatment])
#text(meta, display = "spec", cex=0.7)
ordiellipse(meta, allData$site, conf = 0.95, label = TRUE,cex=1.8)
dev.off()



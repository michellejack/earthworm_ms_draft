library(car)
#read in csv file
allvariables<-read.csv('garlicMustardEarthworm_15March_2018.csv',header=TRUE)
#show head of data frame
head(allvariables)
#test of fit with shannon diversity as a factor of earthworm biomass,treatment & site
shandiv<-lm(shannon~mass+site+Treatment,data=allvariables)
#type II ANOVA of this fit
Anova(shandiv, type="II")
#summary of ANOVA
summary(shandiv)
#new test of fit with shannon diversity as a factor of an interaction between earthworm biomass*treatment and site
shandiv2<-lm(shannon~site+Treatment*mass, data=allvariables)
#type II ANOVA of new fit
Anova(shandiv2, type="II")
#summary of ANOVA
summary(shandiv2)
#plot the effects of this new model fit
plot(allEffects(shandiv2))
#install 'jtools' package https://cran.r-project.org/web/packages/jtools/index.html
library(jtools)
#summary of the model reported
summ(shandiv2)
summ(shandiv2, scale = TRUE)
#list of hex codes for colors associated with color-blind palette "cbPalette"
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#command for interaction plot
interact_plot(shandiv2, pred = "mass", modx = "Treatment", plot.points = TRUE)
#revised model with 'eradicated' sites excluded
shandiv3<-lm(shannon~site+mass, data=allvariables[allvariables$Treatment=="Eradicated",])
#summary of the new model
summary(shandiv3)
#interaction plot of the original model with code for ggplot parameters 
interact_plot(shandiv2, pred = "mass", modx ="Treatment", plot.points = TRUE, x.label = "Earthworm Biomass (g)", y.label = "Native Plant Diversity", legend.main = "Treatment") +
  theme(axis.text = element_text(size=12))+
  theme(axis.title.x = element_text(size=12,face="bold")) +
  theme(axis.title.y = element_text(size=12,face="bold")) +
  theme(legend.text = element_text(size = 12))+
  scale_colour_manual(values=cbPalette)

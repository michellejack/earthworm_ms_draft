library(ggplot2)
#Data frame for the figure with code for ggplot parameters 
myfig1<-transform(fig1,Treatment=reorder(Treatment,-emmean))
ggplot(myfig1,aes(x = Treatment, y = emmean)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE),
                width=0.1, size=0.5, color="black", position=position_dodge(.9))+  labs(x = "Treatment",
                                                                                        y = "Earthworm Biomass (g)")  +
  theme_bw()+
  theme(axis.text = element_text(size=12,colour = "black"))+
  theme(axis.title.x = element_text(size=12,face="bold")) +
  theme(axis.title.y = element_text(size=12,face="bold")) +
  theme(legend.title = element_text(size=12, face="bold"))+
  theme(legend.text = element_text(size = 12))+
  scale_y_continuous(expand = c(0,0)) +
  expand_limits(y=7) +
  theme(legend.position = "none")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))
############################# Exporting the plot #######################################
figure1<-ggplot(myfig1,aes(x = Treatment, y = emmean)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE),
                width=0.1, size=0.5, color="black", position=position_dodge(.9))+  labs(x = "Treatment",
                                                                                        y = "Earthworm Biomass (g)")  +
  theme_bw()+
  theme(axis.text = element_text(size=12,colour = "black"))+
  theme(axis.title.x = element_text(size=12,face="bold")) +
  theme(axis.title.y = element_text(size=12,face="bold")) +
  theme(legend.title = element_text(size=12, face="bold"))+
  theme(legend.text = element_text(size = 12))+
  scale_y_continuous(expand = c(0,0)) +
  expand_limits(y=7) +
  theme(legend.position = "none")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))

ggsave("myfig1.tiff", figure1, units="in", width=5, height=5, dpi=300)
tiff("myfig1.tiff", width=5, height=5)
ggplot(myfig1,aes(x = Treatment, y = emmean)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE),
                width=0.1, size=0.5, color="black", position=position_dodge(.9))+  labs(x = "Treatment",
                                                                                        y = "Earthworm Biomass (g)")  +
  theme_bw()+
  theme(axis.text = element_text(size=12,colour = "black"))+
  theme(axis.title.x = element_text(size=12,face="bold")) +
  theme(axis.title.y = element_text(size=12,face="bold")) +
  theme(legend.title = element_text(size=12, face="bold"))+
  theme(legend.text = element_text(size = 12))+
  scale_y_continuous(expand = c(0,0)) +
  expand_limits(y=7) +
  theme(legend.position = "none")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))




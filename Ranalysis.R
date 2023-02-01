library(ggplot2)
library(dplyr)
statistic <- read.csv("D:/PycharmProjects/staticanalysis/Stats.csv")
statistic <- data.frame(statistic[c(28:81),c(1:9)])
statistic <- na.omit(statistic)
statistic$xylose_Induction <- as.character(statistic$PC_Induction) 
#?????յ???Ũ??Ϊ?ַ???
statistic$Concentration_CA <- as.character(statistic$Concentration_CA)
statistic$Concentration_IPTG <- as.character(statistic$Concentration_IPTG)
#?涨?յ???Ũ?ȱ?��????
statistic$Concentration_CA <- factor(statistic$Concentration_CA, levels = c('-4','-5','-6','-7','-8','-9','-10','0'))
statistic$Concentration_IPTG <- factor(statistic$Concentration_IPTG, levels = c('-4','-5','-6','-7','-8','-9','-10','0'))
#C6_Test <- statistic[c(1:96),c(2:3,5)]
#C12_Test <- statistic[c(97:192),c(2:3,5)]
#C6_Test <- data.frame(C6_Test)
#C12_Test <- data.frame(C12_Test)

induced_RPU_xyl_38 <- statistic[c(28:54),c(2:3,7)]
uninduced_RPU_xyl_38 <- statistic[c(1:27),c(2:3,7)]
RPU_38_xyl <- statistic[c(13:24,37:48),c(2:5)]
RPU_38_xyl <- statistic[c(55:81,136:162),c(1:9)]
RPU_38_xyl$PC_Induction <- as.character(RPU_38_xyl$PC_Induction) 
YFP_xyl_38 <- YFP[c(25:36,73:84),c(1:3)]
YFP_38_xyl <- YFP[c(37:48,85:96),c(1:3)]
RFP_xyl_38 <- RFP[c(25:36,73:84),c(1:3)]
RFP_38_xyl <- RFP[c(37:48,85:96),c(1:3)]
CY1714_13 <- statistic[c(145:168),c(2:5)]
CY1714_2004 <- statistic[c(169:192),c(2:5)]
YFP <- data.frame(YFP)
RFP <- data.frame(RFP)
CY1714_13 <- data.frame(CY1714_13)
CY2004_54 <- data.frame(CY2004_54)
CY1714_2004 <- data.frame(CY1714_2004)
YXP11_53 <- data.frame(YXP11_53)
YXP13_54 <- data.frame(YXP13_54)
YXP53_1724 <- data.frame(YXP53_1724)
CY2004_54$Concentration <- factor(CY2004_54$Concentration, levels = c('0','-14','-13','-12','-11','-10','-9','-8','-7','-6','-5','-4'))
#YXP13_1714 <- statistic[c(96:143),c(2:3,5)]
#YXP13_1714 <- data.frame(YXP13_1714)
#CY2004_54 <- statistic[c(144:191),c(2:3,5)]
#CY2004_54 <- data.frame(CY2004_54)

#Means <- apply(select(CY,c(statistic[c(0),c(2:4)])),2,mean)
#Means <- apply(select(ECF_22,c(X0,X25,X50,X100)),2,mean)
#SD <- apply(select(ECF_11,c(X0,X25,X50,X100)),2,sd)
#SD <- apply(select(ECF_22,c(X0,X25,X50,X100)),2,sd)
#ECF_11 <- rbind(ECF_11,Means,SD)
C6_Test$Concentration <- factor(C6_Test$Concentration, levels = c('0','-14','-13','-12','-11','-10','-9','-8','-7','-6','-5','-4'))
C12_Test$Concentration <- factor(C12_Test$Concentration, levels = c('0','-14','-13','-12','-11','-10','-9','-8','-7','-6','-5','-4'))
p1 <- ggplot(statistic, aes(x=Concentration_IPTG, y=Concentration_CA,fill = Log.RPU.RPU0.))+ xlab("log_IPTG_Concentration(M)") + ylab("log_CA_Concentration(M)")+ ggtitle("log_RPU(ECF)_Test")
p1 <- p1+geom_raster()+scale_fill_gradient2(low="#003366", high="#990033", mid="white")
print(p1)
p2 <- ggplot(C12_Test, aes(x=Concentration, y=Species,fill = RPU_EXPRESSION))+ xlab("C12_concentration(M)") + ylab("Part")
p2 <- p2+geom_raster()+scale_fill_gradient2(low="#003366", high="#990033", mid="white")
print(p2)
#ֻ??ʾ??ǩ??
p1 <- ggplot(RPU_38_xyl ,aes(x=Xylose_Concentration, y=Induction_Strength,group = PC_Induction, color = PC_Induction ))
p1 <- p1 + xlab('Xylose Concentration(mM)') +ylab('Induce_Strength') + ggtitle("mut38-YFP-xyl-RFP curve")  
p1 <- p1+ theme(plot.title = element_text(hjust = 0.5))+geom_point()
p1 <- p1 + annotation_logticks(sides = 'l')+annotation_logticks(sides = 'b')
p1 <- p1 + scale_x_log10()+scale_y_log10()+geom_smooth()
p2 <- ggplot(uninduced_RPU_38_xyl,aes(x=Xylose_Concentration, y=RPU))
p2 <- p2 + annotation_logticks(sides = 'b') + scale_x_log10()+ ggtitle("mut38-YFP-pxyl-RFP curve")
p2 <- p2+ theme(plot.title = element_text(hjust = 0.5)) + geom_point()
p2 <- p2 +geom_smooth() + xlab('Concentration(Log(mM))') +ylab('RPU_Expression')
p3 <- p3 + annotation_logticks(sides = 'b') + scale_x_log10()+ ggtitle("YXP13+CY1714")
p3 <- p3+ theme(plot.title = element_text(hjust = 0.5))
p3 <- p3 +geom_smooth()
p4 <- p4 + annotation_logticks(sides = 'b') + scale_x_log10()+ ggtitle("CY2004+YXP54")
p4 <- p4+ theme(plot.title = element_text(hjust = 0.5))
p4 <- p4 +geom_smooth()
print(p1)
#???????????ݲ???
Data_Comparison <- RFP %>% group_by(Strain,Treatment) 
Data_Comparison <- Data_Comparison %>%
  summarise(mean = mean(RPU_RFP),
            lo = mean(RPU_RFP)-sd(RPU_RFP), 
            hi = mean(RPU_RFP)+sd(RPU_RFP),)
fit <- lm(RPU_YFP ~ Treatment, data = YFP_xyl_38)
aov(fit) %>% TukeyHSD(conf.level = 0.95)
kruskal.test(RPU ~ Treatment, data = RFP_xyl_38)

#??ͼǰ׼??????
Datameans_YFP <- YFP_xyl_38 %>% 
  group_by(Treatment) %>% 
  summarize(
    mean = mean(RPU_YFP),
    lo = mean(RPU_YFP)-sd(RPU_YFP), 
    hi = mean(RPU_YFP)+sd(RPU_YFP),
  )
Datameans_RFP <- RFP_38_xyl %>% 
  group_by(Treatment) %>% 
  summarize(
    mean = mean(RPU_RFP),
    lo = mean(RPU_RFP)-sd(RPU_RFP), 
    hi = mean(RPU_RFP)+sd(RPU_RFP),
  )

#??ͼ
plot <- ggplot(RFP_xyl_38, aes(x=Treatment, y=RPU_RFP)) +
  geom_bar(aes(x = Treatment, y=mean),Data_Comparison, position=position_dodge(0.5),
           stat = 'identity',width = 0.5)+
  geom_errorbar(data = Data_Comparison, width = 0.1, col = 'darkred',position=position_dodge(0.5),aes(ymin=lo, ymax=hi, y = NULL)) + 
  geom_point(data=Data_Comparison, aes(y=mean), shape = 24, position=position_dodge2(0.5),# triangle
             fill='red', size = 0.5) +
  geom_jitter(aes(x=Treatment, y=RPU, fill=Signal_type), RPU_xyl_38,stat = 'identity',position=position_dodge2(width = 0.5), size=2) +
  #geom_point(position=position_jitterdodge())+
  labs(x = 'Treatment', y = 'RPU') +
  theme_classic(base_size = 16) + ggtitle('xylose-RFP-mut38-YFP')
plot
plot <- plot + geom_bar() +
  scale_fill_manual(
  values=c("color_1"="red", "color_2"="gold","color_1"="red", "color_2"="gold","color_1"="red", "color_2"="gold","color_1"="red", "color_2"="gold"), 
  labels=c("RFP", "YFP"))  
plot
plot <-  ggplot(YFP_xyl_38, aes(x=Treatment, y=RPU_YFP)) +
  geom_bar(aes(x = Treatment, y=mean),Datameans_YFP, stat = 'identity',width = 0.3)+
  
  geom_jitter(col='grey', size=2, width = 0.1) +
  geom_errorbar(data = Datameans_YFP, width = 0.1, col = 'darkred',
                aes(ymin=lo, ymax=hi, y = NULL)) +  
  geom_point(data=Datameans_YFP, aes(y=mean), shape = 24, # triangle
             fill='red', size = 4)+
  ggtitle('YFP RPU \n xylose-RFP-mut38-YFP')+
  theme (plot.title=element_text (hjust = 0.5,vjust = -5,size = 20))
plot 

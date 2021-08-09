#load libraries
library(tidyverse)
library(lme4)
library(epiDisplay)

#load data
atr_PCA <- read.csv("C:/Users/jagad/Desktop/ATR_PEDCA_manuscript/PCA_analysis_withSVI.csv", 
                    sep= "," , header = TRUE )

#data formatting
fvar <- c( "Ground_cat", "Surface_cat")
atr_PCA[,fvar]<-lapply(atr_PCA [,fvar], factor)
nvar<- c("ca_count","Avg..E.Pl.Unemp","Avg..Pl.Sngprnt","Avg..Pl.Minority","Avg..E.Pl.Limeng")
atr_PCA[,nvar]<-sapply(atr_PCA[,nvar], as.numeric)
str(atr_PCA)

#SVI variables are percentile proportions

################### NEGATIVE BINOMIAL REG MODEL GROUND ATRAZINE AND PED CANCER ################
#negative binomial model with offset term to adjust for population density
fit_gr <- glmer.nb(ca_count ~ Ground_cat+Avg..R.Pl.Themes+
                     (1|Total_Pead_pop), data=atr_PCA)

#fit_gr <- glmer.nb(ca_count ~ Surface_cat+Avg..E.Pl.Unemp+Avg..Pl.Sngprnt+Avg..Pl.Minority+Avg..E.Pl.Limeng+
#summary(fit_gr)
#plot(fit_gr)
IRR <- fixef(fit_gr)
confnitfixed <- confint(fit_gr, parm = "beta_", method = "Wald")
summary <- exp(cbind(IRR, confnitfixed))
summary

#Figure 2

dat<- read.csv("C:/Users/jagad/Desktop/ATR_PEDCA_manuscript/PCA_SVI_estimates_fin.csv")

dat.fin<- filter(dat, Water.source %in% c("Ground","Surface"))

Model_order<-c("Unadjusted", "Adjusted_1", "Adjusted_2")
dat.fin$Variable <- factor(dat.fin$Variable, levels=c("Low", "Medium", "High"))

cbbPalette <- c("#999999", "#E69F00", "#56B4E9")

ggplot(dat.fin, aes(x = Variable, y = IRR, ymin = LL, ymax = UL))+
  facet_grid(cols = vars(Water.source))+
  geom_pointrange(aes(col = factor(Model, levels=Model_order)),
                  position=position_dodge(width=0.8),size = 1)+
  scale_y_continuous(breaks = seq(0, 13, 1),limits=c(0, 13))+
  geom_hline(yintercept = 1, linetype="longdash")+
  xlab("Atrazine groups")+
  ylab("Pediatric cancer - Incident Rate Ratio (IRR)")+
  scale_colour_manual(values=cbbPalette)+
    theme(plot.title = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        text=element_text(size=15,  family="Arial Black"),
        axis.text = element_text(size = 15, family="Arial Black"),
        legend.text=element_text(size=15,  family="Arial Black"),
        strip.text.x = element_text(size=15,  family="Arial Black"),
        legend.position = "bottom",
        )


ggsave("C:/Users/jagad/Desktop/ATR_PEDCA_manuscript/fig3.tiff", units="in", width=12.5, 
       height=8.5, dpi=300, compression = 'lzw')

#labs(title="",
#caption = "Figure 3. Association between Atrazine and pediatric cancer \n
#Unadjusted: Crude association; Adjusted_1: Model adjusted for variables Unemployment, 
#Single_parent, Minority, and Language_barrier; \nAdjusted_2: Model adjusted for overall 
#social vulnerability index. Ground and Surface represent the water sample source, 
#categorized into four quantile groups. \nEstimated IRR (95% CI) of pediatric cancer by ground 
#and surface water Atrazine concentration. Note: The IRR and 95% confidence interval were 
#estimated \nusing generalized linear model for the negative binomial distribution, with 
#population per watershed as an offset term.")+
  
# 
# 
# summary(fit_gr)
# 
# d=data.frame(Atrazine_Ground_concentration_category=c("Medium-low","Medium","High"),
#              IRR=c(3.56, 3.43, 4.64),lower=c(1.45, 1.29, 1.81), upper=c(8.72, 9.09, 11.85))
# ggplot()+geom_pointrange(data=d, mapping=aes(x=Atrazine_Ground_concentration_category, 
#                                              y=IRR,ymin=upper, ymax=lower), size=1, color="black", fill="black", shape=22)+
#   coord_cartesian(ylim=c(0,14))
# 
# ################### NEGATIVE BINOMIAL REG MODEL Sur ATRAZINE AND PED CANCER ################
# fit_sur <- glmer.nb(ca_count ~ Surface_cat+ (1|Total_pop), data=atr_PCA)
# summary(fit_sur)
# IRR <- fixef(fit_sur)
# confnitfixed <- confint(fit_sur, parm = "beta_", method = "Wald")
# summary1 <- exp(cbind(IRR, confnitfixed))
# summary1
# 
# d=data.frame(Atrazine_Surface_concentration_category=c("2","3","4"),
#              IRR=c(1.61,3.56,3.76),lower=c(0.56,1.26,1.36), upper=c(4.6,10.03,10.37))
# ggplot()+geom_pointrange(data=d, mapping=aes(x=Atrazine_Surface_concentration_category, 
#                                              y=IRR,ymin=upper, ymax=lower), width=0.2, size=1, color="black", fill="black", shape=22)
# 
# 
# d=data.frame(Atrazine_concentration_quantile_category=c("Low","Medium","High", 
#                                                         "Low","Medium","High"),
#              IRR=c(3.56, 3.43, 4.64, 1.61,3.56,3.76),
#              lower=c(1.45, 1.29, 1.81, 0.56,1.26,1.36), 
#              upper=c(8.72, 9.09, 11.85, 4.6,10.03,10.37), 
#              s=rep(c("Ground_water","Surface_water"),each=3))
# ggplot()+geom_pointrange(data=d, mapping=aes(x=Atrazine_concentration_quantile_category,
#                                              y=IRR,ymin=upper, ymax=lower, shape=s), 
#                          size=1, color="black",fill="black")+
#   scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14))+
#   geom_hline(yintercept = 1, linetype = 2)+theme(legend.position="bottom")+
#   facet_grid(cols = vars(s), scales="free_x")+
#   scale_x_discrete(limits=c("Low","Medium","High"))


##################################################################################
##################################################################################
##################################################################################
#with SVI themes 
# 1: Percentile ranking for Socioeconomic theme
# 2: Percentile ranking for Household Composition
# 3: Percentile ranking for Minority Status/Language theme
# 4: Percentile ranking for Housing/Transportation theme

#using overall themes
fit_gr <- glmer.nb(ca_count ~ Surface_cat+ Avg..R.Pl.Theme1+
                     Avg..R.Pl.Theme2+Avg..R.Pl.Theme3+Avg..R.Pl.Theme4+
                     offset(1|Total_Pead_pop)+
                     (1|HUC_code),, data=atr_PCA)

#using component of themes

#theme 3:
fit_sur <- glmer.nb(ca_count ~ Ground_cat+ Avg..Pl.Minority + Avg..E.Pl.Limeng+
                     Avg..P.Age65+Avg..P.Age65+Avg..P.Sngprnt+offset(1|Total_Pead_pop)+
                     (1|HUC_code), data=atr_PCA)

#theme 2: 
fit_gr <- glmer.nb(ca_count ~ atr_PCA$Avg..Pl.Groupq+
                   offset(1|Total_Pead_pop)+(1|HUC_code), data=atr_PCA)
IRR <- fixef(fit_gr)
confnitfixed <- confint(fit_gr, parm = "beta_", method = "Wald")
summary1 <- exp(cbind(IRR, confnitfixed))
summary1


res<-read.csv("C:/Users/jagad/Desktop/ATR_PEDCA_manuscript/SVI_covariate_estimates.csv",
             header=T, fileEncoding="UTF-8-BOM")

library(ggplot2)
cbbPalette <- c("#000000", "#999999")

ggplot(res, aes(x = variable, y = or, ymin = lcl, ymax = ucl)) + 
  geom_pointrange(aes(col = factor(Geography)), 
                  position=position_dodge(width=0.5),size = 0.6) + 
  ylab("Incidence rate ratio [95% CI]") +
  geom_hline(aes(yintercept = 1)) + 
  scale_colour_manual(values=cbbPalette) + 
  ggtitle("Sensitivity of SVI variables")+
  xlab("SVI variables")+
  theme(legend.position = "bottom")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=15,  family="Arial Black"))+
  theme(axis.text = element_text(size = 15, family="Arial Black"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, 16, 1),limits=c(0, 16))+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


#######################################################333
############################################################
# PCA SVI ANALYSIS

PC_SVI <- read_csv("C:/Users/jagad/Desktop/ATR_PEDCA_manuscript/PCA_SVI_working/PCA_SVI_analysis.csv")

ls(PC_SVI)

#theme 2: 
fit_gr <- glmer.nb(PCA_count ~ Ground_cat+Avg..E.Pl.Unemp+Avg..Pl.Sngprnt+Avg..Pl.Minority+
                     Avg..E.Pl.Limeng+
                     (1|ped_popln), data=PC_SVI)
IRR <- fixef(fit_gr)
confnitfixed <- confint(fit_gr, parm = "beta_", method = "Wald")
summary1 <- exp(cbind(IRR, confnitfixed))
summary1


#conceptualizing association between atrazine and pediatric cancer
library(dagitty)
library(ggdag)
library(lavaan)

g<- dagitty('dag {
bb="0,0,1,1"
"Atrazine WQ" [exposure,pos="0.4,0.5"]
"Genetic susceptibility" [latent,pos="0.5,0.1"]
"Household income" [pos="0.2,0.4"]
"Other environmental factors" [latent,pos="0.7,0.2"]
"Pediatric cancer" [outcome,pos="0.6,0.5"]
"Single parent" [pos="0.05,0.3"]
Diet [latent,pos="0.7,0.3"]
Education [pos="0.4,0.2"]
Race [pos="0.2,0.3"]
"Atrazine WQ" -> "Pediatric cancer"
"Genetic susceptibility" -> "Pediatric cancer"
"Household income" -> "Atrazine WQ"
"Household income" -> "Pediatric cancer"
"Household income" -> Diet
"Other environmental factors" -> "Pediatric cancer"
"Other environmental factors" <-> Education
"Single parent" -> "Household income"
Diet -> "Pediatric cancer"
Education -> "Atrazine WQ"
Education -> "Pediatric cancer"
Education -> Diet
Race -> "Genetic susceptibility"
Race -> "Household income"
Race -> "Other environmental factors"
Race -> "Pediatric cancer"
Race -> Diet
Race -> Education
}')
plot(g)

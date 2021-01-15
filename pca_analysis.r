#load libraries
library(epiDisplay)
library(dplyr)
library(MASS)
library(lme4)
library(effects)
library(ggplot2)

#load data
atr_PCA <- read.csv("C:/Users/jagad/Desktop/ATR_PEDCA_manuscript/final_ca_analysis2.csv", 
                    sep= "," , header = TRUE ) 

#data formatting
fvar <- c( "Ground_cat", "Surface_cat")
atr_PCA[,fvar]<-lapply(atr_PCA [,fvar], factor)

str(atr_PCA)

################### NEGATIVE BINOMIAL REG MODEL GROUND ATRAZINE AND PED CANCER ################
#negative binomial model with offset term to adjust for population density
fit_gr <- glmer.nb(ca_count ~ Ground_cat+ (1|Total_pop), data=atr_PCA)
#summary(fit_gr)
#plot(fit_gr)
IRR <- fixef(fit_gr)
confnitfixed <- confint(fit_gr, parm = "beta_", method = "Wald")
summary <- exp(cbind(IRR, confnitfixed))
summary

d=data.frame(Atrazine_Ground_concentration_category=c("Medium-low","Medium","High"),
             IRR=c(3.56, 3.43, 4.64),lower=c(1.45, 1.29, 1.81), upper=c(8.72, 9.09, 11.85))
ggplot()+geom_pointrange(data=d, mapping=aes(x=Atrazine_Ground_concentration_category, 
                                             y=IRR,ymin=upper, ymax=lower), size=1, color="black", fill="black", shape=22)+
  coord_cartesian(ylim=c(0,14))

################### NEGATIVE BINOMIAL REG MODEL Sur ATRAZINE AND PED CANCER ################
fit_sur <- glmer.nb(ca_count ~ Surface_cat+ (1|Total_pop), data=atr_PCA)
summary(fit_sur)
IRR <- fixef(fit_sur)
confnitfixed <- confint(fit_sur, parm = "beta_", method = "Wald")
summary1 <- exp(cbind(IRR, confnitfixed))
summary1

d=data.frame(Atrazine_Surface_concentration_category=c("2","3","4"),
             IRR=c(1.61,3.56,3.76),lower=c(0.56,1.26,1.36), upper=c(4.6,10.03,10.37))
ggplot()+geom_pointrange(data=d, mapping=aes(x=Atrazine_Surface_concentration_category, 
                                             y=IRR,ymin=upper, ymax=lower), width=0.2, size=1, color="black", fill="black", shape=22)


d=data.frame(Atrazine_concentration_quantile_category=c("Low","Medium","High", 
                                                        "Low","Medium","High"),
             IRR=c(3.56, 3.43, 4.64, 1.61,3.56,3.76),
             lower=c(1.45, 1.29, 1.81, 0.56,1.26,1.36), 
             upper=c(8.72, 9.09, 11.85, 4.6,10.03,10.37), 
             s=rep(c("Ground_water","Surface_water"),each=3))
ggplot()+geom_pointrange(data=d, mapping=aes(x=Atrazine_concentration_quantile_category,
                                             y=IRR,ymin=upper, ymax=lower, shape=s), 
                         size=1, color="black",fill="black")+
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14))+
  geom_hline(yintercept = 1, linetype = 2)+theme(legend.position="bottom")+
  facet_grid(cols = vars(s), scales="free_x")+
  scale_x_discrete(limits=c("Low","Medium","High"))

#load libraries
library(tidyverse)
library(MASS)
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

################### NEGATIVE BINOMIAL REG MODEL GROUND ATRAZINE AND PED CANCER ################
#negative binomial model with offset term to adjust for population density
fit_gr <- glm.nb(ca_count ~ Ground_cat+Avg..R.Pl.Themes+
                     offset(log(Total_Pead_pop)), data=atr_PCA)


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


#January 09, 2024

#################################################################################### 
#Trophic ecology and habitat use of an overexploited commercial snapper (Lutjanus jocu)
#in a tropical nursery estuary elucidated by stable isotopes
####################################################################################

#Patrick Derviche, Rodrigo F. Bastos, Mario V. Condini, 
#Ester F. Barbosa, Rafael L. Oliveira, Lorena L. Almeida, 
#Sabrina R. Vollrath, Marcelo Soeth, Alexandre M. Garcia, Maurício Hostim-Silva

#################################################
#Summary
#################################################

  #1. Checking data..............................................................line 100
      #1.1 General results
        #Table 1
      #1.2 Scatter plot by Season
      #1.3 Scatter plot by Length
        #Figure 3
      #1.4 General results

  #2. Statistical analysis.......................................................line 320
      #2.1 Seasonality
      #2.2 Ontogeny
      #2.3 ANOVA - Analysis of Variance

  #3. Trophic position...........................................................line 430
      #3.1 Seasonality - Selecting baseline
        #3.1.1 Dry
        #3.1.2 Wet
      #3.2 Trophic Discrimination Factor (TDF)
      #3.3 Seasonality - Bayesian modelling 
        #3.3.1 Dry 
        #3.3.2 Wet
        #3.3.3 Seasonality results
      #3.4 Ontogeny - Selecting baseline
        #3.4.1 Big juvenile dog snappers (>100 mm)
        #3.4.2 Small juvenile dog snappers (<100 mm)
        #3.4.2 All samples
      #3.5 Trophic Discrimination Factor (TDF)
      #3.6 Ontogeny - Bayesian modelling
        #3.6.1 Big dog snappers
        #3.6.2 Small dog snappers
        #3.6.4 Ontogeny results
      #3.7 All samples
            #Figure 4b

  #4. Standard ellipse area – SEA................................................line 890
      #4.1 SEA rKIN
      #4.2 SEAb SIBER
        #4.2.1 SIBER Seasonality
            #Table 2 - TA, SEA e SEAc values
        #4.2.2 SIBER Ontogeny
            #Table 2 - TA, SEA e SEAc values
      #4.3 SEAb
           #Figure 4a
      #4.4 Trophic position results

  #5. Mixture models.............................................................line 1260
      #5.1 Seasonality          
        #5.1.1 Getting the data into simmr
        #5.1.2 Plotting the data in iso-space
        #5.1.3 Running simmr
        #5.1.4 Checking the algorithm converged
        #5.1.5 Checking the model fit
      #5.2 Ontogeny
        #5.2.1 Getting the data into simmr
        #5.2.2 Plotting the data in iso-space
            #Figure 5
        #5.2.3 Running simmr
        #5.2.4 Checking the algorithm converged
        #5.2.5 Checking the model fit
      #5.3 Mixture models results

  #6. Trophic connectivity.......................................................line 1580
        #6.1 Load data
        #6.2 Model
            #Figure 7
        #6.3 Trophic connectivity Results

  #7. Hydroclimatic data.........................................................line 1700
            #Figure 2















#################################################
#1. Checking data
#################################################

# library
library("ggplot2")
library("ggpubr")
library("car")
library("ggpmisc")
library("stats")
library("dplyr")

#Clean R environment 
rm(list = ls())
graphics.off()

#Set directory
setwd("C:/Users/...")

# dataset:
data <-read.csv2("lutjoc57.csv")

summary (data)
data$d13C <- as.numeric(data$d13C)
data$d15N <- as.numeric(data$d15N)
data$CN <- as.numeric(data$CN)
str(data)

##############
##1.1 General results
##############

mean(data$tl)
sd (data$tl)
summary (data)
summary (data$tl)
#A total of 57 individuals of 101.84 ± 51.93 mm (mean ± standard deviation), 
#ranging from 29 mm to 245 mm of TL.

mean(data$d13C)
sd (data$d13C)
mean(data$d15N)
sd (data$d15N)
#The carbon isotopic ratios range (δ13C) was -22.20 ± 1.04‰, 
#while nitrogen isotopic ratios (δ15N) was 12.17 ± 1.04‰. 

table(data$Season)
#Muscle tissue isotope ratios from a total of 57 dog snapper 
#juveniles were analyzed during wet (44) and dry (13) seasons

#Basic histogram
ggplot(data, aes(x=tl)) + 
  geom_histogram()
table(data$Size)
#Based on the body size frequency distribution of the sampled specimens, 
#we defined two size classes: those smaller than 100 mm in total length – TL (n = 29) 
#and those larger than 100 mm TL (n = 28)

#Table 1
#TL
data %>%
  group_by(Season) %>%
  summarise_at(vars(tl), list(mean = mean)) 
data %>%
  group_by(Season) %>%
  summarise_at(vars(tl), list(sd = sd)) 
data %>%
  group_by(Size) %>%
  summarise_at(vars(tl), list(mean = mean)) 
data %>%
  group_by(Size) %>%
  summarise_at(vars(tl), list(sd = sd)) 

#δ15N
data %>%
  group_by(Season) %>%
  summarise_at(vars(d15N), list(mean = mean)) 
data %>%
  group_by(Season) %>%
  summarise_at(vars(d15N), list(sd = sd)) 
data %>%
  group_by(Size) %>%
  summarise_at(vars(d15N), list(mean = mean)) 
data %>%
  group_by(Size) %>%
  summarise_at(vars(d15N), list(sd = sd)) 

#δ13C
data %>%
  group_by(Season) %>%
  summarise_at(vars(d13C), list(mean = mean)) 
data %>%
  group_by(Season) %>%
  summarise_at(vars(d13C), list(sd = sd)) 
data %>%
  group_by(Size) %>%
  summarise_at(vars(d13C), list(mean = mean)) 
data %>%
  group_by(Size) %>%
  summarise_at(vars(d13C), list(sd = sd)) 

summary(data)

#Scatter plot (all data)
d15n<-ggplot(data, aes(x=tl, y=d15N)) + 
  geom_point() +
  stat_poly_eq() +
  theme_minimal(base_size = )+
  geom_smooth(method=glm , color="steelblue", fill="#69b3a2", se=TRUE)
d15n

d13c<-ggplot(data, aes(x=tl, y=d13C)) + 
  geom_point() +
  stat_poly_eq() +
  theme_minimal(base_size = )+
  geom_smooth(method=glm , color="steelblue", fill="#69b3a2", se=TRUE)
d13c

ggplot(data, aes(x=tl, y=CN)) + 
  geom_point() +
  stat_poly_eq() +
  theme_minimal(base_size = )+
  geom_smooth(method=glm , color="steelblue", fill="#69b3a2", se=TRUE)

##############
##1.2 Scatter plot by Season
##############

legend1 <- expression(delta^15~N)

season_tl_d15N<- ggplot(data, aes(x=tl, y=d15N, color=Season)) + 
  geom_point() +
  stat_poly_eq() +
  theme_minimal(base_size = 15)+
  geom_smooth(method=glm)  +
  ylab(legend1)+   xlab("")
season_tl_d15N

legend2 <- expression(delta^13~C)

season_tl_d13C <- ggplot(data, aes(x=tl, y=d13C, color=Season)) + 
  geom_point() +
  stat_poly_eq(label.x = 0.9) +
  theme_minimal(base_size = 15)+
  geom_smooth(method=glm)+
  ylab(legend2)+ xlab("")
season_tl_d13C

#Figure 3ab
ggarrange(season_tl_d13C, season_tl_d15N, common.legend = TRUE, legend="right", ncol=2, nrow = 1, labels = c("A", "B"))

str(data)

season_d13c_d15N<-ggplot(data, aes(x = d13C, y = d15N, color= Season)) +
  geom_point() +
  theme_minimal(base_size = 15)+
  stat_ellipse(level = 0.45)+
  ylab(legend1)+ xlab(legend2)+
  ggtitle("Season")
season_d13c_d15N

##############
##1.3 Scatter plor by lenght
##############

legend1 <- expression(delta^15~N)

size_tl_d15N<- ggplot(data, aes(x=tl, y=d15N, color=Size)) + 
  geom_point() +
  stat_poly_eq() +
  theme_minimal(base_size = 15)+
  geom_smooth(method=glm)  +
  ylab(legend1)+   xlab("Total length (mm)")
size_tl_d15N

legend2 <- expression(delta^13~C)

size_tl_d13C <- ggplot(data, aes(x=tl, y=d13C, color=Size)) + 
  geom_point() +
  stat_poly_eq(label.x = 0.9) +
  theme_minimal(base_size = 15)+
  geom_smooth(method=glm)+
  ylab(legend2)+ xlab("Total length (mm)")
size_tl_d13C

#Figure 3cd
ggarrange(size_tl_d13C, size_tl_d15N, common.legend = TRUE, legend="right", ncol=2, nrow = 1, labels = c("C", "D"))

str(data)

size_d13c_d15N <-ggplot(data, aes(x = d13C, y = d15N, color= Size)) +
  geom_point() +
  theme_minimal(base_size = 15)+
  stat_ellipse(level = 0.45)+
  ylab(legend1)+ xlab(legend2)+
  ggtitle("Body size")
size_d13c_d15N

ggarrange(season_d13c_d15N, size_d13c_d15N,  common.legend = F, legend="bottom", ncol=2, nrow = 1, labels = c("A", "B"))


##############
##1.4 General results
##############

#Dog snapper’s TL was 101.84 ± 51.93 mm (mean ± standard deviation), 
#ranging from 29 mm to 245 mm (Table 1). The carbon isotopic ratio mean (δ13C) 
#was –22.20 ± 1.04 ‰, while the nitrogen isotopic ratio (δ15N) was 12.17 ± 1.04 ‰ (Table 1). 












#################################################
#2. Statistical analysis
#################################################

#Clean R environment 
rm(list = ls())
graphics.off()

#Set directory
setwd("C:/Users/...")

# dataset:
data <-read.csv2("lutjoc57.csv")

summary (data)
data$d13C <- as.numeric(data$d13C)
data$d15N <- as.numeric(data$d15N)
data$CN <- as.numeric(data$CN)
data$tl <- as.numeric(data$tl)
str(data)

##############
##2.1 Seasonality
##############

table(data$Season)

#d15N
anova <- aov(d15N ~ Season, data=data)
summary(anova) #No difference, p value = 0.471

shapiro.test(anova$residuals) #p-value > 0.05, we can assume the normality
leveneTest(anova) #p-value > 0.05, we can assume the normality

#d13C
anova <- aov(d13C ~ Season, data=data)
summary(anova) #Difference, p value < 0.01

shapiro.test(anova$residuals) #p-value > 0.05, we can assume the normality
leveneTest(anova) #p-value > 0.05, we can assume the homogeneity 



##############
## 2.2 Ontogeny
##############

#d15N - TL
anova <- aov(d15N ~ tl, data=data)
summary(anova) #Difference, p value < 0.01

shapiro.test(anova$residuals) #p-value > 0.05, we can assume the normality
plot(anova, which = 1)
plot(anova, which = 2) #we can assume the homogeneity 

#d13C - TL
anova <- aov(d13C ~ tl, data=data)
summary(anova) #Difference, p value = 0.0127

shapiro.test(anova$residuals) #p-value > 0.05, we can assume the normality
plot(anova, which = 1)
plot(anova, which = 2) #we can assume the homogeneity 

#d15N - Size
anova <- aov(d15N ~ Size, data=data)
summary(anova) #Difference, p value < 0.01

shapiro.test(anova$residuals) #p-value < 0.05, we cannot assume the normality
bartlett.test(d15N ~ Size, data = data) #p-value > 0.05, we can assume the homogeneity 

#Transform data
data$d15Nt <- log10(data$d15N)
data$d15Nt<- 1/(data$d15N)
data$d15Nt <- sqrt(data$d15N)
data$d15Nt <-sqrt(max((data$d15N) + 1)-(data$d15N))
anova <- aov(d15Nt ~ Size, data=data)
summary(anova) #Difference, p value < 0.01

shapiro.test(anova$residuals) #p-value > 0.05, we can assume the normality
bartlett.test(d15N ~ Size, data = data) #p-value > 0.05, we can assume the homogeneity 

#d13C - Size
anova <- aov(d13C ~ Size, data=data)
summary(anova) #Difference, p value = 0.0447

shapiro.test(anova$residuals) #p-value > 0.05, we can assume the normality
bartlett.test(d13C ~ Size, data = data) #p-value > 0.05, we can assume the homogeneity 

##############
#2.3 ANOVA results
##############

#Significant differences were observed in δ13C according to the season (p < 0.01), 
#but this pattern was not observed for δ15N (p = 0.47, Fig. 3). In contrast, 
#significant differences were observed in δ13C (p = 0.04) and δ15N (p < 0.01) 
#according to the size classes (Fig. 3)














#################################################
#3. Trophic position
#################################################

#library
install.packages("remotes")
remotes::install_github("AndrewLJackson/tRophicPosition")
library(dplyr)
library(devtools)
library(tRophicPosition)
library(rjags)
library(ggplot2)

#Clean R environment 
rm(list = ls())
graphics.off()

#Set directory
setwd("C:/Users/...")

# dataset
data <-read.csv2("data_tp57.csv")

data <- data[,c("Season","FG","Spp","d13C","d15N","Size")]
data <-na.omit(data)
data$d13C <- as.numeric(data$d13C)
data$d15N <- as.numeric(data$d15N)
str(data)

##############
#3.1 Seasonality - Selecting baseline
##############

#######
#3.1.1 Dry
#######

#d15N and d13C, Dry, Benthic
dNb1 <- data$d15N[which(data$FG == "Baseline_benthic" & data$Season == "Dry")]
dCb1 <- data$d13C[which(data$FG == "Baseline_benthic" & data$Season == "Dry")]

#d15N and d13C, Dry, Pelagic
dNb2 <- data$d15N[which(data$FG == "Baseline_pelagic" & data$Season == "Dry")]
dCb2 <- data$d13C[which(data$FG == "Baseline_pelagic" & data$Season == "Dry")]

#d15N and d13C, Dry, Snapper
dN_Snapper_dry <- data$d15N[which(data$FG == "Snapper"& data$Season == "Dry")]
dC_Snapper_dry <- data$d13C[which(data$FG == "Snapper"& data$Season == "Dry")]

#Combining data dry
Snapper_dry <- list("dNb1" = dNb1, "dCb1" = dCb1, "dNb2" = dNb2,"dCb2" = dCb2, "dNc" = dN_Snapper_dry, "dCc" = dC_Snapper_dry)

#######
#3.1.2 Wet
#######

#d15N and d13C, Wet, Benthic
dNb3 <- data$d15N[which(data$FG == "Baseline_benthic" & data$Season == "Wet")]
dCb3 <- data$d13C[which(data$FG == "Baseline_benthic" & data$Season == "Wet")]

#d15N and d13C, Wet, Pelagic
dNb4 <- data$d15N[which(data$FG == "Baseline_pelagic" & data$Season == "Wet")]
dCb4 <- data$d13C[which(data$FG == "Baseline_pelagic" & data$Season == "Wet")]

#d15N and d13C, Wet, Snapper
dN_Snapper_Wet <- data$d15N[which(data$FG == "Snapper"& data$Season == "Wet")]
dC_Snapper_Wet <- data$d13C[which(data$FG == "Snapper"& data$Season == "Wet")]

#Combining data Wet
Snapper_Wet <- list("dNb1" = dNb3, "dCb1" = dCb3, "dNb2" = dNb4,"dCb2" = dCb4, "dNc" = dN_Snapper_Wet, "dCc" = dC_Snapper_Wet)

##############
#3.2 Trophic Discrimination Factor (TDF) Vanderklift
##############

TDF <- simulateTDF(nN = 15, meanN = 2.5, sdN = 0.2,nC = 15, meanC = 1.7, sdC = 0.2)
#Values based on Stephens, R. B., Shipley, O. N., & Moll, R. J. (2023). analysis and critical review of trophic discrimination factors ( Δ 13 C and Δ 15 N ): Importance of tissue , trophic level and diet source. June, 1–14. https://doi.org/10.1111/1365-2435.14403

#Adding trophic enrichment factors (TEF) for N (deltaN) and C (deltaC) in snapper matrices
Snapper_dry$deltaN <- TDF$deltaN
Snapper_dry$deltaC <- TDF$deltaC
Snapper_Wet$deltaN <- TDF$deltaN
Snapper_Wet$deltaC <- TDF$deltaC

Snapper_dry
Snapper_Wet

##############
#3.3 Seasonality - Bayesian modelling
##############

#######
#3.3.1 Dry 
#######

str(Snapper_dry)

#omit NA
Isotope.Snapper_dry <- lapply(Snapper_dry, na.omit)

#Visualizing the data (biplot without fractionation correction)
screenIsotopeData(Snapper_dry)

#Defining the Bayesian model: using 2 baselines and using both N and C fractionation
model.string <- jagsTwoBaselinesFull()

#Modelling
model.Snapper_dry <- TPmodel(data = Isotope.Snapper_dry,model.string = model.string, n.adapt = 20000, n.chains= 2)

#Sampling and plotting estimates of the posterior distribution of trophic positions
samples.Snapper_dry <- posteriorTP(model.Snapper_dry, n.iter = 20000,  variable.names = c("TP")) #caso queira monitorar o fracionamento, adicionar "muDeltaN"
summary(samples.Snapper_dry)
#Result trophic position Dry: 3.34 ± 0.19 (mean ± SD)

plot(samples.Snapper_dry)
combined <- as.data.frame(coda::mcmc(do.call(rbind, samples.Snapper_dry)))
plotTP(combined, xlab="Snapper_dry")
boxplot(combined, xlab="Snapper_dry")

#Organizing the data (a posteriori estimates) and making a density plot

#Extracting and combining the simulations from TP 1 and 2)
Snapper_dry.TP <- as.data.frame(samples.Snapper_dry[[1]][,"TP"])$var1
Snapper_dry.TP <- c(Snapper_dry.TP,as.data.frame(samples.Snapper_dry[[2]][,"TP"])$var1)

#Creating the data.frame (df) and making the graph
Species <- c(rep("Snapper_dry", length(Snapper_dry.TP)))
df <- data.frame(Snapper_dry.TP, Species)
colnames(df) <- c("TP", "Species")
summary(df)
trophicDensityPlot(df)

#######
#3.3.2 Wet
#######

#omit NA
Isotope.Snapper_Wet <- lapply(Snapper_Wet, na.omit)

#Visualizing the data (biplot without fractionation correction)
screenIsotopeData(Snapper_Wet)

#Defining the Bayesian model: using 2 baselines and using both N and C fractionation
model.string <- jagsTwoBaselinesFull()

#Modelling
model.Snapper_Wet <- TPmodel(data = Isotope.Snapper_Wet,model.string = model.string, n.adapt = 20000, n.chains= 2)

#Sampling and plotting estimates of the posterior distribution of trophic positions
samples.Snapper_Wet <- posteriorTP(model.Snapper_Wet, n.iter = 20000,  variable.names = c("TP"))
summary(samples.Snapper_Wet)
#Result trophic position Wet: 3.28 ± 0.11 (mean ± SD)

plot(samples.Snapper_Wet)
combined <- as.data.frame(coda::mcmc(do.call(rbind, samples.Snapper_Wet)))
plotTP(combined, xlab="Snapper_Wet")
boxplot(combined, xlab="Snapper_Wet")

#Extracting and combining the simulations from TP 1 and 2)
Snapper_Wet.TP <- as.data.frame(samples.Snapper_Wet[[1]][,"TP"])$var1
Snapper_Wet.TP <- c(Snapper_Wet.TP,as.data.frame(samples.Snapper_Wet[[2]][,"TP"])$var1)

#Creating the data.frame (df) and making the graph
Species <- c(rep("Snapper_Wet", length(Snapper_Wet.TP)))
df <- data.frame(Snapper_Wet.TP, Species)
colnames(df) <- c("TP", "Species")
summary(df)
trophicDensityPlot(df)


#######
#3.3.3 Seasonality results
#######

compareTwoDistributions(Snapper_dry.TP, Snapper_Wet.TP, test = ">")

TP <- c(Snapper_dry.TP, Snapper_Wet.TP)
Species <- c(rep("Snapper_dry", length(Snapper_dry.TP)),
             rep("Snapper_Wet", length(Snapper_Wet.TP)))
df <- data.frame(TP, Species)
trophicDensityPlot(df)

df2 <- data.frame(Snapper_dry.TP, Snapper_Wet.TP)
colnames(df2) <- c("Snapper_dry", "Snapper_Wet")
SIBER::siberDensityPlot(df2, xlab = "Species_Seasons",
                        ylab = "Trophic Position")

#write.table(df2,"tp_season.csv", sep=";", dec=".",row.names = F)


##############
#3.4 Ontogeny - Selecting baseline
##############

#######
#3.4.1 Big juvenile dog snappers (>100 mm)
#######

#d15N and d13C, Benthic
dNb1 <- data$d15N[which(data$FG == "Baseline_benthic")]
dCb1 <- data$d13C[which(data$FG == "Baseline_benthic")]

#d15N and d13C, Pelagic
dNb2 <- data$d15N[which(data$FG == "Baseline_pelagic")]
dCb2 <- data$d13C[which(data$FG == "Baseline_pelagic")]

#d15N and d13C, >100 mm, Snapper
dN_Snapper_big <- data$d15N[which(data$FG == "Snapper"& data$Size == ">100 mm")]
dC_Snapper_big <- data$d13C[which(data$FG == "Snapper"& data$Size == ">100 mm")]

#Combining data >100 mm
Snapper_big <- list("dNb1" = dNb1, "dCb1" = dCb1, "dNb2" = dNb2,"dCb2" = dCb2, "dNc" = dN_Snapper_big, "dCc" = dC_Snapper_big)

#######
#3.4.2 Small juvenile dog snappers (<100 mm)
#######

#d15N and d13C, Benthic
dNb3 <- data$d15N[which(data$FG == "Baseline_benthic")]
dCb3 <- data$d13C[which(data$FG == "Baseline_benthic")]

#d15N and d13C, Pelagic
dNb4 <- data$d15N[which(data$FG == "Baseline_pelagic")]
dCb4 <- data$d13C[which(data$FG == "Baseline_pelagic")]

#d15N and d13C, <100 mm, Snapper
dN_Snapper_small <- data$d15N[which(data$FG == "Snapper"& data$Size == "<100 mm")]
dC_Snapper_small <- data$d13C[which(data$FG == "Snapper"& data$Size == "<100 mm")]

#Combining data Wet
Snapper_small <- list("dNb1" = dNb3, "dCb1" = dCb3, "dNb2" = dNb4,"dCb2" = dCb4, "dNc" = dN_Snapper_small, "dCc" = dC_Snapper_small)

#######
#3.4.2 All samples
#######

#d15N and d13C, Benthic
dNb5 <- data$d15N[which(data$FG == "Baseline_benthic")]
dCb5 <- data$d13C[which(data$FG == "Baseline_benthic")]

#d15N and d13C, Pelagic
dNb6 <- data$d15N[which(data$FG == "Baseline_pelagic")]
dCb6 <- data$d13C[which(data$FG == "Baseline_pelagic")]

#d15N and d13C, Snapper
dN_Snapper_all <- data$d15N[which(data$FG == "Snapper")]
dC_Snapper_all <- data$d13C[which(data$FG == "Snapper")]

#Combining data Wet
Snapper_all <- list("dNb1" = dNb5, "dCb1" = dCb5, "dNb2" = dNb6,"dCb2" = dCb6, "dNc" = dN_Snapper_all, "dCc" = dC_Snapper_all)

##############
#3.5 Trophic Discrimination Factor (TDF) Vanderklift
##############

TDF <- simulateTDF(nN = 15, meanN = 2.5, sdN = 0.2,nC = 15, meanC = 1.7, sdC = 0.2)
#Values based on Stephens, R. B., Shipley, O. N., & Moll, R. J. (2023). analysis and critical review of trophic discrimination factors ( Δ 13 C and Δ 15 N ): Importance of tissue , trophic level and diet source. June, 1–14. https://doi.org/10.1111/1365-2435.14403

#Adding trophic enrichment factors (TEF) for N (deltaN) and C (deltaC) in snapper matrices
Snapper_big$deltaN <- TDF$deltaN
Snapper_big$deltaC <- TDF$deltaC
Snapper_small$deltaN <- TDF$deltaN
Snapper_small$deltaC <- TDF$deltaC
Snapper_all$deltaN <- TDF$deltaN
Snapper_all$deltaC <- TDF$deltaC

Snapper_big
Snapper_small
Snapper_all

##############
###3.6 Ontogeny - Bayesian modelling
##############

#######
#3.6.1 Big dog snappers
#######

str(Snapper_big)

#omit NA
Isotope.Snapper_big <- lapply(Snapper_big, na.omit)

#Visualizing the data (biplot without fractionation correction)
screenIsotopeData(Snapper_big)

#Defining the Bayesian model: using 2 baselines and using both N and C fractionation
model.string <- jagsTwoBaselinesFull()

#Modelling
model.Snapper_big <- TPmodel(data = Isotope.Snapper_big,model.string = model.string, n.adapt = 20000, n.chains= 2)

#Sampling and plotting estimates of the posterior distribution of trophic positions
samples.Snapper_big <- posteriorTP(model.Snapper_big, n.iter = 20000,  variable.names = c("TP")) #caso queira monitorar o fracionamento, adicionar "muDeltaN"
summary(samples.Snapper_big)
#Result trophic position Big: 3.57 ± 0.08 (mean ± SD)

plot(samples.Snapper_big)
combined <- as.data.frame(coda::mcmc(do.call(rbind, samples.Snapper_big)))
plotTP(combined, xlab="Snapper_big")
boxplot(combined, xlab="Snapper_big")

#Organizing the data (a posteriori estimates) and making a density plot

#Extracting and combining the simulations from TP 1 and 2)
Snapper_big.TP <- as.data.frame(samples.Snapper_big[[1]][,"TP"])$var1
Snapper_big.TP <- c(Snapper_big.TP,as.data.frame(samples.Snapper_big[[2]][,"TP"])$var1)

#Creating the data.frame (df) and making the graph
Species <- c(rep("Snapper_big", length(Snapper_big.TP)))
df <- data.frame(Snapper_big.TP, Species)
colnames(df) <- c("TP", "Species")
summary(df)
trophicDensityPlot(df)

#######
#3.6.2 Small dog snappers
#######

#omit NA
Isotope.Snapper_small <- lapply(Snapper_small, na.omit)

#Visualizing the data (biplot without fractionation correction)
screenIsotopeData(Snapper_small)

#Defining the Bayesian model: using 2 baselines and using both N and C fractionation
model.string <- jagsTwoBaselinesFull()

#Modelling
model.Snapper_small <- TPmodel(data = Isotope.Snapper_small,model.string = model.string, n.adapt = 20000, n.chains= 2)

#Sampling and plotting estimates of the posterior distribution of trophic positions
samples.Snapper_small <- posteriorTP(model.Snapper_small, n.iter = 20000,  variable.names = c("TP"))
summary(samples.Snapper_small)
#Result trophic position: 3.01 ± 0.09 (mean ± SD)

plot(samples.Snapper_small)
combined <- as.data.frame(coda::mcmc(do.call(rbind, samples.Snapper_small)))
plotTP(combined, xlab="Snapper_small")
boxplot(combined, xlab="Snapper_small")

#Organizing the data (a posteriori estimates) and making a density plot

#Extracting and combining the simulations from TP 1 and 2)
Snapper_small.TP <- as.data.frame(samples.Snapper_small[[1]][,"TP"])$var1
Snapper_small.TP <- c(Snapper_small.TP,as.data.frame(samples.Snapper_small[[2]][,"TP"])$var1)

#Creating the data.frame (df) and making the graph
Species <- c(rep("Snapper_small", length(Snapper_small.TP)))
df <- data.frame(Snapper_small.TP, Species)
colnames(df) <- c("TP", "Species")
summary(df)
trophicDensityPlot(df)

compareTwoDistributions(Snapper_small.TP, Snapper_big.TP, test = ">")

TP <- c(Snapper_small.TP, Snapper_big.TP)
Species <- c(rep("Snapper_small", length(Snapper_small.TP)),
             rep("Snapper_big", length(Snapper_big.TP)))
df <- data.frame(TP, Species)
trophicDensityPlot(df)

#######
#3.6.3 Ontogeny results
#######

compareTwoDistributions(Snapper_big.TP, Snapper_small.TP, test = ">")

TP <- c(Snapper_big.TP, Snapper_small.TP)
Species <- c(rep("Snapper_big", length(Snapper_big.TP)),
             rep("Snapper_small", length(Snapper_small.TP)))
df <- data.frame(TP, Species)
trophicDensityPlot(df)

df2 <- data.frame(Snapper_big.TP, Snapper_small.TP)
colnames(df2) <- c("Snapper_big", "Snapper_small")
SIBER::siberDensityPlot(df2, xlab = "Species_Seasons",
                        ylab = "Trophic Position", ylim = c(2.8,4))
#write.table(df2,"tp_size.csv", sep=";", dec=".",row.names = F)

#######
#3.6.4 All samples
#######

#omit NA
Isotope.Snapper_all <- lapply(Snapper_all, na.omit)

#Visualizing the data (biplot without fractionation correction)
screenIsotopeData(Snapper_all)

#Defining the Bayesian model: using 2 baselines and using both N and C fractionation
model.string <- jagsTwoBaselinesFull()

#Modelling
model.Snapper_all <- TPmodel(data = Isotope.Snapper_all,model.string = model.string, n.adapt = 20000, n.chains= 2)

#Sampling and plotting estimates of the posterior distribution of trophic positions
samples.Snapper_all <- posteriorTP(model.Snapper_all, n.iter = 20000,  variable.names = c("TP"))
summary(samples.Snapper_all)
#Result trophic position: 3.29 ± 0.09 (mean ± SD)

plot(samples.Snapper_all)
combined <- as.data.frame(coda::mcmc(do.call(rbind, samples.Snapper_all)))
plotTP(combined, xlab="Snapper_all")
boxplot(combined, xlab="Snapper_all")

#Extracting and combining the simulations from TP 1 and 2)
Snapper_all.TP <- as.data.frame(samples.Snapper_all[[1]][,"TP"])$var1
Snapper_all.TP <- c(Snapper_all.TP,as.data.frame(samples.Snapper_all[[2]][,"TP"])$var1)

#Creating the data.frame (df) and making the graph
Species <- c(rep("Snapper_all", length(Snapper_all.TP)))
df <- data.frame(Snapper_all.TP, Species)
colnames(df) <- c("TP", "Species")
summary(df)
trophicDensityPlot(df)



##############
###3.7 Trophic position results
##############

#Juvenile dog snappers maintained a very similar trophic position among wet (3.28 ± 0.11)
#and dry (3.34 ± 0.19) seasons (Fig. 4b). On the other hand, larger juveniles had 
#a higher trophic position (3.57 ± 0.08) than the smaller ones (3.01 ± 0.09; Fig 4b).


#######
#Figure 4b
#######
TP <- c(Snapper_dry.TP,Snapper_Wet.TP,Snapper_small.TP, Snapper_big.TP)
Species <- c(rep("Dry", length(Snapper_dry.TP)),
             rep("Wet", length(Snapper_Wet.TP)),
             rep("<100 mm", length(Snapper_small.TP)),
             rep(">100 mm", length(Snapper_big.TP)))
df <- data.frame(TP, Species)
trophicDensityPlot(df)






















#################################################
#4. Standard ellipse area – SEA  
#################################################

# library
library("rKIN")
library(ggpubr)
library(ggplot2)
library(dplyr)

#Clean R environment 
rm(list = ls())
graphics.off()

#Set directory
setwd("C:/Users/...")

##############
#4.1 SEA rKIN
##############

#Standard ellipse area – SEA  
data <-read.csv2("sea57.csv")

data$Group <- as.character(data$Group)
data$d13C <- as.numeric(data$d13C)
data$d15N <-as.numeric(data$d15N)
str(data)

#estimate niche overlap between 2 species using kernel UD
test.elp<- estEllipse(data=data, x="d13C", y="d15N", group="Group",
                      levels=c(40))

#Color pallete "#FA9F99", "#D8A4FF", "#A4C64D","#4DD2D6"
colors = c("#53868B", "#008B00", "#8B1A1A","#00008B")
#Figure 4a
plotKIN(test.elp, 
        alpha = 0.9,
        xlab = expression({delta}^13*C),
        ylab = expression({delta}^15*N),
        xmin = -24.2,xmax = -19.5,ymin = 9.2,ymax = 14.2,
        colors = c("#FA9F99",  "#A4C64D","#4DD2D6","#D8A4FF"))

dat.olp<- calcOverlap(test.elp)
dat.olp

#Dry vs wet: 0.067 and 0.057

#The isotopic niche breadth of the juvenile dog snappers overlapped 6.2% 
#between dry and wet seasons, while did not overlap between 
#larger and smaller individuals (Table 2; Fig. 4a). 


##############
#4.2 SEAb SIBER
##############

#Packages
library(SIBER)
library('viridis')

#Clean R environment 
rm(list = ls())
graphics.off()

#Set directory
setwd("C:/Users/patri/OneDrive/Documentos/UFES/Tese/Stable isotope/4st step SEA")

#Data import
data_siber <- read.csv2("lutjoc57.csv")

str(data_siber)

#######
#4.2.1 SIBER Seasonality
#######

# Reading data
data_season <- data_siber[,c("d13C","d15N","Season")]
community<-rep("1",nrow(data_season))
data_season<-cbind(data_season,community)
colnames(data_season)[1] <- "iso1"
colnames(data_season)[2] <- "iso2"
colnames(data_season)[3] <- "group"
data_season[data_season == "Dry"] <- "1"
data_season[data_season == "Wet"] <- "2"

#Legend
# 1. Dry
# 2. Wet

data_season$iso1 <- as.numeric(data_season$iso1)
data_season$iso2 <- as.numeric(data_season$iso2)
data_season$group <- as.integer(data_season$group)
data_season$community <- as.integer(data_season$community)
str(data_season)
siber_season <- createSiberObject(data_season)

#Scatter plot
plot(data_season$iso1,data_season$iso2,xlab=expression({delta}^13*C~'\u2030'),ylab=expression({delta}^15*N~'\u2030'))
points(data_season$iso1,data_season$iso2,pch=c(21,21)[data_season$group],bg=c("black", "red")[data_season$group], cex= 1.7)
legend("topright", c("Dry", "Wet"),fill=c("black", "red"),lty=0, bty="n")

plotGroupEllipses(siber_season, n = 10000, p.interval = 0.40,
                  lty = 1, lwd = 4)

community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.95, 
                             lty = 1, lwd = 2)
group.hull.args      <- list(lty = 2, col = "grey20")

plotSiberObject(siber_season,
                ax.pad = 2, 
                hulls = F, community.hulls.args, 
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'\u2030'),
                ylab = expression({delta}^15*N~'\u2030'))

group.hulls.args     <- list(lty = 2, col = "grey20")

plotSiberObject(siber_season,
                ax.pad = 2, 
                hulls = F, community.hulls.args = community.hulls.args, 
                ellipses = T, group.ellipses.args = group.ellipses.args,
                group.hulls = T, group.hulls.args = group.hulls.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'\u2030'),
                ylab = expression({delta}^15*N~'\u2030'))

#######
#TA, SEA e SEAc values
#Table 2
#######

group.ML <- groupMetricsML(siber_season)
print(group.ML)
group.ML<-as.data.frame(group.ML)

#Results
#    Dry  1.1      Wet 1.2
#TA   6.363550 11.224050
#SEA  3.162898  2.665637
#SEAc 3.450434  2.729105

#Modelling Siber

#Options for performing model simulations
parms <- list()
parms$n.iter <- 2 * 10^4   # number of iterations to run the model for
parms$n.burnin <- 1 * 10^3 # discard the first set of values
parms$n.thin <- 10     # thin the posterior by this many
parms$n.chains <- 2        # run this many chains

#Define priors
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

#Adjust ellipses via JAGS method
ellipses.posterior_season <- siberMVN(siber_season, parms, priors)

#Calculates and plots the SEA.B ellipses for each group based on the posterior distributions
SEA.B_season <- siberEllipses(ellipses.posterior_season)

siberDensityPlot(SEA.B_season, xticklabels = colnames(group.ML), ylim=c(0,9), 
                 xlab = c("Species"),
                 ylab = expression("SEA Season "),
                 bty = "L",
                 las = 1,
                 main = "SIBER ellipses on each species")

#Add (red x) the values of the adjusted ellipses for as many samples (SEA-c)
points(1:ncol(SEA.B_season), group.ML[3,], col="red", pch = "x", lwd = 2)

#Comparing data
#Statistically compares (direct probability) the sizes of the ellipses
#How many times (%) was the size of one ellipse larger (gt: 'greater than') than the other?

#Dry x Wet
dry.gt.wet <- sum( SEA.B_season[,1] > SEA.B_season[,2] ) / nrow(SEA.B_season)
dry.gt.wet
#Result: 72.55% of higher SEAb

wet.gt.dry <- sum( SEA.B_season[,2] > SEA.B_season[,1] ) / nrow(SEA.B_season)
wet.gt.dry
#Result: 27.45% of smaller SEAb

#Legend 
#Dry 1.1
#Wet 1.2

#Comparing overlap between ellipses
#https://cran.r-project.org/web/packages/SIBER/vignettes/siber-comparing-populations.html

ellipse1 <- "1.1" 
ellipse2 <- "1.2"

#In absolute values (permil)
overlap_season.1_2 <- maxLikOverlap(ellipse1, ellipse2, siber_season, 
                                    p.interval = 0.40, n = 200)
overlap_season.1_2 

#Results SEAb
#Dry: 3.524555
#Wet: 2.787730

#Proportional overlap (%) the areas of ellipses A and A that overlap each other.
#Varies between 0 (completely distinct ellipses) and 1 (completely overlapping ellipses)
prop.of.both.1_2_season <- as.numeric(overlap_season.1_2["overlap"] / (overlap_season.1_2["area.1"] + overlap_season.1_2["area.2"]))
print(prop.of.both.1_2_season)

#SEAb 3.85% between dry and wet seasons

#######
#4.2.2 SIBER Ontogeny
#######

#Reading data
str(data_siber)
data_lenght <- data_siber[,c("d13C","d15N","Size")]
community<-rep("1",nrow(data_lenght))
data_lenght<-cbind(data_lenght,community)
colnames(data_lenght)[1] <- "iso1"
colnames(data_lenght)[2] <- "iso2"
colnames(data_lenght)[3] <- "group"
data_lenght[data_lenght == "<100 mm"] <- "1"
data_lenght[data_lenght == ">100 mm"] <- "2"

#Legend
# 1. <100 mm
# 2. >100 mm

data_lenght$iso1 <- as.numeric(data_lenght$iso1)
data_lenght$iso2 <- as.numeric(data_lenght$iso2)
data_lenght$group <- as.integer(data_lenght$group)
data_lenght$community <- as.integer(data_lenght$community)
str(data_lenght)

#SIBER
siber_lenght <- createSiberObject(data_lenght)

#Scatter plot
plot(data_lenght$iso1,data_lenght$iso2,xlab=expression({delta}^13*C~'\u2030'),ylab=expression({delta}^15*N~'\u2030'))
points(data_lenght$iso1,data_lenght$iso2,pch=c(21,21,21,21)[data_lenght$group],bg=c("black", "red")[data_lenght$group], cex= 1.4)
legend("topright", c("<100 mm", ">100 mm"),fill=c("black", "red"),lty=0, bty="n")

plotGroupEllipses(siber_lenght, n = 10000, p.interval = 0.40,
                  lty = 1, lwd = 4)

community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.95, 
                             lty = 1, lwd = 2)
group.hull.args      <- list(lty = 2, col = "grey20")
plotSiberObject(siber_lenght,
                ax.pad = 2, 
                hulls = F, community.hulls.args, 
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'\u2030'),
                ylab = expression({delta}^15*N~'\u2030'))

#######
#TA, SEA e SEAc values
#Table 2
#######
group.ML <- groupMetricsML(siber_lenght)
print(group.ML)

#Legend 
#<100 1.1 = TA 9.94, SEA 3.14, SEAc 3.26
#>100 1.2 = TA 4.84, SEA 1.57, SEAc 1.62

# 2.2 Modelling Siber

#Options for performing model simulations
parms <- list()
parms$n.iter <- 2 * 10^4   # number of iterations to run the model for
parms$n.burnin <- 1 * 10^3 # discard the first set of values
parms$n.thin <- 10     # thin the posterior by this many
parms$n.chains <- 2        # run this many chains

#Define priors
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

#Adjust ellipses via JAGS method
ellipses.posterior_lenght <- siberMVN(siber_lenght, parms, priors)

#Calculates and plots the SEA.B ellipses for each group based on the posterior distributions
SEA.B_ontogeny <- siberEllipses(ellipses.posterior_lenght)

siberDensityPlot(SEA.B_ontogeny, xticklabels = colnames(group.ML), ylim=c(0,6), 
                 xlab = c("Species"),
                 ylab = expression("Standard Ellipse Area "),
                 bty = "L",
                 las = 1,
                 main = "SIBER ellipses on each species")

#Add (red x) the values of the adjusted ellipses for as many samples (SEA-c)
points(1:ncol(SEA.B_ontogeny), group.ML[3,], col="red", pch = "x", lwd = 2)

#Comparing data
# Statistically compares (direct probability) the sizes of the ellipses
#How many times (%) was the size of one ellipse larger (gt: 'greater than') than the other?

#Small x Big
small.gt.big <- sum( SEA.B_ontogeny[,1] > SEA.B_ontogeny[,2] ) / nrow(SEA.B_ontogeny)
small.gt.big
#Result: 99.5% of higher SEAb

big.gt.small <- sum( SEA.B_ontogeny[,2] > SEA.B_ontogeny[,1] ) / nrow(SEA.B_ontogeny)
big.gt.small
#Result: 0.5% of smaller SEAb

#Legend 
#<100 mm 1.1
#>100 mm 1.2

#Comparing overlap between ellipses
ellipse1 <- "1.1" 
ellipse2 <- "1.2"

#In absolute values (permil)
overlap.length_1_2 <- maxLikOverlap(ellipse1, ellipse2, siber_lenght, 
                                    p.interval = 0.40, n = 200)
overlap.length_1_2 

#Results SEAb
#<100 mm: 3.33
#>100 mm: 1.66

#Proportional overlap (%) the areas of ellipses A and A that overlap each other.
#Varies between 0 (completely distinct ellipses) and 1 (completely overlapping ellipses)
prop.of.both.1_2_length <- as.numeric(overlap.length_1_2["overlap"] / (overlap.length_1_2["area.1"] + overlap.length_1_2["area.2"]))
print(prop.of.both.1_2_length)

#SEAb 0.0% between <100mm and >100mm classes


##############
#4.4 SEA results
##############

#The isotopic niche breadth of the juvenile dog snappers overlapped 6.2 % between 
#dry and wet seasons, whereas there was no overlap between smaller and larger 
#individuals (Table 2; Fig. 4a). SEAc values were higher in the dry season compared 
#to the wet (3.45 and 2.72, respectively; Table 2). Regarding body size classes, 
#smaller individuals showed nearly two-folds higher SEAc values (3.26) 
#compared to larger individuals (1.62; Table 2). The SEAb of dry (3.52) and 
#wet (2.78) seasons, as well as the size classes of smaller (3.32) and 
#larger (1.66) juveniles, corroborated the patterns observed for SEAc values (Table 2). 










#################################################
#5. Mixture models
#################################################

#Clean R environment 
rm(list = ls())
graphics.off()

#Set directory
setwd("C:/Users/...")

#packages
library("simmr")
library(ggpubr)
library("rjags")


##############
#5.1 Seasonality
##############

#######
#5.1.1 Getting the data into simmr
#######

mix_dry <- matrix(c(-20.25,-20.84,-20.07,-20.32,-19.87,-20.80,-21.85,-22.21,-21.70,-20.34,-21.81,-22.00,-22.06,
                    12.17,12.21,12.45,11.72,12.70,13.46,11.32,12.22,12.66,10.63,10.18,10.16,13.99), ncol = 2, nrow = 13)
colnames(mix_dry) <- c("d13C", "d15N")
s_names <- c("Mangrove crab", "Tidal crabs", "Blue crab ", "Shrimps", "Pelagic fish", "Demersal fish")
s_means <- matrix(c(-23.65,-21.59,-19.96,-19.72,-20.01,-18.81,
                    9.24,8.26,9.73,9.14,11.58,10.31), ncol = 2, nrow = 6)
s_sds <- matrix(c(0.56,0.55,0.38,1.43,1.69,1.75,
                  0.58,0.66,0.74,0.92,1.01,2.26), ncol = 2, nrow = 6)

simmr_in_dry <- simmr_load(
  mixtures = mix_dry,
  source_names = s_names,
  source_means = s_means,
  source_sds = s_sds)

mix_wet <- matrix(c(-22.11,-22.74,-23.27,-21.86,-21.83,-22.57,-21.52,-24.55,-22.14,-23.95,
                    -21.50,-22.91,-22.24,-23.87,-23.41,-22.37,-23.98,-22.63,-22.77,-21.61,
                    -23.35,-22.74,-22.13,-21.51,-22.54,-21.96,-22.27,-22.03,-21.83,-22.48,
                    -21.21,-21.52,-24.01,-21.64,-21.33,-22.99,-23.03,-22.90,-21.46,-22.71,
                    -23.55,-21.96,-23.36,-23.12,
                    13.13,12.59,12.97,13.38,12.99,13.70,12.51,12.35,13.07,11.98,13.12,12.78,
                    13.83,12.03,13.48,13.59,11.73,11.91,12.85,11.60,12.10,11.57,11.62,11.98,
                    11.82,11.17,12.05,11.54,11.48,10.30,9.61,9.80,9.87,12.50,13.45,12.31,11.90,
                    12.52,13.04,12.00,12.34,13.44,11.90,12.22), ncol = 2, nrow = 44)


colnames(mix_wet) <- c("d13C", "d15N")
s_names <- c("Mangrove crab", "Tidal crabs", "Blue crab ", "Shrimps", "Pelagic fish", "Demersal fish")
s_means <- matrix(c(-24.77, -17.39, -24.30, -25.31, -23.35, -23.11,
                    8.95, 8.99, 9.78, 10.56, 11.79, 10.58), ncol = 2, nrow = 6)
s_sds <- matrix(c(0.97, 0.49, 2.02, 1.14, 3.10, 3.15,
                  1.38, 0.52, 0.51, 0.58, 0.80, 2.30), ncol = 2, nrow = 6)

simmr_in_wet <- simmr_load(
  mixtures = mix_wet,
  source_names = s_names,
  source_means = s_means,
  source_sds = s_sds)


#######
#5.1.2 Plotting the data in iso-space
#######

plot(simmr_in_dry)

dry<-plot(simmr_in_dry,
          xlab = expression(paste(delta^13, "C",
                                  sep = "" )),
          ylab = expression(paste(delta^15, "N",
                                  sep = "" )),
          title = "Dry season",
          ggargs = c(ylim(7,14.7), xlim(-27,-16)))
dry

plot(simmr_in_wet)

wet<-plot(simmr_in_wet,
          xlab = expression(paste(delta^13, "C",
                                  sep = "" )),
          ylab = expression(paste(delta^15, "N",
                                  sep = "" )),
          title = "Wet season",
          ggargs = c(ylim(7,14.7), xlim(-27,-16)))
wet

ggarrange(dry, wet, common.legend = TRUE, legend="right", ncol=2, nrow = 1, labels = c("A", "B"))

#######
#5.1.3 Running simmr
#######


#MCMC to determine the dietary proportions

#Dry
plot(simmr_in_dry)
simmr_out_dry <- simmr_mcmc(simmr_in_dry)
compare_sources(simmr_out_dry,
                source_names = c(
                  "Mangrove crab", "Tidal crabs", "Blue crab ", "Shrimps", "Pelagic fish", "Demersal fish"))
summary(simmr_out_dry, type = "statistics")

#Results Dry
#               mean    sd
#deviance      96.127 4.795
#Mangrove crab  0.213 0.101
#Tidal crabs    0.115 0.101
#Blue crab      0.130 0.109
#Shrimps        0.099 0.083
#Pelagic fish   0.333 0.190
#Demersal fish  0.110 0.082
#sd[d13C]       0.959 0.375
#sd[d15N]       2.674 0.842

#Wet
plot(simmr_in_wet)
simmr_out_wet <- simmr_mcmc(simmr_in_wet)
compare_sources(simmr_out_wet,
                source_names = c(
                  "Mangrove crab", "Tidal crabs", "Blue crab ", "Shrimps", "Pelagic fish", "Demersal fish"))
summary(simmr_out_wet, type = "statistics")

#Results Wet
#                 mean    sd
#deviance      308.732 4.644
#Mangrove crab   0.058 0.041
#Tidal crabs     0.229 0.038
#Blue crab       0.087 0.058
#Shrimps         0.259 0.093
#Pelagic fish    0.252 0.087
#Demersal fish   0.116 0.062
#sd[d13C]        0.449 0.184
#sd[d15N]        2.205 0.310

#######
#5.1.4 Checking the algorithm converged
#######

summary(simmr_out_dry, type = "diagnostics")
summary(simmr_out_wet, type = "diagnostics")

#######
#5.1.5 Checking the model fit
#######

post_pred_dry <- posterior_predictive(simmr_out_dry)
print(post_pred_dry)

post_pred_wet <- posterior_predictive(simmr_out_wet)
print(post_pred_wet)


##############
#5.2 Ontogeny
##############

#######
#5.2.1 Getting the data into simmr
#######

mix_small <- matrix(c(-20.25,-20.84,-20.07,-20.32,-19.87,-21.70,-20.34,-21.81,-22.00,-21.61,
                      -23.35,-22.74,-22.13,-21.51,-22.54,-21.96,-22.27,-22.03,-21.83,-22.48,
                      -21.21,-21.52,-24.01,-22.99,-23.55,-23.36,-21.85,-23.03,-22.9,
                      12.17,12.21,12.45,11.72,12.70,12.66,10.63,10.18,10.16,11.60,12.10,11.57,
                      11.62,11.98,11.82,11.17,12.05,11.54,11.48,10.30,9.61,9.80,9.87,12.31,
                      12.34,11.90,11.32,11.89,12.52), ncol = 2, nrow = 29)

colnames(mix_small) <- c("d13C", "d15N")
s_names <- c("Mangrove crab", "Tidal crabs", "Blue crab ", "Shrimps", "Pelagic fish", "Demersal fish")
s_means <- matrix(c(
  -24.21, -20.19, -22.13, -23.53, -21.68, -21.66,
  9.09, 8.50, 9.75, 10.29, 11.68, 11.07), ncol = 2, nrow = 6)
s_sds <- matrix(c(0.96,2.11,2.66,2.77,2.98,2.82,
                  1.01,0.69,0.60,0.73,0.89,1.90), ncol = 2, nrow = 6)

simmr_in_small <- simmr_load(
  mixtures = mix_small,
  source_names = s_names,
  source_means = s_means,
  source_sds = s_sds)


mix_big <- matrix(c(-22.21, -22.06, -22.11, -22.74, -23.27, -21.86, -21.83, -22.57, -21.52, -22.14,
                    -21.50, -22.91, -22.24,-23.87, -23.41, -22.37, -23.98, -22.63, -22.77, -21.64,
                    -21.33, -21.96, -23.12, -20.80, -21.46, -22.71,
                    12.22, 13.99, 13.13, 12.59, 12.97, 13.38, 12.99, 13.70, 12.51, 13.07, 13.12,
                    12.78, 13.83, 12.03, 13.48, 13.59, 11.73, 11.91, 12.85, 12.50, 13.45, 13.44,
                    12.22, 13.46, 13.04,12.00), ncol = 2, nrow = 26)
colnames(mix_big) <- c("d13C", "d15N")

simmr_in_big <- simmr_load(
  mixtures = mix_big,
  source_names = s_names,
  source_means = s_means,
  source_sds = s_sds)

#######
#5.2.2 Plotting the data in iso-space
#######

plot(simmr_in_small)
small<-plot(simmr_in_small,
            xlab = expression(paste(delta^13, "C",
                                    sep = "" )),
            ylab = expression(paste(delta^15, "N",
                                    sep = "" )),
            title = "Dog snappers <100 mm",
            ggargs = c(ylim(7,14.7), xlim(-27,-16)))
small

plot(simmr_in_big)
big<-plot(simmr_in_big,
          xlab = expression(paste(delta^13, "C",
                                  sep = "" )),
          ylab = expression(paste(delta^15, "N",
                                  sep = "" )),
          title = "Dog snappers >100 mm",
          ggargs = c(ylim(7,14.7), xlim(-27,-16)))
big
ggarrange(small, big, common.legend = TRUE, legend="bottom", ncol=2, nrow = 1, labels = c("A","B","C", "D"))

#Figure 5
ggarrange(dry, wet, small, big, common.legend = TRUE, legend="bottom", ncol=2, nrow = 2, labels = c("A","B","C", "D"))

#######
#5.2.3 Running simmr
#######

#MCMC to determine the dietary proportions

#Small
simmr_out_small <- simmr_mcmc(simmr_in_small)
compare_sources(simmr_out_small,
                source_names = c(
                  "Mangrove crab", "Tidal crabs", "Blue crab ", "Shrimps", "Pelagic fish", "Demersal fish"))
summary(simmr_out_small, type = "statistics")

#Results
#                 mean    sd
#deviance      187.054 4.600
#Mangrove crab   0.064 0.042
#Tidal crabs     0.053 0.037
#Blue crab       0.089 0.061
#Shrimps         0.113 0.070
#Pelagic fish    0.432 0.117
#Demersal fish   0.249 0.111
#sd[d13C]        0.682 0.301
#sd[d15N]        1.007 0.266

#Big
simmr_out_big <- simmr_mcmc(simmr_in_big)
compare_sources(simmr_out_big,
                source_names = c(
                  "Mangrove crab", "Tidal crabs", "Blue crab ", "Shrimps", "Pelagic fish", "Demersal fish"))
summary(simmr_out_big, type = "statistics")

#Results
#                 mean    sd
#deviance      197.656 4.135
#Mangrove crab   0.156 0.090
#Tidal crabs     0.083 0.054
#Blue crab       0.122 0.076
#Shrimps         0.155 0.083
#Pelagic fish    0.268 0.112
#Demersal fish   0.216 0.099
#sd[d13C]        0.549 0.247
#sd[d15N]        2.750 0.547

#######
#5.2.4 Checking the algorithm converged
#######

summary(simmr_out_small, type = "diagnostics")
summary(simmr_out_big, type = "diagnostics")

#######
#5.2.5 Checking the model fit
#######

post_pred_small <- posterior_predictive(simmr_out_small)
print(post_pred_small)

post_pred_big <- posterior_predictive(simmr_out_big)
print(post_pred_big)

##############
#5.3 Mixture models results
##############

#Overall, the isotopic values of juvenile dog snappers were within the range of
#isotopic variability among prey items (Fig. 5). Our mixing models revealed variations
#in prey assimilation across seasons and body size classes. In the dry season, pelagic 
#fish was the most assimilated prey (33.3 ± 19.0 %), followed by mangrove crabs (21.3 ± 10.1 %),
#blue crabs (13.0 ± 10.9 %), tidal crab (11.5 ± 10.1 %), demersal fish (11.0 ± 8.2 %), 
#and shrimps (9.9 ± 8.3 %; Fig. 6a). While, in the wet season, shrimps were the most 
#assimilated prey (25.9 ± 9.3 %), followed by pelagic fish (25.2 ± 8.7 %), 
#tidal crabs (22.9 ± 3.8 %), demersal fish (11.6 ± 6.2 %), blue crab (8.7 ± 5.8 %), 
#and mangrove crabs (5.8 ± 4.1 %; Fig. 6b). As well, considering demersal and
#pelagic fish together, it was the most representative for both smaller (68.1 %) 
#and larger individuals (48.8 %; Fig. 6cd).














#################################################
#6. Habitat connectivity
#################################################

#Clean R environment 
rm(list = ls())
graphics.off()

#Set directory
setwd("C:/Users/...")

library(MixSIAR)
library(ggplot2)

rm(list=ls())
graphics.off()

##############
#6.1 Load data
##############

#Load mix data
mix.filename <- "lutjoc_mix.csv"
mix <- load_mix_data(filename=mix.filename,
                     iso_names=c("d13C","d15N"),
                     factors=NULL,
                     fac_random=NULL,
                     fac_nested=NULL,
                     cont_effects="tl")

#Load source data
source.filename <- "sources.csv"
source <- load_source_data(filename=source.filename,
                           source_factors=NULL,
                           conc_dep=FALSE,
                           data_type="means",
                           mix)

#Load discrimination/TDF data
discr.filename <- "discrimination.csv"
discr <- load_discr_data(filename=discr.filename, mix)

#Make isospace plot
plot_data(filename = "biplot",
          plot_save_png = TRUE, 
          plot_save_pdf = FALSE, 
          mix = mix,
          source = source, 
          discr = discr)

#Calculate standardized convex hull area
if(mix$n.iso==2) calc_area(source=source,mix=mix,discr=discr)

# Plot your prior
plot_prior(alpha.prior=1,source)

##############
#6.2 Model
##############

#Define model structure and write JAGS model file
model_filename <- "MixSIAR_model.txt"
resid_err <- FALSE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)

#Run the JAGS model ("test" first, then "normal")
jags.1 <- run_model(run="test", mix, source, discr, model_filename, alpha.prior=1)
jags.1 <- run_model(run="normal", mix, source, discr, model_filename,alpha.prior=1)
jags.1 <- run_model(run="long", mix, source, discr, model_filename,alpha.prior=1)

#Show results
output_options <- list(summary_save = TRUE,                 
                       summary_name = "summary_statistics", 
                       sup_post = TRUE,                    
                       plot_post_name = "posterior_density",
                       sup_pairs = TRUE,
                       plot_pairs_name = "pairs_plot",
                       sup_xy = TRUE,
                       plot_xy_name = "xy_plot",
                       gelman = TRUE,
                       heidel = FALSE,  
                       geweke = TRUE,   
                       diag_save = TRUE,
                       diag_name = "diagnostics",
                       indiv_effect = FALSE,       
                       plot_post_save_png = TRUE, 
                       plot_pairs_save_png = TRUE,
                       plot_xy_save_png = TRUE,
                       plot_intervals_save_png = TRUE,
                       diag_save_ggmcmc = FALSE,
                       return_obj = TRUE)


#Process diagnostics, summary stats, and posterior plots
output_JAGS(jags.1, mix, source)

#Plot Figure 6
plot_continuous_var(jags.1, mix, source, output_options, alphaCI=0.2,exclude_sources_below = 0.05)

##############
#6.3 Habitat connectivity results
##############

#Our Bayesian mixing models assessing the habitat use by juvenile dog snappers 
#displayed a good performance, with the Deviance Information Criterion (DIC) 
#value of 48.9. In general, the higher proportions come from the mangrove (73.6 ± 0.06 %)
#compared to the marine (18.5 ± 1.16 %) and the grasses sources (7.9 ± 0.06 %, Fig. 7).
#However, we observed an increase in the mean contribution from the mangrove as the 
#juvenile dog snappers' TL increased (from 65.8 % to 81.4 %), while the contribution
#from the grasses remained relatively stable (from 7.2 % to 7.0 %). In contrast, the 
#mean marine contribution decreased considerably (from 27.0 % to 11.6 %, Fig. 7). 








#################################################
#7. Hydroclimatic data
#################################################

#Clean R environment 
rm(list = ls())
graphics.off()

#Set directory
setwd("C:/Users/patri/OneDrive/Documentos/UFES/Tese/Stable isotope/1st and 2st Data")

library(ggplot2)

rm(list=ls())
graphics.off()

# dataset:
data <-read.csv2("data_weather.csv")
str(data)
summary (data)
data$Precipitation <- as.numeric(data$Precipitation)
data$Temperature <- as.numeric(data$Temperature)

data$Month <- factor(data$Month, levels = c("September","October",
                                            "November","December","January",
                                             "February","March","April",
                                            "May","June", "July",
                                            "August"))
data$season <- ifelse(data$Month %in% c("April", "May", "June", "July", "August", "September"), "dry", "wet")

library(dplyr)
data %>%
  group_by(season) %>%
  summarise_at(vars(Precipitation), list(mean = mean)) 
data %>%
  group_by(season) %>%
  summarise_at(vars(Precipitation), list(sd = sd)) 
data %>%
  group_by(season) %>%
  summarise_at(vars(Temperature), list(mean = mean)) 
data %>%
  group_by(season) %>%
  summarise_at(vars(Temperature), list(sd = sd)) 

rain <- ggplot(data = data,aes(x=Month, y=Precipitation,fill = Month)) + 
  geom_boxplot(fill="slateblue", alpha=0.2)+   theme_minimal(base_size = 15)
rain

temp <- ggplot(data = data,aes(x=Month, y=Temperature,fill = Month)) + 
  geom_boxplot(fill="gray", alpha=0.2)+   theme_minimal(base_size = 15)
temp

#Plot Figure 2
ggarrange(temp, rain, common.legend = F, legend="none", ncol=1, nrow = 2, labels = c("A", "B"))



#END

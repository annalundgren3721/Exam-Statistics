rm(list=ls())
library(ggplot2)
library(MASS)

#Exam Statistic course
dat <- read.csv("C:/Users/Anna/Documents/Statistics course/Exam/exam2023_data.csv")

###############################################################################
#Linear regression

#For this, I only want native plats, exotic plants and eucalyptus
sele_dat <- c(10:19, 26:28)
lin_dat <- dat[,sele_dat]

#I also want to do some new columns
lin_dat$Exotic_Plants <- rowSums(lin_dat[, 1:5], na.rm = TRUE) #summing all the exotic plants coverage
lin_dat$Native_Plants <- rowSums(lin_dat[, 6:10], na.rm=TRUE)  #summing all the native plants covergae
lin_dat$Euc_all <- rowSums(lin_dat[,11:13], na.rm = TRUE)      #summing all the eucalyptus seedlings
lin_dat$Plants <- rowSums(lin_dat[, 14:15], na.rm = TRUE)      #summing exotic and native coverage

#Take away NA
lin_dat <- na.omit(lin_dat)

#Histograms
hist(lin_dat$Exotic_Plants)
hist(lin_dat$Native_Plants)
hist(lin_dat$Euc_all)       

#taking away the outlier
value_to_remove <- 88

#Remove rows with the specific value
lin_dat <- lin_dat[lin_dat$Euc_all != value_to_remove, ]

#Making some plots
y_axis <- seq(0,100, by = 0.01)

ggplot(lin_dat, aes(x = Euc_all)) +
  geom_point(aes(y = Exotic_Plants, color = "Exotic", shape = "Exotic"), col = "maroon2", pch = 1) +
  geom_point(aes(y = Native_Plants, color = "Native", shape = "Native"), col = "darkblue", pch = 1) +
  geom_smooth(aes(y = Exotic_Plants, color = "Exotic"), method = "lm", formula = y ~ x, se = FALSE) +
  geom_smooth(aes(y = Native_Plants, color = "Native"), method = "lm", formula = y ~ x, se = FALSE) +
  geom_smooth(aes(y = Native_Plants + Exotic_Plants, color = "Combined"), method = "lm", se = FALSE) +
  theme_classic() +
  labs(
    title = "",
    x = "Number of Eucalyptus Seedlings",
    y = "Coverage"
  ) +
  scale_color_manual(values = c("Exotic" = "maroon2", "Native" = "darkblue", "Combined" = "darkgrey"), name = "Lines") +
  scale_shape_manual(values = c("Exotic" = 1, "Native" = 2, "Combined" = 3)) + 
  theme(
    legend.position = c(0.85, 0.85),  # Adjust the position
    legend.background = element_rect(color = "black", fill = "white"),  # Outline the legend box
  )

###################################################################################
#Poisson distribution

dat$Euc_all <- rowSums(dat[,26:28], na.rm = TRUE)

########################################################################
#Exotic plants

#Making a new dataset
dat$Exotic_Plants <- rowSums(dat[, 10:14], na.rm = TRUE)

dat_s <- dat[, c(40, 29, 39)]
dat_s <- na.omit(dat_s)

#taking away the outliers
value_to_remove <- 88

# Remove rows with the specific value
dat_s <- dat_s[dat_s$Euc_all != value_to_remove, ]

#Annual precipitation as a mean
dat_s$mcMAP = dat_s$annual_precipitation - mean(dat_s$annual_precipitation, na.rm=T)

#Model
m_exo = glm.nb(Euc_all ~ mcMAP + Exotic_Plants, data = dat_s)
summary(m_exo)

#Plotting the poisson distribution
plot(dat_s$Exotic_Plants, dat_s$Euc_all, col="grey", las=1,
     xlab="Exotic plants cover (%)",
     ylab="Eucalyptus abundance")
newexo = seq(min(dat_s$Exotic_Plants), max(dat_s$Exotic_Plants), length.out=200)
newMAP = rep(mean(dat_s$mcMAP), length(newexo))
y_hat_e = predict(m_exo, newdata=list(mcMAP=newMAP,
                                      Exotic_Plants=newexo),
                  type="response")
lines(newexo, y_hat_e, lwd=2)
newMAP2 = rep(mean(dat_s$mcMAP)+sd(dat_s$mcMAP), length(newexo))
y_hat_exo2 = predict(m_exo, newdata=list(mcMAP=newMAP2,
                                         Exotic_Plants=newexo),
                     type="response")
newMAP3 = rep(mean(dat_s$mcMAP)-sd(dat_s$mcMAP), length(newexo))
y_hat_exo3 = predict(m_exo, newdata=list(mcMAP=newMAP3,
                                         Exotic_Plants=newexo),
                     type="response")
lines(newexo, y_hat_exo2, lwd=2, col="maroon2")
lines(newexo, y_hat_exo3, lwd=2, col="cyan")
legend("topleft", lty=1, lwd=2, col=c(1, "maroon2", "cyan"), bty="n",
       legend=c("MAP = Mean",
                "MAP = Mean + SD",
                "MAP = Mean - SD"))


coefsexo = summary(m_exo)$coef
exp(coefsexo[1,1])

exp(coefsexo[1,1]+coefsexo[3,1])

#pseudo r-square
1-(m_exo$deviance/m_exo$null.deviance)
m_exo$deviance
m_exo$null.deviance


########################################################################
#Native plants

#New row with all the native plants coverage
dat$Native_plants <- rowSums(dat[, 15:19], na.rm = TRUE)

#remaking the dataset, now with native plants instead of exotic plants
dat_s <- dat[, c(41, 29, 39)]
dat_s <- na.omit(dat_s)

#taking away the outliers
value_to_remove <- 88

# Remove rows with the specific value
dat_s <- dat_s[dat_s$Euc_all != value_to_remove, ]

#Annual precipitation as a mean
dat_s$mcMAP = dat_s$annual_precipitation - mean(dat_s$annual_precipitation, na.rm=T)

#Model
m_nat = glm.nb(Euc_all ~ mcMAP + Native_plants, data = dat_s)
summary(m_nat)

#Making the plot
plot(dat_s$Native_plants, dat_s$Euc_all, col="grey", las=1,
     xlab="Native plants cover (%)",
     ylab="Eucalyptus abundance")
newnat = seq(min(dat_s$Native_plants), max(dat_s$Native_plants), length.out=200)
newMAP = rep(mean(dat_s$mcMAP), length(newnat))
y_hat_n = predict(m_nat, newdata=list(mcMAP=newMAP,
                                      Native_plants=newnat),
                  type="response")
lines(newnat, y_hat_n, lwd=2)
newMAP2 = rep(mean(dat_s$mcMAP)+sd(dat_s$mcMAP), length(newnat))
y_hat_nat2 = predict(m_nat, newdata=list(mcMAP=newMAP2,
                                         Native_plants=newnat),
                     type="response")
newMAP3 = rep(mean(dat_s$mcMAP)-sd(dat_s$mcMAP), length(newnat))
y_hat_nat3 = predict(m_nat, newdata=list(mcMAP=newMAP3,
                                         Native_plants=newnat),
                     type="response")
lines(newnat, y_hat_nat2, lwd=2, col="maroon2")
lines(newnat, y_hat_nat3, lwd=2, col="cyan")
legend("topleft", lty=1, lwd=2, col=c(1, "maroon2", "cyan"), bty="n",
       legend=c("MAP = Mean",
                "MAP = Mean + SD",
                "MAP = Mean - SD"))


coefsnat = summary(m_nat)$coef
exp(coefsnat[1,1])

exp(coefsnat[1,1]+coefsnat[3,1])

#pseudo r-square
1-(m_nat$deviance/m_nat$null.deviance)
m_nat$deviance
m_nat$null.deviance


############################################################################
#Model selection

#For this, I only want native plats, exotic plants and eucalyptus
sele_dat <- c(10:19, 26:28,29 )
dat_s <- dat[,sele_dat]

#I also want to do some new columns
dat_s$Exotic_Plants <- rowSums(dat_s[, 1:5], na.rm = TRUE)
dat_s$Native_Plants <- rowSums(dat_s[, 6:10], na.rm=TRUE)
dat_s$Euc_all <- rowSums(dat_s[,11:13], na.rm = TRUE)
dat_s$mcMAP = dat_s$annual_precipitation - mean(dat_s$annual_precipitation, na.rm=T)

#Take away NA and outlier
dat_s <- na.omit(dat_s)

#taking away the outliers
value_to_remove <- 88

# Remove rows with the specific value
dat_s <- dat_s[dat_s$Euc_all != value_to_remove, ]

#Models
m_1 <- lm(Euc_all ~ Exotic_Plants, data = dat_s)
m_2 <- lm(Euc_all ~ Native_Plants, data = dat_s)
m_3 <- lm(Euc_all ~ Native_Plants + Exotic_Plants, data = dat_s)
m_4 <- lm(Euc_all ~ Native_Plants * Exotic_Plants, data = dat_s)
m_5 <- lm(Euc_all ~ annual_precipitation, data = dat)
m_6 <- glm.nb(Euc_all ~ mcMAP + Exotic_Plants, data = dat_s)
m_7 <- glm.nb(Euc_all ~ mcMAP + Native_Plants, data = dat_s)
m_8 <- glm.nb(Euc_all ~ mcMAP + Exotic_Plants + Native_Plants, data = dat_s)
m_9 <- glm.nb(Euc_all ~ mcMAP + Exotic_Plants * Native_Plants, data = dat_s)
m_10 <- lm(Euc_all ~ 1, data = dat_s)


#Making a table for selection, containg df, AIC, delta, logLIK and weight
mlist = list(m_1, m_2, m_3, m_4, m_5, m_6, m_7, m_8, m_9, m_10)
AICTab = AIC(m_1, m_2, m_3, m_4, m_5, m_6, m_7, m_8, m_9, m_10)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab

#summaries needed for tables
summary(m_1)  
summary(m_2)  
summary(m_3) 
summary(m_4)  
summary(m_5)  
summary(m_6)  
summary(m_7)  
summary(m_8)  
summary(m_9) 
summary(m_10)





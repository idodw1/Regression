library(rlang)
library(MASS)
library(fitdistrplus)
library(magrittr)
library(dplyr)
library(lazyeval)
library(parallel)
library(e1071)
library(plotly)
library(ggplot2)
library(triangle)
library(sqldf)
library(simmer)
library(simmer.plot)
library(strucchange)

#2.1
Spotify_Dataset_Ido<-read.csv(file.choose(),header = T) 
cor(Spotify_Dataset_Ido$Spotify, Spotify_Dataset_Ido$youtube.views)
cor(Spotify_Dataset_Ido$Spotify,Spotify_Dataset_Ido$energy)
cor(Spotify_Dataset_Ido$Spotify,Spotify_Dataset_Ido$loudness)
cor(Spotify_Dataset_Ido$Spotify,Spotify_Dataset_Ido$crowd)
cor(Spotify_Dataset_Ido$Spotify,Spotify_Dataset_Ido$speechiness)
cor(Spotify_Dataset_Ido$Spotify,Spotify_Dataset_Ido$acousticness)
cor(Spotify_Dataset_Ido$Spotify,Spotify_Dataset_Ido$tempo)
cor(Spotify_Dataset_Ido$Spotify,Spotify_Dataset_Ido$valence)
cor(Spotify_Dataset_Ido$Spotify,Spotify_Dataset_Ido$duration)

#2.2
cor(Spotify_Dataset_Ido$singer_gender, Spotify_Dataset_Ido$Spotify, method = "spearman")
cor(Spotify_Dataset_Ido$mode, Spotify_Dataset_Ido$Spotify, method = "spearman")

#2.3
model <- lm(Spotify_Dataset_Ido$Spotify ~ Spotify_Dataset_Ido$youtube.views + Spotify_Dataset_Ido$energy + Spotify_Dataset_Ido$loudness 
            + Spotify_Dataset_Ido$crowd + Spotify_Dataset_Ido$speechiness + Spotify_Dataset_Ido$mode + Spotify_Dataset_Ido$acousticness + 
              Spotify_Dataset_Ido$tempo + Spotify_Dataset_Ido$valence + Spotify_Dataset_Ido$duration + Spotify_Dataset_Ido$singer_gender)
bictest <- step(model, k=log(204), test = "F", direction = "backward")

#2.4

ggplot(data = Spotify_Dataset_Ido, aes(x = youtube.views, y = Spotify, color = factor(mode))) + 
       geom_point() +
       geom_smooth(method = "lm", se = FALSE) +
       xlim(0, 4600) +
       ylim(0, 20) +
       xlab("youtube views") +
       ylab("spotify") +
       ggtitle("Spotify by youtube views")

ggplot(data = Spotify_Dataset_Ido, aes(x = loudness, y = Spotify, color = factor(mode))) + 
       geom_point() +
       geom_smooth(method = "lm", se = FALSE) +
       xlim(min(Spotify_Dataset_Ido$loudness), max(Spotify_Dataset_Ido$loudness)) +
       ylim(0, 20) +
       xlab("loudness") +
       ylab("spotify") +
       ggtitle("Spotify by loudness")

ggplot(data = Spotify_Dataset_Ido, aes(x = speechiness, y = Spotify, color = factor(mode))) + 
       geom_point() +
       geom_smooth(method = "lm", se = FALSE) +
       xlim(min(Spotify_Dataset_Ido$speechiness), max(Spotify_Dataset_Ido$speechiness)) +
       ylim(0, 20) +
       xlab("speechiness") +
       ylab("spotify") +
       ggtitle("Spotify by speechiness")

ggplot(data = Spotify_Dataset_Ido, aes(x = tempo, y = Spotify, color = factor(mode))) + 
       geom_point() +
       geom_smooth(method = "lm", se = FALSE) +
       xlim(min(Spotify_Dataset_Ido$tempo), max(Spotify_Dataset_Ido$tempo)) +
       ylim(0, 20) +
       xlab("tempo") +
       ylab("spotify") +
       ggtitle("Spotify by tempo")

ggplot(data = Spotify_Dataset_Ido, aes(x = valence, y = Spotify, color = factor(mode))) + 
       geom_point() +
       geom_smooth(method = "lm", se = FALSE) +
       xlim(min(Spotify_Dataset_Ido$valence), max(Spotify_Dataset_Ido$valence)) +
       ylim(0, 20) +
       xlab("valence") +
       ylab("spotify") +
       ggtitle("Spotify by valence")

ggplot(data = Spotify_Dataset_Ido, aes(x = duration, y = Spotify, color = factor(mode))) + 
       geom_point() +
       geom_smooth(method = "lm", se = FALSE) +
       xlim(min(Spotify_Dataset_Ido$duration), max(Spotify_Dataset_Ido$duration)) +
       ylim(0, 20) +
       xlab("duration") +
       ylab("spotify") +
       ggtitle("Spotify by duration")

#3.1
model <- lm(Spotify_Dataset_Ido$Spotify ~ Spotify_Dataset_Ido$`youtube.views` + Spotify_Dataset_Ido$loudness* Spotify_Dataset_Ido$mode + 
            Spotify_Dataset_Ido$speechiness + Spotify_Dataset_Ido$mode + Spotify_Dataset_Ido$tempo + Spotify_Dataset_Ido$valence + Spotify_Dataset_Ido$duration)
valence_dummy <- factor(Spotify_Dataset_Ido$valence, levels = c("no", "yes"), labels = c(0, 1))
dummy <- factor(Spotify_Dataset_Ido$valence)
dummy <- relevel(dummy,ref = c(1))
summary(model) 

#3.2  
bac.bic<- step(model, k=log(204), test="F", direction = "backward")
MinModel = lm( Spotify ~ 1, data = Spotify_Dataset_Ido)
for.bic <- step(MinModel, direction = "forward", k=log(204), test="F", scope = list(upper = model))
summary(MinModel)

MinModel <- lm( Spotify_Dataset_Ido$Spotify ~ Spotify_Dataset_Ido$`youtube.views` + Spotify_Dataset_Ido$loudness + Spotify_Dataset_Ido$valence )
BIC(MinModel)
summary(MinModel)

#3.2.1
bm <- MinModel
predictedY <- fitted(bm)
residuale <- residuals(bm)
s.e_res <- sqrt(var(residuale))
stand_res <- (residuals(bm)/s.e_res)
plot( predictedY, stand_res, xlab = "predicted value" , ylab = "normalized error")
abline(0,0)

#3.2.2
vec <- Spotify_Dataset_Ido$Spotify
vec <- sort(vec)
third <- round (length(vec)/3)
twoThird <- round (length(vec) * (2/3))
thirdData <- vec[1:third]
twoThirdData <- vec [twoThird:length((vec))]
var.test(x = thirdData, y = twoThirdData, ratio = 1, alternative = c("two.sided"), conf.level = 0.95)

sctest(MinModel, type = "Chow")
ks.test( x=stand_res, y="pnorm", alternative = "two.sided" , exact = NULL)


#3.2.3
qqnorm(stand_res)
abline(a=0, b=1, col = "black")

hist( stand_res, col="grey", xlab = "Normalized residuals", main ="Histogram normalized residuals",  prob = TRUE)
lines(density(stand_res), col = "blue", lwd = 5)

#4
MinModel_Sqrt <- lm(formula = (Spotify_Dataset_Ido$Spotify)^0.5 ~ Spotify_Dataset_Ido$`youtube.views` + Spotify_Dataset_Ido$loudness + Spotify_Dataset_Ido$valence)
summary(MinModel_Sqrt)
BIC(MinModel_Sqrt)

MinModel_Power2 <- lm(formula = (Spotify_Dataset_Ido$Spotify)^2 ~ Spotify_Dataset_Ido$`youtube.views` + Spotify_Dataset_Ido$loudness + Spotify_Dataset_Ido$valence)
summary(MinModel_Power2)
BIC(MinModel_Power2)

MinModel_log0.5 <- lm(formula = log(Spotify_Dataset_Ido$Spotify)/log(0.5) ~ Spotify_Dataset_Ido$`youtube.views` + Spotify_Dataset_Ido$loudness + Spotify_Dataset_Ido$valence)
summary(MinModel_log0.5)
BIC(MinModel_log0.5)

data <- read.csv("Homicide.csv", header = TRUE)
data
# descriptive analysis
names(data)
head(data)
tail(data)
str(data)
install.packages('mosaic')
library(mosaic)
summary(data)
sum(is.na(data))
data[is.na(data)] = 0
sum(is.na(data))
summary(data)
install.packages('DataExplorer')
library(DataExplorer)
plot_boxplot(data, by = 'Time')
install.packages('DescTools')
library(DescTools)
Desc(data, plotit = TRUE)
H_Denmark<-data[data$Country=="Denmark",]
H_Denmark
H_Sweden<-data[data$Country=="Sweden",]
H_Sweden
H_Finland<-data[data$Country=="Finland",]
H_Finland
H_Netherlands<-data[data$Country=="Netherlands",]
H_Netherlands
H_Switzerland<-data[data$Country=="Switzerland",]
H_Switzerland
H_Germany<-data[data$Country=="Germany",]
H_Germany
H_Honduras<-data[data$Country=="Honduras",]
H_Honduras
H_Trinidad<-data[data$Country=="Trinidad and Tobago",]
H_Trinidad
H_Guatemala<-data[data$Country=="Guatemala",]
H_Guatemala
H_Venezuela<-data[data$Country=="Venezuela, RB",]
H_Venezuela
H_Jamaica<-data[data$Country=="Jamaica",]
H_Jamaica
H_ElSalvador<-data[data$Country=="El Salvador",]
H_ElSalvador
# correlation analysis
install.packages("datarium")
install.packages("qqplotr")
install.packages("RVAideMemoire")
install.packages("car")
install.packages("corrplot")
install.packages("tidyverse")
library(datarium)
library(qqplotr)
library(RVAideMemoire)
library(car)
library(corrplot)
library(tidyverse)
dt <- data %>% select(-Time, -Country, -Region)
dt
# correlation for all variables and rounded to 2 decimals
round(cor(dt), digits = 2)
corrplot(cor(dt), method = "number", type = "upper")
# regression analysis
install.packages("caret")
library(caret)
corrplot(cor(dt))
m_1 <-lm(Homicide ~ GDP_PC, dt)
summary.lm(m_1)
m_2 <-lm(Homicide ~ GDP_PC + SETM, dt)
summary.lm(m_2) 
m_3 <-lm(Homicide ~ GDP_PC + SESM, dt)
summary.lm(m_3)# since no much significant difference between the R-squared of m_1 and m_2
plot(Homicide ~ GDP_PC, dt,
     col = "blue",
     main = "Regression: Homicide & GDP Per Capital",
     xlab = "GDP Per Capital",
     ylab = "Homicide Rate")
abline(m_1, col="red")
plot(m_1, 1)
plot(m_1, 2)
plot(m_1, 3)
# Hypothesis testing
H_Europe<-data[data$Region=="Europe",]
H_Europe
shapiro.test(H_Europe$Homicide)
H_Latin<-data[data$Region=="Latin America",]
H_Latin
shapiro.test(H_Latin$Homicide) 
ggplot(mapping = aes(sample=H_Europe$Homicide)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")
ggplot(mapping = aes(sample=H_Latin$Homicide)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")
log_h <- log10(H_Europe$Homicide)
shapiro.test(log_h)
log_hl <- log10(H_Latin$Homicide)
shapiro.test(log_hl)
sqrt_hl <- sqrt(H_Latin$Homicide)
shapiro.test(sqrt_hl)
cube_hl <- H_Latin$Homicide^(1/3)
shapiro.test(cube_hl)
wilcox.test(Homicide ~ Region, data=data)
#ANOVA
boxplot(Homicide ~ Region , data=data)
byf.shapiro(Homicide ~ Region, data=data)
bartlett.test(Homicide ~ Region, data=data)
oneway.test(Homicide ~ Region,data=data, var.equal = FALSE)
# TIME SERIES
install.packages("TTR")
install.packages("forecast")
library(TTR)
library(forecast)
dts <- read.csv("TIME-SERIES.csv", header = FALSE)
datatimeseries <- ts(dts, start = c(2010))
datatimeseries
plot.ts(datatimeseries)
datatimeseriesSMA2 <- SMA(datatimeseries,n=2)
plot.ts(datatimeseriesSMA2)
dtsforcast <- HoltWinters(datatimeseries, gamma=FALSE)
dtsforcast
dtsforcast$fitted
plot(dtsforcast)
dtsforcast$SSE
dtsforcast2 <- forecast(dtsforcast, h=5)
plot(dtsforcast2)
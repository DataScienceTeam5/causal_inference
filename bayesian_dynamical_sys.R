library(plm)
library("readxl")
library(Ecdat)
library("ggpubr")
library(dplyr)
library(mosaic)
datap
data <- read.table(file = "clipboard", sep = "\t", header=TRUE)
data$HDI
data_filtered <-data%>%
select(Country, Year, HDI, Productivity)%>%
filter( Year == "2018")
data_filtered
data[match(data$Year, 2008),]
colnames(data)
plot(ggscatter(data_filtered, x = "Productivity", y = "HDI", 
          add = "reg.line", , cor.method = "pearson",
          xlab = "Productivity", ylab = "HDI", label = "Country", label.rectangle = TRUE))
#text(data_filtered$Productivity, data_filtered$HDI, labels=data_filtered$Country, cex= 0.7)
panel$logProd <- log(panel$Productivity)
panel

 listy <-unique(data$Country)
listy[1]
data$Country_Id <-match(data$Country, listy)
data
panel <- pdata.frame(data, c("Country_Id", "Year"))

bdynsys(panel, 2, 6, panel$HDI,  panel$logProd)
bayfacfig(2, 6,  c(-178.449413855594, -177.11844878525,-177.600786401187, -180.30452263865, -181.393510950107, -182.120064301302)   ,4)
bayfacfig(2, 6,  c(-794.247693416269, -755.361133634555, -766.493888756317, -756.688246551897,-754.881648217339, -755.111656529972) ,4)
bayfacfig(2,6, c( -195.030082042787, -171.274163957911, -179.658775980182,-266.47757149958, -187.192069327813, -310.409260899829),4)

#plot heatmap 
mine.heatmap <- ggplot(data = data, mapping = aes(x = data$Productivity,
                                                       y = data$HDI,
                                                       ) +
  geom_tile() +
  xlab(label = "Sample"))

mine.heatmap

dif_eq<- function(x){
  30 - 0.63* y - 300/(x*y) + 0.0049* y^2 -1.2*10^-05 y^3

}

soln <-integrateODE(dy ~ 30 - 0.63* y - 300/(x*y) + 0.0049* y^2 -1.2&10^-5* y^3, tdur = list(from=0,to=1,dt=.001))


meshgrid(0:1, 0:1, 0.01)







plot(out, lwd=2, main
     ="Logistic equation\nr=0.1, K=1000, N0=10")
data$Productivity
na.omit(data)
heatmap(data$Productivity)
xv <- panel$GDP
yv <- panel$HDI
xwide <- as.matrix(xv)
ywide <- as.matrix(yv)
numTimes <- length(xwide[1, ])
numEntities <- length(xwide[, 1])
tmpxw <- xwide[, -(ncol(xwide))]
tmpyw <- ywide[, -(ncol(ywide))]
allXt <- as.vector(tmpxw)
allYt <- as.vector(tmpyw)
tmpchx <- rowDiffs(xwide)
tmpchy <- rowDiffs(ywide)
tiChXt <- as.vector(tmpchx)
tiChYt <- as.vector(tmpchy)
mx <- mean(allXt, na.rm = TRUE)
my <- mean(allYt, na.rm = TRUE)
#idx <- which(is.na(allXt + allYt + tiChXt + tiChYt))
idx <- which(is.na(allXt + allYt))
allX <- allXt[-idx]
allY <- allYt[-idx]
tiChX <- tiChXt[-idx]
tiChY <- tiChYt[-idx]
xs <- allX/mx
ys <- allY/my
chXs <- tiChX/mx
chYs <- tiChY/my

typeof(datap)

listy <-unique(data$Country)
listy[1]
data$Country_Id <-match(data$Country, listy)
data
data <-na.omit(data) 
data
new <-plm(data$GDP ~data$HDI, panel, model="random")
new
summary(new)
 summary(data)
datap
panel <- pdata.frame(data, c("Country_Id", "Year"))
summary(lm(data$Productivity ~data$Productivity + data$HDI, data))
PANEL
data <-panel
data
panel
data(Wages)
Wages <- pdata.frame(Wages,595)
Wages

data("Produc", package = "plm")
Produc
zz <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
          data = Produc, index = c("state","year"))
summary(zz)
data

summary(plm(data$GDP ~data$HDI, panel, model="random"))
panel
panel_2 <-data(data, package = "plm")

detect.lindep(panel, suppressPrint = FALSE)
is.pseries(panel)
is.pseries(datap)
panel
pvar(datap)
lm.fit(data$Productivity, data$HDI, offset = offset, singular.ok = singular.ok) 
panel <- select(panel, -Country_Id)
summary(diff(panel$Productivity))
length(data$Productivity)
length(data$HDI)
data$Productivity
data$HDI
formula(datap)
data$logProductivity <- log(data$Productivity)

data
any(is.na(data))
any(is.na(data$Productivity))
any(is.na(data$HDI))

any(is.na(t(data$Productivity)))
data

attr(data, 'GDP')

panel
bdynsys(data, 2, 1, data$GDP, data$HDI)
bdynsys(panel, 2, 1, panel$HDI, panel$GDP)

bdynsys(datap, 2, 6, datap$logGDP, datap$DemocrH)
panel
datap
apply(data,1,function(x) sum(is.na(x)))
datap$logGDP
data
all(is.na(data$HDI^trans))
plot_data(datap, datap$logGDP, datap$EmanzV, seq(0, 12, by = 0.5), seq(0, 1, by = 0.1),
          1, 2, 3, 4, 5, 6)

plot_data(datap, datap$logGDP, datap$EmanzV, seq(0, 12, by = 0.5), seq(0, 1, by = 0.1),
          1, 2, 3, 4, 5, 6)
x <- paste(datap$logGDP, datap$DemocrH)
x

sapply(x, as.numeric)
heatmap(x)

bayfacfig(2, 5, c(-5.4534, -5.3955, -5.235, -4.99948, -5.321), 4)

jpeg('rplot.jpg')
bayfacfig(2, 5, c(-5.4534, -5.3955, -5.235, -4.99948, -5.321), 4)

dev.off()
bayfacfig(2, 5, c(-5.4534, -5.3955, -5.235, -4.99948, -5.321), 4)


table(rowSums(is.na(data[data$GDP, data$HDI])))
nrow(data)
ncol(data)
data("Grunfeld", package = "Ecdat")
Grunfeld
grun.re <- plm(inv ~ value + capital, data = Grunfeld, model = "random")
summary(grun.re)
grun.re
bdynsys(Grunfeld, 3, 5, Grunfeld$inv, Grunfeld$value, Grunfeld$capital)
lm.fit()

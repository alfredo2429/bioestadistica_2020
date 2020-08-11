# COLORES 
colores1 <- c(rgb(219/255,84/255,97/255,alpha = 1),
              rgb(255/255,200/255,87/255,alpha = 1),
              rgb(87/255,204/255,153/255,alpha = 1),
              rgb(56/255,163/255,165/255,alpha = 1),
              rgb(98/255,76/255,171/255,alpha = 1)
              )

colores2 <- c(rgb(219/255,84/255,97/255,alpha = .5),
              rgb(255/255,200/255,87/255,alpha = .5),
              rgb(87/255,204/255,153/255,alpha = .5),
              rgb(56/255,163/255,165/255,alpha = .5),
              rgb(98/255,76/255,171/255,alpha = .5)
              )


# 1. IMPORTACION BASE DE DATOS ===========================
# cargar libreria
library(utils)

#importar base de datos
covid19 <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")

# https://ourworldindata.org/coronavirus
owid <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")


#proyectar en hoja de cálculo nueva
View(covid19)
View(owid)


#filtrando datos de Peru
peru <- subset(x = covid19,
               subset = countriesAndTerritories == "Peru")
 
#dimension
dim(covid19)
dim(peru)

# correccion fecha
peru$dateRep <- as.Date(x = as.character(peru$dateRep), 
                        format = "%d/%m/%Y")

# 2. MEDIDAS DE TENDENCIA CENTRAL ===================
# 2.1 Media ----
# 2.1.1 Media de muertes por covid en Peru
mean(peru$deaths)

# 2.1.2 Media de muertes por covid por Pais
# - obtener el total de defunciones por pais
tapply(X = covid19$deaths,
       INDEX = covid19$countriesAndTerritories,
       FUN = sum)

# - obtener la media de defunciones por pais
mean(tapply(X = covid19$deaths,
            INDEX = covid19$countriesAndTerritories,
            FUN = sum)
     )

# - obtener la media de defunciones por pais, considerando solo el 40% de los valres centrales 
mean(tapply(X = covid19$deaths,
            INDEX = covid19$countriesAndTerritories,
            FUN = sum),
     trim = 0.3
      )

# - Anexo 1
write.table(x = sort(tapply(covid19$deaths,covid19$countriesAndTerritories, sum)),
            file = paste0(getwd(),.Platform$file.sep,"chp_2",.Platform$file.sep,"result",.Platform$file.sep,"deathsBYcountry.txt"),
            sep = "\t",col.names = T)

# 2.2 Mediana ----
# - generando base de datos def.tot
def.tot <- tapply(X = covid19$deaths,
                  INDEX = covid19$countriesAndTerritories,
                  FUN = sum)

# - ordenando datos
def.tot <- sort(def.tot)

# - longitud del vector
length(def.tot)

# - mediana
mean(def.tot[c(105,106)])

# - obtenemos la mediana
median(def.tot)

# 2.3 Moda ----
# - tabla de contingencia
table(def.tot)

# - Ordenando tabla de contingencia
sort(table(def.tot))

# - ejemplo
p1 <- c(rep(x = 2, 20), rep(x = 15,20), 3,4,5,6,7,8,9,11,12,13)

sort(table(p1))

hist()
par(mar = c(8,8,8,2))
barplot(sort(def.tot, decreasing = TRUE)[1:20],
        las = 2,
        main = "Número total de defunciones por pais\nCOVID19",
        cex.main = 3,
        col = colores1)
mtext(text = "Paises",side = 1,line = 7,cex = 2)
mtext(text = "Nro. de defunciones",side = 2,line = 5, cex = 2)
mtext(text = "(20 paises con mayor tasa de mortalidad)", side = 3, line = -1, cex = 2)

head(def.tot)
t1 <- subset(x = covid19,
             select = c("countriesAndTerritories","popData2019") )

t1 <- unique(t1)
t1$countriesAndTerritories <- as.character(t1$countriesAndTerritories)
def.tot2 <- data.frame(countriesAndTerritories = names(def.tot),
                       deaths = def.tot)
t1 <- merge(x = t1, y = def.tot2, by = "countriesAndTerritories")
t1$ratio <- (100000*t1$deaths)/t1$popData2019 

t2 <- t1[order(t1$ratio,decreasing = T),]
head(t2,20)
v2 <- t2$ratio
names(v2) <- t2$countriesAndTerritories


barplot(v2[1:20],
        las = 2,
        main = "Tasa de defunción por 100 mil habitantes por pais\nCOVID19",
        cex.main = 3,
        col = colores1)
mtext(text = "Paises",side = 1,line = 7,cex = 2)
mtext(text = "Nro. de defunciones",side = 2,line = 5, cex = 2)
mtext(text = "x 100,000 habitantes",side = 2,line = 4, cex = 1.5)
mtext(text = "(20 paises con mayor tasa de mortalidad)", side = 3, line = -1, cex = 2)

# 2.4 Media ponderada ----
# importando base de datos https://ourworldindata.org/coronavirus
owid <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
dim(owid)
names(owid)

# generando la base de datos tot.deaths
tot.deaths <- tapply(X = owid$total_deaths,INDEX = owid$location,FUN = sum, na.rm = TRUE)
tot.deaths <- data.frame(location = rownames(tot.deaths),
                         deaths = tot.deaths)

# generando la base de datos de poblacion por pais
pop <- unique(subset(x = owid, select = c("location", "population")))

# uniendo las dos bases de datos
tot.deaths <- merge(x = tot.deaths, y = pop, by = "location")
tot.deaths$deathBYpop <- 100000*tot.deaths$deaths/tot.deaths$population
head(tot.deaths)

# calculo de la medias
mean(tot.deaths$deaths, na.rm = T)
mean(tot.deaths$deathBYpop, na.rm = T)
tot.deaths[tot.deaths$location == "Peru",]


# 2.5 Sesgo -----
p1 <- na.exclude(tot.deaths)
p2 <- density(p1$deathBYpop)
plot(p2, las = 1, ylab = "frecuencia", xlab = "Nro de muertes", bty = "l", 
     main = "Tasa de muerte x 100,000 habitantes\ncausada por SARS-CoV-2", 
     col = colores1[1])
polygon(p2, col = colores2[1])

p3 <- rnorm(n = 1000,mean = mean(c(max(p1$deathBYpop),min(p1$deathBYpop))), sd = 200)
lines(density(p3), lwd = 2, col = colores1[3], lty = 1)
polygon(density(p3), col = colores2[3])



hist(p1$deathBYpop,las = 1, ylab = "Nro de paises", xlab = "Nro de muertes", bty = "l", 
     main = "Tasa de muerte x 100,000 habitantes\ncausada por SARS-CoV-2\npor país", 
     col = colores1, 
     breaks = seq(min(p1$deathBYpop),
                  max(p1$deathBYpop),
                  length.out = 50))

hist(p1$deathBYpop,las = 1, ylab = "Frecuencia", xlab = "Nro de muertes", bty = "l", 
     main = "Tasa de muerte x 100,000 habitantes\ncausada por SARS-CoV-2\npor país", 
     col = colores1, 
     breaks = seq(min(p1$deathBYpop),
                  max(p1$deathBYpop),
                  length.out = 50),
     freq = F)
lines(p2, lwd = 2, col = colores1[3], lty = 1)


# 1. Operaciones básicas ------------
# operaciones matemáticas
4+5
20/8
2*15
2^8
5/2
5%%2   # residuo
5%/%2  # entero de la division

# operaciones lógicas
5<3
5<=5
5==3
5!=3
!TRUE
TRUE|FALSE
TRUE&FALSE
T&T
isTRUE(FALSE)
isTRUE(T)

# asignaición de objeto
a <- 1
a
b <- "juan"
b
c <- TRUE
c

# concatenar
d <- c(4,5,7,8,9,5,0)
d
d <- c("juan", "jose", "joaquin")
d

# indice de un vector
d[1]

# reemplazo en un vector
d[1] <- "Alberto"
d

# longitud
length(d)

# repetición
nom <- rep("muestra", 50)
nom

# secuencia
num <- seq(from = 0, to = 50, by = 1)
num

# pegar
codexp <- paste(nom, num, sep = "_")
codexp

# muestreo
pob <- c(rep("rojo",300), rep("verde",500), rep("azul",200))
muestra <- sample(x = pob, size = 100,replace = T)



# 2. Datos estructurados ------------
# tabla de contingencia
table(muestra)
table(muestra)/length(muestra)

table(pob)
table(pob)/length(pob)

# data.frame
df <- data.frame(nombres = c("Juan", "Jose", "Alberto"),
                 edad = c(22,23,21),
                 origen = c("MX", "PE", "USA"))
df

# matriz
mt <- matrix(data = seq(1,12,1),nrow = 3,ncol = 4,byrow = T)
mt

# array (arreglos)
a1 <- array(data = seq(1,24,1),dim = c(3,4,2))
a1

#lista
lista <-list(a, b, num, nom, df, mt, a1) 
lista

lista <-list(numero = a, 
             nombre = b,
             numeros = num,
             nombres = nom,
             data.frame = df,
             matriz = mt,
             arreglo = a1) 
lista$numero
lista$nombre
lista$data.frame
lista$arreglo


# 3. Importacion de datos ------------------------
# importanto datos de fuente remota
# Fuente: European Centre for Disease Prevention and Control (https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide)

#these libraries need to be loaded
library(utils)

#read the Dataset sheet into “R”. The dataset will be called "data".
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")


# 4. Exploracion basica de base de datos ------------------------
# primera lineas
head(data)

# ultimas lineas
tail(data)

# estructura de la base de datos
str(data)

# resumen por columna
summary(data)

# nombres de columnas
colnames(data)
names(data)

# dimesion
dim(data)

# numero de filas
nrow(data)

# numero de columnas
ncol(data)

# proyectar la base de datos como excel
View(data)

# llamar a una columna
data$dateRep
data[,1]

# llamar a una fila
data[1,]

# apuntar a una ubicacion especifica
data[1,1]
data$dateRep[1]

# niveles por categoria
levels(data$countriesAndTerritories)

# subgrupo de una base de datos
subset(x = data, subset = countriesAndTerritories == "Peru")

# subgrupo multiples valores por variable
subgroup <- subset(x = data, subset = countriesAndTerritories == "USA" |
                          countriesAndTerritories =="Peru" |
                          countriesAndTerritories == "Italy" |
                          countriesAndTerritories ==  "Mexico" |
                          countriesAndTerritories == "China")

# Otra forma de hacer subgrupos
peru <- subgroup[subgroup$countriesAndTerritories =="Peru",]
italy <- subgroup[subgroup$countriesAndTerritories =="Italy",]
mex <- subgroup[subgroup$countriesAndTerritories =="Mexico",]
china <- subgroup[subgroup$countriesAndTerritories =="China",]
usa <- subgroup[subgroup$countriesAndTerritories =="USA",]

# operaciones con bases de datos
tapply(X = subgroup$deaths,INDEX = subgroup$countriesAndTerritories,FUN = sum)  # se conservan los niveles, aun los que no contengan datos
aggregate(formula = subgroup$deaths~subgroup$countriesAndTerritories,data = subgroup,FUN =  sum)

# limpiar niveles no utilizados
subgroup$countriesAndTerritories <- droplevels(subgroup$countriesAndTerritories) 

# datos limpios
tapply(subgroup$deaths,subgroup$countriesAndTerritories,sum)  


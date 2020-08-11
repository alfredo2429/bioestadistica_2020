# GENERALIDADES

# 1. Muestra y población
# 
pob <- c(rep("red",300), rep("green",500), rep("blue",200))
db <- data.frame(muestra = paste(rep("muestra",100), seq(from = 1, to = 100, by = 1), sep = "_" ),
                 color = sample(x = pob,size = 100))

pie(x = table(pob),
    labels = names(table(pob)),
    col = c("blue","red","green"))

pie(x = table(db$color),
    labels = names(table(db$color)),
    col = c("blue","red","green"))

plot(x = runif(n = 100, min = 0,max = 1),
     y = runif(n = 100, min = 0, max = 1),
     type = "p",
     pch = 16,
     col = db$color)

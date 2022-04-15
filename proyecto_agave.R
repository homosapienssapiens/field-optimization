library(genalg)

getwd()
setwd('C:/Users/ibane/OneDrive/Escritorio/Proyecto Agave')

parcelas <- read.csv('parcelas.csv', stringsAsFactors = F)
colnames(parcelas)
colnames.clean <- c('id.parcela','area.cultivada','area','bounds',
                    'convex','area.ha','centroid.x','centroid.y')
colnames(parcelas) <- colnames.clean
head(parcelas)

n <- nrow(parcelas)

area.diaria.objetivo <- 200

distancia.parcelas <- read.csv('distancia_parcelas.csv', stringsAsFactors = F)
distancia.parcelas$X <- NULL


#FUNCIÓN FITNESS
fitness.1 <- function(c) {
  #print(c)
  resultado <- 0
  #area
  area.total <- c %*% parcelas$area.cultivada
  delta <- area.total / area.diaria.objetivo
  if (delta > 1.005) {
    resultado <- area.diaria.objetivo * exp(delta^3 - 1) 
  } 
  if (delta < .995) {
    resultado <- area.diaria.objetivo * exp(1 - delta^2) 
  }
  #distancia
  if (sum(c)<=1) {
    resultado <- resultado*1000
  } else {
    
    parcelas.medir <- parcelas[c==1,]
    parcelas.distancia <- parcelas.medir$id.parcela+1
    distancia.total <- 0
    rango <-seq(1,length(parcelas.distancia)-1)
    for (i in rango) {
      distancia.total <- distancia.total + distancia.parcelas[parcelas.distancia[i],parcelas.distancia[i+1]]
    }
    #print(distancia.total)
    if (distancia.total>35000) {
      distancia.total <- distancia.total*2/100 + resultado
    }
    resultado <- resultado + distancia.total/100
  }
  return(resultado)
}



#SABER AREA, DISTANCIA, parcelas X DÍA
best %*% parcelas$area.cultivada

parcelas.medir <- parcelas[best==1,]
parcelas.distancia <- parcelas.medir$id.parcela+1
distancia.total <- 0
rango <-seq(1,length(parcelas.distancia)-1)
for (i in rango) {
  distancia.total <- distancia.total + distancia.parcelas[parcelas.distancia[i],parcelas.distancia[i+1]]
}
distancia.total

index.parcelas <- parcelas$id.parcela[best==1]
index.parcelas


areas <- numeric()
distancias <- numeric()
index.parcela <- numeric()

serie <- c(1,2,3,4,5)
###CHECAR RBIND y RBINDC
for (i in serie) {
  ga.parcelas <- rbga.bin(size=nrow(parcelas), popSize=nrow(parcelas)*10,
                          mutationChance=.01,
                          elitism=as.integer(nrow(parcelas)*10 / 5), 
                          iters=100, 
                          evalFunc=fitness.1 ,
                          verbose = TRUE)
  
  best <- ga.parcelas$population[ga.parcelas$evaluations == min(ga.parcelas$best),][1,]
 
  #Calculo de param por dia
  ##area <- best %*% parcelas$area.cultivada
  ##paste(areas,area)
  parcelas.medir <- parcelas[best==1,]
  parcelas.distancia <- parcelas.medir$id.parcela+1
  distancia.total <- 0
  rango <-seq(1,length(parcelas.distancia)-1)
  for (i in rango) {
    distancia.total <- distancia.total + distancia.parcelas[parcelas.distancia[i],parcelas.distancia[i+1]]
  }
  ##paste(distancias,distancia.total)
  
  index.parcelas <- parcelas$id.parcela[best==1]
  ##paste(index.parcela,index.parcelas)
  
  #Empezar dia siguiente
  parcelas.dia.siguiente <- parcelas[best==0,]
  parcelas <- parcelas.dia.siguiente
}



#DIA 1 [1  2  5 19 20 28 33][199.1ha][41.2km]

ga.parcelas <- rbga.bin(size=nrow(parcelas), popSize=nrow(parcelas)*10,
                        mutationChance=.01,
                        elitism=as.integer(nrow(parcelas)*10 / 5), 
                        iters=100, 
                        evalFunc=fitness.1 ,
                        verbose = TRUE)

best <- ga.parcelas$population[ga.parcelas$evaluations == min(ga.parcelas$best),][1,]
best


#DIA 2 [4  6  8  9 17 18 24 26][200.5ha][57.2km]

parcelas.dia.2 <- parcelas[best==0,]
parcelas <- parcelas.dia.2

ga.parcelas <- rbga.bin(size=nrow(parcelas), popSize=nrow(parcelas)*10,
                        mutationChance=.01,
                        elitism=as.integer(nrow(parcelas)*10 / 5), 
                        iters=100, 
                        evalFunc=fitness.1 ,
                        verbose = TRUE)

best <- ga.parcelas$population[ga.parcelas$evaluations == min(ga.parcelas$best),][1,]
best


#DIA 3 [6 22 33][199.7ha][6.4km]

parcelas.dia.3 <- parcelas[best==0,]
parcelas <- parcelas.dia.3

ga.parcelas <- rbga.bin(size=nrow(parcelas), popSize=nrow(parcelas)*10,
                        mutationChance=.01,
                        elitism=as.integer(nrow(parcelas)*10 / 5), 
                        iters=100, 
                        evalFunc=fitness.1 ,
                        verbose = TRUE)

best <- ga.parcelas$population[ga.parcelas$evaluations == min(ga.parcelas$best),][1,]
best

#DIA 4 [1  9 19 24 31][199.8ha][13.34km]

parcelas.dia.4 <- parcelas[best==0,]
parcelas <- parcelas.dia.4

ga.parcelas <- rbga.bin(size=nrow(parcelas), popSize=nrow(parcelas)*10,
                        mutationChance=.01,
                        elitism=as.integer(nrow(parcelas)*10 / 5), 
                        iters=100, 
                        evalFunc=fitness.1 ,
                        verbose = TRUE)

best <- ga.parcelas$population[ga.parcelas$evaluations == min(ga.parcelas$best),][1,]
best

#DIA 5 [1  4  5  8 15 16 18 21 23 26 27 29 32][155.7ha][60.8km]

parcelas.dia.5 <- parcelas[best==0,]
parcelas <- parcelas.dia.5

ga.parcelas <- rbga.bin(size=nrow(parcelas), popSize=nrow(parcelas)*10,
                        mutationChance=.01,
                        elitism=as.integer(nrow(parcelas)*10 / 5), 
                        iters=100, 
                        evalFunc=fitness.1 ,
                        verbose = TRUE)

best <- ga.parcelas$population[ga.parcelas$evaluations == min(ga.parcelas$best),][1,]
best

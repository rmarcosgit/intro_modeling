library(dplyr)
library(tidyr)
library(factoextra)
library(NbClust)
library(fmsb)
library(sf)
library(ggplot2)
library(forcats)
library(scales)
library(trend)

#READING AND PREPARING DATA
fires <- read.csv2('data/fires.csv')
grid <- read_sf('data/Fires_grid_CCAA.shp')

# TIME SERIES ANALYSIS #########

fires.year <- fires %>%
  filter(YEAR>1974) %>%
  group_by(YEAR) %>%
  summarise(N=n(),BA=sum(BAREA))
  
ggplot(data=fires.year) +
    geom_bar(aes(x=YEAR,y=BA/50),
           stat = 'identity',fill='red') +
  geom_line(aes(x=YEAR,y=N),color = 'blue') +
  scale_y_continuous(sec.axis = sec_axis(~.*50, name = "Burned area (ha)")) +
  # scale_colour_manual(values = c("red", "blue")) +
  geom_vline(xintercept = 1994,lty=2)+
  labs(y = "Number of fires",
       x = "",
       colour = "") +
  theme_bw() +
  theme(legend.position = 'bottom')

#Trend tests for N and BA: signicant decrease in BA
#Major breakpoint aproximately in 1994 (pettit 17-20, 1991-1994)

mk.test(fires.year$N)
mk.test(fires.year$BA)
pettitt.test(fires.year$BA) 

# MAPPING FIRE REGIMES AT PRESENT #########

#Create fire features
features_1 <- fires %>%
  filter(YEAR>1994,BAREA>1) %>%
  group_by(CODCUAD, YEAR,.drop = F) %>%
  summarise(N = n(),BA = sum(BAREA)) %>%
  group_by(CODCUAD,.drop = F) %>%
  summarise(N = mean(N),BA = mean(BA))

features_2 <- fires %>%
  filter(YEAR>1994,BAREA>1) %>%
  filter(CAUSA == 1) %>%
  group_by(CODCUAD, YEAR, MONTH, .drop = F) %>%
  summarise(NL= n(),BAL=sum(BAREA))%>%
  group_by(CODCUAD,.drop = F) %>%
  summarise(BAL = mean(BAL))

features_3 <- fires %>%
  filter(YEAR>1994,BAREA>1) %>%
  filter(BAREA>100) %>%
  group_by(CODCUAD,YEAR,MONTH,.drop = F) %>%
  summarise(BA100 = mean(BAREA))%>%
  group_by(CODCUAD,.drop = F) %>%
  summarise(BA100 = mean(BA100))

features_4 <- fires %>%
  filter(YEAR>1994,BAREA>1) %>%
  filter(MONTH < 5 | MONTH>9) %>%
  group_by(CODCUAD, YEAR,.drop = F) %>%
  summarise(NW=n())%>%
  group_by(CODCUAD,.drop = F) %>%
  summarise(NW = mean(NW))

features.present <- left_join(features_1,features_2,by='CODCUAD') %>%
  left_join(features_3,by='CODCUAD') %>%
  left_join(features_4,by='CODCUAD') 

# features.present[is.na(features.present)] <- 0
# features.present$NW <- features.present$NW/features.present$N
features.present[is.na(features.present)] <- 0

#CLUSTERING

#PCA analysis
pca <- prcomp(features.present[,2:6],scale=T,center = T)
summary(pca)
pca$rotation

cluster.data <- pca$x[,1:3]

#Cluster parameters
d <- "canberra" 
m <- "ward.D2"
index <- "silhouette"

#Optimizing cluster model
cluster.present <- NbClust(cluster.data,index= index, distance = d, 
                    min.nc = 3,
                    max.nc = 3, method = m)

features.present$Cluster <- cluster.present$Best.partition
grid.present <- left_join(grid,features.present,
                          by=c('Cod_Cuad' ='CODCUAD'))

map.present <- ggplot(grid.present,aes(fill=factor(Cluster))) +
  geom_sf(color=NA) +
  theme_bw()

# normalized <- function(x){
#   (x-min(x))/(max(x)-min(x))
# }

features.cluster <- features.present %>%
  na.omit() %>%
  group_by(Cluster) %>%
  # summarise_at(vars(N:NW),mean) %>%
  mutate_at(vars(N:NW),log) %>%
  # summarise_at(vars(N:NW),mean) %>%
  gather('vars','values',2:6)

features.cluster$vars <- fct_relevel(as.factor(features.cluster$vars),'N','BA','BA100','BAL','NW')

ggplot(features.cluster,aes(x=factor(Cluster),y=values,fill=vars)) +
  geom_boxplot(outlier.shape = NA) +
  # geom_bar(stat = 'identity') +
  facet_wrap(~vars, scales = 'free_y') +
  theme_bw()

features.present$Cluster <- recode(as.character(features.present$Cluster),
                                   '1' = '3', '2' ='1', '3' = '2' )

grid.present <- left_join(grid,features.present,
                          by=c('Cod_Cuad' ='CODCUAD'))

map.present <- ggplot(na.omit(grid.present),aes(fill=factor(Cluster))) +
  geom_sf(color=NA) +
  scale_fill_manual(name = '',
                    values = c('1' = '#45b39d','2'='#f9e79f','3'='#cd6155')) +
  theme_bw()

# CLASSIFICATION INTO THE PAST #########

#Create fire features
features_1 <- fires %>%
  filter(YEAR < 1994, BAREA > 1) %>%
  group_by(CODCUAD, YEAR, .drop = F) %>%
  summarise(N = n(), BA = sum(BAREA)) %>%
  group_by(CODCUAD, .drop = F) %>%
  summarise(N = mean(N), BA = mean(BA))

features_2 <- fires %>%
  filter(YEAR < 1994, BAREA > 1) %>%
  filter(CAUSA == 1) %>%
  group_by(CODCUAD, YEAR, MONTH, .drop = F) %>%
  summarise(NL = n(), BAL = sum(BAREA)) %>%
  group_by(CODCUAD, .drop = F) %>%
  summarise(BAL = mean(BAL))

features_3 <- fires %>%
  filter(YEAR < 1994, BAREA > 1) %>%
  filter(BAREA > 100) %>%
  group_by(CODCUAD, YEAR, MONTH, .drop = F) %>%
  summarise(BA100 = mean(BAREA)) %>%
  group_by(CODCUAD, .drop = F) %>%
  summarise(BA100 = mean(BA100))

features_4 <- fires %>%
  filter(YEAR < 1994, BAREA > 1) %>%
  filter(MONTH < 5 | MONTH > 9) %>%
  group_by(CODCUAD, YEAR, .drop = F) %>%
  summarise(NW = n()) %>%
  group_by(CODCUAD, .drop = F) %>%
  summarise(NW = mean(NW))

features.past <-
  left_join(features_1, features_2, by = 'CODCUAD') %>%
  left_join(features_3, by = 'CODCUAD') %>%
  left_join(features_4, by = 'CODCUAD')

# features.present[is.na(features.present)] <- 0
# features.past$NW <- features.past$NW / features.past$N
features.past[is.na(features.past)] <- 0  
    
library(knnGarden)
library(class)
    
# knn.past <- knnVCN(TrnX = features.present[,2:6],
#                 OrigTrnG = features.present$Cluster,
#                 TstX = features.past[,2:6],
#                 ShowObs=FALSE,K=1,method="canberra",p = 2)

knn.past <- knn(train = features.present[,2:6], 
                test = features.past[,2:6],
                cl = features.present$Cluster, k=5)

features.past$Cluster <- knn.past
grid.past <- left_join(grid,features.past,
                          by=c('Cod_Cuad' ='CODCUAD'))

map.past <- ggplot(na.omit(grid.past),aes(fill=factor(Cluster))) +
  geom_sf(color=NA) +
  scale_fill_manual(name = '',
                    values = c('1' = '#45b39d','2'='#f9e79f','3'='#cd6155')) +
  theme_bw()

cv <-knn.cv(train = features.present[,2:6],k = 5,
       cl = features.present$Cluster)

caret::confusionMatrix(as.factor(cv),as.factor(features.present$Cluster))

tm <- as.data.frame(table(past = features.past$Cluster,
      present = features.present$Cluster))

ggplot(tm, aes(x=as.numeric(present),y=as.numeric(past),fill=Freq)) +
  geom_tile(color='white') +
  scale_fill_viridis_c(option = 'D') +
  scale_y_reverse(name='Past') +
  scale_x_continuous(name = 'Present', position = "top") +
  geom_text(aes(label=Freq),color='white') +
  theme_minimal() +
  theme(panel.grid = element_blank())

library(ggpubr)
ggarrange(map.past, map.present,
          labels = c("Past", "Current"),
          ncol = 2)

# MODELING FIRE REGIME CHANGE ###############

variables <- read.csv2('data/variables.csv')
clusters <- cbind(features.past[,c(1,7)],features.present[,7])
names(clusters)[2:3] <- c('Past','Present')
clusters$Change <- paste(clusters$Past,clusters$Present,sep = '-')

data.regresion <- left_join(variables,clusters,by=c('Cod_Cuad' = 'CODCUAD'))

# Why do fire activity increased? Model 1-3 vs 1-1

data.model <- filter(data.regresion,Change=='2-3' | Change=='2-2')
data.model <- mutate(data.model,Change = ifelse(Change=='2-3',1,0))

# Split into training and testing
set.seed(100)
training.rows <- sample(row(data.model),floor(nrow(data.model)*0.7))
data.train <- data.model[training.rows,]
data.test <- data.model[-training.rows,]

#Analizing collinearity. Important in logit models
library(corrplot)

M<-cor(data.model[,-c(1,12:14)])
sig <- cor.mtest(data.model[,-c(1,12:14)], conf.level = .95)

corrplot.mixed(M,
               # p.mat = sig$p, sig.level = .1,
               lower = "color", 
               upper = "number", 
               tl.col = "black")

model.logit  <- glm(Change~wuiCh+elev+slope+dempot+temp+prec+roads,
                    data=data.train,
                    family = binomial())

summary(model.logit)

# Model evaluation with test sample
library(dismo)

eval.logit<-evaluate(p=data.test[data.test$Change==1,],
                     a=data.test[data.test$Change==0,],
                     model=model.logit)

par(mfrow=c(1,3))
plot(eval.logit,'ROC')
boxplot(eval.logit, col = c('#45b39d', '#cd6155'))
density(eval.logit)

library(randomForest)

model.rf  <- randomForest(Change~wai90+elev+slope+dempot+prec+roads,
                    data=data.train)

model.rf

# Model evaluation with test sample
eval.rf<-evaluate(p=data.test[data.test$Change==1,],
                     a=data.test[data.test$Change==0,],
                     model=model.rf)

par(mfrow=c(1,3))
plot(eval.rf,'ROC')
boxplot(eval.rf, col = c('#45b39d', '#cd6155'))
density(eval.rf)

varImpPlot(model.rf)

library(plotmo)
plotmo(model.rf)



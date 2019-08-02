

library(reshape2)
library(knitr)
library(kableExtra)
library(htmltools)
library(dplyr)
library(tictoc)
library(tidyr)    
library(viridis)   
library(ggplot2)   
library(gtable) 
library(ggfortify)
library(factoextra)
library(plotly)
library(gridExtra)
library(grid)
library(MatchIt)
library(ROracle)

con <- dbConnect(Oracle(), user="expeam", password="!febrero2019", dbname="DWH/dwh_olap")


###############
####CONTROL###
###############

tic()
query <- dbSendQuery(con,"
                     
                     
                     
                      select t.ar_sscrbr_dd,t.p,b.engagement_segment
                      from EXPEAM.T_ANTICHURN_10_FEB_19 t
                     join expeam.base_exacaster_antichurn b on t.ar_sscrbr_dd = '0'||b.sub_msisdn
                     
                     
                     
                     ")
df_bkp <- fetch(query)
toc()
df_control<-df_bkp

length(unique(df_control$AR_SSCRBR_DD))
df_tg<-df_control

#####################
####BRAKETS - CUT - break - quantiles - cuantiles###
#####################

options(scipen=999)
breaks<-3
df_tg$P_sgm<- cut( df_tg$P, breaks = breaks,dig.lab=4,ordered_result = FALSE)

breaks<-data.frame(classIntervals(my_data$HS_SIN_SALDO,n=3)[2])[,1]
df_tg$HS_SIN_SALDO_sgm<- cut( df_tg$P, breaks = breaks,dig.lab=4,ordered_result = FALSE)

#CUT POR QUANTILES - PRETTY PRINT
a<-c(1,10,100,1000,100000,1000000)
my_data$HS_SIN_SALDO_SGM<-cut(my_data$HS_SIN_SALDO,breaks=data.frame(classIntervals(my_data$HS_SIN_SALDO,n=3)[2])[,1],include.lowest=T,dig.lab=10)
table(my_data$HS_SIN_SALDO_SGM)


##################
####INSERTAR BASE
##################
base.insertar <-df_tg
rs <- dbSendQuery(con, "truncate table expeam.TMP_ANTICH_EXACASTER_SHELDON")
dbCommit(con)
rs <- dbSendQuery(con, "insert into expeam.TMP_ANTICH_EXACASTER_SHELDON values(:1,:2,:3,:4)", data=base.insertar)
dbCommit(con)

#############
####BOXPLOT###
tiff('C:/Users/expeam/Documents/BI/2019/02-febrero/antichurn_/comparativo_exacaster.tiff', width = 35, height = 55, units = 'in', res = 300)
boxplot(P ~ ENGAGEMENT_SEGMENT, data = df_tg, xlab = "SEGMENTOS EXACASTER",
        ylab = "P.ALIVE", main = "DISTRIBUCION")

dev.off()

### ELIMINAR NA
df<-na.omit(df)
### FACTORS
df<-mutate_if(df,is.character,as.factor)
##SAMPLE
df.idpdv.sample <- sample(unique(df$ID_PDV),3000)
df<- subset(df,ID_PDV %in% df.idpdv.sample)

##EXCLUIR COLUMNA

df_target<-df_target[,-which(names(df_target) == "SITIO")] 
df_control<-df_control[,-which(names(df_control) == "SITIO")] 

## COMBINAR DATA FRAMES

merge(df_tg,df_matched, by = "AR_KEY", all.x = TRUE)


df_tg_matched<-merge(df_tg,df_matched[,c("AR_KEY","distance","weights")], by = "AR_KEY", all.x = TRUE,no.dups = TRUE)

##ORA_TZ()

Sys.setenv(TZ = "GMT")
Sys.setenv(ORA_SDTZ = "GMT")
dbWriteTable(con, "ORACLE_DB_TABLE", base.insertar, overwrite = F, append = T, row.names = F)

### Correlation Histograma Matriz

library("PerformanceAnalytics")
my_data <- mtcars[, c(1,3,4,5,6,7)]
chart.Correlation(my_data, histogram=TRUE, pch=19)

library(GGally)
ggpairs(my_data, title = "Scatterplot Matrix")
 
 
# Install packages
debug(utils:::unpackPkgZip)
install.packages("psych")

## Data transformation normalization


library(rcompanion)
plotNormalHistogram(df$QUIEBRE)
qqnorm(df$QUIEBRE,ylab="Sample Quantiles for Turbidity")
qqline(df$QUIEBRE,col="red")
T_sqrt = sqrt(df$QUIEBRE)
plotNormalHistogram(T_sqrt)

qqnorm(df$QUIEBRE,ylab="Sample Quantiles for Turbidity")
qqline(df$QUIEBRE,col="red")





## OUTLIERS

outlierKD <- function(dt, var) {
     var_name <- eval(substitute(var),eval(dt))
     na1 <- sum(is.na(var_name))
     m1 <- mean(var_name, na.rm = T)
     par(mfrow=c(2, 2), oma=c(0,0,3,0))
     #boxplot(var_name, main="With outliers")
     #hist(var_name, main="With outliers", xlab=NA, ylab=NA)
     outlier <- boxplot.stats(var_name)$out
     mo <- mean(outlier)
     var_name <- ifelse(var_name %in% outlier, NA, var_name)
     #boxplot(var_name, main="Without outliers")
     #hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
     #title("Outlier Check", outer=TRUE)
     na2 <- sum(is.na(var_name))
     cat("Outliers identified:", na2 - na1, "n")
     cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
     cat("Mean of the outliers:", round(mo, 2), "n")
     m2 <- mean(var_name, na.rm = T)
     cat("Mean without removing outliers:", round(m1, 2), "n")
     cat("Mean if we remove outliers:", round(m2, 2), "n")
     #response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
     #if(response == "y" | response == "yes"){
          dt[as.character(substitute(var))] <- invisible(var_name)
          assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
          cat("Outliers successfully removed", "n")
          return(invisible(dt))
     #} else{
     #     cat("Nothing changed", "n")
     #     return(invisible(var_name))
     #}
}


outlierKD(df, REVENUE_JUN17)
yes
df<-na.omit(df)



#########################
### kmeans outlier detection##
########################

https://rpubs.com/maulikpatel/228345

iris2 <- iris[,1:4]
kmeans.result <- kmeans(iris2, centers=3)
kmeans.result$centers
kmeans.result$cluster
centers <- kmeans.result$centers[kmeans.result$cluster, ] # "centers" is a data frame of 3 centers but the length of iris dataset so we can canlculate distance difference easily.

distances <- sqrt(rowSums((iris2 - centers)^2))

outliers <- order(distances, decreasing=T)[1:5]

print(outliers) # these rows are 5 top outliers

print(iris2[outliers,])
plot(iris2[,c("Sepal.Length", "Sepal.Width")], pch=19, col=kmeans.result$cluster, cex=1)

points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=15, cex=2)

points(iris2[outliers, c("Sepal.Length", "Sepal.Width")], pch="+", col=4, cex=3)



### LOGISTIC REGRESSION

fit <- glm(F~x1+x2+x3,data=mydata,family=binomial(link = "logit"))

### LINEAR REGRESSION

telecomModel <- lm(VENTAS ~  QUIEBRE,data=my_data)

as.formula(
  paste0("y ~ ", round(coefficients(telecomModel)[1],2), "", 
         paste(sprintf(" %+.2f*%s ", 
                       coefficients(telecomModel)[-1],  
                       names(coefficients(telecomModel)[-1])), 
               collapse="")
  )
)

## names 

names(table(my_data$HS_SIN_SALDO_SGM))

########################
##sumar agrupar - porcentajes por grupo - dplyr##
#######################


my_data %>%
  group_by(HS_SIN_SALDO_SGM) %>%  ## las demas variables deben ser numericas obvio
  summarize_all(sum)
  
df_tg[c(2,11)] %>%
  group_by(DAYS_LAST_OUT_ANY_SGM) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)*100)


###############################
######sumar contar unicos#####
##############################

df_tg[c(2,15)] %>%
  group_by(DAYS_LAST_OUT_ANY_SGM) %>%
  summarize (unique_types= n_distinct (AR_KEY))
  
  ######################################
  ######imprimir pantalla PRINT OUT####
  ######################################
  
tiff('C:/Users/expeam/Documents/BI/2019/02-febrero/quiebre_ventas_epin/confidence_interval_ventas_quiebre.tiff', width = 35, height = 25, units = 'in', res = 200)
ggplot...
dev.off()

##otra manera
par(mfrow=c(2,2))
plot(telecomModel)
#grid.arrange(g1, g2, g3, nrow=1)  # one row
dev.copy(png,'threeplots.png')   # to save the array of plots     
dev.off() 

### plot multiple

par(mfrow=c(2,2))

#################
## TIME SERIES###
#################


#ts (inputData, frequency = 4, start = c(1959, 2)) # frequency 4 => Quarterly Data
ts_data_traffic<-ts (inputData$DATA_TRFF_GB, frequency = 12, start = 2017) # freq 12 => Monthly data. 
#ts (inputData, start=c(2009), end=c(2014), frequency=1) # Yearly Data
decomposedRes <- decompose(ts_data_traffic, type="mult")
plot (decomposedRes)
stlRes <- stl(ts_data_traffic, s.window = "periodic")
summary(stlRes)

library(forecast)

ts.stl <- stl(ts_data_traffic,"periodic")  # decompose the TS
ts.sa <- seasadj(ts.stl)  # de-seasonalize
plot(ts_data_traffic, type="l")  # original series
plot(ts.sa, type="l")  # seasonal adjusted
seasonplot(ts.sa, 12, col=rainbow(12), year.labels=TRUE, main="Seasonal plot: Airpassengers") # seasonal frequency set as 12 for monthly data.

################################################
###plot function ggplot graficarr funciones ###
#################################################

library(ggplot2)
p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
fun.1 <- function(x) 35747473508 -525*x
p + stat_function(fun = fun.1) + xlim(0,610811)


########################
##partir la pantalla  imprimir####
########################

 par(mfrow=c(2, 2)
 
 ####################
 ####ggplot boxplot######
 ##################
 
library(ggplot2)
bp <- ggplot(df_control, aes(x="", y=REVENUE)) + 
  geom_boxplot()+
  coord_cartesian(ylim = c(0, 150000)) # I set the y axis scale so the plot looks better.

bp

##########################
##convertir a char a Date#
###########################


df_control$FECHA<-as.Date(fasttime::fastPOSIXct(format(df_control$FECHA)))

#######################################################
######big data file - levantar archivos grandes ######
########################################################


base_pos_trafico<-fread("C:/Users/expeam/Documents/BI/2019/02-febrero/pospago_brackets_consumo/base_pospago_consumo_yyyymmdd.csv")
str(base_pos_trafico)
library(data.table)
library(fasttime)

base_pos_trafico$FECHA<-as.Date(fasttime::fastPOSIXct(format(base_pos_trafico$FECHA)))

base_pos_trafico[,FECHA := fastPOSIXct(FECHA)]
tail(base_pos_trafico)


###########################################
########venn intersecciones grupos#########
##########################################



#upload library
library(ROracle)
library(VennDiagram)
library(gplots)
library(tictoc)


con <- dbConnect(Oracle(), user="expeam", password="!marzo2019", dbname="DWH/dwh_olap")
##ene-2018
tic()
query <- dbSendQuery(con,"
                     
                  select p.ar_key
                  from expeam.tmp_base_paq_4 p
                  where p.servicio = '800mbx7000gsx2d'
                     
                     ")
base1 <- fetch(query)
toc()

##ene-2019
tic()
query <- dbSendQuery(con,"
                     
                     
                    select p.ar_key
                    from expeam.tmp_base_paq_4 p
                    where p.servicio = '1,5gbx10000gsx3d'
                     
                     ")
base2 <- fetch(query)
toc()
##ene-2019
tic()
query <- dbSendQuery(con,"
                     
                  select p.ar_key
                  from expeam.tmp_base_paq_4 p
                  where p.servicio = '1,5gb+Ilitigo+30minx12000gsx3d'
                     
                     ")
base3 <- fetch(query)
toc()
##TIGO_BILL_PAYS
tic()
query <- dbSendQuery(con,"
                     
            select p.ar_key
            from expeam.tmp_base_paq_4 p
            where p.servicio = '2gb+Ilimtigo+100minx15000gsx5d'   

                     ")
base4 <- fetch(query)
toc()



#Then generate 3 sets of words.There I generate 3 times 200 SNPs names.
base1<-base1$AR_KEY
#base1[is.na(base1)]<-"1"
base2<-base2$AR_KEY
#base2[is.na(base2)]<-"1"
base3<-base3$AR_KEY
#base3[is.na(base3)]<-"1"
base4<-base4$AR_KEY
#base4[is.na(base4)]<-"1"



#The goal of the Venn Diagram is to count how many words are common between SNP_pop_1 and SNP_pop_2, between SNP_pop_1 and SNP_pop_3 and so on...
#The venn.diagram function do it automatically and draw it! (you will get a png file in your current working directory)

tic()
venn.diagram(
  x = list(base1,base2,base3,base4),
  category.names = c("800mbx7000","1_5gbx10000","1_5gbx12000","2gbx15000"),
  filename = 'C:/Users/expeam/Documents/BI/2019/03-marzo/sombra_paquetes_1/graficos/venn_diagramm_paquetes_1.png',
  output = TRUE ,
  imagetype="png" ,
  height = 768 , 
  width = 1024 , 
  resolution = 300,
  compression = "lzw",
  lwd = 2,
  lty = 'blank',
  fill = c('yellow','purple','green','red'),
  cex = 0.5,
  fontface = "bold",
  fontfamily = "sans",
  cat.cex = 0.5,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  #cat.pos = c(-27, 27, 135),
  #cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  #print.mode = 'percent'
  
)

toc() 

##extraer datos de venn


itemlist<-venn(list(B7000=base1,B10000=base2,B12000=base3,B15000=base4))

BASE7000<-data.frame(AR_KEY=attr(itemlist,"intersections")[12],SERVICIO="800mbx7000")
BASE10000<-data.frame(AR_KEY=attr(itemlist,"intersections")[13],SERVICIO="1_5gbx10000")
BASE12000<-data.frame(AR_KEY=attr(itemlist,"intersections")[14],SERVICIO="1_5gbx12000")
BASE15000<-data.frame(AR_KEY=attr(itemlist,"intersections")[15],SERVICIO="2gbx15000")

colnames(BASE7000) <-c("AR_KEY","SERVICIO")
colnames(BASE10000) <-c("AR_KEY","SERVICIO")
colnames(BASE12000) <-c("AR_KEY","SERVICIO")
colnames(BASE15000) <-c("AR_KEY","SERVICIO")

BASE_PAQUETES_FEB <- rbind(BASE7000,BASE10000,BASE12000,BASE15000)
##############################
###GGPLOT FACET WRAP  capas###
#############################3

ggplot(df_sum_brk,aes(x=FECHA,y=MB_DATA_TRAFFIC_SUM))+
  #geom_line(color="blue",size=1)+
  scale_x_date(breaks = pretty_breaks(n=25))+
  facet_wrap(~DATA_LMT_BRACKET,scales = "free_y",nrow=breaks,ncol=1)+
  theme(axis.text.x = element_text(angle = 45))+
  geom_smooth(span=0.5,method='loess',size=0.5)+
  #geom_point(size=1,color="blue")+
  theme(text=element_text(size=32))
dev.off()

############
##rename##
#########
names(data) <- c("new_name", "another_new_name")
names(data)[3]<-"new_name"

##############
#####UNPIVOT####
#############

#Country     2001    2002    2003
#Nigeria     1       2       3
#UK          2       NA       1

#Country    Year    Value
#Nigeria    2001    1
#Nigeria    2002    2
#Nigeria    2003    3
#UK         2001    2
#UK         2002    NA
#UK         2003    1

my.result <-cbind(my.df[1], stack(my.df[-1]))

############################
######MUTATE TRUNCAR CON FUNCION###
############################

fun <- function(x) (trunc(x,0))
my.df<-mutate_if(my.df,is.numeric,fun)

#################################
##########replace NA with 0####
#################################

library(imputeTS)
na.replace(yourDataframe, 0)
#################################
##########replace 0 with NA####
#################################
df[df == 0] <- NA

#################################################
##########interpretation logistic regression####
#############################################


## 
 ## Call:
 ## glm(formula = admit ~ gre + gpa + rank, family = "binomial", 
 ##     data = df)
 ## 
 ## Deviance Residuals: 
 ##     Min       1Q   Median       3Q      Max  
 ## -1.6268  -0.8662  -0.6388   1.1490   2.0790  
 ## 
 ## Coefficients:
 ##              Estimate Std. Error z value Pr(>|z|)    
 ## (Intercept) -3.989979   1.139951  -3.500 0.000465 ***
 ## gre          0.002264   0.001094   2.070 0.038465 *  
 ## gpa          0.804038   0.331819   2.423 0.015388 *  
 ## rank2       -0.675443   0.316490  -2.134 0.032829 *  
 ## rank3       -1.340204   0.345306  -3.881 0.000104 ***
 ## rank4       -1.551464   0.417832  -3.713 0.000205 ***
 ## ---
 ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
 ## 
 ## (Dispersion parameter for binomial family taken to be 1)
 ## 
 ##     Null deviance: 499.98  on 399  degrees of freedom
 ## Residual deviance: 458.52  on 394  degrees of freedom
 ## AIC: 470.52
 ## 
 ## Number of Fisher Scoring iterations: 4
Brief Interpretation
1- Each one-unit change in gre will increase the log odds of getting admit by 0.002, 
and its p-value indicates that it is somewhat significant in determining the admit.

2- Each unit increase in GPA increases the log odds of getting admit by 0.80 and 
p-value indicates that it is somewhat significant in determining the admit.

3- The interpretation of rank is different from others, going to rank-2 college from rank-1 
college will decrease the log odds of getting admit by -0.67. Going from rank-2 to rank-3 will decrease it by -1.340.

4- The difference between Null deviance and Residual deviance tells us that the model is a good fit. 
Greater the difference better the model. Null deviance is the value when you only have intercept in your 
equation with no variables and Residual deviance is the value when you are taking all the variables into account. 
It makes sense to consider the model good if that difference is big enough.

Prediction


####reemplazo de outliers con KNN
library(DMwR)
df <- knnImputation(df_bkp[,-c(1)],k=100)  -- SE NECESITAN 3 COLUMNAS
anyNA(df)

df$CIRCUITO<-df_bkp$CIRCUITO


###################################
#######mejor variable predictora####
####################################

library(party)
options(scipen=999)
cf1 <- cforest(CANT_CLIENTES ~ . , data= df, control=cforest_unbiased(mtry=2,ntree=50),) # fit the random forest
sort(varimp(cf1),decreasing = TRUE) # get variable importance, based on mean decrease in accuracy
sort(varimp(cf1, conditional=TRUE),decreasing = TRUE)  # conditional=True, adjusts for correlations between predictors


#################################################
##########graficos graficar columanas NA########
###############################################

library(naniar)

vis_miss(airquality)

df %>% purrr::map(~ mean(is.na(.)))

####eliminar todos los datos de data frame
## eliminar filas de data frame
data5<-data5[0,]

###############################
###renombrar factores##############
################################

library(plyr)

df$dptorep<-as.factor(df$dptorep)
df$dptorep<-revalue(df$dptorep, c("0" = "Asuncion","2" = "San Pedro","5" = "Caaguazu","6" = "Caazapa","7" = "Itapua","10" = "Alto Parana","11" = "Central","20" = "Resto"))

##################################
####Transformacion de los datos###
##################################
df.k<-scale(df[c("e01aimde","ipcm")])

################################
###Implementacion de K-Means####
################################

## Cantidad optima de clusters.

wss <- (nrow(df.k)-1)*sum(apply(df.k,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(df.k,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

## Implementacion de K-means

set.seed(20)
ingresos_clusters <- kmeans(df.k, 5)

ingresos_clusters$cluster <- as.factor(ingresos_clusters$cluster)

## Grafico de los clusters (titulo, subtitulo,leyenda, fuente)
ggplot(df, aes(e01aimde/1000, ipcm/1000, color = ingresos_clusters$cluster)) + 
  geom_point() + 
  labs(title= "Clusters de Ingresos por Unidad Familiar",subtitle = "(Miles de Guaranies)",caption = "Fuente: Direccion general de Estadisticas, Encuestas y Censos - Encuesta Permanente de Hogares 2018",x = "Ingreso por actividad principal",y="Ingreso per cápita mensual") +
  labs(color='Clusters') +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))


#################################

####limpiar graficos
graphics.off()


#####################################
##exportar importar data frames R Python####
######################################
############################
library(feather)
write_feather(df, "C:/Users/morel/Documents/nexter/datos/df.feather")
## en python

# pip install feather-format
#importar data frame desde R
import feather
path = 'C:/Users/morel/Documents/nexter/datos/df.feather'
df = feather.read_dataframe(path)

#############  R GUI 
#Sys.setenv(LANGUAGE="en")
#library(rattle)
#rattle()

## porcentaje de valores nulos

df %>% purrr::map(~ mean(is.na(.)))
df<-na.omit(df)

### seleccionar solo variables numericas
select_if(Auto, is.numeric)

#### determinar la distribucion de la variable 
## normal, uniform, lognormal

library(ISLR)
library(fitdistrplus)
library(logspline)
descdist(Auto$year, discrete = FALSE)

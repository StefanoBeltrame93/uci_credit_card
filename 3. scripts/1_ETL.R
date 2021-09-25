#1 ETL - UCI Credit Card

#levantamos el excel
library(tidyverse)
library(pROC)
library(xlsx)
library(readxl)
library(gmodels) #crosstable esta dentro de paquete "gmodels"


data_path <- c("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/uci_credit_card/1. data/")

UCI_credit_card <- 
  read_excel(stringr::str_c(data_path, "UCI_Credit_Card.xlsx")) %>% 
  as_tibble()


########## Analisis Cualitiativo Var. dependiente ###########

# Call CrossTable() on default.payments.next.month
CrossTable(UCI_credit_card$default.payment.next.month)
#23364 obs no_default (77,9%)
#6636 obs default (22,1%) 
#DATA SET SUPER UNBALANCED


# Call CrossTable() on SEX and default.payments.next.month
CrossTable(UCI_credit_card$default.payment.next.month, UCI_credit_card$SEX,
           prop.r = TRUE,
           prop.c = FALSE,
           prop.t= FALSE,
           prop.chisq = FALSE)

#Creamos nueva variable mean_LIMIT_BAL
UCI_credit_card <- 
  UCI_credit_card %>% 
  mutate(mean_LIMIT_BAL = mean(LIMIT_BAL))

# Miramos para SEX y MARRIAGE el summary y crosstable respectivo

summary(UCI_credit_card$SEX) #solo 1 (hombres) y 2 (mujeres)

summary(UCI_credit_card$MARRIAGE) #tenemos 1 (married), 2 (single) y 3 (other)

# Vemos cuanto son 3 para intentar "volarlos"
CrossTable(UCI_credit_card$MARRIAGE)

#Total Observations in Table:  30000 


#  |         0 |         1 |         2 |         3 | 
#  |-----------|-----------|-----------|-----------|
#  |        54 |     13659 |     15964 |       323 | 
#  |     0.002 |     0.455 |     0.532 |     0.011 | 
#  |-----------|-----------|-----------|-----------|

#Encontramos que hay "0". En la explicaci?n del data set no est?n por lo que podr?amos tambi?n volarlos.

54+323
#377 OUTLIERS en MARRIAGE

perc_of_0and3 = (54+323)/(30000)
print(perc_of_0and3)
#0.01256667
#Tenemos solamente 1,26 % del data set que es 0 y 3. Los eliminamos


marriage_na_index <- 
  UCI_credit_card %>%
  dplyr::filter(MARRIAGE %in% c(0,3)) %>% 
  dplyr::mutate(MARRIAGE = NA)

#Tenemos solamente 1,26 % del data set que es 0 y 3. Los eliminamos

UCI_credit_card$MARRIAGE[which(UCI_credit_card$MARRIAGE == 0)] = NA
UCI_credit_card$MARRIAGE[which(UCI_credit_card$MARRIAGE == 3)] = NA
na_row_index = which(is.na(UCI_credit_card$MARRIAGE))
UCI_credit_card = UCI_credit_card[-na_row_index,]

#Borramos todos los indices que son 0 y 3

######### Agrupaci?n de variables cualitiativas EDUCATION ###########

summary(UCI_credit_card$EDUCATION) #tenemos EDUCATION que va de 1 a 6

# 0 = unknown
# 1 = graduate school (PhD)
# 2 = University (Univ)
# 3 = high school
# 4 = others
# 5 & 6 = unknown
#
#Cross table EDUCATION
CrossTable(UCI_credit_card$EDUCATION)


#Total Observations in Table:  29623 


#  |         0 |         1 |         2 |         3 |         4 | 
#  |-----------|-----------|-----------|-----------|-----------|
#  |        14 |     10531 |     13862 |      4770 |       120 | 
#  |     0.000 |     0.356 |     0.468 |     0.161 |     0.004 | 
#  |-----------|-----------|-----------|-----------|-----------|
#  
#  
#  |         5 |         6 | 
#  |-----------|-----------|
#  |       277 |        49 | 
#  |     0.009 |     0.002 | 
#  |-----------|-----------|


#Obs0 +Obs4 +Obs5 + Obs6
(14+120+277+49)
#460


#Obs0 + Obs4+ Obs5 + Obs6 / Total'
(14+120+277+49)/(29623)
# 0.01552847

# 0,4, 5 y 6 suman el 1,55 % del nuevo data set (sin marriage outliers) 


# ---> ELIMINAMOS EDUCATION == 0; 4; 5; Y 6 
UCI_credit_card$EDUCATION[which(UCI_credit_card$EDUCATION == 0)] = NA #para todo EDUCATION = 0 --> NA
UCI_credit_card$EDUCATION[which(UCI_credit_card$EDUCATION == 4)] = NA #para todo EDUCATION = 4 --> NA
UCI_credit_card$EDUCATION[which(UCI_credit_card$EDUCATION == 5)] = NA #para todo EDUCATION = 5 --> NA
UCI_credit_card$EDUCATION[which(UCI_credit_card$EDUCATION == 6)] = NA #para todo EDUCATION = 6 --> NA

na_row_index = which(is.na(UCI_credit_card$EDUCATION))
UCI_credit_card = UCI_credit_card[-na_row_index, ]

#EN TOTAL YA ELIMINAMOS 377 (MARRIAGE) + 460 (EDUCATION) = 837 (2,79 % data set original)
30000-377-460
############## ANALISIS EXPLORATORIO VARIABLES CATEGORICAS ###############

# PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6 --> repayment status
library(ggplot2)

#PAY_0
bar_pay0 = ggplot(data = UCI_credit_card)+
  geom_bar(mapping = aes(x = PAY_0), stat = "count")
bar_pay0

#PAY_2
bar_pay2 = ggplot(data = UCI_credit_card)+
  geom_bar(mapping = aes(x = PAY_2), stat = "count")
bar_pay2

#PAY_3
bar_pay3 = ggplot(data = UCI_credit_card)+
  geom_bar(mapping = aes(x = PAY_3), stat = "count")
bar_pay3

#PAY_4
bar_pay4 = ggplot(data = UCI_credit_card)+
  geom_bar(mapping = aes(x = PAY_4), stat = "count")
bar_pay4

#PAY_5
bar_pay5 = ggplot(data = UCI_credit_card)+
  geom_bar(mapping = aes(x = PAY_5), stat = "count")
bar_pay5

#PAY_6
bar_pay6 = ggplot(data = UCI_credit_card)+
  geom_bar(mapping = aes(x = PAY_6), stat = "count")
bar_pay6

#Para juntar los graficos ---> usamos library "gridExtra"
install.packages("gridExtra")
library(gridExtra)

bar_pay0_to_6 = grid.arrange(bar_pay0, bar_pay2, bar_pay3, bar_pay4, bar_pay5, bar_pay6)

############## ANALISIS EXPLORATORIO VARIABLES CONTINUAS ###############

#AGE

bar_age = ggplot(data = UCI_credit_card)+
  geom_histogram(mapping = aes(x = AGE), #Esto es para el Histograma
                 binwidth = 1,
                 stat = "count",
                 colour = "black", fill = "white")+
  geom_vline(mapping = aes(xintercept = 60), #Linea roja vertical en 60 
             colour = "red",
             linetype = "dashed")
bar_age



scatter_age = ggplot(data = UCI_credit_card)+
  geom_point(mapping = aes(x = ID, y = AGE),
             colour = "black",
             fill = "white",
             size = 0.75)+ #Scatter graph
  geom_vline(mapping = aes(xintercept = 15000),
             colour = "red",
             linetype = "dashed") #Linea roja horizontal en 60 
scatter_age

#Eliminamos obs con AGE > 60 anios
count(UCI_credit_card %>% filter(AGE > 60))
#264 obs AGE > 60. Las borramos:
UCI_credit_card$AGE[which(UCI_credit_card$AGE > 60)] = NA # Si AGE > 60 then NA
age_na_index = which(is.na(UCI_credit_card$AGE)) #generamos indice con edad > 60
UCI_credit_card = UCI_credit_card[-age_na_index, ]  #eliminamos obs con AGE>60
#EN TOTAL YA ELIMINAMOS 377 (MARRIAGE) + 460 (EDUCATION) + 264 (AGE) = 1101 (3,67 % data set original)
30000-377-460-264

#Violin Histogram for AGE


#BILL_AMT1 a 6
#i) Histograms
# Getting BILL_AMT1..6 columns only
BILL_AMT_1to6 <- UCI_credit_card %>% select(starts_with("BILL_AMT"))  


Hist_BILL_AMT1 = ggplot(data = BILL_AMT_1to6)+
  geom_histogram(mapping = aes(x = BILL_AMT1),
                 binwidth = 5000)+
  geom_vline(mapping = aes(xintercept = quantile(BILL_AMT1, 0.25) -1.5*IQR((BILL_AMT1))),
             colour = "red",
             linetype = "dashed")+
  geom_vline(mapping = aes(xintercept = quantile(BILL_AMT1, 0.75) +1.5*IQR((BILL_AMT1))),
             colour = "red",
             linetype = "dashed")
Hist_BILL_AMT1

#BILL_AMT2
Hist_BILL_AMT2 = ggplot(data = BILL_AMT_1to6)+
  geom_histogram(mapping = aes(x = BILL_AMT2),
                 binwidth = 5000)+
  geom_vline(mapping = aes(xintercept = quantile(BILL_AMT2, 0.25) -1.5*IQR((BILL_AMT2))),
             colour = "red",
             linetype = "dashed")+
  geom_vline(mapping = aes(xintercept = quantile(BILL_AMT2, 0.75) +1.5*IQR((BILL_AMT2))),
             colour = "red",
             linetype = "dashed")
Hist_BILL_AMT2

#BILL_AMT3
#BILL_AMT2
Hist_BILL_AMT3 = ggplot(data = BILL_AMT_1to6)+
  geom_histogram(mapping = aes(x = BILL_AMT3),
                 binwidth = 5000)+
  geom_vline(mapping = aes(xintercept = quantile(BILL_AMT3, 0.25) -1.5*IQR((BILL_AMT3))),
             colour = "red",
             linetype = "dashed")+
  geom_vline(mapping = aes(xintercept = quantile(BILL_AMT3, 0.75) +1.5*IQR((BILL_AMT3))),
             colour = "red",
             linetype = "dashed")
Hist_BILL_AMT3

#BILL_AMT4
Hist_BILL_AMT4 = ggplot(data = BILL_AMT_1to6)+
  geom_histogram(mapping = aes(x = BILL_AMT4),
                 binwidth = 5000)+
  geom_vline(mapping = aes(xintercept = quantile(BILL_AMT4, 0.25) -1.5*IQR((BILL_AMT4))),
             colour = "red",
             linetype = "dashed")+
  geom_vline(mapping = aes(xintercept = quantile(BILL_AMT4, 0.75) +1.5*IQR((BILL_AMT4))),
             colour = "red",
             linetype = "dashed")
Hist_BILL_AMT4

#BILL_AMT5
Hist_BILL_AMT5 = ggplot(data = BILL_AMT_1to6)+
  geom_histogram(mapping = aes(x = BILL_AMT5),
                 binwidth = 5000)+
  geom_vline(mapping = aes(xintercept = quantile(BILL_AMT5, 0.25) -1.5*IQR((BILL_AMT5))),
             colour = "red",
             linetype = "dashed")+
  geom_vline(mapping = aes(xintercept = quantile(BILL_AMT5, 0.75) +1.5*IQR((BILL_AMT5))),
             colour = "red",
             linetype = "dashed")
Hist_BILL_AMT5

#BILL_AMT6
Hist_BILL_AMT6 = ggplot(data = BILL_AMT_1to6)+
  geom_histogram(mapping = aes(x = BILL_AMT6),
                 binwidth = 5000)+
  geom_vline(mapping = aes(xintercept = quantile(BILL_AMT6, 0.25) -1.5*IQR((BILL_AMT6))),
             colour = "red",
             linetype = "dashed")+
  geom_vline(mapping = aes(xintercept = quantile(BILL_AMT6, 0.75) +1.5*IQR((BILL_AMT6))),
             colour = "red",
             linetype = "dashed")
Hist_BILL_AMT6

Hist_BILL_AMT1_to_6 = grid.arrange(Hist_BILL_AMT1, Hist_BILL_AMT2, Hist_BILL_AMT3, Hist_BILL_AMT4, Hist_BILL_AMT5, Hist_BILL_AMT6)

library(gridExtra)
#ii) scater plot 




#Resultado --> borrar BILL_AMT 1 a 6 > 260000 aprox


#PAY_AMT1 a 6 
#LIMIT_BAL
#Bar graph -->LIMIT_BAL
bar_limit_bal = ggplot(data = UCI_credit_card)+
  geom_bar(mapping = aes(x = LIMIT_BAL),
           stat = "count")+
  geom_vline(mapping = aes(xintercept = 500000),
             colour = "red",
             linetype = "dashed")
bar_limit_bal

#Scatter Graph LIMIT_BAL
scatter_limit_bal = ggplot(data = UCI_credit_card)+
  geom_point(mapping = aes(x = ID, y = LIMIT_BAL),
             colour = "black",
             fill = "white",
             size = 0.75) + #Scatter graph
  geom_hline(mapping = aes(yintercept = 500000),
             colour = "red",
             linetype = "dashed") #Linea roja horizontal en 60 
scatter_limit_bal

# LIMIT_BAL > 500000 ES "OUTLIER"
#Eliminamos LIMIT_BAL por arriba de LIMIT_BAL > 500000
count(UCI_credit_card %>% filter(LIMIT_BAL>500000)) #196 obs solamente
UCI_credit_card$LIMIT_BAL[which(UCI_credit_card$LIMIT_BAL > 500000)] = NA # NA de toda la ROW si LIMIT_BAL > 500000
na_row_index = which(is.na(UCI_credit_card$LIMIT_BAL)) #generamos indice con NA de LIMIT_BAL
UCI_credit_card = UCI_credit_card[-na_row_index, ]  #eliminamos NA ROWS

#llevamos eliminado 1297 obs equivalente a 4,3% del data set.
28899 - 196
(30000-28703)/30000

############# AGE ################ 

#Histograma para AGE
Hist_AGE = hist(x = UCI_credit_card$AGE,
                xlim = c(0,80),
                ylim = c(0,8000),
                #breaks = c(1, 80, 1),
                freq = NULL,
                main = paste("Histograma de AGE"),
                xlab = "AGE",
                ylab = "Age clients frequency",
                col  = "gray",
                cex.lab =1, cex.axis=1, cex.main=1, cex.sub=1)

readr::write_csv(UCI_credit_card, 
            path = stringr::str_c(data_path, "/uci_credit_card_final_ddbb.csv"),
            col_names = T)
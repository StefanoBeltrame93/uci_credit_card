#2 feature engineering

data_path <- c("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/uci_credit_card/1. data/")

UCI_credit_card <- readr::read_csv(stringr::str_c(data_path, "/uci_credit_card_final_ddbb.csv"))

#Variables como factores

#SEX
UCI_credit_card$SEX = as.factor(UCI_credit_card$SEX)

#EDUCATION
UCI_credit_card$EDUCATION = as.factor(UCI_credit_card$EDUCATION)

#MARRIAGE
UCI_credit_card$MARRIAGE = as.factor(UCI_credit_card$MARRIAGE)

#PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6
UCI_credit_card$PAY_0 = as.factor(UCI_credit_card$PAY_0)
UCI_credit_card$PAY_2 = as.factor(UCI_credit_card$PAY_2)
UCI_credit_card$PAY_3 = as.factor(UCI_credit_card$PAY_3)
UCI_credit_card$PAY_4 = as.factor(UCI_credit_card$PAY_4)
UCI_credit_card$PAY_5 = as.factor(UCI_credit_card$PAY_5)
UCI_credit_card$PAY_6 = as.factor(UCI_credit_card$PAY_6)

#default.payment.next.month
UCI_credit_card$default.payment.next.month = factor(UCI_credit_card$default.payment.next.month)

#Eliminamos ID
UCI_credit_card$ID = NULL

########### NORMALIZACI?N DEL DATA SET ############
# normalizamos con funci?n "scale" las variables continuas:

#LIMIT_BAL
UCI_credit_card[,1] = scale(UCI_credit_card$LIMIT_BAL, center=T, scale=T) #em col 1 esta LIMIT_BAL

#AGE
UCI_credit_card[,5] = scale(UCI_credit_card$AGE, center = T, scale = T) #en col 5 esta age

#BILL_AMT(t) para todo t = 1...6
UCI_credit_card[,12] = scale(UCI_credit_card$BILL_AMT1, center=T, scale=T)
UCI_credit_card[,13] = scale(UCI_credit_card$BILL_AMT2, center=T, scale=T)
UCI_credit_card[,14] = scale(UCI_credit_card$BILL_AMT3, center=T, scale=T)
UCI_credit_card[,15] = scale(UCI_credit_card$BILL_AMT4, center=T, scale=T)
UCI_credit_card[,16] = scale(UCI_credit_card$BILL_AMT5, center=T, scale=T)
UCI_credit_card[,17] = scale(UCI_credit_card$BILL_AMT6, center=T, scale=T)

#PAY_AMT(t) para todo t = 1...6             
UCI_credit_card[,18] = scale(UCI_credit_card$PAY_AMT1, center=T, scale=T)
UCI_credit_card[,19] = scale(UCI_credit_card$PAY_AMT2, center=T, scale=T)
UCI_credit_card[,20] = scale(UCI_credit_card$PAY_AMT3, center=T, scale=T)
UCI_credit_card[,21] = scale(UCI_credit_card$PAY_AMT4, center=T, scale=T)
UCI_credit_card[,22] = scale(UCI_credit_card$PAY_AMT5, center=T, scale=T)
UCI_credit_card[,23] = scale(UCI_credit_card$PAY_AMT6, center=T, scale=T)



########## SEPARAMOS EN TRAINING (0.7) Y TEST SET (0.3) ##################
set.seed(1234)
index_train = sample(1:nrow(UCI_credit_card), 0.7*nrow(UCI_credit_card))

#train_set
train_set = UCI_credit_card[index_train, ]
readr::write_csv(x = train_set, 
                 path = stringr::str_c(data_path, "/credit_card_train_data.csv"),
                 col_names = T)
#test_set
test_set = UCI_credit_card[-index_train, ]
readr::write_csv(x = test_set, 
                 path = stringr::str_c(data_path, "/credit_card_test_data.csv"),
                 col_names = T)



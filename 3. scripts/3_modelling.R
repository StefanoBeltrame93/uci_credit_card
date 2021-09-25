#3 Modelling

library(rpart)
library(rpart.plot)


data_path <- c("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/uci_credit_card/1. data/")

train_set <- readr::read_csv(stringr::str_c(data_path, "/credit_card_train_data.csv"))

test_set <- readr::read_csv(stringr::str_c(data_path, "/credit_card_test_data.csv"))

########### 1. REG LOGISTICA #################
# 1.1 Reg Log con todas las de variables todas las variables 
set.seed(1234)
logit <- glm(formula = default.payment.next.month ~ ., #logit con todas las variables
             data = train_set,
             family = binomial(link = "logit"))
summary(logit) #vemos resultados
logit_pred <- predict(logit, #prediccion con test data 
                         newdata = test_set,
                         type = "response")
range(logit_pred) #rango de la prob de default

# 1.2 Reg Log con subset de variables (las significativas en 1.1)
set.seed(1234)
logit_subset1 = glm(formula = default.payment.next.month ~ LIMIT_BAL
                                                            +SEX
                                                            +MARRIAGE
                                                            +AGE
                                                            +BILL_AMT1
                                                            +BILL_AMT2
                                                            +BILL_AMT3
                                                            +BILL_AMT4
                                                            +BILL_AMT5
                                                            +BILL_AMT6
                                                            +PAY_AMT1
                                                            +PAY_AMT2
                                                            +PAY_AMT3
                                                            +PAY_AMT4
                                                            +PAY_AMT5
                                                            +PAY_AMT6,
                    data = train_set,
                    family = binomial(link = "logit"))
summary(logit_subset1) # vemos p-values
logit_subset1_pred = predict(logit_subset1, #model name en este caso "logit_subset1"
                             newdata = test_set,
                             type = "response") #para que nos tire la probabilidad de default 
range(logit_subset1_pred) #vemos el rango de la prob of default predicted
#el rango de las prob de default va entre el 0% (un poco positivo) y 58%

# 1.3 Reg Log con subset de variables 1.2 y sacamos las no significativas al 5%
#V.No Significativas al 5% en subset1 AGE, BILL_AMT3, BILL_AMT4, BILL_AMT6, PAY_AMT5, PAY_AMT6. Estas las sacamos para correr el 2do logit

set.seed(1234)
logit_subset2 = glm(formula = default.payment.next.month ~ LIMIT_BAL
                    +SEX
                    +MARRIAGE
                    #+AGE
                    +BILL_AMT1
                    +BILL_AMT2
                    #+BILL_AMT3
                    #+BILL_AMT4
                    +BILL_AMT5
                    #+BILL_AMT6
                    +PAY_AMT1
                    +PAY_AMT2
                    +PAY_AMT3
                    +PAY_AMT4,
                    #+PAY_AMT5
                    #+PAY_AMT6,
                    data = train_set,
                    family = binomial(link = "logit"))

# Obtain significance levels using summary()
summary(logit_subset2)

logit_subset2_pred = predict(logit_subset2,
                             newdata = test_set,
                             type = "response") #cuando ponemos response es que queremos la probability
range(logit_subset2_pred) #vemos el rango de la prob of default predicted


################ ROC y AUC-ROC ###################
library(pROC)

#Construct the ROC-objects for the four logistic regression models using function roc(response, predictor). Remember that the response is the loan status indicator in the test_set, which can be obtained through test_set$loan_status.
# Construct the objects containing ROC-information
ROC_logit         <- roc(test_set$default.payment.next.month, logit_pred) 
ROC_logit_subset1 <- roc(test_set$default.payment.next.month, logit_subset1_pred)
ROC_logit_subset2 <- roc(test_set$default.payment.next.month, logit_subset2_pred)

#Use the previously created objects to construct ROC-curves. To draw them all on one plot, use plot() for the first ROC-curve drawn (for ROC_logit), and use [lines()](http://www.rdocumentation.org/packages/graphics/functions/lines to add the ROC-curves) for the other three models to the same plot.
# Draw all ROCs on one plot
plot(ROC_logit)
lines(ROC_logit_subset1, col = "red")
lines(ROC_logit_subset2, col="blue")

# Compute the AUCs
auc(ROC_logit) #0.7261
auc(ROC_logit_subset1) #0.663
auc(ROC_logit_subset2) #0.6623



############ DESICION TREE ##################
library(rpart)
set.seed(1234)
tree_subset1 = rpart(formula = default.payment.next.month ~ LIMIT_BAL
                     +SEX
                     +MARRIAGE
                     +AGE
                     +BILL_AMT1
                     +BILL_AMT2
                     +BILL_AMT3
                     +BILL_AMT4
                     +BILL_AMT5
                     +BILL_AMT6
                     +PAY_AMT1
                     +PAY_AMT2
                     +PAY_AMT3
                     +PAY_AMT4
                     +PAY_AMT5
                     +PAY_AMT6,
                     method = "class",
                     data = train_set,
                     control = rpart.control(cp = 0.001)) #is the complexity parameter, is the threshold value for a decrease in overall lack of fit for any split

plot(tree_subset1, uniform = TRUE) #lo graficamos. TRUE : to get equal-sized branches
text(tree_subset1) #le agregamos labels


tree_subset1_pred = predict(object = tree_subset1,
                            newdata = test_set,
                            type = "prob")[,2]

ROC_tree_subset1 <- roc(test_set$default.payment.next.month, tree_subset1_pred) #queremos que nos devuelva el vector de col 2 porque representa el "default" (cuando y = 1)

auc(ROC_tree_subset1)

############### ARBOL 2 ##################

set.seed(1234)
tree_subset1_opc2 = rpart(formula = default.payment.next.month ~ LIMIT_BAL
                          +SEX
                          +MARRIAGE
                          +AGE
                          +BILL_AMT1
                          +BILL_AMT2
                          +BILL_AMT3
                          +BILL_AMT4
                          +BILL_AMT5
                          +BILL_AMT6
                          +PAY_AMT1
                          +PAY_AMT2
                          +PAY_AMT3
                          +PAY_AMT4
                          +PAY_AMT5
                          +PAY_AMT6,
                          method = "class",
                          data = train_set,
                          parms = list(prior = c(0.7, 0.3)),
                          control = rpart.control(cp = 0.001)) #is the complexity parameter, is the threshold value for a decrease in overall lack of fit for any split

class(tree_subset1_opc2)
plot(tree_subset1_opc2, uniform = TRUE) #lo graficamos. TRUE : to get equal-sized branches
text(tree_subset1_opc2) #le agregamos labels

tree_subset1.opc2_pred = predict(object = tree_subset1_opc2,
                                 newdata = test_set,
                                 type = "prob")[,2]

ROC_tree_subset1.opc2 <- roc(test_set$default.payment.next.month, tree_subset1.opc2_pred) #queremos que nos devuelva el vector de col 2 porque representa el "default" (cuando y = 1)

auc(ROC_tree_subset1.opc2)


# Plot the cross-validated error rate as a function of the complexity parameter
plotcp(tree_subset1_opc2)

# Use printcp() to identify for which complexity parameter the cross-validated error rate is minimized

printcp(tree_subset1_opc2)

tree_subset1_opc2$cptable[, "xerror"]

# Create an index for of the row with the minimum xerror
index <- which.min(tree_subset1_opc2$cptable[, "xerror"])

# Create tree_min
tree_min <- tree_subset1_opc2$cptable[index, "CP"]
tree_min

#  Prune the tree using tree_min
ptree_subset1_opc2 <- prune(tree_subset1_opc2, cp = tree_min)

# Use prp() to plot the pruned tree
rpart.plot::prp(x = ptree_subset1_opc2)

plot(ptree_subset1_opc2)
text(ptree_subset1_opc2)



ptree_subset1_opc2_pred = predict(object = ptree_subset1_opc2,
                                  newdata = test_set,
                                  type = "prob")[,2]

ROC_ptree_subset1_opc2 = roc(test_set$default.payment.next.month, ptree_subset1_opc2_pred)

plot(ROC_ptree_subset1_opc2)
auc(ROC_ptree_subset1_opc2)



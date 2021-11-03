#---
  #title: Term Project on "Telco Churn Prediction using Logisitc Regression, Naive Bayes, KNN, Decision Tree and Random Forest"
  #prepared by Group 1
#---
rm(list=ls())

  #library packages
  #install.packages("plyr", "corrplot", "ggplot2", "gridExtra", "ggthemes","data.table", "caret", "MASS", "randomForest", "party","plyr",'imputeMissings')
  library(corrplot);library(ggplot2);library(gridExtra);library(randomForest);library(imputeMissings);library(plyr); #library(miscset)
  
  set.seed(10)
######## 1. Data Understanding
  #import data
  setwd('')
  data<-read.csv('Edinburgh_train_sample.csv')
  classes<-sapply(data,class)
  data<-read.csv('Edinburgh_train_sample.csv',header=TRUE,sep=',',colClasses=classes)

  data[data==""]=NA
  #summary statisics and observations
  dim(data) #dimensions of the data
  str(data) #data types
  head(data,5) #top 5 rows from the list
  names(data) #variables names from the list  
  summary(data)
  lapply(data, unique) #except continuous variables, only customerID contains fully unique values, which should be deleted as no predicting power can be brought from it.  
                       #Other categorical variable contain at least 2 unique values that might be used in further training. 
                       #There are variables that contain values of "No internet service" | "No internet service", which can be trimmed to --> "No".
                       #Tenure varaible values explain duration of being with company for customers and it will be grouped in to buckets later. 
  sapply(data, function(x) sum(is.na(x))) #missing values check: dataset contains 21 rows in 8 variables, which will be imputed later.
  
####### 2. Data Preparation
  #remove customerID as no predicting power from it
  data$customerID <-NULL
  #variables with "No internet service" | "No phone service" --> "No", which is done in order to trim the variety
  transform1 <- c("No phone service","No internet service") 
  transform2 <- which(Vectorize(function(x) x %in% transform1)(data), arr.ind=TRUE) #see rows & columns with "No internet service" | "No phone service" 
  cols_trnfrm <- c("MultipleLines", "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection","TechSupport", "StreamingTV","StreamingMovies") #columns from 8 to 15 contain values that need to be replaced.  
  library(plyr)
  for(i in 1:ncol(data[,cols_trnfrm])) { 
      data[,cols_trnfrm][,i] <- as.factor(mapvalues 
                                (data[,cols_trnfrm][,i], from =c("No phone service","No internet service"),to=c("No","No"))) 
  }
  lapply(data[,cols_trnfrm], unique) #recheck transformed values 
  
  #missing value: median
  install.packages('naniar')
  library(imputeMissings); library(naniar)
  missing_vars1 <- as.vector(colnames(data)[colSums(is.na(data)) > 0]) #create a vector for numerical missing vars
  data[data$PhoneService=="No",missing_vars1]<-0 #reflect as "0" for Phone Service - "No", as customers don't use the phone
  sapply(data, function(x) sum(is.na(x))) #check for remaining missing
  data_imp <- imputeMissings::impute(data = data, flag = FALSE) #impute with median remaining variables wigh Phone Service - "Yes"
  data<-as.data.frame(data_imp)
  sapply(data, function(x) sum(is.na(x))) #check for remaining missing
  data$VoiceMailPlan[data$PhoneService=="No"]<-"No"
  data$InternationalPlan[data$PhoneService=="No"]<-"No"
  sapply(data, function(x) sum(is.na(x))) #check for remaining missing
  
  #place group tenure into buckets, so the data will 
  min(data$tenure); max(data$tenure) #since minimum tenure is 1 and maximum is 72, we can group values into 5 buckets, representing each year
  group_tenure <- function(tenure){
                  if (tenure >= 0 & tenure <= 12){
                      return('0-12 Month')
                  }else if(tenure > 12 & tenure <= 24){
                            return('12-24 Month')
                  }else if (tenure > 24 & tenure <= 48){
                            return('24-48 Month')
                  }else if (tenure > 48 & tenure <=60){
                            return('48-60 Month')
                  }else if (tenure > 60){
                            return('> 60 Month')
                  }
  }
  data$tenure_group <- sapply(data$tenure,group_tenure)
  data$tenure_group <-as.factor(data$tenure_group )
  
####### 3. Exploratory data analysis after above steps
  #for analysis, let's visualize & review churn relationship in:
  #1)  men vs women?
  #2)  churn likelihood for senior citizens
  #3)  churn likelihood for customers with partners
  #4)  churn likelihood for customer with depedents
  
  # library(dplyr,warn.conflicts = F); library(ggplot2)
  # plotGender <- ggplot(data) +
  # geom_bar(aes(x = gender, fill = Churn), position = "dodge") #churners split almost evenly for both genders, will go for deeper review
  
  #data %>% 
    #group_by(gender,Churn) %>% 
    #summarise(n=n()) #plot's summary in table
  
  #plotSeniorCit <- ggplot(data) + 
    #geom_bar(aes(x = SeniorCitizen, fill = Churn), position = "dodge") #numbers show that from 84% non-senior customers have 22% churning rate and 16% of senior citizens have 25% churning rate.
  
  #data %>% 
    #group_by(SeniorCitizen) %>% 
    #summarise(n = n()) %>% 
    #mutate(freq = n / sum(n)) #plot's summary table split yes & no in Senior Citizens
  
  #data %>% 
    #group_by(SeniorCitizen, Churn) %>% 
    #summarise(n = n()) %>% 
    #mutate(freq = n / sum(n)) #plot's summary table in churning rate in split of Senior Citizens
  
  #plotPartner <- ggplot(data) +
    #geom_bar(aes(x=Partner, fill = Churn), position = "dodge") #partner category is split almost evenly, and partner="No" have 30% churn, while others only 14%
  
  #data %>% 
    #group_by(Partner) %>% 
    #summarise(n = n()) %>% 
    #mutate(freq = n / sum(n)) #plot's summary table split yes & no in Partner
  
  #data %>% 
    #group_by(Partner, Churn) %>% 
    #summarise(n = n()) %>% 
    #mutate(freq = n / sum(n)) #plot's summary table in churning rate in split of Partners
  
  #plotDepend <- ggplot(data) + 
    #geom_bar(aes_string(x="Dependents", fill="Churn"), position = "dodge") #majority of 67% of customers don't have dependents and they have churning rate of 27% churn.
  
  #data %>% group_by(Dependents) %>% 
    #summarise(n = n()) %>% 
    #mutate(freq = n / sum(n)) #plot's summary table split yes & no in Dependents
  
  #data %>% group_by(Dependents, Churn) %>% 
    #summarise(n=n()) %>% 
    #mutate(freq = n / sum(n)) #plot's summary table in churning rate in split of Dependents

  ###As preliminary outcome, it is should be highlighted that from 3 customer segments, dependents.no have highest number that result in churners. 
  ###So, in further visual analysis we will focus on them in more depth, to see other categories affected.  
  # library(miscset); library(ps)
  # dependents <- data %>% filter(Dependents == "No") #filter for plots variables selection
  # 
  # miscset::ggplotGrid(ncol=2,
  # lapply(c("PhoneService","MultipleLines","InternetService","OnlineSecurity","OnlineBackup",
  # "DeviceProtection"),
  # function(col){
  # ggplot(dependents,aes_string(col)) + geom_bar(aes(fill=Churn),position="dodge")
  # })) #visualiztion of customers from segment dependents.no for various variables in dataset
  # 
  # miscset::ggplotGrid(ncol=2,
  # lapply(c("TechSupport","StreamingTV","StreamingMovies","Contract",
  # "PaperlessBilling"),
  # function(col){
  # ggplot(dependents,aes_string(col)) + geom_bar(aes(fill=Churn),position="dodge")
  # })) #visualiztion of customers from segment dependents.no for various variables in dataset

  #ggplot(dependents) +
    #geom_bar(aes(x=PaymentMethod,fill=Churn), position = "dodge") #visualiztion of customers from segment dependents.no for various variables in dataset
  
###Outcome from exploratory  analysis:
  #Customers with no dependents tend to churn more in following attributes: 
            #Out of them, highest churners are shown in phone service yes. 
            #Vast majority of customer with fiber optic internet churned much more than people with DSL or no internet at all.  
            #People without online backup, device protection, and online security churn fairly frequently. 
            #Similarly to online backup and security, those without device protection tended to churn more than those that subscribed ot the service.  Adding device protection to their plans may be a good way to prevent churn.
            #Those without tech support tend to churn more frequently than those with tech support. 
  
  #correlation
  # library(reshape); library(corrplot)
  # ndata<-data[, sapply(data, is.numeric)]
  # a <- cor(ndata)
  # a[a == 1] <- NA #drop perfect
  # a[abs(a) < 0.5] <- NA # drop less than abs(0.5)
  # a <- na.omit(melt(a)) 
  # a[order(abs(a$value),decreasing = TRUE),] 
  # ###plot
  # # numeric.var <- sapply(data, is.numeric) ## Find numerical variables
  # # corr.matrix <- cor(data[,numeric.var])  ## Calculate the correlation matrix
  # # corrplot(corr.matrix)
  
  #finally, we remove variables with high correlated & not needed in further steps (as identified in above data preparation part)
  data$TotalCharges <- NULL #highly correlated with Monthly Charges
  data$TotalCall <- NULL ##highly correlated with Total Day Calls
  data$tenure <- NULL #remove tenure as it is not needed for analysis after we made grouping
  
  
######### The below code is not used when you predict the new test data. 
######### 4. Model Buiding
  # k-fold Cross-validation: train and test data
  # install.packages("hdi")
  library(AUC); library(glmnet); library(hdi); library(rpart);library("caret")

  #k- group
  k <- 10
  #create array containing fold-number for every row; 10th fold only has 9 numbers
  folds <- rep_len(1:k, nrow(data))
  #table(folds)

  #store AUC
  AUC_LR<-numeric()
  AUC_LRl<-numeric()
  optimal_coefv<-matrix()
  AUC_RF<- numeric()
  AUC_DT<- numeric() 
  AUC_KNN<- numeric()
  AUC_NB<- numeric()
  # Best_NB<-numeric()
  # BestAUC_NB<-0
  # Best_KNN<-numeric()
  # BestAUC_KNN<-0
  Best_RF<-list()
  BestAUC_RF<-0
  MSE_LRtest <-numeric()
  MSE_LRtrain<-numeric()
-----------------------------------------------------------------------------------------------------------------
######### Model 1: Logistic Regression
    for(i in 1:k) {
      # actual split of the data
      fold <- which(folds == i)
      train <- data[-fold,]
      test <- data[fold,]
      # separate the response
      y_train <- train$Churn
      train$Churn <- NULL
      y_test <- test$Churn
      test$Churn <- NULL
      
      ######## Model1. Logistic Regression
      LR1 <- glm(formula = y_train~., data = train, family = binomial("logit"))
      predLR1 <- as.numeric(predict(LR1,test, type='response'))
      predLRmse <- as.numeric(predict(LR1,train, type='response'))
      AUC_LR[i] <- AUC::auc(roc(predLR1, y_test))
      
      ####### MSE: Check whether it is overfitted becasue mean(MSE_LR) is positive
      MSE_LRtrain[i] <- mean((as.numeric(y_train) - predLRmse)**2)
      MSE_LRtest[i] <- mean((as.numeric(y_test) - predLR1)**2)
      MSE_LR<-MSE_LRtest-MSE_LRtrain
      mean(MSE_LR)

      
      ######## Lasso: k-fold cross validation
      # original lasso with 10 folds
      LRl0<-glmnet(x=data.matrix(train),y=y_train,family="binomial")
      # plot(LRl0, xvar= 'lambda')
      # lasso with train data using 10 folds to obtain min lambda
      LRl <- cv.glmnet(x=data.matrix(train),y=y_train, family="binomial",nfolds=10,alpha = 1,keep = TRUE)
      # plot(LRl)
      #Predict: use min lambda
      predLR <- as.numeric(predict(LRl,newx=data.matrix(test), type="response",s=LRl$lambda.min))
      AUC_LRl[i] <- AUC::auc(roc(predLR,y_test))
      optimal_coef <- coef(LRl, s = LRl$lambda.min)
      optimal_coefv[i] <- data.frame(name = optimal_coef@Dimnames[[1]][optimal_coef@i + 1], coefficient = optimal_coef@x)
    }

  
  
-----------------------------------------------------------------------------------------------------------------  
######### Model 2: Naive Bayes
    if (!require('e1071')) { 
    install.packages('e1071',
                     repos="https://cran.rstudio.com/",
                     quiet=TRUE)
    require('e1071') 
     }
  
  for(i in 1:k) {
    # actual split of the data
    fold <- which(folds == i)
    train <- data[-fold,]
    test <- data[fold,]
    # separate the response
    y_train <- train$Churn
    train$Churn <- NULL
    y_test <- test$Churn
    test$Churn <- NULL
    
    NB <- naiveBayes(x = train, y = y_train)
    predNB <- predict(NB,test, type = "raw", threshold = 0.001)[,2]
    AUC_NB[i] <- AUC::auc(roc(predNB, as.factor(y_test)))
    predict<-predict(NB,test)
    # if (AUC_NB[i]>BestAUC_NB){
    #   BestAUC_NB<-AUC_NB[i]
    #   Best_NB<-NB
    # }
    #plot(roc(predNB, y_test))
    
  }
------------------------------------------------------------------------------------------------------------------
####### Model 3: Decision Tree
      if (!require('rpart')) { 
        install.packages('rpart',
                         repos="https://cran.rstudio.com/",
                         quiet=TRUE)
        require('rpart') 
      }
    
#cross validation  
  
  #split into Train and test data
  ind <-sample(x =1:nrow(data), size = nrow(data),replace = FALSE)
  trainind <- ind[1:round(length(ind)*.70)]
  valind <- ind[(round(length(ind)*.70)+1):round(length(ind)*.85)]
  testind <- ind[(round(length(ind)*.85)+1):length(ind)] 
  
  #Test whether there are no intersects
  intersect(trainind, testind)
  intersect(trainind, valind)
  intersect(valind, testind)
  
  
  #Create the sets and separate the response
  train <- data[trainind,]
  y_train <- train$Churn
  train$Churn <- NULL
  
  val <- data[valind,]
  y_val <- val$Churn
  val$Churn <- NULL
  
  test <- data[testind,]
  y_test <- test$Churn
  test$Churn <- NULL
  
 
  trainBIG <- rbind(train,val)
  y_trainBIG <- as.factor(c(as.character(y_train),as.character(y_val)))
  
  
  #Build the model and evaluate performance: remove 2 variables of the 14 important variables from lasso
  #since we tried the 12 variables, it shows the reasonable picture  
  (DT <- rpart(y_trainBIG ~OnlineSecurity+InternetService+Contract+PaperlessBilling+CustomerServiceCalls+tenure_group+TechSupport+MonthlyCharges+Dependents+DeviceProtection+StreamingMovies+InternationalPlan, trainBIG))
   
    predDT <- predict(object = DT,newdata = test, type = 'prob')[,2]
    (AUC_DT <- AUC::auc(roc(predDT,y_test)))
    
    
    par(xpd = TRUE)
    #plot.rpart
    plot(DT, compress = TRUE, minbranch = 1)
    text(DT, use.n = TRUE)
    
    #Pretty plot, easy syntax
    library(rpart.plot)
    rpart.plot(DT)
    
    #Now tune the the complexity parameter
    candidates <- seq(0.00,0.1,by=0.001)
    aucstore <- numeric(length(candidates))
    j <- 0
    for (i in candidates) {
      j <- j + 1
      tree <- rpart(y_train ~ .,control=rpart.control(cp = i),
                    data = train)
      predTree <- predict(tree,test)[,2]
      aucstore[j] <- AUC::auc(roc(predTree,y_test))
      if (j %% 20==0) cat(j/length(candidates)*100,"% finished\n")
    }
    
    plot(aucstore)
    #Select the pest cp parameter
    which.max(aucstore) 
    
    #Re-model and evaluate on the test set
    tree <- rpart(y_train ~ .,
                  control=rpart.control(cp = candidates[which.max(aucstore)])
                  ,train)
    predTree <- predict(tree,test)[,2]
    AUC_DT <- AUC::auc(roc(predTree,y_test))
    plot(roc(predTree,y_test))
    
    #The Gini function looks like this:
    plot(seq(0,1,0.1),seq(0,1,0.1)-seq(0,1,0.1)^2,type="l",
         ylab="Gini index",xlab="Proportion of ones")
-----------------------------------------------------------------------------------------------------------
######## Model 4: Random Forest
      if (!require('randomForest')) { 
        install.packages('randomForest',
                         repos="https://cran.rstudio.com/",
                         quiet=TRUE)
        require('randomForest') 
      }
    
  #loop for RF initial model
  for(i in 1:k) {
      #actual split of the data
      fold <- which(folds == i)
      train <- data[-fold,]
      test <- data[fold,]
      #separate the response
      y_train <- train$Churn
      train$Churn <- NULL
      y_test <- test$Churn
      test$Churn <- NULL
  
  #create random forest model
  rFmodel <- randomForest(x=train,
                          y=y_train,
                          ntree=500,
                          importance=TRUE)
    #look at the importance of the variables
  importance(rFmodel, type=1)
  # varImpPlot(rFmodel,type=1)
  
  #prediction
  predrF <- predict(rFmodel,test,type="prob")[,2]
  
  #assess final performance
  AUC_RF[i] <- AUC::auc(roc(predrF, y_test))
  library("caret")
  
  if (AUC_RF[i] > BestAUC_RF & AUC_RF[i]!= 1){
       Best_predrF <- predict(rFmodel,test)
       BestAUC_RF <- AUC_RF[i]
       Best_RF <- predrF
       conf_matrix <- confusionMatrix(y_test,Best_predrF)
       (plot(roc(Best_predrF,y_test)))
       varImpPlot(rFmodel,type=1)
  }
  }
-----------------------------------------------------------------------------------------------------------
######## Model 5: KNN
  #data transformation 1: e.g. No internet service-->NO
  data$MultipleLines <- as.numeric(mapvalues(data$MultipleLines, 
                                               from=c("No","Yes"),
                                               to=c("0","1")))-1
  data$OnlineSecurity <- as.numeric(mapvalues(data$OnlineSecurity, 
                                              from=c("No","Yes"),
                                              to=c("0","1")))-1
  data$OnlineBackup <- as.numeric(mapvalues(data$OnlineBackup, 
                                            from=c("No","Yes"),
                                            to=c("0","1")))-1
  data$DeviceProtection <- as.numeric(mapvalues(data$DeviceProtection, 
                                                from=c("No","Yes"),
                                                to=c("0","1")))-1
  data$TechSupport<- as.numeric(mapvalues(data$TechSupport, 
                                          from=c("No","Yes"),
                                          to=c("0","1")))-1
  data$StreamingTV<- as.numeric(mapvalues(data$StreamingTV, 
                                          from=c("No","Yes"),
                                          to=c("0","1")))-1
  data$StreamingMovies<- as.numeric(mapvalues(data$StreamingMovies, 
                                              from=c("No","Yes"),
                                              to=c("0","1")))-1
  data$InternationalPlan <- as.numeric(mapvalues(data$InternationalPlan,
                                                 from=c("No","Yes"),
                                                 to=c("0","1")))-1
  data$VoiceMailPlan<- as.numeric(mapvalues(data$VoiceMailPlan,
                                            from=c("No","Yes"),
                                            to=c("0","1")))-1
  data$PaperlessBilling<- as.numeric(mapvalues(data$PaperlessBilling,
                                               from=c("No","Yes"),
                                               to=c("0","1")))-1
  data$PhoneService<- as.numeric(mapvalues(data$PhoneService,
                                           from=c("No","Yes"),
                                           to=c("0","1")))-1
  # Female: 0, Male: 1
  data$gender<- as.numeric(mapvalues(data$gender,
                                     from=c("Female","Male"),
                                     to=c("0","1")))-1
  data$Partner<- as.numeric(mapvalues(data$Partner,
                                      from=c("No","Yes"),
                                      to=c("0","1")))-1
  data$Dependents<- as.numeric(mapvalues(data$Dependents,
                                         from=c("No","Yes"),
                                         to=c("0","1")))-1
  data$Churn<- as.numeric(mapvalues(data$Churn,
                                    from=c("No","Yes"),
                                    to=c("0","1")))-1
  data$Contract<- as.numeric(data$Contract)-1
  data$InternetService <- as.numeric(data$InternetService)-1
  data$PaymentMethod <- as.numeric(data$PaymentMethod)-1
  data$tenure_group <- as.numeric(data$tenure_group)-1 
  data$SeniorCitizen <- as.numeric(data$SeniorCitizen)
  
  if (!require('FNN')) { 
    install.packages('FNN',
                     repos="https://cran.rstudio.com/",
                     quiet=TRUE)
    require('FNN') 
  }
  
  ##### k-fold cross validation
  #k group
  k <- 4
  # create array containing fold-number for every row; 10th fold only has 9 numbers
  folds <- rep_len(1:k, nrow(data))
  
  # Loop
  for(i in 1:k) {
    # actual split of the data
    fold <- which(folds == i)
    train <- data[-fold,]
    test <- data[fold,]
    # separate the response
    y_train <- as.factor(train$Churn)
    train$Churn <- NULL
    y_test <- as.factor(test$Churn)
    test$Churn <- NULL
    
    #### Check whether all columns are numeric
    all(sapply(train, is.numeric))
    
    
    #### Scale the data which are not two levels: center with the mean and the stdev
    trainKNN <- scale(train, center = TRUE, scale = TRUE)
    testKNN <- scale(test, center = TRUE, scale = TRUE)
    
    #### Build a KNN
    aucs <- numeric() 
    j <- 0
    for (a in 1:10) { 
      j <- j + 1
      #retrieve the indicators of the k nearest neighbors of the query data
      indicatorsKNN <- as.integer(knnx.index(data=trainKNN,
                                             query=testKNN,
                                             k=a))
      #retrieve the actual y from the training set
      predKNN <- as.integer(as.character(y_train[indicatorsKNN]))
      #if k > 1 then we take the proportion of 1s
      predKNN <- rowMeans(data.frame(matrix(data=predKNN,
                                            ncol=a,
                                            nrow=nrow(testKNN))))
      #auc
      aucs[j] <- AUC::auc(roc(predKNN,y_test))
      
    }
    
    #p <- barplot(aucs, names.arg = 1:10, ylim = c(0,max(aucs)+0.10))
    #text(x=p, y=aucs+0.05, labels=round(aucs,4))
    #find the maximum aucs
    b <- as.numeric(which.max(aucs))
    
    #Now, re-model KNN with test and k = 3
    indicatorsKNN <- as.integer(as.character(knnx.index(data=trainKNN,
                                                        query=testKNN,
                                                        k=b)))
    
    #retrieve the actual y from the training set
    predKNN <- as.integer(as.character(y_train[indicatorsKNN]))
    
    #if k > 1 then we take the proportion of 1s
    predKNN <- rowMeans(data.frame(matrix(data=predKNN,
                                          ncol=b,
                                          nrow=nrow(testKNN))))
    #auc
    
    (AUC_KNN[i] <- AUC::auc(roc(predKNN, y_test)))
    # if (AUC_KNN[i] > BestAUC_KNN & AUC_KNN[i]!= 1){
    #   BestAUC_KNN <- AUC_KNN[i]
    #   Best_KNN <- predKNN
    # }

  }    
########3.0 Performance Metrics NEED REVISION
  ##3.1 confusion matrix
  ##3.2 ROC:Accuracy,Sensitivity
  # plot(roc(predLRl, y_test),col="blue")
  # plot(roc(predNB, y_test), add=TRUE, col="red")
  # plot(roc(predKNN, y_test), add=TRUE, col="green")
  # plot(roc(predTree,y_test), add=TRUE, col="yellow")
  # plot(roc(predDF, y_test), add=TRUE, col="black")
 


  ##3.2 AUC
  (AUCKNN <- mean(AUC_KNN))
  (AUCLR<-mean(AUC_LR))
  (AUCLRl<-mean(AUC_LRl))
  (AUCNB <- mean(AUC_NB))
  (AUCRF <- mean(AUC_RF))
  (AUCDT <- AUC_DT)


######## 4.Plot for Learning Curve
  # ## 4.1 AUC of LR
  # library(ggplot2)
  # x=c(1:10)
  # f=data.frame(sapply(AUC_LR, function(x) as.numeric(as.character(x))))
  # ggplot(data=f,aes(x=x , y= AUC_LR))+geom_line(data=f,size=2,colour="grey") +
  #       geom_point(data=f,size = 2, shape = 22, colour = "red", fill = "red")+
  #       xlim(c("1","2","3","4","5","6","7","8","9","10"))+
  #       labs(x= "Times of K Fold Cross Validation" , y = "AUC", title = "AUC of Logistic Regression")+
  #       theme(plot.title=element_text(hjust = 0.5,size = 20,color = "black"))+
  #       geom_hline(yintercept = 0.5,linetype="dashed")
  # 
  # 
  # # #AUC of Lasso LR
  # x=c(1:10)
  # f2=data.frame(sapply(AUC_LRl, function(x) as.numeric(as.character(x))))
  # ggplot(data=f2,aes(x=x , y= AUC_LRl))+geom_line(data=f2,size=2,colour="grey") +
  #       geom_point(data=f2,size = 2, shape = 22, colour = "red", fill = "red")+
  #       xlim(c("1","2","3","4","5","6","7","8","9","10"))+
  #       labs(x= "Times of K Fold Cross Validation" , y = "AUC", title = "AUC of Logistic Lasso Regression")+
  #       theme(plot.title=element_text(hjust = 0.5,size = 20,color = "black"))+
  #       geom_hline(yintercept = 0.5,linetype="dashed")
  # 
  # # #4.2 AUC of NB
  # x=c(1:10)
  # f=data.frame(sapply(AUC_NB, function(x) as.numeric(as.character(x))))
  # ggplot(data=f,aes(x=x , y= AUC_NB))+geom_line(data=f,size=2,colour="grey") +
  #       geom_point(data=f,size = 2, shape = 22, colour = "red", fill = "red")+
  #       xlim(c("1","2","3","4","5","6","7","8","9","10"))+
  #       labs(x= "Times of K Fold Cross Validation" , y = "AUC", title = "AUC of Naive Bayes")+
  #       theme(plot.title=element_text(hjust = 0.5,size = 20,color = "black"))+
  #       geom_hline(yintercept = 0.5,linetype="dashed")
  # 
  # # #4.3 AUC of KNN
  # x=c(1:4)
  # f=data.frame(sapply(AUC_KNN, function(x) as.numeric(as.character(x))))
  # ggplot(data=f,aes(x=x , y= AUC_KNN))+geom_line(data=f,size=2,colour="grey") +
  #       geom_point(data=f,size = 2, shape = 22, colour = "red", fill = "red")+
  #       xlim(c("1","2","3","4"))+
  #       labs(x= "Times of K Fold Cross Validation" , y = "AUC", title = "AUC of KNN")+
  #       theme(plot.title=element_text(hjust = 0.5,size = 20,color = "black"))+
  #       geom_hline(yintercept = 0.5,linetype="dashed")
  # #4.4 AUC of KNN
  # x=c(1:10)
  # f=data.frame(sapply(AUC_RF, function(x) as.numeric(as.character(x))))
  # ggplot(data=f,aes(x=x , y= AUC_RF))+geom_line(data=f,size=2,colour="grey") +
  #       geom_point(data=f,size = 2, shape = 22, colour = "red", fill = "red")+
  #       xlim(c("1","2","3","4"))+
  #       labs(x= "Times of K Fold Cross Validation" , y = "AUC", title = "AUC of RF")+
  #       theme(plot.title=element_text(hjust = 0.5,size = 20,color = "black"))+
  #       geom_hline(yintercept = 0.5,linetype="dashed")

  
  
#===================================================================
    
# ######## Run test data
# Run train data again since we transform train data into numeric variables! Thank you
# ######## Data Understanding
  #import data
  testdata<-read.csv('Edinburgh_test.csv')
  ####### Data Preparation
  testdata[testdata==""]=NA
  

  #remove customerID as no predicting power from it
  CusotmerID<-testdata$customerID
  testdata$customerID <-NULL
  #variables with "No internet service" | "No phone service" --> "No", which is done in order to trim the variety
  transform1 <- c("No phone service","No internet service")
  transform2 <- which(Vectorize(function(x) x %in% transform1)(testdata), arr.ind=TRUE) #see rows & columns with "No internet service" | "No phone service"
  cols_trnfrm <- c("MultipleLines", "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection","TechSupport", "StreamingTV","StreamingMovies") #columns from 8 to 15 contain values that need to be replaced.
  library(plyr)
  for(i in 1:ncol(testdata[,cols_trnfrm])) {
    testdata[,cols_trnfrm][,i] <- as.factor(mapvalues
                                        (testdata[,cols_trnfrm][,i], from =c("No phone service","No internet service"),to=c("No","No")))
  }
  lapply(testdata[,cols_trnfrm], unique) #recheck transformed values

  #missing value: median
  #install.packages('naniar')
  library(imputeMissings); library(naniar)
  missing_vars1 <- as.vector(colnames(testdata)[colSums(is.na(testdata)) > 0]) #create a vector for numerical missing vars
  testdata[testdata$PhoneService=="No",missing_vars1]<-0 #reflect as "0" for Phone Service - "No", as customers don't use the phone
  sapply(testdata, function(x) sum(is.na(x))) #check for remaining missing
  testdata_imp <- imputeMissings::impute(data = testdata, flag = FALSE) #impute with median remaining variables wigh Phone Service - "Yes"
  testdata<-as.data.frame(testdata_imp)
  sapply(testdata, function(x) sum(is.na(x))) #check for remaining missing
  testdata$VoiceMailPlan[testdata$PhoneService=="No"]<-"No"
  testdata$InternationalPlan[testdata$PhoneService=="No"]<-"No"

  #place group tenure into buckets, so the data will
  min(testdata$tenure); max(testdata$tenure) #since minimum tenure is 1 and maximum is 72, we can group values into 5 buckets, representing each year
  group_tenure <- function(tenure){
    if (tenure >= 0 & tenure <= 12){
      return('0-12 Month')
    }else if(tenure > 12 & tenure <= 24){
      return('12-24 Month')
    }else if (tenure > 24 & tenure <= 48){
      return('24-48 Month')
    }else if (tenure > 48 & tenure <=60){
      return('48-60 Month')
    }else if (tenure > 60){
      return('> 60 Month')
    }
  }
  testdata$tenure_group <- sapply(testdata$tenure,group_tenure)
  testdata$tenure_group <-as.factor(testdata$tenure_group )

  #finally, we remove variables with high correlated & not needed in further steps (as identified in above data preparation part)
  testdata$TotalCharges <- NULL #highly correlated with Monthly Charges
  testdata$TotalCall <- NULL ##highly correlated with Total Day Calls
  testdata$tenure <- NULL #remove tenure as it is not needed for analysis after we made grouping


  #Check every variables in two data
  names(data)
  names(testdata)

  y_train <- data$Churn
  data$Churn <- NULL

  library(randomForest)
  rFmodel <- randomForest(x=data,
                          y=y_train,
                          ntree=500,
                          importance=TRUE)

  Churn <- predict(rFmodel,newdata =testdata)
  
  
  # write the results into new.Excel
  Result<-cbind(as.data.frame(CusotmerID),Churn)
  colnames(Result) = c("CusotmerID", "Churn")

  write.table(Result,file="Result.csv",append=F,sep=",",col.names=T,row.names=F)

 
  
  
                    
  



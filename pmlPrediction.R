### Greg Janesch, last updated 01/24/2015
### Description: function for predicting how a barbell lift was performed, given certain training data


pmlPrediction <- function(){
    
    library(caret)
    
    ## Read in the data set
    training <- read.csv("pml-training.csv")
    
    ## Convert non-numeric columns to numeric
    columnClasses <- sapply(training,class)
    for(i in 8:159){
        if(columnClasses[i] != "numeric"){
            if(columnClasses[i] == "factor"){
                training[training[,i] == "",i] <- NA
                training[,i] <- as.numeric(as.character(training[,i]))
            }
            training[,i] <- as.numeric(training[,i])
        }
    }
    
    ## Eliminate all columns with NAs
    NAcount <- sapply(training, function(x) sum(is.na(x)))
    training <- training[,ifelse(NAcount == 0, TRUE, FALSE)]
    
    ## The first seven columns contain data that isn't relevant here
    training <- training[,-c(1:7)]
    
    avgright <- 0
    a <- 10
    
    ## Divide up the training data into a training subset and a test subset
    splitter <- createDataPartition(training$classe, p = 0.6, list = FALSE)
    training_train <- training[splitter,]
    training_test <- training[-splitter,]
    
    ## Train the model
    modFit <- train(data = training_train, classe ~ yaw_belt + accel_belt_z + gyros_arm_x + accel_arm_x +
                    roll_dumbbell + gyros_dumbbell_x)
    
    ## Make predictions and report back the accuracies of the model
    pred_train <- predict(modFit, training_train)
    print(paste("Fraction correct on training:", sum(pred_train == training_train$classe)/length(pred_train)))
    pred_test <- predict(modFit, training_test)
    print(paste("Fraction correct on testing:", sum(pred_test == training_test$classe)/length(pred_test)))
    
    return(training)
    
}
pmlPrediction <- function(){
    
    library(caret)
    
    #set.seed(1701)
    
    # Read in the data set
    training <- read.csv("pml-training.csv")
    
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
    
    training <- training[,-c(1:7)]
    #training <- training[,-c(1,2,56)]
    
    avgright <- 0
    a <- 10
    
    #for(i in 1:a){
        
        #print(paste("Now testing iteration number ", i, "...", sep = ""))
        splitter <- createDataPartition(training$classe, p = 0.6, list = FALSE)
        training_train <- training[splitter,]
        training_test <- training[-splitter,]
        
        modFit <- train(data = training_train, classe ~ yaw_belt + accel_belt_z + gyros_arm_x + accel_arm_x + roll_dumbbell + gyros_dumbbell_x)
        
        pred <- predict(modFit, training_train)
        print(paste("Fraction correct on training:", sum(pred == training_train$classe)/length(pred)))
        
        pred <- predict(modFit, training_test)
        print(paste("Fraction correct on testing:", sum(pred == training_test$classe)/length(pred)))
        
        #avgright <- avgright + pred
        
    #}
    
    #avgright <- avgright/a
    #print(paste("Average proportion correct:", avgright))
    
    return(training)
    
    #modFit <- train(classe ~ yaw_belt + accel_belt_z + gyros_arm_x, data = training_train)
}
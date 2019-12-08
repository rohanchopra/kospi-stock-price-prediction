deep.learning.class <- function(){
    
    
    library(h2o)
    library(ggplot2)
    
    h2o.init(nthreads=1,max_mem_size="3g",enable_assertions=FALSE)
    #Use the init below if you want faster computation. Reproducable models won't be generated
    #h2o.init(nthreads=1,max_mem_size="3g",enable_assertions=FALSE)
    #setwd(getSrcDirectory(function(x) {x}))
    
    
    raw.data <- read.csv(file = "CleanData1.csv",header = TRUE,sep = ",")
    
    # For classification
    raw.data[,"Company"] <- as.factor(raw.data[,"Company"])
    raw.data[,"case1"] <- as.factor(raw.data[,"case1"])
    # predicting only case 1
    # raw.data[,"case2"] <- as.factor(raw.data[,"case2"])
    # raw.data[,"case3"] <- as.factor(raw.data[,"case3"])
    # raw.data[,"case4"] <- as.factor(raw.data[,"case4"])
    raw.data[,"Quarter"] <- as.factor(raw.data[,"Quarter"])
    raw.data[,"Year"] <- as.factor(raw.data[,"Year"])
    
    set.seed(1)
    #80% train 20% test
    indexes = sample(1:nrow(raw.data),size=0.2*nrow(raw.data))
    test=raw.data[indexes,]
    train=raw.data[-indexes,]
    
    
    train.hex<-as.h2o(train)
    test.hex<-as.h2o(test)
    all.hex<-as.h2o(raw.data)
    
    regression_model<-h2o.deeplearning(
        training_frame =train.hex,
        validation_frame = test.hex,
        hidden=c(280,110,45),
        epochs=30.0,
        activation="Rectifier",
        x = -c(95),
        y=3,
        seed = 100,
        nfolds=10,
        reproducible = T,
        balance_classes= T,
        fast_mode = F)
    print(regression_model)
    predictions <- as.data.frame(h2o.predict(regression_model, test.hex))
    #print(predictions)
    
    predictions["actual"]<-test[3]
    #print(head(predictions))
    write.csv(predictions,file = "ActualvsPredictedDeepLearning.csv",row.names = FALSE)
    
    roc <- as.data.frame(regression_model@model$validation_metrics@metrics$thresholds_and_metric_scores$tpr)
    roc <- cbind(regression_model@model$validation_metrics@metrics$thresholds_and_metric_scores$fpr,roc)
    colnames(roc) <- c("fpr","tpr")
    write.csv(roc,file = "rocValidationDeepLearning.csv",row.names = FALSE)
    
    roc <- as.data.frame(regression_model@model$cross_validation_metrics@metrics$thresholds_and_metric_scores$tpr)
    roc <- cbind(regression_model@model$cross_validation_metrics@metrics$thresholds_and_metric_scores$fpr,roc)
    colnames(roc) <- c("fpr","tpr")
    write.csv(roc,file = "rocCrossValidationDeepLearning.csv",row.names = FALSE)
    
    #ROC
    #plot(h2o.performance(regression_model,xval = T),type='roc',typ='b')
    
    #plot <- ggplot(data=predicted,aes(x=predict,y=actual)) + geom_point(color='blue',size=1) + geom_line(aes(predict,predict),size=1 ) + labs(title="Temperature",x="Predicted",y="Actual")
    #print(p)
    #plot(predictions$p0,predictions$p1)
    #plot( predictions$p1,predictions$predict,type="l")
}

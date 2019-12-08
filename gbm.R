gbm.class <- function(){
    
    library(h2o)
    library(ggplot2)
    
    h2o.init(nthreads=-1,max_mem_size="3g",enable_assertions=FALSE)
    setwd(getSrcDirectory(function(x) {x}))
    
    
    raw.data <- read.csv(file = "CleanData.csv",header = TRUE,sep = ",")
    
    raw.data[,"Company"] <- as.factor(raw.data[,"Company"])
    raw.data[,"case1"] <- as.factor(raw.data[,"case1"])
    # raw.data[,"case2"] <- as.factor(raw.data[,"case2"])
    # raw.data[,"case3"] <- as.factor(raw.data[,"case3"])
    # raw.data[,"case4"] <- as.factor(raw.data[,"case4"])
    raw.data[,"Quarter"] <- as.factor(raw.data[,"Quarter"])
    raw.data[,"Year"] <- as.factor(raw.data[,"Year"])
    
    set.seed(1)
    indexes = sample(1:nrow(raw.data),size=0.2*nrow(raw.data))
    test=raw.data[indexes,]
    train=raw.data[-indexes,]
    
    
    train.hex<-as.h2o(train)
    test.hex<-as.h2o(test)
    
    regression_model<-h2o.gbm(
        training_frame =train.hex,
        validation_frame=test.hex,
        y=3,
        nfolds=10,
        seed =100,
        ntrees = 50)
    print(regression_model)
    
    predictions <- as.data.frame(h2o.predict(regression_model, test.hex))
    #print(predictions)
    
    predictions["actual"]<-test[3]
    #print(head(predictions))
    write.csv(predictions,file = "ActualvsPredictedGBM.csv",row.names = FALSE)
    
    roc <- as.data.frame(regression_model@model$validation_metrics@metrics$thresholds_and_metric_scores$tpr)
    roc <- cbind(regression_model@model$validation_metrics@metrics$thresholds_and_metric_scores$fpr,roc)
    colnames(roc) <- c("fpr","tpr")
    write.csv(roc,file = "rocValidationGBM.csv",row.names = FALSE)
    
    roc <- as.data.frame(regression_model@model$cross_validation_metrics@metrics$thresholds_and_metric_scores$tpr)
    roc <- cbind(regression_model@model$cross_validation_metrics@metrics$thresholds_and_metric_scores$fpr,roc)
    colnames(roc) <- c("fpr","tpr")
    write.csv(roc,file = "rocCrossValidationGBM.csv",row.names = FALSE)
    
    #ROC
    plot(h2o.performance(regression_model,xval = T),type='roc',typ='b')
}





p <- plot_ly(dl,x=~fpr,y=~tpr,type='scatter',mode='lines',name="Deep Learning ROC") %>%
    add_trace(x=a,y=a,name="Baseline",line = list(dash='dash')) %>%
    layout(title="ROC")







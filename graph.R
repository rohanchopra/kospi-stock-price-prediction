plotly.roc.graph <- function(){
    library(plotly)
    
    gbm <- read.csv(file = "rocGBM.csv")
    dl <- read.csv(file = "rocDeepLearning.csv")
    rf <- read.csv(file = "rocRandomForest.csv")
    
    colnames(rf) <- c("fpr","tpr")
    colnames(rf) <- c("fpr","tpr")
    colnames(gbm) <- c("fpr","tpr")
    
    df <- cbind(dl,rf)
    df <- cbind(df,gbm)
    
    colnames(df) <- c("fprd","tprd","fprr","tprr","fprg","tprg")
    
    
    plot_ly(df,x=~fprd,y=~tprd,type='scatter',mode='lines',name="Deep Learning") %>%
        add_trace(x=~fprr,y=~tprr,name="Random Forest") %>%
        add_trace(x=~fprg,y=~tprg,name="GBM") %>%
        add_trace(x=a,y=a,name="Baseline",line = list(dash='dash')) %>%
        layout(title="ROC")
    
    
}
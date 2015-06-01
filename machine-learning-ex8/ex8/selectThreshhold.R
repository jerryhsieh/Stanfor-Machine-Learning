#
#
# 
selectThreshhold <- function(yval, pval) {
    
    bestThreshhold <- 0
    bestF1 <- 0
    F1 <- 0
    
    stepsize <- (max(pval) - min(pval))/1000
    
    st <- seq(min(pval), max(pval), by=stepsize)
    
    for (epsilon in st) {
        cvPreditions <- (pval < epsilon)
        
        fp = sum((cvPreditions == 1) & (yval == 0));
        tp = sum((cvPreditions == 1) & (yval == 1));
        fn = sum((cvPreditions == 0) & (yval == 1));
        
        prec = (tp) /(tp + fp);
        rec = (tp) / (tp + fn);
        
        F1 = (2 * prec * rec) / (prec + rec);
        

        if (!is.nan(F1) & F1 > bestF1) {
            bestF1 <- F1
            bestEpsilon <- epsilon
        }
    }
    
    best <- c(bestF1, bestEpsilon)
    
}
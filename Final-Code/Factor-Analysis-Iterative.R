set.seed(478)
X = totalDaily[,1:11]

n <- length(totalDaily$Snowfall14DaysLater)
trn <- sample(seq_len(n), .8*n)

rmse <- c()
count = 1;

for(i in c(1,2,3)) {
  for(j in c("varimax", "none")) {
    f <- fa(r = X, nfactors = i, rotate = j, fm = "mle")
    p <- principal(r = X, nfactors = i, rotate = j)
    
    #MLE
    data <- cbind(totalDaily$Snowfall14DaysLater, f$scores)
    data = as.data.frame(data) 
    
    training <- data[trn,]
    testing <- data[-trn,]
    
    model <- lm(V1 ~ ., data = training)
    summary(model)
    
    pred <- predict(model, newdata = testing, type = "response")
    cbind(testing$V1, pred)
    
    rmse[count] <- RMSE(testing$V1, pred)
    count = count + 1;
    
    #Principal
    data <- cbind(totalDaily$Snowfall14DaysLater, p$scores)
    data = as.data.frame(data) 
    
    training2 <- data[trn,]
    testing2 <- data[-trn,]
    
    model2 <- lm(V1 ~ ., data = training2)
    summary(model2)
    
    pred2 <- predict(model2, newdata = testing2, type = "response")
    cbind(testing2$V1, pred2)
    
    rmse[count] <- RMSE(testing2$V1, pred2)
    count = count + 1;
  }
}

rmse

# Reproducible imbalanced-learning workflow: chronological split, validation threshold,
# untouched final test metrics. All fitting and preprocessing are based on train rows.

split_timewise <- function(data) {
  n <- nrow(data)
  i_train <- floor(.70 * n); i_valid <- floor(.85 * n)
  if (i_train < 20 || i_valid - i_train < 5 || n - i_valid < 5)
    stop("Not enough rows for chronological train / validation / test partitions.")
  parts <- list(train=data[seq_len(i_train),,drop=FALSE],
                validation=data[seq.int(i_train+1L,i_valid),,drop=FALSE],
                test=data[seq.int(i_valid+1L,n),,drop=FALSE])
  if (any(vapply(parts, function(x) length(unique(x$Class)) < 2L, logical(1))))
    stop("Each chronological partition needs both classes. Try another dataset or review its time ordering.")
  parts
}

prepare_features <- function(train, parts) {
  features <- setdiff(names(train), "Class")
  medians <- vapply(train[features], function(x) median(x, na.rm=TRUE), numeric(1))
  for (nm in features) {
    for (part in names(parts)) {
      z <- parts[[part]][[nm]]
      z[!is.finite(z)] <- NA_real_
      z[is.na(z)] <- medians[[nm]]
      parts[[part]][[nm]] <- z
    }
  }
  keep <- features[vapply(parts$train[features], function(x) length(unique(x)) > 1L, logical(1))]
  if (!length(keep)) stop("No varying numeric features are available for training.")
  centers <- vapply(parts$train[keep], mean, numeric(1))
  spreads <- vapply(parts$train[keep], sd, numeric(1))
  spreads[!is.finite(spreads) | spreads == 0] <- 1
  scaled <- parts
  for (part in names(scaled)) for (nm in keep)
    scaled[[part]][[nm]] <- (scaled[[part]][[nm]] - centers[[nm]]) / spreads[[nm]]
  list(parts=parts, scaled=scaled, features=keep, medians=medians, centers=centers, spreads=spreads)
}

fit_one_model <- function(name, train, features) {
  y <- train$Class
  imbalance <- sum(y == 0) / max(1, sum(y == 1))
  weights <- ifelse(y == 1, min(50, imbalance), 1)
  formula <- stats::reformulate(features, response="Class")
  if (name == "Logistic regression") {
    # `train` is already scaled using train-period statistics.
    z <- train[c("Class", features)]
    fit <- stats::glm(formula, data=z, family=stats::binomial(), weights=weights,
                      control=stats::glm.control(maxit=80))
    return(list(kind="glm", fit=fit, features=features, centers=centers, spreads=spreads))
  }
  if (name == "Decision tree") {
    z <- train; z$Class <- factor(z$Class, levels=c(0,1))
    fit <- rpart::rpart(formula, data=z, method="class", weights=weights,
                        control=rpart::rpart.control(cp=.002, maxdepth=6, minbucket=20))
    return(list(kind="rpart", fit=fit, features=features))
  }
  if (name == "Class-weighted random forest") {
    z <- train; z$Class <- factor(z$Class, levels=c(0,1))
    fit <- ranger::ranger(formula, data=z, probability=TRUE, num.trees=180,
                          mtry=max(1,floor(sqrt(length(features)))), min.node.size=10,
                          class.weights=c(1,min(50,imbalance)), seed=2026,
                          respect.unordered.factors="order", num.threads=2)
    return(list(kind="ranger", fit=fit, features=features))
  }
  if (name == "Gradient boosting") {
    fit <- gbm::gbm(formula, data=train, distribution="bernoulli", weights=weights,
                    n.trees=220, interaction.depth=2, n.minobsinnode=20,
                    shrinkage=.03, bag.fraction=.7, train.fraction=1,
                    verbose=FALSE)
    return(list(kind="gbm", fit=fit, features=features, n.trees=220))
  }
  stop("Unknown model: ", name)
}

predict_model <- function(model, newdata) {
  x <- newdata[model$features]
  for (nm in model$features) {
    x[[nm]][!is.finite(x[[nm]])] <- NA_real_
    x[[nm]][is.na(x[[nm]])] <- if (!is.null(model$medians[[nm]])) model$medians[[nm]] else 0
  }
  if (model$kind == "glm") {
    for (nm in model$features) x[[nm]] <- (x[[nm]] - model$centers[[nm]]) / model$spreads[[nm]]
    return(as.numeric(stats::predict(model$fit, newdata=x, type="response")))
  }
  if (model$kind == "rpart") {
    p <- stats::predict(model$fit, newdata=x, type="prob")
    return(if ("1" %in% colnames(p)) as.numeric(p[,"1"]) else rep(0,nrow(x)))
  }
  if (model$kind == "ranger") {
    p <- predict(model$fit, data=x)$predictions
    return(if ("1" %in% colnames(p)) as.numeric(p[,"1"]) else rep(0,nrow(x)))
  }
  if (model$kind == "gbm") return(as.numeric(stats::predict(model$fit, newdata=x, n.trees=model$n.trees, type="response")))
  stop("Unsupported model type.")
}

average_precision <- function(y, p) {
  idx <- order(p, decreasing=TRUE); positives <- y[idx] == 1
  if (!any(positives)) return(0)
  precision <- cumsum(positives) / seq_along(positives)
  mean(precision[positives])
}

choose_threshold <- function(y, p, target_precision=.80) {
  candidates <- sort(unique(c(seq(.01,.99,by=.01), p)), decreasing=TRUE)
  scores <- lapply(candidates, function(t) {
    pred <- p >= t; tp <- sum(pred & y==1); fp <- sum(pred & y==0); fn <- sum(!pred & y==1)
    precision <- if(tp+fp) tp/(tp+fp) else 1
    recall <- if(tp+fn) tp/(tp+fn) else 0
    c(threshold=t, precision=precision, recall=recall, f1=if(precision+recall)2*precision*recall/(precision+recall) else 0)
  })
  m <- do.call(rbind,scores); eligible <- which(m[,"precision"] >= target_precision & m[,"recall"] > 0)
  if(length(eligible)) return(unname(m[eligible[which.max(m[eligible,"recall"])],"threshold"]))
  unname(m[which.max(m[,"f1"]),"threshold"])
}

evaluate_probabilities <- function(y, p, threshold) {
  pred <- p >= threshold
  tp <- sum(pred & y==1); fp <- sum(pred & y==0); fn <- sum(!pred & y==1); tn <- sum(!pred & y==0)
  precision <- if(tp+fp) tp/(tp+fp) else 0; recall <- if(tp+fn) tp/(tp+fn) else 0
  ranks <- rank(p, ties.method="average"); n1 <- sum(y==1); n0 <- sum(y==0)
  auc <- if(n1 && n0) (sum(ranks[y==1])-n1*(n1+1)/2)/(n1*n0) else NA_real_
  list(precision=precision, recall=recall, f1=if(precision+recall)2*precision*recall/(precision+recall) else 0,
       average_precision=average_precision(y,p), roc_auc=auc,
       confusion=data.frame(Actual=factor(c("Legitimate","Legitimate","Fraud","Fraud"),levels=c("Legitimate","Fraud")),
                            Predicted=factor(c("Legitimate","Fraud","Legitimate","Fraud"),levels=c("Legitimate","Fraud")),Count=c(tn,fp,fn,tp)))
}

train_model_suite <- function(data) {
  data <- validate_fraud_data(data)
  split <- split_timewise(data)
  prep <- prepare_features(split$train, split)
  models <- list(); records <- list(); validation_curves <- list(); test_confusions <- list()
  names <- c("Logistic regression", "Decision tree", "Class-weighted random forest", "Gradient boosting")
  for (name in names) {
    fit_data <- if(name == "Logistic regression") prep$scaled$train else prep$parts$train
    model <- fit_one_model(name, fit_data, prep$features)
    model$medians <- prep$medians[prep$features]
    if(model$kind == "glm") {
      model$centers <- prep$centers[prep$features]
      model$spreads <- prep$spreads[prep$features]
    }
    # Keep score inputs in raw feature units for the app's individual scoring screen.
    if(model$kind != "glm") model$features <- prep$features
    pv <- predict_model(model, prep$parts$validation)
    threshold <- choose_threshold(prep$parts$validation$Class, pv)
    pt <- predict_model(model, prep$parts$test)
    valid <- evaluate_probabilities(prep$parts$validation$Class,pv,threshold)
    test <- evaluate_probabilities(prep$parts$test$Class,pt,threshold)
    records[[name]] <- data.frame(Model=name, Validation_AP=valid$average_precision,
       Test_AP=test$average_precision, Test_ROC_AUC=test$roc_auc, Threshold=threshold,
       Test_Precision=test$precision, Test_Recall=test$recall, Test_F1=test$f1,
       True_Negative=test$confusion$Count[1], False_Positive=test$confusion$Count[2],
       False_Negative=test$confusion$Count[3], True_Positive=test$confusion$Count[4],check.names=FALSE)
    models[[name]] <- model
    validation_curves[[name]] <- data.frame(precision=valid$precision, recall=valid$recall,
                                             average_precision=valid$average_precision,
                                             y=prep$parts$validation$Class, p=pv)
    test_confusions[[name]] <- test$confusion
  }
  metrics <- do.call(rbind,records); metrics <- metrics[order(-metrics$Validation_AP),,drop=FALSE]
  best_name <- metrics$Model[[1]]
  curve_data <- validation_curves[[best_name]]
  ord <- order(curve_data$p,decreasing=TRUE); ys<-curve_data$y[ord]
  curve_data$recall <- cumsum(ys==1)/max(1,sum(ys==1)); curve_data$precision <- cumsum(ys==1)/seq_along(ys)
  list(models=models, metrics=metrics, best_name=best_name,
       threshold=metrics$Threshold[match(best_name,metrics$Model)], pr=curve_data,
       confusion=test_confusions[[best_name]])
}

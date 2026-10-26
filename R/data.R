# Data handling and safe synthetic examples for the Sentinel Shiny application.

make_demo_data <- function(n = 2400L, seed = 2026L) {
  set.seed(seed)
  time <- sort(runif(n, 0, 30 * 24 * 60 * 60))
  amount <- pmax(0.5, rlnorm(n, meanlog = 3.5, sdlog = 1.2))
  fraud <- rbinom(n, 1, 0.012)
  # Signal is deliberately imperfect and artificial; no real people/cards are represented.
  v1 <- rnorm(n, mean = ifelse(fraud == 1, 1.15, 0), sd = 1.15)
  v2 <- rnorm(n, mean = ifelse(fraud == 1, -0.85, 0), sd = 1.25)
  data.frame(Time = time, Amount = amount, V1 = v1, V2 = v2,
             V3 = rnorm(n), V4 = rnorm(n), Class = fraud)
}

validate_fraud_data <- function(data) {
  if (!is.data.frame(data) || nrow(data) < 30L) stop("The CSV needs at least 30 transaction rows.")
  label <- intersect(names(data), c("Class", "class", "IsFraud", "is_fraud"))
  if (!length(label)) stop("Could not find a binary label. Expected a `Class` column (1 = fraud).")
  names(data)[match(label[[1]], names(data))] <- "Class"
  y <- data$Class
  if (is.factor(y)) y <- as.character(y)
  if (is.logical(y)) y <- as.integer(y)
  suppressWarnings(y_num <- as.numeric(y))
  if (anyNA(y_num) || !all(y_num %in% c(0, 1))) stop("Class must contain only 0 (legitimate) and 1 (fraud).")
  data$Class <- as.integer(y_num)
  # Keep only numeric model inputs; silently dropping text IDs prevents accidental leakage.
  inputs <- setdiff(names(data), "Class")
  numeric_inputs <- inputs[vapply(data[inputs], is.numeric, logical(1))]
  if (!length(numeric_inputs)) stop("At least one numeric transaction feature is required.")
  data <- data[c(numeric_inputs, "Class")]
  if (!all(c("Time", "Amount") %in% names(data)))
    stop("This dashboard expects numeric `Time` and `Amount` columns, as in creditcard.csv.")
  if (any(vapply(data[numeric_inputs], function(x) all(is.na(x)), logical(1))))
    stop("At least one feature is entirely missing; remove it or provide numeric values.")
  if (length(unique(data$Class)) != 2L) stop("The data must include both legitimate and fraud examples.")
  if ("Time" %in% names(data)) data <- data[order(data$Time, na.last = TRUE), , drop = FALSE]
  rownames(data) <- NULL
  data
}

make_review_queue <- function(data, n = 8L, model = NULL) {
  d <- tail(data, min(nrow(data), 5000L))
  # In demo mode use artificial, label-independent signals. After training use model scores.
  if (!is.null(model)) {
    score <- 100 * predict_model(model, d)
  } else {
    amount_rank <- rank(log1p(pmax(d$Amount, 0)), ties.method = "average") / nrow(d)
    signal <- if ("V1" %in% names(d)) pmin(1, abs(d$V1) / 4) else rep(0.35, nrow(d))
    score <- pmin(99, pmax(1, round(22 + 45 * amount_rank + 28 * signal)))
  }
  order_idx <- order(score, decreasing = TRUE)
  out <- data.frame(
    Transaction = sprintf("TXN-%06d", rev(seq_len(nrow(d)))[order_idx]),
    Amount = scales::dollar(d$Amount[order_idx], accuracy = .01),
    Risk = ifelse(score[order_idx] >= 85, "Critical", ifelse(score[order_idx] >= 65, "High", "Review")),
    `Risk score` = round(score[order_idx]),
    `Time (seconds)` = round(d$Time[order_idx]),
    check.names = FALSE
  )
  head(out, n)
}

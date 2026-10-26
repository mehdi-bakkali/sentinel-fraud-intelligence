# Sentinel | Fraud Intelligence — Shiny portfolio application
# Run with: shiny::runApp()

required_packages <- c("shiny", "bslib", "ggplot2", "DT", "rpart", "ranger", "gbm", "scales")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install project packages first: install.packages(c(",
       paste(sprintf("\"%s\"", missing_packages), collapse = ", "), ")).", call. = FALSE)
}

library(shiny)
library(ggplot2)

source("R/data.R", local = TRUE)
source("R/models.R", local = TRUE)

theme_sentinel <- function() {
  theme_minimal(base_size = 11) + theme(
    plot.background = element_rect(fill = "#101827", color = NA),
    panel.background = element_rect(fill = "#101827", color = NA),
    panel.grid.major = element_line(color = "#26344a", linewidth = .35),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#aab8cc"),
    axis.title = element_text(color = "#8392a8"),
    plot.title = element_text(color = "#edf3fc", face = "bold"),
    plot.subtitle = element_text(color = "#93a2b7"),
    legend.text = element_text(color = "#c5d0df"),
    legend.title = element_text(color = "#aab8cc"),
    strip.text = element_text(color = "#dfe9f6", face = "bold")
  )
}

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "darkly", bg = "#080d18",
                          fg = "#ecf2fc", primary = "#5c9cff", secondary = "#182438"),
  tags$head(tags$style(HTML(" 
    body{background:#080d18!important}.container-fluid{max-width:1600px;padding:20px 28px}
    .hero{display:flex;justify-content:space-between;align-items:center;padding:12px 0 20px;border-bottom:1px solid #202c3f;margin-bottom:20px}
    .brand{font:800 13px Manrope,Arial,sans-serif;letter-spacing:1.4px;color:#edf3fc}.brand small{display:block;color:#8190a7;font:9px monospace;letter-spacing:1px;margin-top:4px}
    h1{font-size:26px!important;font-weight:700!important;margin:8px 0 3px!important}.subhead{color:#91a0b5;font-size:12px}
    .eyebrow{color:#61d2a4;font:10px monospace;letter-spacing:1px}.panel-card,.metric-card{background:#101827;border:1px solid #202d40;border-radius:9px;padding:17px;margin-bottom:14px}
    .metric-card{min-height:105px}.metric-label{font:9px monospace;letter-spacing:.7px;color:#95a4b9}.metric-value{font:700 25px Manrope,Arial;color:#f0f5fc;margin-top:8px}.metric-note{color:#8190a6;font-size:10px;margin-top:3px}
    .section-title{font-size:14px;font-weight:700;color:#e9f0fa;margin:0 0 4px}.section-note{font-size:10px;color:#8493a8;margin-bottom:14px}
    .sidebar{background:#0b111e!important;border-color:#1d293b!important}.control-label{font-size:11px;color:#cbd7e7}.form-control,.form-select{background:#111b2a!important;color:#e6edf7!important;border-color:#2a394f!important}
    .nav-pills .nav-link{color:#9eacc0}.nav-pills .nav-link.active{background:#20334e;color:#eaf2ff}.btn-primary{font-size:11px}.badge-risk{padding:4px 8px;border-radius:20px;font-size:9px}
    .alert{font-size:11px}.dataTables_wrapper{font-size:11px;color:#cbd7e7}.table{--bs-table-bg:#101827;--bs-table-color:#cbd7e7;--bs-table-border-color:#263348}
    .form-text{color:#8291a7}.progress{background:#202d40}.small-muted{font-size:10px;color:#8796aa}.callout{border-left:3px solid #f0b851;background:#1e2939;padding:12px 14px;border-radius:4px;color:#bdcadd;font-size:11px;line-height:1.6}
    .footer{border-top:1px solid #1d293b;padding:16px 0;color:#718198;font:9px monospace;display:flex;justify-content:space-between}
    @media(max-width:767px){.container-fluid{padding:12px}.hero{align-items:flex-start;gap:12px;flex-direction:column}.sidebar{padding:12px!important}}
  "))),
  div(class = "hero",
      div(div(class = "brand", "◈ SENTINEL", tags$small("FRAUD INTELLIGENCE LAB")),
          h1("Fraud intelligence"), div(class = "subhead", "An interactive, review-first credit card fraud detection project.")),
      div(class = "eyebrow", span(class = "pulse", "●"), "  PORTFOLIO DEMO · HUMAN REVIEW")),
  sidebarLayout(
    sidebarPanel(width = 2,
      selectInput("page", "WORKSPACE", c("Overview", "Explore data", "Model lab", "Score a transaction", "Methodology")),
      tags$hr(), h5("DATA SOURCE"),
      fileInput("file", "Upload creditcard.csv", accept = c(".csv")),
      div(class = "small-muted", "Expected label: Class (1 = fraud). Use anonymized data only."),
      tags$hr(), uiOutput("data_status"),
      actionButton("reset_demo", "↻  Use synthetic demo", class = "btn btn-outline-secondary btn-sm"),
      tags$hr(), div(class = "small-muted", "Demo scores are simulated until you train models. Never upload payment card data.")),
    mainPanel(width = 10, uiOutput("page_content"))
  ),
  div(class = "footer", span("SENTINEL RISK LABS · STUDENT PROJECT"), span("SIMULATED DEMO · NOT FOR PAYMENT DECISIONS"))
)

server <- function(input, output, session) {
  data_state <- reactiveVal(make_demo_data())
  data_label <- reactiveVal("Synthetic demo data")
  model_state <- reactiveVal(NULL)

  observeEvent(input$reset_demo, {
    data_state(make_demo_data()); data_label("Synthetic demo data"); model_state(NULL)
    showNotification("Synthetic demo data loaded. No real cardholder information is used.", type = "message")
  })
  observeEvent(input$file, {
    req(input$file$datapath)
    tryCatch({
      incoming <- read.csv(input$file$datapath, check.names = FALSE)
      clean <- validate_fraud_data(incoming)
      data_state(clean); data_label(input$file$name); model_state(NULL)
      showNotification(sprintf("Loaded %s rows. Model results cleared for the new dataset.", scales::comma(nrow(clean))), type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = 8))
  })
  output$data_status <- renderUI({
    d <- data_state()
    tagList(div(class = "small-muted", strong(data_label())),
            div(class = "small-muted", paste(scales::comma(nrow(d)), "rows ·", ncol(d) - 1, "features")),
            div(class = "small-muted", paste(scales::comma(sum(d$Class == 1)), "fraud labels")))
  })

  output$page_content <- renderUI({
    switch(input$page,
      "Overview" = tagList(
        fluidRow(column(3, uiOutput("metric_rows")), column(3, uiOutput("metric_fraud")), column(3, uiOutput("metric_rate")), column(3, uiOutput("metric_status"))),
        fluidRow(column(8, div(class="panel-card", h4(class="section-title", "Transaction activity"), p(class="section-note", "Synthetic activity profile or transaction counts from the uploaded data."), plotOutput("activity_plot", height="260px"))),
                 column(4, div(class="panel-card", h4(class="section-title", "Class balance"), p(class="section-note", "Rare-event imbalance makes accuracy a poor headline metric."), plotOutput("balance_plot", height="260px")))),
        div(class="panel-card", h4(class="section-title", "Priority review queue"), p(class="section-note", "Scores use artificial demo signals until training, then the selected model. Ground-truth labels stay hidden from the review queue."), DT::DTOutput("queue_table"))),
      "Explore data" = tagList(
        fluidRow(column(6, div(class="panel-card", h4(class="section-title", "Transaction amount"), p(class="section-note", "Distribution shown on a log scale to make the long tail readable."), plotOutput("amount_plot", height="300px"))),
                 column(6, div(class="panel-card", h4(class="section-title", "Amount by class"), p(class="section-note", "A difference in distributions is a clue, not proof of predictive value."), plotOutput("class_amount_plot", height="300px")))),
        div(class="panel-card", h4(class="section-title", "Dataset profile"), DT::DTOutput("profile_table"))),
      "Model lab" = tagList(
        div(class="panel-card", h4(class="section-title", "Train and compare four models"), p(class="section-note", "Chronological 70 / 15 / 15 train, validation, and final test periods. Training may take a few minutes on the full dataset."),
            actionButton("train_models", "Train model suite", class="btn-primary"), span("  "), downloadButton("download_metrics", "Download test metrics"), span("  "), downloadButton("download_model", "Download best model"),
            div(class="small-muted", "Thresholds are chosen on validation only. Test metrics are used for the final report, never for tuning."), br(), uiOutput("training_state")),
        fluidRow(column(8, div(class="panel-card", h4(class="section-title", "Model comparison"), p(class="section-note", "Models ranked using validation average precision; final test results are shown for every model."), DT::DTOutput("model_table"))),
                 column(4, div(class="panel-card", h4(class="section-title", "Precision / recall"), p(class="section-note", "Best validation model."), plotOutput("pr_plot", height="285px")))),
        fluidRow(column(6, div(class="panel-card", h4(class="section-title", "Confusion matrix · test set"), plotOutput("confusion_plot", height="240px"))),
                 column(6, div(class="panel-card", h4(class="section-title", "How to read the results"), div(class="callout", "Average precision summarizes the precision-recall curve and is more informative than accuracy for rare fraud. Precision asks how many alerts were truly fraud; recall asks how much fraud was caught. The threshold is a human-review operating choice, not an automatic card decline."))))),
      "Score a transaction" = tagList(
        fluidRow(column(5, div(class="panel-card", h4(class="section-title", "Score an example"), p(class="section-note", "Enter values in the feature schema from the uploaded dataset."), uiOutput("score_form"), actionButton("score_one", "Score transaction", class="btn-primary"))),
                 column(7, div(class="panel-card", h4(class="section-title", "Risk assessment"), uiOutput("score_result"), div(class="callout", "A model score is a prioritization signal, not proof of fraud. Investigate the context and keep a person in the decision loop.")))),
        div(class="panel-card", h4(class="section-title", "Data and model note"), p(class="section-note", "The public Kaggle dataset uses anonymized PCA features V1–V28 plus Time and Amount. Those features cannot explain a real merchant, location, or customer. This project does not invent those explanations."))),
      "Methodology" = tagList(
        fluidRow(column(6, div(class="panel-card", h4(class="section-title", "Project goal"), p("Prioritize suspicious transactions for analyst review while keeping legitimate customers in mind."), p("This student project extends the DataFlair R tutorial with a review workflow, imbalance-aware metrics, reproducible model comparison, and temporal holdout evaluation."))),
                 column(6, div(class="panel-card", h4(class="section-title", "Evaluation safeguards"), tags$ul(tags$li("Sort by transaction time when available; otherwise preserve row order."), tags$li("Use chronological 70% train, 15% validation, and 15% final test partitions."), tags$li("Choose decision thresholds on validation data only."), tags$li("Report average precision, precision, recall, F1, ROC AUC, and confusion counts."), tags$li("Show the test set once after model selection."))))),
        div(class="panel-card", h4(class="section-title", "Models in the lab"), tags$ul(tags$li("Class-weighted logistic regression: interpretable baseline."), tags$li("Cost-sensitive decision tree: transparent rule-based comparator."), tags$li("Class-weighted random forest: ensemble that can capture nonlinear patterns."), tags$li("Gradient boosting: sequential tree ensemble."))),
        div(class="callout", strong("Limits and responsible use. "), "The public dataset is historical and anonymized; its features and fraud rates do not represent a current bank. Synthetic examples are generated for interface demonstration. Scores and explanations are not production-grade. Do not upload personal or payment-card data, and do not use this app to approve, decline, or block real transactions."))
    )
  })

  metric_card <- function(label, value, note) div(class="metric-card", div(class="metric-label", label), div(class="metric-value", value), div(class="metric-note", note))
  output$metric_rows <- renderUI(metric_card("TRANSACTIONS", scales::comma(nrow(data_state())), data_label()))
  output$metric_fraud <- renderUI(metric_card("FRAUD LABELS", scales::comma(sum(data_state()$Class == 1)), "Known positive examples"))
  output$metric_rate <- renderUI(metric_card("FRAUD RATE", scales::percent(mean(data_state()$Class), accuracy=.01), "Rare event classification"))
  output$metric_status <- renderUI(metric_card("MODEL STATUS", if (is.null(model_state())) "Demo mode" else "Trained", if (is.null(model_state())) "Train the model suite in Model lab" else paste("Selected:", model_state()$best_name)))

  output$activity_plot <- renderPlot({
    d <- data_state(); d$bucket <- cut(d$Time, breaks = 12, include.lowest = TRUE)
    agg <- aggregate(cbind(transactions = rep(1, nrow(d)), fraud = d$Class), list(period = d$bucket), sum)
    long <- rbind(data.frame(period=agg$period, series="Transactions", count=agg$transactions), data.frame(period=agg$period, series="Fraud labels", count=agg$fraud))
    ggplot(long, aes(period, count, color=series, group=series)) + geom_line(linewidth=1) + geom_point(size=2) + scale_color_manual(values=c("Transactions"="#5c9cff", "Fraud labels"="#ff6978")) + labs(x=NULL,y="Rows",color=NULL) + theme_sentinel() + theme(axis.text.x=element_text(angle=35,hjust=1))
  })
  output$balance_plot <- renderPlot({d <- data_state(); d$label <- factor(d$Class, levels=c(0,1), labels=c("Legitimate", "Fraud")); ggplot(d, aes(label, fill=label)) + geom_bar() + geom_text(stat="count", aes(label=scales::comma(after_stat(count))), vjust=-.35, color="#eaf1fa", size=4) + scale_fill_manual(values=c("Legitimate"="#5c9cff", "Fraud"="#ff6978")) + labs(x=NULL,y="Transactions") + theme_sentinel() + theme(legend.position="none")})
  output$amount_plot <- renderPlot({d <- data_state(); ggplot(d, aes(Amount)) + geom_histogram(bins=45, fill="#5c9cff", color="#101827") + scale_x_continuous(trans="log1p", labels=scales::label_number()) + labs(x="Amount · log1p scale",y="Transactions") + theme_sentinel()})
  output$class_amount_plot <- renderPlot({d <- data_state(); d$label <- factor(d$Class, levels=c(0,1), labels=c("Legitimate","Fraud")); ggplot(d, aes(label, Amount, fill=label)) + geom_boxplot(outlier.alpha=.12) + scale_y_continuous(trans="log1p", labels=scales::label_dollar()) + scale_fill_manual(values=c("Legitimate"="#5c9cff","Fraud"="#ff6978")) + labs(x=NULL,y="Amount · log1p scale") + theme_sentinel() + theme(legend.position="none")})
  output$profile_table <- DT::renderDT({d<-data_state(); profile<-data.frame(Feature=names(d), Type=vapply(d,function(x) class(x)[1],""), Missing=vapply(d,function(x)sum(is.na(x)),0L), Unique=vapply(d,function(x)length(unique(x)),0L)); DT::datatable(profile, rownames=FALSE, options=list(dom="t",pageLength=30,scrollX=TRUE))})
  output$queue_table <- DT::renderDT({d<-data_state(); fit<-model_state(); selected<-if(is.null(fit)) NULL else fit$models[[fit$best_name]]; q<-make_review_queue(d,model=selected); DT::datatable(q, rownames=FALSE, options=list(pageLength=6,dom="tip",scrollX=TRUE), class="compact hover")})

  observeEvent(input$train_models, {
    d <- data_state()
    showNotification("Training model suite. Keep this tab open while the models fit.", type="message", duration=5)
    tryCatch({fit <- train_model_suite(d); model_state(fit); showNotification("Training complete. Review validation ranking and final test metrics.", type="message", duration=6)},
             error=function(e) showNotification(paste("Training could not complete:", conditionMessage(e)), type="error", duration=10))
  })
  output$training_state <- renderUI({if(is.null(model_state())) div(class="small-muted", "No model has been trained in this session.") else div(class="alert alert-success", paste("Training complete · best validation model:", model_state()$best_name, "· threshold:", round(model_state()$threshold,3)))})
  output$model_table <- DT::renderDT({req(model_state()); DT::datatable(model_state()$metrics, rownames=FALSE, options=list(dom="t",pageLength=10,scrollX=TRUE), class="compact hover")})
  output$pr_plot <- renderPlot({req(model_state()); p<-model_state()$pr; ggplot(p,aes(recall,precision)) + geom_step(color="#5c9cff",linewidth=1) + geom_hline(yintercept=.8,linetype="dashed",color="#f0b851") + coord_cartesian(xlim=c(0,1),ylim=c(0,1)) + labs(x="Recall",y="Precision",subtitle="Validation set · dashed line marks 80% precision target") + theme_sentinel()})
  output$confusion_plot <- renderPlot({req(model_state()); cm<-model_state()$confusion; ggplot(cm,aes(Predicted,Actual,fill=Count)) + geom_tile(color="#101827") + geom_text(aes(label=scales::comma(Count)),color="white",size=5) + scale_fill_gradient(low="#1b2a41",high="#4d91e8") + labs(x="Predicted",y="Actual",fill="Rows") + theme_sentinel()})
  output$download_metrics <- downloadHandler(filename=function() "sentinel-model-metrics.csv", content=function(file){req(model_state()); write.csv(model_state()$metrics,file,row.names=FALSE)})
  output$download_model <- downloadHandler(filename=function() "sentinel-best-model.rds", content=function(file){req(model_state()); saveRDS(model_state()$models[[model_state()$best_name]],file)})

  output$score_form <- renderUI({d<-data_state(); features<-setdiff(names(d),"Class"); features<-head(features,12); tagList(lapply(features,function(n) numericInput(paste0("feature_",make.names(n)), n, value=if(n=="Amount") 120 else if(n=="Time") max(d$Time,na.rm=TRUE) else 0, min=if(n=="Amount") 0 else NULL, step=.1)))})
  score_result <- eventReactive(input$score_one, {
    req(model_state()); d<-data_state(); vals<-lapply(head(setdiff(names(d),"Class"),12),function(n) input[[paste0("feature_",make.names(n))]]); names(vals)<-head(setdiff(names(d),"Class"),12)
    if(any(vapply(vals,is.null,logical(1)))) return(list(error="Fill in each shown feature."))
    row<-as.data.frame(lapply(d[setdiff(names(d),"Class")],function(x) median(x,na.rm=TRUE))); for(n in names(vals)) row[[n]]<-vals[[n]]
    tryCatch(list(score=predict_model(model_state()$models[[model_state()$best_name]],row),threshold=model_state()$threshold),error=function(e)list(error=conditionMessage(e)))
  })
  output$score_result <- renderUI({x<-score_result(); if(is.null(x)) return(div(class="small-muted","Train a model first, then score a hypothetical example.")); if(!is.null(x$error))return(div(class="alert alert-warning",x$error)); pct<-round(100*x$score); tagList(div(class="metric-value",paste0(pct," / 100")),p(if(x$score>=x$threshold) "Review recommended: this example exceeds the selected threshold." else "Lower risk signal: this example is below the selected threshold."),div(class="small-muted",paste("Validation threshold:",round(x$threshold,3),"· Score is not a calibrated real-world probability.")))})
}

shinyApp(ui, server)

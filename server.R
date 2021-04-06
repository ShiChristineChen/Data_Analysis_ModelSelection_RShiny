shinyServer(function(input, output, session) {
  
  # initialisation ----
  models <- reactiveValues()  # this is a collection of the models
  
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  # load the previously trained models - Note: you can delete files in the SavedModels directory
  for (rdsfile in list.files(path = "SavedModels", pattern = "\\.rds")) {
    name <- gsub(rdsfile, pattern = "\\.rds$", replacement = "")
    rdsfile <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, rdsfile)
    showNotification(paste("Loading trained model", name, "from file", rdsfile), session = session, duration = 3)
    m <- readRDS(file = rdsfile)  
    models[[name]] <- m
    
    # try to update the preprocessing steps with the ones that were used
    inpId <- paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)), "Preprocess")
    steps <- m$recipe$steps
    seld <- c()
    for (step in steps) {
      s <- gsub(pattern = "step_", replacement = "", x = class(step)[1])
      seld <- c(seld, s)
    }
    if (length(seld) > 0 && seld[1] == "date") { 
      seld <- seld[2:length(seld)] #discard initial date step 
      seld <- seld[seld != "rm"] #discard rm step
    }
    updateSelectizeInput(session = session, inputId = inpId, choices = ppchoices, selected = seld)
    if (length(seld) > 0) {
      showNotification(paste("Setting preprocessing for", name, "to", paste(seld, collapse = ",")), session = session, duration = 3)
    }
  }
  
  # reactive getData ----
  getData <- reactive({
    d <- read.csv(file = "Ass3Data.csv", row.names = "ID", stringsAsFactors = TRUE)
    d$TreatmentDate <- as.Date(d$TreatmentDate)
    d
  })
  
  # output BoxPlots ----
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  # output Missing ----
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  # output Corr ----
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  # output MixedPairs ----  
  output$MixedPairs <- renderPlot({
    d <- getData()
    GGally::ggpairs(data = d,  
                    mapping = ggplot2::aes(colour = "rainbowl"), 
                    title = "Pairs of variables", progress = FALSE)
  })
  
  # output DataSummary ----
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  # output Table ----
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  # reactive get Split
  getSplit <- reactive({
    set.seed(199)
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  # reactive getMethods ----
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  # output Available ----
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15,136)), rownames = FALSE)
  })
  
  # reactive getTrainData ----
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  # reactive getTestData ----
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  # reactive getTrControl ----
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Y"]
    n <- 25
    set.seed(673)
    seeds <- vector(mode="list", length = n+1)
    for (i in 1:n) {
      seeds[[i]] <- as.integer(c(runif(n = 15, min = 1000, max = 5000)))
    }
    seeds[[n+1]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "random",
                 index = caret::createResample(y = y, times = n), savePredictions = "final", seeds = seeds, trim = TRUE)
  })
  
  # output SplitSummary ----
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  

  
  # METHOD * null ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getNullRecipe ----
  getNullRecipe <- reactive({
    recipe <- recipes::recipe(Y ~ ., data = getTrainData())
  })
  
  # observeEvent NullGo ----
  observeEvent(
    input$NullGo,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 0)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # observeEvent NullGo ----
  output$NullMetrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  # output NullRecipe
  output$NullRecipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  
  
  # METHOD * glmnet ---------------------------------------------------------------------------------------------------------------------------

  # reactive getGlmnetRecipe ----
  getGlmnetRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%  # add a numeric date variables
      steps(input$GlmnetPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent GlmnetGo ----
  observeEvent(
    input$GlmnetGo,
    {
      library(glmnet)
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output GlmnetModelSummary (text) ----
  output$GlmnetModelSummary0 <- renderText({
    description("glmnet")
  })
  
  # output GlmnetMetrics (table) ----
  output$GlmnetMetrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  # output GlmnetModelPlots (plot) ----
  output$GlmnetModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })
  
  # output GlmnetRecipe (print) ----
  output$GlmnetRecipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  # output GlmnetModelSummary2 (print) ----
  output$GlmnetModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })


  
  
  # METHOD * pls ---------------------------------------------------------------------------------------------------------------------------

  # reactive getPlsRecipe ----
  getPlsRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$PlsPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent PlsGo ----
  observeEvent(
    input$PlsGo,
    {
      library(pls)
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output PlsModelSummary0 (text) ----
  output$PlsModelSummary0 <- renderText({
    description("pls")
  })

  # output PlsMetrics (table) ----
  output$PlsMetrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  # output PlsModelPlots (plot) ----
  output$PlsModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  # output PlsRecipe (print) ----
  output$PlsRecipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  # output PlsModelSummary2 (print) ----
  output$PlsModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls)
  })
  

  
  # METHOD * rpart ---------------------------------------------------------------------------------------------------------------------------

  # reactive getRpartRecipe ----
  getRpartRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$RpartPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent RpartGo ----
  observeEvent(
    input$RpartGo,
    {
      library(rpart)
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  # output RpartModelSummary0 (print) ----
  output$RpartModelSummary0 <- renderText({
    description("rpart")
  })
  
  # output RpartMetrics (table) ----
  output$RpartMetrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  # output RpartRecipe (print) ----
  output$RpartRecipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  # output RpartModelPlots (plot) ----
  output$RpartModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  # output RpartModelTree (plot) ----
  output$RpartModelTree <- renderPlot({
    library(rpart.plot)
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel, roundint = FALSE)
  })     
  

  
  # maintenance point ---------------------------------------------------------------------------------------------------------------------------
  # add further methods here  
  # METHOD * rqlasso ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getRqlassoRecipe ----
  getrqlassoRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$rqlassoPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent rqlassoGo ----
  observeEvent(
    input$rqlassoGo,
    {
      library(rqPen)
      method <- "rqlasso"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getrqlassoRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output rqlassoModelSummary0 (print) ----
  output$rqlassoModelSummary0 <- renderText({
    description("rqlasso")
  })
  
  # output rqlassoMetrics (table) ----
  output$rqlassoMetrics <- renderTable({
    req(models$rqlasso)
    models$rqlasso$results[ which.min(models$rqlasso$results[, "RMSE"]), ]
  })
  
  # output rqlassoRecipe (print) ----
  output$rqlassoRecipe <- renderPrint({
    req(models$rqlasso)
    models$rqlasso$recipe
  })  
  
  # output rqlassoModelPlots (plot) ----
  output$rqlassoModelPlots <- renderPlot({
    req(models$rqlasso)
    plot(models$rqlasso)
  })
 
  # output rqlassoModelSummary2 (print) ----
  output$rqlassoModelSummary2 <- renderPrint({
    req(models$rqlasso)
    summary(models$rqlasso)
  })
  
  
  
  # METHOD * glmboost ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getglmboostRecipe ----
  getglmboostRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$glmboostPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent glmboostGo ----
  observeEvent(
    input$glmboostGo,
    {
      library(plyr)
      library(mboost)
      method <- "glmboost"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getglmboostRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output glmboostModelSummary0 (print) ----
  output$glmboostModelSummary0 <- renderText({
    description("glmboost")
  })
  
  # output glmboostMetrics (table) ----
  output$glmboostMetrics <- renderTable({
    req(models$glmboost)
    models$glmboost$results[ which.min(models$glmboost$results[, "RMSE"]), ]
  })
  
  # output glmboostRecipe (print) ----
  output$glmboostRecipe <- renderPrint({
    req(models$glmboost)
    models$glmboost$recipe
  })  
  
  # output glmboostModelPlots (plot) ----
  output$glmboostModelPlots <- renderPlot({
    req(models$glmboost)
    plot(models$glmboost)
  })

  # output glmboostSummary2 (print) ----
  output$glmboostModelSummary2 <- renderPrint({
    req(models$glmboost)
    summary(models$glmboost)
  })
  
  

  
  
   
  # METHOD * svmLinear ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive svmLinearRecipe ----
  getsvmLinearRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$svmLinearPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent svmLinearGo ----
  observeEvent(
    input$svmLinearGo,
    {
      library(kernlab)
      method <- "svmLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getsvmLinearRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output svmLinearModelSummary0 (print) ----
  output$svmLinearModelSummary0 <- renderText({
    description("svmLinear")
  })
  
  # output svmLinearMetrics (table) ----
  output$svmLinearMetrics <- renderTable({
    req(models$svmLinear)
    models$svmLinear$results[ which.min(models$svmLinear$results[, "RMSE"]), ]
  })
  
  # output svmLinearRecipe (print) ----
  output$svmLinearRecipe <- renderPrint({
    req(models$svmLinear)
    models$svmLinear$recipe
  })  
  
  # output svmLinearModelPlots (plot) ----
  output$svmLinearModelPlots <- renderPlot({
    req(models$svmLinear)
    plot(models$svmLinear)
  })  
  
  # output svmLinearSummary2 (print) ----
  output$svmLinearModelSummary2 <- renderPrint({
    req(models$svmLinear)
    summary(models$svmLinear)
  })
  
  
  # METHOD * xgbTree ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getxgbTreeRecipe ----
  getxgbTreeRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$xgbTreePreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent xgbTreeGo ----
  observeEvent(
    input$xgbTreeGo,
    {
      library(xgboost)
      method <- "xgbTree"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getxgbTreeRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output xgbTreeModelSummary0 (print) ----
  output$xgbTreeModelSummary0 <- renderText({
    description("xgbTree")
  })
  
  # output xgbTreeMetrics (table) ----
  output$xgbTreeMetrics <- renderTable({
    req(models$xgbTree)
    models$xgbTree$results[ which.min(models$xgbTree$results[, "RMSE"]), ]
  })
  
  # output xgbTreeRecipe (print) ----
  output$xgbTreeRecipe <- renderPrint({
    req(models$xgbTree)
    models$xgbTree$recipe
  })  
  
  # output xgbTreeModelPlots (plot) ----
  output$xgbTreeModelPlots <- renderPlot({
    req(models$xgbTree)
    plot(models$xgbTree)
  })

  # # METHOD * plsRglm ---------------------------------------------------------------------------------------------------------------------------

  # reactive plsRglmRecipe ----
  getplsRglmRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$plsRglmPreprocess) %>%
      step_rm(has_type("date"))
  })
  observeEvent(
    input$plsRglmGo,
    {
      library(plsRglm)
      method <- "plsRglm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getplsRglmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  # output plsRglmModelSummary0 (print) ----
  output$plsRglmModelSummary0 <- renderText({
    description("plsRglm")
  })

  # output plsRglmMetrics (table) ----
  output$plsRglmMetrics <- renderTable({
    req(models$plsRglm)
    models$plsRglm$results[ which.min(models$plsRglm$results[, "RMSE"]), ]
  })

  # output plsRglmRecipe (print) ----
  output$plsRglmRecipe <- renderPrint({
    req(models$plsRglm)
    models$plsRglm$recipe
  })

  # output plsRglmModelPlots (plot) ----
  output$plsRglmModelPlots <- renderPlot({
    req(models$plsRglm)
    plot(models$plsRglm)
  })

  # output plsRglmSummary2 (print) ----
  output$plsRglmModelSummary2 <- renderPrint({
    req(models$plsRglm)
    summary(models$plsRglm)
  })

#
#
  # METHOD * bayesglm   ---------------------------------------------------------------------------------------------------------------------------

  # reactive bayesglmRecipe ----
  getbayesglmRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$bayesglmPreprocess) %>%
      step_rm(has_type("date"))
  })
  observeEvent(
    input$bayesglmGo,
    {
      library(arm)
      method <- "bayesglm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getbayesglmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  # output bayesglmModelSummary0 (print) ----
  output$bayesglmModelSummary0 <- renderText({
    description("bayesglm")
  })

  # output bayesglmMetrics (table) ----
  output$bayesglmMetrics <- renderTable({
    req(models$bayesglm)
    models$bayesglm$results[ which.min(models$plsRglm$results[, "RMSE"]), ]
  })

  # output bayesglmRecipe (print) ----
  output$bayesglmRecipe <- renderPrint({
    req(models$bayesglm)
    models$bayesglm$recipe
  })

  # output bayesglmModelPlots (plot) ----
  output$bayesglmModelPlots <- renderPlot({
    req(models$bayesglm)
    plot(models$bayesglm)
  })

  # output bayesglmSummary2 (print) ----
  output$bayesglmModelSummary2 <- renderPrint({
    req(models$bayesglm)
    summary(models$bayesglm)
  })



# METHOD * msaenet   ---------------------------------------------------------------------------------------------------------------------------

# reactive msaenetRecipe ----
getmsaenetRecipe <- reactive({
  recipes::recipe(Y ~ ., data = getTrainData()) %>%
    step_date(has_type("date"), features = "decimal") %>%
    steps(input$msaenetPreprocess) %>%
    step_rm(has_type("date"))
})
observeEvent(
  input$msaenetGo,
  {
    library(msaenet)
    method <- "msaenet"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      deleteRds(method)
      model <- caret::train(getmsaenetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# output msaenetModelSummary0 (print) ----
output$msaenetModelSummary0 <- renderText({
  description("msaenet")
})

# output msaenetMetrics (table) ----
output$msaenetMetrics <- renderTable({
  req(models$msaenet)
  models$msaenet$results[ which.min(models$msaenet$results[, "RMSE"]), ]
})

# output msaenetRecipe (print) ----
output$msaenetRecipe <- renderPrint({
  req(models$msaenet)
  models$msaenet$recipe
})

# output msaenetModelPlots (plot) ----
output$msaenetModelPlots <- renderPlot({
  req(models$msaenet)
  plot(models$msaenet)
})

# output msaenetSummary2 (print) ----
output$msaenetModelSummary2 <- renderPrint({
  req(models$msaenet)
  summary(models$msaenet)
})


# METHOD * glmnet_h2o   ---------------------------------------------------------------------------------------------------------------------------

# reactive glmnet_h2oRecipe ----
getglmnet_h2oRecipe <- reactive({
  recipes::recipe(Y ~ ., data = getTrainData()) %>%
    step_date(has_type("date"), features = "decimal") %>%
    steps(input$glmnet_h2oPreprocess) %>%
    step_rm(has_type("date"))
})
observeEvent(
  input$glmnet_h2oGo,
  {
    library(h2o)
    method <- "glmnet_h2o"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      deleteRds(method)
      model <- caret::train(getglmnet_h2oRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# output glmnet_h2oModelSummary0 (print) ----
output$glmnet_h2oModelSummary0 <- renderText({
  description("glmnet_h2o")
})

# output glmnet_h2oMetrics (table) ----
output$glmnet_h2oMetrics <- renderTable({
  req(models$glmnet_h2o)
  models$glmnet_h2o$results[ which.min(models$glmnet_h2o$results[, "RMSE"]), ]
})

# output glmnet_h2oRecipe (print) ----
output$glmnet_h2oRecipe <- renderPrint({
  req(models$glmnet_h2o)
  models$glmnet_h2o$recipe
})

# output glmnet_h2oModelPlots (plot) ----
output$glmnet_h2oModelPlots <- renderPlot({
  req(models$glmnet_h2o)
  plot(models$glmnet_h2o)
})

# output glmnet_h2oSummary2 (print) ----
output$glmnet_h2oModelSummary2 <- renderPrint({
  req(models$glmnet_h2o)
  summary(models$glmnet_h2o)
})
  
  
# METHOD * kernelpls   ---------------------------------------------------------------------------------------------------------------------------

# reactive kernelplsRecipe ----
getkernelplsRecipe <- reactive({
  recipes::recipe(Y ~ ., data = getTrainData()) %>%
    step_date(has_type("date"), features = "decimal") %>%
    steps(input$kernelplsPreprocess) %>%
    step_rm(has_type("date"))
})
observeEvent(
  input$kernelplsGo,
  {
    library(pls)
    method <- "kernelpls"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      deleteRds(method)
      model <- caret::train(getkernelplsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# output kernelplsModelSummary0 (print) ----
output$kernelplsModelSummary0 <- renderText({
  description("kernelpls")
})

# output kernelplsMetrics (table) ----
output$kernelplsMetrics <- renderTable({
  req(models$kernelpls)
  models$kernelpls$results[ which.min(models$kernelpls$results[, "RMSE"]), ]
})

# output kernelplsRecipe (print) ----
output$kernelplsRecipe <- renderPrint({
  req(models$kernelpls)
  models$kernelpls$recipe
})

# output kernelplsModelPlots (plot) ----
output$kernelplsModelPlots <- renderPlot({
  req(models$kernelpls)
  plot(models$kernelpls)
})

# output kernelplsSummary2 (print) ----
output$kernelplsModelSummary2 <- renderPrint({
  req(models$kernelpls)
  summary(models$kernelpls)
})  



# METHOD * spls  ---------------------------------------------------------------------------------------------------------------------------

# reactive splsRecipe ----
getsplsRecipe <- reactive({
  recipes::recipe(Y ~ ., data = getTrainData()) %>%
    step_date(has_type("date"), features = "decimal") %>%
    steps(input$splsPreprocess) %>%
    step_rm(has_type("date"))
})
observeEvent(
  input$splsGo,
  {
    library(spls)
    method <- "spls"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      deleteRds(method)
      model <- caret::train(getsplsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# output splsModelSummary0 (print) ----
output$splsModelSummary0 <- renderText({
  description("spls")
})

# outputsplsMetrics (table) ----
output$splsMetrics <- renderTable({
  req(models$spls)
  models$spls$results[ which.min(models$spls$results[, "RMSE"]), ]
})

# output splsRecipe (print) ----
output$splsRecipe <- renderPrint({
  req(models$spls)
  models$spls$recipe
})

# output splsModelPlots (plot) ----
output$splsModelPlots <- renderPlot({
  req(models$spls)
  plot(models$spls)
})

# output splsSummary2 (print) ----
output$splsModelSummary2 <- renderPrint({
  req(models$spls)
  summary(models$spls)
})  



# METHOD * widekernelpls   ---------------------------------------------------------------------------------------------------------------------------

# reactive widekernelplsRecipe ----
getwidekernelplsRecipe <- reactive({
  recipes::recipe(Y ~ ., data = getTrainData()) %>%
    step_date(has_type("date"), features = "decimal") %>%
    steps(input$widekernelplsPreprocess) %>%
    step_rm(has_type("date"))
})
observeEvent(
  input$widekernelplsGo,
  {
    library(pls)
    method <- "widekernelpls"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      deleteRds(method)
      model <- caret::train(getwidekernelplsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# output widekernelplsModelSummary0 (print) ----
output$widekernelplsModelSummary0 <- renderText({
  description("widekernelpls")
})

# output widekernelplsMetrics (table) ----
output$widekernelplsMetrics <- renderTable({
  req(models$widekernelpls)
  models$widekernelpls$results[ which.min(models$widekernelpls$results[, "RMSE"]), ]
})

# output widekernelplsRecipe (print) ----
output$widekernelplsRecipe <- renderPrint({
  req(models$widekernelpls)
  models$widekernelpls$recipe
})

# output widekernelplsModelPlots (plot) ----
output$widekernelplsModelPlots <- renderPlot({
  req(models$widekernelpls)
  plot(models$widekernelpls)
})

# output widekernelplsSummary2 (print) ----
output$widekernelplsModelSummary2 <- renderPrint({
  req(models$widekernelpls)
  summary(models$widekernelpls)
})  


# METHOD * simpls   ---------------------------------------------------------------------------------------------------------------------------

# reactive simplsRecipe ----
getsimplsRecipe <- reactive({
  recipes::recipe(Y ~ ., data = getTrainData()) %>%
    step_date(has_type("date"), features = "decimal") %>%
    steps(input$simplsPreprocess) %>%
    step_rm(has_type("date"))
})
observeEvent(
  input$simplsGo,
  {
    library(pls)
    method <- "simpls"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      deleteRds(method)
      model <- caret::train(getsimplsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# output simplsModelSummary0 (print) ----
output$simplsModelSummary0 <- renderText({
  description("simpls")
})

# output simplsMetrics (table) ----
output$simplsMetrics <- renderTable({
  req(models$simpls)
  models$simpls$results[ which.min(models$simpls$results[, "RMSE"]), ]
})

# output simplsRecipe (print) ----
output$simplsRecipe <- renderPrint({
  req(models$simpls)
  models$simpls$recipe
})

# output simplsModelPlots (plot) ----
output$simplsModelPlots <- renderPlot({
  req(models$simpls)
  plot(models$simpls)
})

# output simplsSummary2 (print) ----
output$simplsModelSummary2 <- renderPrint({
  req(models$simpls)
  summary(models$simpls)
}) 



# METHOD * gamboost   ---------------------------------------------------------------------------------------------------------------------------

# reactive gamboostRecipe ----
getgamboostRecipe <- reactive({
  recipes::recipe(Y ~ ., data = getTrainData()) %>%
    step_date(has_type("date"), features = "decimal") %>%
    steps(input$gamboostPreprocess) %>%
    step_rm(has_type("date"))
})
observeEvent(
  input$gamboostGo,
  {
    library(mboost)
    method <- "gamboost"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      deleteRds(method)
      model <- caret::train(getgamboostRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# output gamboostModelSummary0 (print) ----
output$gamboostModelSummary0 <- renderText({
  description("gamboost")
})

# output gamboostMetrics (table) ----
output$gamboostMetrics <- renderTable({
  req(models$gamboost)
  models$gamboost$results[ which.min(models$gamboost$results[, "RMSE"]), ]
})

# output gamboostRecipe (print) ----
output$gamboostRecipe <- renderPrint({
  req(models$gamboost)
  models$gamboost$recipe
})

# output gamboostModelPlots (plot) ----
output$gamboostModelPlots <- renderPlot({
  req(models$gamboost)
  plot(models$gamboost)
})

# output gamboostSummary2 (print) ----
output$gamboostModelSummary2 <- renderPrint({
  req(models$gamboost)
  summary(models$gamboost)
}) 



# METHOD * blackboost   ---------------------------------------------------------------------------------------------------------------------------

# reactive blackboostRecipe ----
getblackboostRecipe <- reactive({
  recipes::recipe(Y ~ ., data = getTrainData()) %>%
    step_date(has_type("date"), features = "decimal") %>%
    steps(input$blackboostPreprocess) %>%
    step_rm(has_type("date"))
})
observeEvent(
  input$blackboostGo,
  {
    library(party)
    method <- "blackboost"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      deleteRds(method)
      model <- caret::train(getblackboostRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# output blackboostModelSummary0 (print) ----
output$blackboostModelSummary0 <- renderText({
  description("blackboost")
})

# output blackboostMetrics (table) ----
output$blackboostMetrics <- renderTable({
  req(models$blackboost)
  models$blackboost$results[ which.min(models$blackboost$results[, "RMSE"]), ]
})

# output blackboostRecipe (print) ----
output$blackboostRecipe <- renderPrint({
  req(models$blackboost)
  models$blackboost$recipe
})

# output blackboostModelPlots (plot) ----
output$blackboostModelPlots <- renderPlot({
  req(models$blackboost)
  plot(models$blackboost)
})

# output blackboostSummary2 (print) ----
output$blackboostModelSummary2 <- renderPrint({
  req(models$blackboost)
  summary(models$blackboost)
}) 


# METHOD * cubist ---------------------------------------------------------------------------------------------------------------------------

# reactive getcubistRecipe ----
getcubistRecipe <- reactive({
  recipes::recipe(Y ~ ., data = getTrainData()) %>%
    step_date(has_type("date"), features = "decimal") %>%
    steps(input$cubistPreprocess) %>%
    step_rm(has_type("date"))
})

# observeEvent cubistGo ----
observeEvent(
  input$cubistGo,
  {
    library(Cubist)
    method <- "cubist"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      deleteRds(method)
      model <- caret::train(getcubistRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
      saveToRds(model, method)
      models[[method]] <- model
    }, 
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# output cubistModelSummary0 (print) ----
output$cubistModelSummary0 <- renderText({
  description("cubist")
})

# output cubistMetrics (table) ----
output$cubistMetrics <- renderTable({
  req(models$cubist)
  models$cubist$results[ which.min(models$cubist$results[, "RMSE"]), ]
})

# output cubistRecipe (print) ----
output$cubistRecipe <- renderPrint({
  req(models$cubist)
  models$cubist$recipe
})  

# output cubistModelPlots (plot) ----
output$cubistModelPlots <- renderPlot({
  req(models$cubist)
  plot(models$cubist)
})



# METHOD * evtree ---------------------------------------------------------------------------------------------------------------------------

# reactive getevtreetRecipe ----
getevtreeRecipe <- reactive({
  recipes::recipe(Y ~ ., data = getTrainData()) %>%
    step_date(has_type("date"), features = "decimal") %>%
    steps(input$evtreePreprocess) %>%
    step_rm(has_type("date"))
})

# observeEvent evtreeGo ----
observeEvent(
  input$evtreeGo,
  {
    library(evtree)
    method <- "evtree"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      deleteRds(method)
      model <- caret::train(getevtreeRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
      saveToRds(model, method)
      models[[method]] <- model
    }, 
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# output evtreeModelSummary0 (print) ----
output$evtreeModelSummary0 <- renderText({
  description("evtree")
})

# output evtreeMetrics (table) ----
output$evtreeMetrics <- renderTable({
  req(models$evtree)
  models$evtree$results[ which.min(models$evtree$results[, "RMSE"]), ]
})

# output evtreeRecipe (print) ----
output$evtreeRecipe <- renderPrint({
  req(models$evtree)
  models$evtree$recipe
})  

# output evtreeModelPlots (plot) ----
output$evtreeodelPlots <- renderPlot({
  req(models$evtree)
  plot(models$evtree)
})


# METHOD * M5 ---------------------------------------------------------------------------------------------------------------------------

# reactive getM5Recipe ----
getM5Recipe <- reactive({
  recipes::recipe(Y ~ ., data = getTrainData()) %>%
    step_date(has_type("date"), features = "decimal") %>%
    steps(input$M5Preprocess) %>%
    step_rm(has_type("date"))
})

# observeEvent M5Go ----
observeEvent(
  input$M5Go,
  {
    library(RWeka)
    method <- "M5"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      deleteRds(method)
      model <- caret::train(getM5Recipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
      saveToRds(model, method)
      models[[method]] <- model
    }, 
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# output M5ModelSummary0 (print) ----
output$M5ModelSummary0 <- renderText({
  description("M5")
})

# output M5Metrics (table) ----
output$M5Metrics <- renderTable({
  req(models$M5)
  models$M5$results[ which.min(models$M5$results[, "RMSE"]), ]
})

# output M5Recipe (print) ----
output$M5Recipe <- renderPrint({
  req(models$M5)
  models$M5$recipe
})  

# output M5ModelPlots (plot) ----
output$M5ModelPlots <- renderPlot({
  req(models$M5)
  plot(models$M5)
})



# METHOD * gbm ---------------------------------------------------------------------------------------------------------------------------

# reactive getgbmRecipe ----
getgbmRecipe <- reactive({
  recipes::recipe(Y ~ ., data = getTrainData()) %>%
    step_date(has_type("date"), features = "decimal") %>%
    steps(input$gbmPreprocess) %>%
    step_rm(has_type("date"))
})

# observeEvent gbmGo ----
observeEvent(
  input$gbmGo,
  {
    library(gbm)
    method <- "gbm"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      deleteRds(method)
      model <- caret::train(getgbmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
      saveToRds(model, method)
      models[[method]] <- model
    }, 
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# output gbmModelSummary0 (print) ----
output$gbmModelSummary0 <- renderText({
  description("gbm")
})

# output gbmMetrics (table) ----
output$gbmMetrics <- renderTable({
  req(models$gbm)
  models$gbm$results[ which.min(models$gbm$results[, "RMSE"]), ]
})

# output gbmRecipe (print) ----
output$gbmRecipe <- renderPrint({
  req(models$gbm)
  models$gbm$recipe
})  

# output gbmModelPlots (plot) ----
output$gbmModelPlots <- renderPlot({
  req(models$gbm)
  plot(models$gbm)
})


  

# METHOD * treebag ---------------------------------------------------------------------------------------------------------------------------

# reactive gettreebagRecipe ----
gettreebagRecipe <- reactive({
  recipes::recipe(Y ~ ., data = getTrainData()) %>%
    step_date(has_type("date"), features = "decimal") %>%
    steps(input$treebagPreprocess) %>%
    step_rm(has_type("date"))
})

# observeEvent treebagGo ----
observeEvent(
  input$treebagGo,
  {
    library(ipred)
    method <- "treebag"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      deleteRds(method)
      model <- caret::train(gettreebagRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
      saveToRds(model, method)
      models[[method]] <- model
    }, 
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# output treebagModelSummary0 (print) ----
output$treebagModelSummary0 <- renderText({
  description("treebag")
})

# output treebagMetrics (table) ----
output$treebagMetrics <- renderTable({
  req(models$treebag)
  models$treebag$results[ which.min(models$treebag$results[, "RMSE"]), ]
})

# output treebagRecipe (print) ----
output$treebagRecipe <- renderPrint({
  req(models$treebag)
  models$treebag$recipe
})  

# output treebagModelPlots (plot) ----
output$treebagModelPlots <- renderPlot({
  req(models$treebag)
  plot(models$treebag)
})



# METHOD * avNNet   ---------------------------------------------------------------------------------------------------------------------------

# reactive avNNetRecipe ----
getavNNetRecipe <- reactive({
  recipes::recipe(Y ~ ., data = getTrainData()) %>%
    step_date(has_type("date"), features = "decimal") %>%
    steps(input$avNNetPreprocess) %>%
    step_rm(has_type("date"))
})
observeEvent(
  input$avNNetGo,
  {
    library(nnet)
    method <- "avNNet"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      deleteRds(method)
      model <- caret::train(getavNNetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# output avNNetModelSummary0 (print) ----
output$avNNetModelSummary0 <- renderText({
  description("avNNet")
})

# output avNNetMetrics (table) ----
output$avNNetMetrics <- renderTable({
  req(models$avNNet)
  models$avNNet$results[ which.min(models$avNNet$results[, "RMSE"]), ]
})

# output avNNetRecipe (print) ----
output$avNNetRecipe <- renderPrint({
  req(models$avNNet)
  models$avNNet$recipe
})

# output avNNetModelPlots (plot) ----
output$avNNetModelPlots <- renderPlot({
  req(models$avNNet)
  plot(models$avNNet)
})

# output avNNetSummary2 (print) ----
output$avNNetModelSummary2 <- renderPrint({
  req(models$avNNet)
  summary(models$avNNet)
})  


# METHOD * brnn   ---------------------------------------------------------------------------------------------------------------------------

# reactive brnnRecipe ----
getbrnnRecipe <- reactive({
  recipes::recipe(Y ~ ., data = getTrainData()) %>%
    step_date(has_type("date"), features = "decimal") %>%
    steps(input$brnnPreprocess) %>%
    step_rm(has_type("date"))
})
observeEvent(
  input$brnnGo,
  {
    library(brnn)
    method <- "brnn"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      deleteRds(method)
      model <- caret::train(getbrnnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# output brnnModelSummary0 (print) ----
output$brnnModelSummary0 <- renderText({
  description("brnn")
})

# output brnnMetrics (table) ----
output$brnnMetrics <- renderTable({
  req(models$brnn)
  models$brnn$results[ which.min(models$brnn$results[, "RMSE"]), ]
})

# output brnnRecipe (print) ----
output$brnnRecipe <- renderPrint({
  req(models$brnn)
  models$brnn$recipe
})

# output brnnModelPlots (plot) ----
output$brnnModelPlots <- renderPlot({
  req(models$brnn)
  plot(models$brnn)
})

# output brnnSummary2 (print) ----
output$brnnModelSummary2 <- renderPrint({
  req(models$brnn)
  summary(models$brnn)
})  



# METHOD * qrnn   ---------------------------------------------------------------------------------------------------------------------------

# reactive qrnnRecipe ----
getqrnnRecipe <- reactive({
  recipes::recipe(Y ~ ., data = getTrainData()) %>%
    step_date(has_type("date"), features = "decimal") %>%
    steps(input$qrnnPreprocess) %>%
    step_rm(has_type("date"))
})
observeEvent(
  input$qrnnGo,
  {
    library(qrnn)
    method <- "qrnn"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      deleteRds(method)
      model <- caret::train(getqrnnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# output qrnnModelSummary0 (print) ----
output$qrnnModelSummary0 <- renderText({
  description("qrnn")
})

# output qrnnMetrics (table) ----
output$qrnnMetrics <- renderTable({
  req(models$qrnn)
  models$qrnn$results[ which.min(models$qrnn$results[, "RMSE"]), ]
})

# output qrnnRecipe (print) ----
output$qrnnRecipe <- renderPrint({
  req(models$qrnn)
  models$qrnn$recipe
})

# output qrnnModelPlots (plot) ----
output$qrnnModelPlots <- renderPlot({
  req(models$qrnn)
  plot(models$qrnn)
})

# output qrnnSummary2 (print) ----
output$qrnnModelSummary2 <- renderPrint({
  req(models$qrnn)
  summary(models$qrnn)
}) 



# METHOD * mlpKerasDropout   ---------------------------------------------------------------------------------------------------------------------------

# reactive mlpKerasDropoutRecipe ----
getmlpKerasDropoutRecipe <- reactive({
  recipes::recipe(Y ~ ., data = getTrainData()) %>%
    step_date(has_type("date"), features = "decimal") %>%
    steps(input$mlpKerasDropoutPreprocess) %>%
    step_rm(has_type("date"))
})
observeEvent(
  input$mlpKerasDropoutGo,
  {
    library(keras)
    method <- "mlpKerasDropout"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      deleteRds(method)
      model <- caret::train(getmlpKerasDropoutRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# output mlpKerasDropoutModelSummary0 (print) ----
output$mlpKerasDropoutModelSummary0 <- renderText({
  description("mlpKerasDropout")
})

# output mlpKerasDropoutMetrics (table) ----
output$mlpKerasDropoutMetrics <- renderTable({
  req(models$mlpKerasDropout)
  models$mlpKerasDropout$results[ which.min(models$mlpKerasDropout$results[, "RMSE"]), ]
})

# output mlpKerasDropoutRecipe (print) ----
output$mlpKerasDropoutRecipe <- renderPrint({
  req(models$mlpKerasDropout)
  models$mlpKerasDropout$recipe
})

# output mlpKerasDropoutModelPlots (plot) ----
output$mlpKerasDropoutModelPlots <- renderPlot({
  req(models$mlpKerasDropout)
  plot(models$mlpKerasDropout)
})

# output mlpKerasDropoutSummary2 (print) ----
output$mlpKerasDropoutModelSummary2 <- renderPrint({
  req(models$mlpKerasDropout)
  summary(models$mlpKerasDropout)
})  

  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------

  
  # reactive getResamples ----
  getResamples <- reactive({
    models <- reactiveValuesToList(models)
    results <- caret::resamples(models)
    
    #scale metrics using null model. Tough code to follow -sorry
    NullModel <- "null"
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }

    #hide results worse than null model
    subset <- rep(TRUE, length(models))
    if (input$HideWorse & NullModel %in% names(models)) {
      actualNames <- colnames(results$values)
      col <- paste(sep = "~", "null","RMSE" )
      if (col %in% actualNames) {
        nullMetric <- mean(results$values[, col], na.rm = TRUE)
        if (!is.na(nullMetric)) {
          m <- 0
          for (model in results$models) {
            m <- m + 1
            mcol <- paste(sep = "~", model, "RMSE")
            if (mcol %in% actualNames) {
              subset[m] <- mean(results$values[, mcol], na.rm = TRUE) <= nullMetric
            }
          }
        }
      }
      results$models <- results$models[subset]
    }
    
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models)
    results
  })
  
  # output SelectionBoxPlot (plot) ----
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  # output Title (UI) ----
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  # reactive getTestResults ----
  getTestResults <- reactive({
    dat <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Y, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })

  # reactive getTrainResults ----
  getTrainResults <- reactive({
    dat <- getTrainData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Y, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # output TestSummary (print)
  output$TestSummary <- renderPrint({
    if (is.na(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  # output TestPlot (plot) ----
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  })
  
  # output TestResiduals (plot) ----
  output$TestResiduals <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Test-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })

  # output TrainResiduals (plot) ----
  output$TrainResiduals <- renderPlot({
    d <- getTrainResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Train-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
    
})

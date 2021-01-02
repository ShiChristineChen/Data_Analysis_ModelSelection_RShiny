shinyUI(fluidPage(
  
 # Application title
  titlePanel("Assignment 3 - Shi Chen"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             #plotOutput(outputId = "MixedPairs"),
             DT::dataTableOutput(outputId = "Table")
             
    ), 
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             helpText("The preprocessing steps and their order are important. ", 
                      "See:", a("Documentation", href="https://www.rdocumentation.org/packages/recipes/versions/0.1.13")),
             
             tabsetPanel(type = "pills",
               tabPanel("NULL Model",
                        br(),
                        fluidRow(
                          column(width = 4),
                          column(width = 1, 
                                 actionButton(inputId = "NullGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "PlsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "NullMetrics"),
                        hr(),
                        verbatimTextOutput(outputId = "NullRecipe"),
               ),
               tabPanel("GLMnet Model",
                        verbatimTextOutput(outputId = "GlmnetModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "GlmnetPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("dummy","date","knnimpute"))
                          )
                        ),
                        column(width = 1, 
                               actionButton(inputId = "GlmnetGo", label = "Train", icon = icon("play")),
                               bsTooltip(id = "GlmnetGo", title = "This will train or retrain your model")
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "GlmnetMetrics"),
                        hr(),
                        plotOutput(outputId = "GlmnetModelPlots"),
                        verbatimTextOutput(outputId = "GlmnetRecipe"),
                        verbatimTextOutput(outputId = "GlmnetModelSummary2")
               ),
               tabPanel("PLS Model",
                        verbatimTextOutput(outputId = "PlsModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "PlsPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("dummy","date","knnimpute"))
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "PlsGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "PlsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "PlsMetrics"),
                        hr(),
                        plotOutput(outputId = "PlsModelPlots"),
                        verbatimTextOutput(outputId = "PlsRecipe"),
                        verbatimTextOutput(outputId = "PlsModelSummary2")
               ),
               tabPanel("Rpart Model",
                        verbatimTextOutput(outputId = "RpartModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "RpartPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("date","knnimpute")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "RpartGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "RpartGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "RpartMetrics"),
                        hr(),
                        plotOutput(outputId = "RpartModelPlots"),
                        plotOutput(outputId = "RpartModelTree"),
                        verbatimTextOutput(outputId = "RpartRecipe"),
               ),
               
               # maintenance point ------------------------------------------------------------------------------
               # add further tabs (with controls) here
               ## rqlasso (Quantile regression and Lasso shrinkage) 
               tabPanel("rqlasso",
                        verbatimTextOutput(outputId = "rqlassoModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "rqlassoPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("dummy","date","knnimpute")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "rqlassoGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "rqlassoGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "rqlassoMetrics"),
                        hr(),
                        plotOutput(outputId = "rqlassoModelPlots"),
                        verbatimTextOutput(outputId = "rqlassoModelSummary2"),
                        verbatimTextOutput(outputId = "rqlassoRecipe"),
               ),
               
               ## glmboost 
               tabPanel("glmboost",
                        verbatimTextOutput(outputId = "glmboostModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "glmboostPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("dummy","date","knnimpute"))
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "glmboostGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "glmboostGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "glmboostMetrics"),
                        hr(),
                        plotOutput(outputId = "glmboostModelPlots"),
                        verbatimTextOutput(outputId = "glmboostModelSummary2"),
                        verbatimTextOutput(outputId = "glmboostRecipe"),
               ),

               ## svmLinear
               tabPanel("svmLinear",
                        verbatimTextOutput(outputId = "svmLinearModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "svmLinearPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("dummy","date","knnimpute"))
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "svmLinearGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "svmLinearGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "svmLinearMetrics"),
                        hr(),
                        plotOutput(outputId = "svmLinearModelPlots"),
                        verbatimTextOutput(outputId = "svmLinearModelSummary2"),
                        verbatimTextOutput(outputId = "svmLinearRecipe"),
               ),
               
               # ## xgbTree
               # tabPanel("xgbTree",
               #          verbatimTextOutput(outputId = "xgbTreeModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "xgbTreePreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("date","knnimpute"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "xgbTreeGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "xgbTreeGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "xgbTreeMetrics"),
               #          hr(),
               #          plotOutput(outputId = "xgbTreeModelPlots"),
               #          verbatimTextOutput(outputId = "xgbTreeModelSummary2"),
               #          verbatimTextOutput(outputId = "xgbTreeRecipe"),
               # ),
               
               ## plsRglm
               tabPanel("plsRglm",
                        verbatimTextOutput(outputId = "plsRglmModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "plsRglmPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing",
                                                choices = ppchoices,
                                                multiple = TRUE,
                                                selected = c("dummy","date","knnimpute"))
                          ),
                          column(width = 1,
                                 actionButton(inputId = "plsRglmGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "plsRglmGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "plsRglmMetrics"),
                        hr(),
                        plotOutput(outputId = "plsRglmModelPlots"),
                        verbatimTextOutput(outputId = "plsRglmModelSummary2"),
                        verbatimTextOutput(outputId = "plsRglmRecipe"),
               ),

               # bayesglm
               tabPanel("bayesglm",
                        verbatimTextOutput(outputId = "bayesglmModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "bayesglmPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing",
                                                choices = ppchoices,
                                                multiple = TRUE,
                                                selected = c("dummy","date","knnimpute"))
                          ),
                          column(width = 1,
                                 actionButton(inputId = "bayesglmGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "bayesglmGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "bayesglmMetrics"),
                        hr(),
                        plotOutput(outputId = "bayesglmModelPlots"),
                        plotOutput(outputId = "bayesglmModelSummary2"),
                        verbatimTextOutput(outputId = "bayesglmRecipe"),
               ),

               # msaenet
               tabPanel("msaenet",
                        verbatimTextOutput(outputId = "msaenetModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "msaenetPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing",
                                                choices = ppchoices,
                                                multiple = TRUE,
                                                selected = c("dummy","date","knnimpute"))
                          ),
                          column(width = 1,
                                 actionButton(inputId = "msaenetGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "msaenetGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "msaenetMetrics"),
                        hr(),
                        plotOutput(outputId = "msaenetModelPlots"),
                        verbatimTextOutput(outputId = "msaenetModelSummary2"),
                        verbatimTextOutput(outputId = "msaenetRecipe"),
               ),

               # ## glmnet_h2o
               # tabPanel("glmnet_h2o",
               #          verbatimTextOutput(outputId = "glmnet_h2oModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "glmnet_h2oPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("dummy","date","knnimpute"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "glmnet_h2oGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "glmnet_h2oGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "glmnet_h2oMetrics"),
               #          hr(),
               #          plotOutput(outputId = "glmnet_h2oModelPlots"),
               #          verbatimTextOutput(outputId = "glmnet_h2oRecipe"),
               #          verbatimTextOutput(outputId = "glmnet_h2oModelSummary2"),
               # ),

               # kernelpls
               tabPanel("kernelpls",
                        verbatimTextOutput(outputId = "kernelplsModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "kernelplsPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing",
                                                choices = ppchoices,
                                                multiple = TRUE,
                                                selected = c("dummy","date","knnimpute"))
                          ),
                          column(width = 1,
                                 actionButton(inputId = "kernelplsGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "kernelplsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "kernelplsMetrics"),
                        hr(),
                        plotOutput(outputId = "kernelplsModelPlots"),
                        verbatimTextOutput(outputId = "kernelplsModelSummary2"),
                        verbatimTextOutput(outputId = "kernelplsRecipe"),
               ),

               ## spls
               tabPanel("spls",
                        verbatimTextOutput(outputId = "splsModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "splsPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing",
                                                choices = ppchoices,
                                                multiple = TRUE,
                                                selected = c("dummy","date","knnimpute"))
                          ),
                          column(width = 1,
                                 actionButton(inputId = "splsGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "splsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "splsMetrics"),
                        hr(),
                        plotOutput(outputId = "splsModelPlots"),
                        verbatimTextOutput(outputId = "splsModelSummary2"),
                        verbatimTextOutput(outputId = "splsRecipe"),
               ),

               ## widekernelpls
               tabPanel("widekernelpls",
                        verbatimTextOutput(outputId = "widekernelplsModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "widekernelplsPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing",
                                                choices = ppchoices,
                                                multiple = TRUE,
                                                selected = c("dummy","date","knnimpute"))
                          ),
                          column(width = 1,
                                 actionButton(inputId = "widekernelplsGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "widekernelplsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "widekernelplsMetrics"),
                        hr(),
                        plotOutput(outputId = "widekernelplsModelPlots"),
                        verbatimTextOutput(outputId = "widekernelplsModelSummary2"),
                        verbatimTextOutput(outputId = "widekernelplsRecipe"),
               ),

               ## simpls
               tabPanel("simpls",
                        verbatimTextOutput(outputId = "simplsModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "simplsPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing",
                                                choices = ppchoices,
                                                multiple = TRUE,
                                                selected = c("dummy","date","knnimpute"))
                          ),
                          column(width = 1,
                                 actionButton(inputId = "simplsGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "simplsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "simplsMetrics"),
                        hr(),
                        plotOutput(outputId = "simplsModelPlots"),
                        verbatimTextOutput(outputId = "simplsModelSummary2"),
                        verbatimTextOutput(outputId = "simplsRecipe"),
               ),
               ## gamboost
               tabPanel("gamboost",
                        verbatimTextOutput(outputId = "gamboostModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "gamboostPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing",
                                                choices = ppchoices,
                                                multiple = TRUE,
                                                selected = c("dummy","date","knnimpute"))
                          ),
                          column(width = 1,
                                 actionButton(inputId = "gamboostGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "gamboostGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "gamboostMetrics"),
                        hr(),
                        plotOutput(outputId = "gamboostModelPlots"),
                        verbatimTextOutput(outputId = "gamboostModelSummary2"),
                        verbatimTextOutput(outputId = "gamboostRecipe"),
               ),
               ## blackboost
               tabPanel("blackboost",
                        verbatimTextOutput(outputId = "blackboostModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "blackboostPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing",
                                                choices = ppchoices,
                                                multiple = TRUE,
                                                selected = c("dummy","date","knnimpute"))
                          ),
                          column(width = 1,
                                 actionButton(inputId = "blackboostGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "blackboostGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "blackboostMetrics"),
                        hr(),
                        plotOutput(outputId = "blackboostModelPlots"),
                        verbatimTextOutput(outputId = "blackboostModelSummary2"),
                        verbatimTextOutput(outputId = "blackboostRecipe"),
               ),

               ## cubist
               tabPanel("cubist",
                        verbatimTextOutput(outputId = "cubistModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "cubistPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing",
                                                choices = ppchoices,
                                                multiple = TRUE,
                                                selected = c("dummy","date","knnimpute"))
                          ),
                          column(width = 1,
                                 actionButton(inputId = "cubistGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "cubistGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "cubistMetrics"),
                        hr(),
                        plotOutput(outputId = "cubistModelPlots"),
                        verbatimTextOutput(outputId = "cubistModelSummary2"),
                        verbatimTextOutput(outputId = "cubistRecipe"),
               ),

               ## evtree
               tabPanel("evtree",
                        verbatimTextOutput(outputId = "evtreeModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "evtreePreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing",
                                                choices = ppchoices,
                                                multiple = TRUE,
                                                selected = c("dummy","date","knnimpute"))
                          ),
                          column(width = 1,
                                 actionButton(inputId = "evtreeGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "evtreeGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "evtreeMetrics"),
                        hr(),
                        plotOutput(outputId = "evtreeModelPlots"),
                        verbatimTextOutput(outputId = "evtreeModelSummary2"),
                        verbatimTextOutput(outputId = "evtreeRecipe"),
               ),
               
               # ## M5
               # tabPanel("M5",
               #          verbatimTextOutput(outputId = "M5ModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "M5Preprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("dummy","date","knnimpute"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "M5Go", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "M5Go", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "M5Metrics"),
               #          hr(),
               #          plotOutput(outputId = "M5ModelPlots"),
               #          verbatimTextOutput(outputId = "M5Summary2"),
               #          verbatimTextOutput(outputId = "M5Recipe"),
               # ),

               ## gbm
               tabPanel("gbm",
                        verbatimTextOutput(outputId = "gbmModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "gbmPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing",
                                                choices = ppchoices,
                                                multiple = TRUE,
                                                selected = c("dummy","date","knnimpute"))
                          ),
                          column(width = 1,
                                 actionButton(inputId = "gbmGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "gbmGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "gbmMetrics"),
                        hr(),
                        plotOutput(outputId = "gbmModelPlots"),
                        verbatimTextOutput(outputId = "gbmModelSummary2"),
                        verbatimTextOutput(outputId = "gbmRecipe"),
               ),

               ## treebag
               tabPanel("treebag",
                        verbatimTextOutput(outputId = "treebagModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "treebagPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing",
                                                choices = ppchoices,
                                                multiple = TRUE,
                                                selected = c("dummy","date","knnimpute"))
                          ),
                          column(width = 1,
                                 actionButton(inputId = "treebagGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "treebagGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "treebagMetrics"),
                        hr(),
                        plotOutput(outputId = "treebagModelPlots"),
                        verbatimTextOutput(outputId = "treebagModelSummary2"),
                        verbatimTextOutput(outputId = "treebagRecipe"),
               ),

               ## avNNet
               tabPanel("avNNet",
                        verbatimTextOutput(outputId = "avNNetModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "avNNetPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing",
                                                choices = ppchoices,
                                                multiple = TRUE,
                                                selected = c("dummy","date","knnimpute"))
                          ),
                          column(width = 1,
                                 actionButton(inputId = "avNNetGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "avNNetGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "avNNetMetrics"),
                        hr(),
                        plotOutput(outputId = "avNNetModelPlots"),
                        verbatimTextOutput(outputId = "avNNetModelSummary2"),
                        verbatimTextOutput(outputId = "avNNetRecipe"),
               ),
               # brnn
               tabPanel("brnn",
                        verbatimTextOutput(outputId = "brnnModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "brnnPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing",
                                                choices = ppchoices,
                                                multiple = TRUE,
                                                selected = c("dummy","date","knnimpute"))
                          ),
                          column(width = 1,
                                 actionButton(inputId = "brnnGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "brnnGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "brnnMetrics"),
                        hr(),
                        plotOutput(outputId = "brnnModelPlots"),
                        verbatimTextOutput(outputId = "brnnRecipe"),
                        verbatimTextOutput(outputId = "brnnModelSummary2")
               ),

               ## qrnn
               tabPanel("qrnn",
                        verbatimTextOutput(outputId = "qrnnModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "qrnnPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing",
                                                choices = ppchoices,
                                                multiple = TRUE,
                                                selected = c("dummy","date","knnimpute"))
                          ),
                          column(width = 1,
                                 actionButton(inputId = "qrnnGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "qrnnGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "qrnnMetrics"),
                        hr(),
                        plotOutput(outputId = "qrnnModelPlots"),
                        verbatimTextOutput(outputId = "qrnnRecipe"),
                        verbatimTextOutput(outputId = "qrnnModelSummary2")
               )
               
               # ## mlpKerasDropout
               # tabPanel("mlpKerasDropout",
               #          verbatimTextOutput(outputId = "mlpKerasDropoutModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "mlpKerasDropoutPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("dummy","date","knnimpute"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "mlpKerasDropoutGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "mlpKerasDropoutGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "mlpKerasDropoutMetrics"),
               #          hr(),
               #          plotOutput(outputId = "mlpKerasDropoutModelPlots"),
               #          verbatimTextOutput(outputId = "mlpKerasDropoutRecipe"),
               #          verbatimTextOutput(outputId = "mlpKerasDropoutModelSummary2")
               #          
               # 
               # )
               
               

               

               
############## add model before this line ###########               
             )
    ),
    
    
  
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             checkboxInput(inputId = "HideWorse", label = "Hide models worse than null model", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             fluidRow(
               column(offset = 2, width=4,
                      plotOutput(outputId = "TestPlot")
               ),
               column(width=2,
                      plotOutput(outputId = "TestResiduals")
               ),
               column(width=2,
                      plotOutput(outputId = "TrainResiduals"),
               )
             ),
             sliderInput(inputId = "IqrM", label = "IQR multiplier", min = 0, max = 5, value = 1.5, step = 0.1),
    )
  )
))

#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(gtools)
library(biomformat)
library(tibble)
library(tools)
library(MXM)
library(caret)
library(e1071)

## Defining the size of file to be accepted. Here it can accept any size.
options(shiny.maxRequestSize= -1) 


#' Main program starts from here
#' Define server logic required
shinyServer(function(input, output) {
  
  output$pic1 = renderUI({
    if(input$slider == 200){
      img(src = "Prediction.png", width = "100%", align = 'middle')
    }else{
      img(src = "image.png", width = as.integer(input$slider), align = 'middle')
    }
  })
  
  # Image
  output$pic2 = renderUI({
    img(src = "IMG_6815.jpg", width = as.integer(input$slider), align = 'middle')
  })
  
  # Image
  output$pic3 = renderUI({
    img(src = "IMG_6816.jpg", width = as.integer(input$slider), align = 'middle')
  })
  
  # # Image
  # output$pic4 = renderUI({
  #   img(src = "Prediction.png", width = as.integer(input$slider), align = 'middle')
  # })
  
  
  #' input data file which need to be randomized
  inputfile <- reactive({
    
    if(is.null(input$InputFile)){
      return(NULL)
    }
    withProgress(message = 'Patience, Processing Data...',{
    if(length(input$InputFile[['datapath']]) > 1) {
      # Show a modal when the button is pressed
      shinyalert("Oops!", "Only one file needed", type = "error")
    }else {
      path = input$InputFile[['datapath']]
      ext = file_ext(path)
      print(ext)
      if(ext == "csv"){
        data <- read.csv(path, stringsAsFactors = FALSE)
        colnames(data) <- gsub("X", "", colnames(data))
        data
      }else if(ext == "biom"){
          message("Print here in biom")
          path <- input$InputFile[['datapath']]
          print(path)
          biom_file <- read_biom(path)
          b_data_m <- as(biom_data(biom_file), "matrix")
          b_data_m_t <- t(b_data_m)
          observation <- observation_metadata(biom_file)
          # print(head(observation))
          data <- list(b_data_m_t, observation)
        }
      }
      return(data)
    })
    })

  
  
  observe(print(paste("Randomize columns", input$RandomizeCols)))
  observe(print(input$items == 'PCA'))
  observe(print(input$items))
  
  
  output$Data <- renderUI({
      fluidRow(
        if ((length(input$InputFile[['datapath']]) & 
             input$items == "PCA")){
          tabBox(
            id = "tabset", width = 8, height = "530px",
            tabPanel("Input Data", DT::dataTableOutput("datafile")),
            tabPanel("Plot", plotOutput("PCAPlotSummary"))
          )
        },
        if (length(input$InputFile[['datapath']]) & 
            input$items == "PrePrBIOM"){
            tabBox(
              id = "tabset1", width = 8, height = "530px",
              tabPanel("BIOM OTU Data", DT::dataTableOutput("biomdata")),
              tabPanel("BIOM Observations", DT::dataTableOutput("biomobv")),
              tabPanel("Plot", plotOutput("PlotSummary"))
            )
          },
        if ((length(input$InputFile[['datapath']]) & 
             input$items == "MLearning")){
          tabBox(
            id = "tabset2", width = 8, height = "530px",
            tabPanel("Input Data", DT::dataTableOutput("MLdata")),
            tabPanel("Accuracy",DT::dataTableOutput("MLaccuracy")),
            tabPanel("Plot", plotOutput("LMPlotSummary")),
            tabPanel("Pval Plot", plotOutput("heatPlot") ),
            tabPanel("Significant Plot", plotOutput("pvalPlot") )
            
          )
        },
        if(input$items == "PCA" | input$items == "PrePrBIOM" | input$items == "MLearning"){
        box(
          br(),
          title = "File Browser", width = 4, solidHeader = TRUE, status = "primary", colour = 'green',
          h5("Upload Data Files",style="bold"),
          # if(input$items == "PCA"){
            fileInput("InputFile", "Browse data file",
                      multiple = "TRUE",
                      accept=c('text/csv','text/comma-separated-values,text/plain',
                               '.csv','.biom','.TXT','.txt')),
          # }else if (input$items == "PrePrBIOM"){
          #   fileInput("InputFileBiom", "Choose BIOM file",
          #             multiple = "TRUE",
          #             accept=c('text/csv','text/comma-separated-values,text/plain',
          #                      '.biom'))
          #   
          # }else if (input$items == "MLearning"){
          #   fileInput("InputFileML", "Choose a CSV file for Machine Learning",
          #             multiple = "TRUE",
          #             accept=c('text/csv','text/comma-separated-values,text/plain',
          #                      '.csv'))
          #   
          # },
          br(),
          # checkboxInput("Controls", "Insert Controls", value = FALSE),
          
          br(),
          actionButton("Submit", label = "Submit",
                       icon("floppy-o"),style="color: white ;
                   background-color: #008B8B;border-color: rgb(51, 153, 255)")
        )
      }
    )
  })
  
  # Plot labels
  output$PlotLabels <- renderUI({
    message("=============================")
    print(input$tabset)
    if(!is.null(inputfile()) & input$items == "PCA"){
      box(
        title = "Plot Labels", width = 3, solidHeader = TRUE, status = "primary",
        textInput("maintitle", "Main Title", value = "", placeholder = "Enter label for main title"),
        textInput("legend", "Legend Text", value = "", placeholder = "Enter legend"),
        textInput("xaxis", "Xlab", value = "", placeholder = "Enter label for x-axis"),
        textInput("yaxis", "Ylab", value = "", placeholder = "Enter label for y-axix"),
        numericInput("dotsize", "Dot", value = 3.5, min = 2.5, max = 10)
      )
    }
  })
  
  
  # Machin Learning Options
  output$MLParameters <- renderUI({
    message("=============================")
    if(!is.null(inputfile()) & input$items == "MLearning"){
      box(
        title = "ML Algorithms", width = 3, solidHeader = TRUE, status = "primary",
        radioButtons("MLParameters", label = "", 
                     choices = list("Support Vector Machine" = "SVM",
                                    "K Nearest Neighbour" = "knn",
                                    "AdaBoost" = "adaboost"
                                    ), selected = "SVM")
      )
    }
  })
  
  
  
  observe({
    message("Testing plot labels")
    print(input$maintitle)
    print(input$xaxis)
    print(input$yaxis)
    print(input$dotsize)
    cols <- input$datafile_columns_selected
    print(colnames(inputfile())[cols])
    print(input$legend)
  })
  
  
  output$Download <- renderUI({
    if (!is.null(inputfile()) & input$items == "PrePrBIOM"){
      box(
        title = "Download", width = 3, solidHeader = TRUE, status = 'primary',
        downloadButton("BIOMdownload",'BIOM Processed Data')
        # numericInput("Divideinto", "Number to divide into", 1,  min = 2, max = 10, step = 1)
      )
    }
  })
  
  
  output$DownloadPCA <- renderUI({
    if (!is.null(inputfile()) & input$items == "PCA"){
      box(
        title = "Download", width = 3, solidHeader = TRUE, status = 'primary',
        downloadButton("downloadpca",'Download PCA Plot')
        # numericInput("Divideinto", "Number to divide into", 1,  min = 2, max = 10, step = 1)
      )
    }
  })
  
  #' #' Options to display final plating data, one plate at a time
  # output$Display_final_design <- renderUI({
  #   print(input$tabset1)
  #   # print(head(randomized_data()))
  #   if(!is.null(randomized_data()) & input$tabset1 == "Randomized Data"){
  #     plates <- ceiling(nrow(inputfile()) / 96)
  #     plates <- paste0("Plate_", 1:plates)
  #     print("You are here............")
  #     if(input$items == "PrePrBIOM"){
  #       box(
  #         title = "Display Data", width = 3, solidHeader = TRUE, status = 'primary',
  #         selectInput('Finaldesign', "Select plate to display data",
  #                     c("Please select plate" = '', plates), 
  #                     selected = plates[1], multiple = FALSE)
  #       )
  #     }
  #   }
  # })
  # 

  #' To display the input file in tabpanel
  output$biomdata <- renderDataTable({
    message("You are here =================")
    print(length(input$InputFileBiom[['datapath']]))
    if(length(input$InputFile[['datapath']]) & input$items == "PrePrBIOM"){
      cat("Display BIOM information...\n")
      df <- inputfile()[[1]]
      df <- as.data.frame(head(df[, 1:100], 100), stringsAsFactors = FALSE)
      infile <- DT::datatable(df, options = list(scrollX= '300px'), 
                              selection = list(target = 'column'))
    }
  })
  
  # BIOM observation data
  output$biomobv <- renderDataTable({
    message("You are here =================")
    print(length(input$InputFileBiom[['datapath']]))
    if(length(input$InputFile[['datapath']]) & input$items == "PrePrBIOM"){
      cat("Display BIOM information...\n")
      df <- inputfile()[[2]]
      df <- as.data.frame(head(df, 100), stringsAsFactors = FALSE)
      infile <- DT::datatable(df, options = list(scrollX= '300px'), 
                              selection = list(target = 'column'))
    }
  })
  
  output$MLdata <- renderDataTable({
    if(length(input$InputFile[['datapath']]) & input$items == "MLearning"){
      cat("Display ML data...\n")
      df <- inputfile()
      df <- df[, c(1,2, (ncol(df) - 200):ncol(df))]
      infile <- DT::datatable(df, options = list(scrollX= '300px'), 
                              selection = list(target = 'column'))
    }
  })
  
  output$datafile <- renderDataTable({
    if(length(input$InputFile[['datapath']]) & input$items == "PCA"){
      cat("Display of meta-analysis files done...\n")
      df <- inputfile()
      show_df <- df[, c(1,2, (ncol(df) - 200):ncol(df))]
      infile <- DT::datatable(show_df, options = list(scrollX= '300px'), 
                              selection = list(target = 'column'))
    }
  })
  
  
 
  
  
  observe(print(input$datafile_columns_selected))
  # randomized_df <- eventReactive({
  #   
  # }
  # )
  
  pcaplot <- reactive({
    df <- inputfile()
    # plot(mtcars)
    show_df <- df[, c(1,2, (ncol(df) - 200):ncol(df))]
    cols <- input$datafile_columns_selected
    print(colnames(show_df)[cols])
    colnames(df) <- gsub("X", "", colnames(df))
    # show_df <- df[, c(1,2, (ncol(df) - 200):ncol(df))]
    data1$family<-factor(data1$family)
    data2 <- data1[c(1:5462)]
    data2.pca <- prcomp(data2, scale = TRUE)
    
    if(input$xaxis == "" & input$yaxis == ""){
      plot <- autoplot(data2.pca,data=data1, colour = colnames(show_df)[cols], size = input$dotsize) +
        theme_classic() + ggtitle(input$maintitle) + 
        theme(plot.title = element_text(hjust = 0.5, size = 22),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 18),
              legend.text = element_text(size = 16),
              legend.title = element_text(size = 18 ))
    }else{
      plot <- autoplot(data2.pca,data=data1, colour = colnames(show_df)[cols], size = input$dotsize) +
        theme_classic() + ggtitle(input$maintitle) + 
        xlab(input$xaxis) + ylab(input$yaxis) +
        guide_legend(title = input$legend) +
        theme(plot.title = element_text(hjust = 0.5, size = 22),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 18),
              legend.text = element_text(size = 16),
              legend.text = element_text(size = 16),
              legend.title = element_text(size = 18 ))
    }    
    plot
  })
  
  pcaplot1 <- function(){
    df <- inputfile()
    # plot(mtcars)
    show_df <- df[, c(1,2, (ncol(df) - 200):ncol(df))]
    cols <- input$datafile_columns_selected
    print(colnames(show_df)[cols])
    colnames(df) <- gsub("X", "", colnames(df))
    # show_df <- df[, c(1,2, (ncol(df) - 200):ncol(df))]
    data1$family<-factor(data1$family)
    data2 <- data1[c(1:5462)]
    data2.pca <- prcomp(data2, scale = TRUE)
    plot <- autoplot(data2.pca,data=data1, colour = colnames(show_df)[cols], size = input$dotsize) +
      theme_classic() + ggtitle(input$maintitle) + xlab(input$xaxis) + ylab(input$yaxis) +
      theme(plot.title = element_text(hjust = 0.5, size = 22),
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 18))
  }
  
  
  output$PCAPlotSummary <- renderPlot({
    pcaplot()
  })
   
  
   
  
  #' Display plot options
  # output$Plots <- renderUI({
  #   if(!is.null(randomized_data()) & input$tabset1 == "Plot"){
  #     if(input$items == "analysis"){
  #       box(
  #         title = "Plots", width = 3, solidHeader = TRUE, status = "primary",
  #         radioButtons("PlotSettings", label = "", 
  #                      choices = list("Sunflower Plot" = "sunflower",
  #                                     "Violin Plot" = "violin",
  #                                     "Density Plot" = "density", 
  #                                     "Box Plot" = "box",
  #                                     "Dot Plot" = "dotplot",
  #                                     "Histogram" = "histogram"), selected = "sunflower")
  #       )
  #     }
  #   }
  # })
  # 
  
  
  mldata <- reactive({
    if(is.null(inputfile()))
      return(NULL)
    
    withProgress(message = 'Patience, Running Machine Learning ..',{
      
      sel_col <- colnames(inputfile())[input$datafile_columns_selected]
      print(sel_col)
      
      names(ts_whole)[5469] = 'obesity_bicat' 
      ts = ts_whole[,-ncol(ts_whole)]
      feat_clr = MMPC(ts_whole$obesity_bicat[which(ts_whole$sampledate=='TimePoint1')],ts)
      ts_mmmb = cbind(ts[,feat_clr@selectedVars],ts_whole$obesity_bicat[which(ts_whole$sampledate=='TimePoint1')])
      names(ts_mmmb)[ncol(ts_mmmb)] = 'obesity_bicat'
      
      # Create different weights to deal with class-imbalance
      model_weights = ifelse(ts_mmmb$obesity_bicat=='Lean',
                             (1/table(ts_mmmb$obesity_bicat)[1])*0.5,
                             (1/table(ts_mmmb$obesity_bicat)[2])*0.5)
      
      
      # linear regression
      # mod_test_clr = glm(obesity_bicat~.,family='binomial',data=ts_mmmb)
      # #to get OR:
      # exp(coef(mod_test_clr$finalModel))
      
      # linear SVM
      # 10 fold cross validation with 3 repeats
      set.seed(1)
      if(input$MLParameters == "SVM"){
        ctrl = trainControl(method='repeatedcv',number=10,repeats=3)
        grid = expand.grid(C=c(0.001,0.01,seq(0.1,1,0.1),1.5,2,3,5))
        mod_test_clr = train(obesity_bicat~., data=ts_mmmb,weights = model_weights, 
                             trControl=ctrl,method='svmLinear',tuneGrid = grid)
      }else if (input$MLParameters == "knn"){
        ctrl = trainControl(method='repeatedcv',number=10,repeats=3)
        grid = expand.grid(k = c(3,5,7,9,11,13))
        mod_test_clr = train(obesity_bicat~.,data=ts_mmmb, 
                             weights = model_weights,trControl=ctrl, 
                             method = 'knn',tuneGrid=grid)
      }else if (input$MLParameters == "knn"){
        ctrl = trainControl(method='repeatedcv',number=10,repeats=3)
        grid = expand.grid(mtry = c(seq(2,length(feat_clr@selectedVars),2)))
        mod_test_clr = train(obesity_bicat~.,data=ts_mmmb, 
                             weights = model_weights,trControl=ctrl, 
                             methods = 'Adaboost.M2',tuneGrid = grid)
      }
      mod_test_clr
    })
  })
  
  
  # Display ML Accuracy
  output$MLaccuracy <- renderDataTable({
    print(head(mod_test_clr$results))
    df <- as.data.frame(mldata()$results, stringsAsFactors = FALSE)
  })

  
  # render machine learning plot
  output$LMPlotSummary <-renderPlot({
    plot(mldata())
  })
  
  
  # Correlation heatmap Plot
  output$heatPlot <- renderPlot({
    withProgress(message = 'Patience, Generating heatmap ..',{
    library(reshape2)
    names(ts_whole)[5469] = 'obesity_bicat' 
    ts = ts_whole[,-ncol(ts_whole)]
    feat_clr = MMPC(ts_whole$obesity_bicat[which(ts_whole$sampledate=='TimePoint1')],ts)
    
    name_list = list()
    for (i in 1:length(names(ts)[feat_clr@selectedVars])){
      name_list= append(name_list,gsub('X','',names(ts)[feat_clr@selectedVars][i]))
    }
    
    OTU_range = feat_clr@selectedVars
    corr = cor(ts[,OTU_range])
    heat_labels = unlist(name_list)
    colnames(corr) <- heat_labels
    rownames(corr) <- heat_labels
    corr_df = melt(corr)
    corr_df$Var1 = as.factor(corr_df$Var1)
    corr_df$Var2 = as.factor(corr_df$Var2)
    ggplot(data = corr_df, aes(Var1, Var2, fill=value)) +
      geom_tile()+
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.text = element_text(size = 15),
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 65,vjust = 0.6))+
      scale_fill_gradient(low="white", high="blue")+
      labs(title = 'Correlation matrix between selected OTUs')
    
  })
  })

  
  # Correlation Plot
  output$pvalPlot <- renderPlot({
    withProgress(message = 'Patience, Generating Significance ..',{
      library(reshape2)
      names(ts_whole)[5469] = 'obesity_bicat' 
      ts = ts_whole[,-ncol(ts_whole)]
      feat_clr = MMPC(ts_whole$obesity_bicat[which(ts_whole$sampledate=='TimePoint1')],ts)
      
      name_list = list()
      for (i in 1:length(names(ts)[feat_clr@selectedVars])){
        name_list= append(name_list,gsub('X','',names(ts)[feat_clr@selectedVars][i]))
      }
      
      df_label = as.data.frame(cbind(feat_clr@selectedVars,name_list))
      df_scatter = as.data.frame(cbind(feat_clr@pvalues,1:length(feat_clr@pvalues)))
      df_scatter$Sig_otu = NA
      for (i in 1:nrow(df_scatter)){
        if (df_scatter[i,2] %in% feat_clr@selectedVars){
          df_scatter$Sig_otu[i] = df_label$name_list[match(df_scatter[i,2],feat_clr@selectedVars)]
        }
      }
      ggplot(df_scatter, aes(x=V2, y=V1)) +
        geom_point(alpha = 0.3,shape = 1) +
        geom_hline(aes(yintercept = log(0.05)),linetype = 2, color = 'blue', size = 1)+
        theme(
          axis.text = element_text(size = 15)
        ) +
        geom_text(aes(label=ifelse(V1<log(0.05),Sig_otu,'')),
                  color = 'firebrick',cex = 5,hjust=0, vjust=0)+
        xlab('Features')+
        ylab('Natural log transformed P-values')+
        labs(title = 'Independent test P-values for all features pre-selection')+
        theme_classic()
      
      
    })
  })
  
  
  #' 
  #' # validate selecting columns to balance randomization
  #' message("Testing above plot ------------")
  #' observe({
  #'   if(input$tabset1 == "Plot"){
  #'     if(!is.null(randomized_data())){
  #'       if(is.null(input$biomdata_columns_selected)){
  #'         shinyalert("Oops!", "Please select columns from randomized data to plot by clicking",
  #'                    type = "error")
  #'       }
  #'     }
  #'   }
  #' })
  #' 
  #' message("Testing here at reactive plot_info ------------")
  #' #' Render plot in the tab panel
  #' plot_info <- reactive({
  #'   if(is.null(input$InputFile) | is.null(inputfile()))
  #'     return(NULL)
  #'   if(is.null(randomized_data()))
  #'     return(NULL)
  #'   
  #'   if(input$tabset1 == "Plot"){
  #'     message("I am testing inside the plot tabset------")
  #'     if(!is.null(randomized_data())){
  #'       message("I am testing more inside the plot tabset------")
  #'       if(is.null(input$biomdata_columns_selected)){
  #'         shinyalert("Oops!", "Please select columns from randomized data to plot by clicking",
  #'                    type = "error")
  #'         return(NULL)
  #'       }
  #'       else if(length(input$biomdata_columns_selected) > 2){
  #'         shinyalert("Oops!", "Selecting more than two columns not allowed",
  #'                    type = "error")
  #'         return(NULL)
  #'       }
  #'       else if(length(input$biomdata_columns_selected) == 1){
  #'         shinyalert("Oops!", "Please select two columns to plot",
  #'                    type = "error")
  #'         return(NULL)
  #'       }
  #'     }
  #'   }
  #'   
  #'   message("Now testing here ----000000000000000000000000")
  #'   plot_type <- input$PlotSettings
  #'   print(plot_type)
  #'   print(head(randomized_data()))
  #'   print(input$biomdata_columns_selected)
  #'   xaxis <- colnames(randomized_data())[input$biomdata_columns_selected[1]]
  #'   yaxis <- colnames(randomized_data())[input$biomdata_columns_selected[2]]
  #'   print(xaxis)
  #'   print(yaxis)
  #'   df <- data.frame(randomized_data()[randomized_data()$ParticipantID != 0, ], check.names = FALSE)
  #'   print(head(df))
  #'   print(is.factor(df[[xaxis]]))
  #'   print(is.factor(df[[yaxis]]))
  #'   if(is.null(plot_type))
  #'     return(NULL)
  #'   if(plot_type == 'sunflower'){
  #'     df[[xaxis]] <- as.factor(df[[xaxis]])
  #'     df[[yaxis]] <- as.factor(df[[yaxis]])
  #'     sunflowerplot(df[[xaxis]], df[[yaxis]], xlab = input$xaxis, ylab = input$yaxis,
  #'                   cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  #'     par(xpd=TRUE)
  #'     title(main = input$maintitle)
  #'     legend(4, -3, c("Unique","Duplicates"), cex=1,
  #'            col=c("black","red"), pch=21:22, lty=1:2)
  #'   }
  #'   else if(plot_type == 'violin'){
  #'     p <- ggplot(df, aes_string(x = xaxis, y = yaxis, fill = xaxis)) + geom_violin() + theme_classic() 
  #'     p + ggtitle(input$maintitle) + xlab(input$xaxis) + ylab(input$yaxis) +
  #'       theme(plot.title = element_text(hjust = 0.5, size = 22),
  #'             axis.text = element_text(size = 15),
  #'             axis.title = element_text(size = 18),
  #'             legend.position = "none") + 
  #'       stat_summary(fun.y = mean, geom = "point", size = 5)
  #'   }
  #'   else if(plot_type == 'density'){
  #'     p <- ggplot(df, aes_string(x = xaxis, color = yaxis)) + geom_density() + theme_classic()
  #'     p + ggtitle(input$maintitle) + xlab(input$xaxis) + ylab(input$yaxis) +
  #'       theme(plot.title = element_text(hjust = 0.5, size = 22),
  #'             axis.text = element_text(size = 15),
  #'             axis.title = element_text(size = 18))
  #'   }
  #'   else if(plot_type == 'box'){
  #'     p <- ggplot(df, aes_string(x = xaxis, y = yaxis, fill = xaxis)) +
  #'       geom_boxplot(outlier.color = "red") + theme_classic()
  #'     p + ggtitle(input$maintitle) + xlab(input$xaxis) + ylab(input$yaxis) +
  #'       theme(plot.title = element_text(hjust = 0.5, size = 22),
  #'             axis.text = element_text(size = 15),
  #'             axis.title = element_text(size = 18),
  #'             legend.position = "none") +
  #'       stat_summary(fun.y = mean, geom = "point", size = 5)
  #'   }
  #'   else if(plot_type == 'dotplot'){
  #'     p <- ggplot(df, aes_string(x = xaxis, y = yaxis, fill = xaxis)) +
  #'       geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.7) + theme_classic()
  #'     p + ggtitle(input$maintitle) + xlab(input$xaxis) + ylab(input$yaxis) +
  #'       theme(plot.title = element_text(hjust = 0.5, size = 22),
  #'             axis.text = element_text(size = 15),
  #'             axis.title = element_text(size = 18),
  #'             legend.position = "none") +
  #'       stat_summary(fun.y = mean, geom = "point", size = 3)
  #'   }
  #'   else if(plot_type == 'histogram'){
  #'     p <- ggplot(df, aes_string(x = xaxis, color = yaxis)) + geom_histogram() + theme_classic()
  #'     p + ggtitle(input$maintitle) + xlab(input$xaxis) + ylab(input$yaxis) +
  #'       theme(plot.title = element_text(hjust = 0.5, size = 22),
  #'             axis.text = element_text(size = 15),
  #'             axis.title = element_text(size = 18))
  #'   }
  #' })
  #' 
  #' 
  #' # display plot
  #' output$PlotSummary <- renderPlot({
  #'   if(is.null(plot_info()))
  #'     return(NULL)
  #'   plot_info()
  #' })
  #' 
  #' 
  #' 
  #' #' This function is work around for downloading the plot
  #' #' It is a bit tricky. if we don't use this function, the plot is not downloaded
  #' #' 
  #' plot_info1 <- function(){
  #'   if(is.null(input$InputFile) | is.null(inputfile()))
  #'     return(NULL)
  #'   if(is.null(randomized_data()))
  #'     return(NULL)
  #'   
  #'   if(input$tabset1 == "Plot"){
  #'     message("I am testing inside the plot tabset------")
  #'     if(!is.null(randomized_data())){
  #'       message("I am testing more inside the plot tabset------")
  #'       if(is.null(input$biomdata_columns_selected)){
  #'         shinyalert("Oops!", "Please select columns from randomized data to plot by clicking",
  #'                    type = "error")
  #'         return(NULL)
  #'       }
  #'       else if(length(input$biomdata_columns_selected) > 2){
  #'         shinyalert("Oops!", "Selecting more than two columns not allowed",
  #'                    type = "error")
  #'         return(NULL)
  #'       }
  #'       else if(length(input$biomdata_columns_selected) == 1){
  #'         shinyalert("Oops!", "Please select two columns to plot",
  #'                    type = "error")
  #'         return(NULL)
  #'       }
  #'     }
  #'   }
  #'   
  #'   message("Now testing here ----000000000000000000000000")
  #'   plot_type <- input$PlotSettings
  #'   print(plot_type)
  #'   print(head(randomized_data()))
  #'   print(input$biomdata_columns_selected)
  #'   xaxis <- colnames(randomized_data())[input$biomdata_columns_selected[1]]
  #'   yaxis <- colnames(randomized_data())[input$biomdata_columns_selected[2]]
  #'   print(xaxis)
  #'   print(yaxis)
  #'   df <- data.frame(randomized_data()[randomized_data()$ParticipantID != 0, ], check.names = FALSE)
  #'   print(head(df))
  #'   print(is.factor(df[[xaxis]]))
  #'   print(is.factor(df[[yaxis]]))
  #'   if(is.null(plot_type))
  #'     return(NULL)
  #'   if(plot_type == 'sunflower'){
  #'     df[[xaxis]] <- as.factor(df[[xaxis]])
  #'     df[[yaxis]] <- as.factor(df[[yaxis]])
  #'     sunflowerplot(df[[xaxis]], df[[yaxis]], xlab = input$xaxis, ylab = input$yaxis,
  #'                   cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  #'     par(xpd=TRUE)
  #'     title(main = input$maintitle)
  #'     legend(4, -3, c("Unique","Duplicates"), cex=1,
  #'            col=c("black","red"), pch=21:22, lty=1:2)
  #'   }
  #'   else if(plot_type == 'violin'){
  #'     p <- ggplot(df, aes_string(x = xaxis, y = yaxis, fill = xaxis)) + geom_violin() + theme_classic() 
  #'     p + ggtitle(input$maintitle) + xlab(input$xaxis) + ylab(input$yaxis) +
  #'       theme(plot.title = element_text(hjust = 0.5, size = 22),
  #'             axis.text = element_text(size = 15),
  #'             axis.title = element_text(size = 18),
  #'             legend.position = "none") + 
  #'       stat_summary(fun.y = mean, geom = "point", size = 5)
  #'   }
  #'   else if(plot_type == 'density'){
  #'     p <- ggplot(df, aes_string(x = xaxis, color = yaxis)) + geom_density() + theme_classic()
  #'     p + ggtitle(input$maintitle) + xlab(input$xaxis) + ylab(input$yaxis) +
  #'       theme(plot.title = element_text(hjust = 0.5, size = 22),
  #'             axis.text = element_text(size = 15),
  #'             axis.title = element_text(size = 18))
  #'   }
  #'   else if(plot_type == 'box'){
  #'     p <- ggplot(df, aes_string(x = xaxis, y = yaxis, fill = xaxis)) +
  #'       geom_boxplot(outlier.color = "red") + theme_classic()
  #'     p + ggtitle(input$maintitle) + xlab(input$xaxis) + ylab(input$yaxis) +
  #'       theme(plot.title = element_text(hjust = 0.5, size = 22),
  #'             axis.text = element_text(size = 15),
  #'             axis.title = element_text(size = 18),
  #'             legend.position = "none") +
  #'       stat_summary(fun.y = mean, geom = "point", size = 5)
  #'   }
  #'   else if(plot_type == 'dotplot'){
  #'     p <- ggplot(df, aes_string(x = xaxis, y = yaxis, fill = xaxis)) +
  #'       geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.7) + theme_classic()
  #'     p + ggtitle(input$maintitle) + xlab(input$xaxis) + ylab(input$yaxis) +
  #'       theme(plot.title = element_text(hjust = 0.5, size = 22),
  #'             axis.text = element_text(size = 15),
  #'             axis.title = element_text(size = 18),
  #'             legend.position = "none") +
  #'       stat_summary(fun.y = mean, geom = "point", size = 3)
  #'   }
  #'   else if(plot_type == 'histogram'){
  #'     p <- ggplot(df, aes_string(x = xaxis, color = yaxis)) + geom_histogram() + theme_classic()
  #'     p + ggtitle(input$maintitle) + xlab(input$xaxis) + ylab(input$yaxis) +
  #'       theme(plot.title = element_text(hjust = 0.5, size = 22),
  #'             axis.text = element_text(size = 15),
  #'             axis.title = element_text(size = 18))
  #'   }
  #' }
  #' 
  #' 
  #' 
  #' #' To download summary data
  #' output$randdownload <- downloadHandler(
  #'   filename <- function() { paste('Randomized_data', '.csv', sep='')},
  #'   content <- function(file) {
  #'     write.csv(randomized_data(), file)
  #'   })
  #' 
  #' 
  #' 
  #' #' Download sample data
  #' #' 
  #' output$sampledata <- downloadHandler(
  #'   filename = "Sample Data.csv",
  #'   content = function(file){
  #'     file.copy("www/Dummydata.csv", file)
  #'   }
  #' )
  #' outputOptions(output, "sampledata", suspendWhenHidden = FALSE)
  #' 
  #' 
  #' 
  #' #' Download tutorial 
  #' #' 
  #' output$TutorialDownl <- downloadHandler(
  #'   filename = "Tutorial.pdf",
  #'   content = function(file){
  #'     file.copy("www/Tutorial.pdf", file)
  #'   }
  #' )
  #' outputOptions(output, "TutorialDownl", suspendWhenHidden = FALSE)
  #' 
  #' 
  #' To download the final design data
  #' We will creat a zip folder for all the plates and download as zip
  output$BIOMdownload <- downloadHandler(
    filename <- function() { paste('BIOM Processed Data', '.zip', sep='')},
    content <- function(file) {
      files <- NULL
      for (i in 1:length(inputfile())) {
        fileName = paste0("File", i, ".csv")
        write.csv(inputfile()[[i]], fileName)
        files <- c(fileName, files)
      }
      zip(file, files)
    })
  #' 
  #' 
  #' 
  ## Download PCA plot
  output$downloadpca <- downloadHandler(
    filename = "plot.png",
    content = function(file){
      if(input$items == "PCA"){
        message("You are trying to plot")
        ggsave(file, pcaplot())
      #   png(file)
      #   pcaplot1()
      #   dev.off()
      # }else{
      #   ggsave(file, pcaplot())
      # }
      }
    })
})
# End ----------------------------------
   
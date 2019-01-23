library(shiny)
library(shinyAce)
library(ggplot2)
library(DT)
library(dplyr)
library(tidyverse)

shinyServer(function(input, output, session){
  
  #-----------------------------------IMPORT---------------------------IMPORT---------------------------------
  # Just some example datasets
  Iris<-read.csv("./Data/Iris.csv")
  Abalone<-read.csv("./Data/Abalone.csv")
  Wine<-read.csv("./Data/Wine.csv")
  
  datasetInput <- reactive({
    if (input$own == FALSE) {switch(input$dataset,
                                    "Iris" = Iris,
                                    "Abalone" = Abalone,
                                    "Wine" = Wine)
    } else {x <- read.csv(input$file$datapath)}
  })
  
  # This is to create a datasetInput variable wchich will hold the selected dataset
  MYDATA1 <- read.csv("./Data/Iris.csv")
  MYDATA2 <- read.csv("./Data/Abalone.csv")
  MYDATA3 <- read.csv("./Data/Wine.csv")
  #MYDATA4 <- read.csv(input$file$datapath)
  
  new_data <- reactiveValues(data = NULL)
  observeEvent(input$save_button,{
    new_data$data <- datasetInput()
  })
  
  # This is for making a table of contents in IMPORT
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(datasetInput(), options = list(lengthMenu = c(5, 20, 50), pageLength = 5))
  })
  
  # Maximum file upload size = 10MB
  options(shiny.maxRequestSize = 10 * 1024 ^ 2)
  
  #----------------------------TIDY----------------------TIDY----------------------TIDY----------------------
  output$contents <- renderTable({

    req(input$file)
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)

    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  output$select_name_to_filter <- renderUI({
    selectInput("name_to_filter","Select column to separate", choices = colnames(new_data$data),
                selected=names(new_data$data)[[1]])
  })
  
  output$select_name_to_normalize <- renderUI({
    selectInput("name_to_normalize","Select column to normalize", choices = colnames(new_data$data),
                selected=names(new_data$data)[[1]])
  })
  
  output$select_name_to_separate <- renderUI({
    selectInput("name_to_separate","Select column to separate", choices = colnames(new_data$data),
                selected=names(new_data$data)[[1]])
  })
  
  output$select_name_to_group <- renderUI({
    selectInput("name_to_group","Select column to group by", choices = colnames(new_data$data),
                selected=names(new_data$data)[[1]])
  })
  
  output$select_name_to_arrange <- renderUI({
    selectInput("name_to_arrange","Select column to arrange", choices = colnames(new_data$data),
                selected=names(new_data$data)[[1]])
  })
  
  output$select_name_to_delete <- renderUI({
    selectInput("name_to_delete","Select column to delete", choices = colnames(new_data$data),
                selected=names(new_data$data)[[1]])
  })
  
  observeEvent(input$filter_button, {
    if(input$value!="" && !is.null(input$value)){
      if (input$comparison == "=") {
        new_data$data <- new_data$data %>% 
          filter(new_data$data[,input$name_to_filter] == input$value)
        myCode$data <<- paste(myCode$data, "myData <- myData %>%\n", sep="")
        myCode$data <<- paste(myCode$data, "  filter(",input$name_to_filter,"=='",input$value,"')\n\n", sep="")
      }
      if (input$comparison == ">") {
        new_data$data <- new_data$data %>% 
          filter(new_data$data[,input$name_to_filter] > input$value)
        myCode$data <<- paste(myCode$data, "myData <- myData %>%\n", sep="")
        myCode$data <<- paste(myCode$data, "  filter(",input$name_to_filter,">'",input$value,"')\n\n", sep="")
      }
      if (input$comparison == "<") {
        new_data$data <- new_data$data %>% 
          filter(new_data$data[,input$name_to_filter] < input$value)
        myCode$data <<- paste(myCode$data, "myData <- myData %>%\n", sep="")
        myCode$data <<- paste(myCode$data, "  filter(",input$name_to_filter,"<'",input$value,"')\n\n", sep="")
      }
      updateAceEditor(session, "myEditor", myCode$data)
    }
  })
  
  observeEvent(input$normalize_button,{
    new_data$data <- new_data$data %>% 
      mutate_each_(funs(scale(.) %>% as.vector), vars=c(input$name_to_normalize))
    myCode$data <<- paste(myCode$data, "myData <- myData %>%\n", sep="")
    myCode$data <<- paste(myCode$data, "  mutate_each_(funs(scale(.) %>% as.vector), vars=c('",input$name_to_normalize,"'))\n\n", sep="")
    updateAceEditor(session, "myEditor", myCode$data)
  })
  
  observeEvent(input$sep_button,{
    if(input$name_separated1!="" && !is.null(input$name_separated1) && input$name_separated2!="" && !is.null(input$name_separated2)){
      new_data$data <- new_data$data %>% 
        separate(input$name_to_separate, into = c(input$name_separated1, input$name_separated2))
      myCode$data <<- paste(myCode$data, "myData <- myData %>%\n", sep="")
      myCode$data <<- paste(myCode$data, "  separate(",input$name_to_separate,",into = c('",input$name_separated1,"','",input$name_separated2,"'))\n\n", sep="")
      updateAceEditor(session, "myEditor", myCode$data)
    }
  })
  
  observeEvent(input$del_button,{
    new_data$data <- new_data$data %>% 
      select(-c(input$name_to_delete))
    myCode$data <<- paste(myCode$data, "myData <- myData %>%\n", sep="")
    myCode$data <<- paste(myCode$data, "  select(-c('",input$name_to_delete,"'))\n\n", sep="")
    updateAceEditor(session, "myEditor", myCode$data)
  })

  observeEvent(input$arr_button,{
    if(input$order=="ascending"){
      new_data$data <- new_data$data %>%
        arrange(new_data$data[,input$name_to_arrange])
      myCode$data <<- paste(myCode$data, "myData <- myData %>%\n", sep="")
      myCode$data <<- paste(myCode$data, "  arrange(",input$name_to_arrange,")\n\n", sep="")
    }
    if(input$order=="descending"){
      new_data$data <- new_data$data %>%
        arrange(desc(new_data$data[,input$name_to_arrange]))
      myCode$data <<- paste(myCode$data, "myData <- myData %>%\n", sep="")
      myCode$data <<- paste(myCode$data, "  arrange(desc(",input$name_to_arrange,"))\n\n", sep="")
    }
    updateAceEditor(session, "myEditor", myCode$data)
  })
  
  observeEvent(input$NA_button,{
    if(input$fill_delete=="fill"){
      new_data$data <- new_data$data %>%
        replace(is.na(.), 0)
      myCode$data <<- paste(myCode$data, "myData <- myData %>%\n", sep="")
      myCode$data <<- paste(myCode$data, "  replace(is.na(.), 0)\n\n", sep="")
    }
    if(input$fill_delete=="delete"){
      new_data$data <- new_data$data %>%
        drop_na()
      myCode$data <<- paste(myCode$data, "myData <- myData %>%\n", sep="")
      myCode$data <<- paste(myCode$data, "  drop_na()\n\n", sep="")
    }
    updateAceEditor(session, "myEditor", myCode$data)
  })
  
  output$table_tidy <- DT::renderDataTable({
    DT::datatable(new_data$data, options = list(lengthMenu = c(5, 20, 50), pageLength = 5))
  })
  
  #-------------TRANSFORM--------------------TRANSFORM--------------------TRANSFORM---------------------------
  output$mytable2_make <- DT::renderDataTable({
    DT::datatable(new_data$data,
                  options = list(lengthMenu = c(5, 20, 50), pageLength = 5))
  })
  
  output$ColumnSelector_xx <- renderUI({
    selectInput("SelectedColumn_xx","Select X Variable", choices = colnames(new_data$data),
                selected=names(new_data$data)[[1]])
  })
  
  output$ColumnSelector_yy <- renderUI({
    selectInput("SelectedColumn_yy","Select Y Variable", choices = colnames(new_data$data),
                selected=names(new_data$data)[[2]])
  })
  
  observeEvent(input$btn, {
      if(input$NewCol!="" && !is.null(input$NewCol) && input$btn>0){
        if (input$op == "X+Y") {
          newcol <- as.data.frame(sapply(new_data$data[input$SelectedColumn_xx], as.numeric)+sapply(new_data$data[input$SelectedColumn_yy], as.numeric))
          myCode$data <<- paste(myCode$data, input$NewCol, " <- myData[,'", input$SelectedColumn_xx,"'] + myData[,'", input$SelectedColumn_yy,
                                "']\n", sep = "")
        }
        if (input$op == "X-Y") {
          newcol <- as.data.frame(sapply(new_data$data[input$SelectedColumn_xx], as.numeric)-sapply(new_data$data[input$SelectedColumn_yy], as.numeric))
          myCode$data <<- paste(myCode$data, input$NewCol, " <- myData[,'", input$SelectedColumn_xx,"'] - myData[,'", input$SelectedColumn_yy,
                                "']\n", sep = "")
        }
        if (input$op == "a*X") {
          newcol <- as.data.frame(sapply(new_data$data[input$SelectedColumn_xx], as.numeric)*input$const)
          myCode$data <<- paste(myCode$data, input$NewCol, " <- myData[,'", input$SelectedColumn_xx,"'] * ", input$const, "\n", sep = "")
        }
        if (input$op == "X^a") {
          newcol <- as.data.frame((sapply(new_data$data[input$SelectedColumn_xx], as.numeric))^input$const)
          myCode$data <<- paste(myCode$data, input$NewCol, " <- myData[,'", input$SelectedColumn_xx,"'] ^ ", input$const, "\n", sep = "")
        }
        names(newcol) <- input$NewCol
        new_data$data <<- cbind(new_data$data, newcol)
        myCode$data <<- paste(myCode$data, "myData <- cbind(myData, ", input$NewCol, ")\n\n", sep = "")
        updateAceEditor(session, "myEditor", myCode$data)
    }
  })
  
  #-------------------VISUALIZATION-------------------------VISUALIZATION------------------------------------
  # This is a reactive selective input, its choices depend on the selected dataset
  output$ColumnSelector <- renderUI({
    selectInput("SelectedColumn","Select a column", choices = colnames(new_data$data),
                selected=names(new_data$data)[[1]])
  })
  
  output$sample_two <- renderUI({
    selectInput("sample_two_selected","Select second column", choices = colnames(new_data$data),
                selected=names(new_data$data)[[1]])
  })
  
  # This is for rendering the histogram
  plot_variable <- renderPlot({
    if (is.null(new_data$data)){return(NULL)}
    else{
    x <- new_data$data[,input$SelectedColumn]
    bins <- seq(min(x),max(x),length.out=input$bins+1)
    hist(x, breaks=bins, col='darkgray', border='white')
    }
  })
  output$distPlot <- plot_variable
  
  #needs to have a duplicate so that I can write it in files
  select_plot2 = function() {
    x <- new_data$data[,input$SelectedColumn]
    bins <- seq(min(x),max(x),length.out=input$bins+1)
    hist(x, breaks=bins, col='darkgray', border='white')
  }
  # this is for numerical
  output$summary <- renderPrint({
    dataset <- new_data$data
    summary(new_data$data[input$SelectedColumn])
  })
  
  ttestout <- reactive({
    var1 <- new_data$data[input$SelectedColumn]
    if (is.null(var1)){return(NULL)}
    t1 <- t.test(var1, mu=input$mu)
    var2 <- new_data$data[,input$sample_two_selected]
    if (is.null(var2)){return(NULL)}
    t2 <- t.test(var1, var2)
    if(input$sample == "oneSamp") {return(t1)}
    if(input$sample == "twoSamp") {return(t2)}
  })
  
  # Output of one sample t value of t-test
  output$tvalue <- renderPrint({
    vals <- ttestout()
    if (is.null(vals)){return(NULL)}
    vals$statistic
  })
  output$pvalue <- renderPrint({
    vals <- ttestout()
    if (is.null(vals)){return(NULL)}
    vals$p.value 
  })
  
  output$parametric <- renderTable({
    var1 <- new_data$data[,input$SelectedColumn]
    if (is.null(var)){return(NULL)}
    var2 <- new_data$data[,input$sample_two_selected]
    if (is.null(var)){return(NULL)}
    mean1 <- mean(var1)
    mean2 <- mean(var2)
    standard_deviation1 <- sd(var1)
    standard_deviation2 <- sd(var2)
    standard_error1 <- sd(var1)/sqrt(length(var1))
    standard_error2 <- sd(var2)/sqrt(length(var2))
    parametric1 <- data.frame(mean = mean1, 
                              standard_deviation=standard_deviation1, 
                              standard_error=standard_error1)
    rownames(parametric1) <- input$var1
    parametric2 <- data.frame(mean = mean2, 
                              standard_deviation=standard_deviation2, 
                              standard_error=standard_error2)
    rownames(parametric2) <- input$var2
    if(input$sample == "oneSamp") {return(parametric1)}
    if(input$sample == "twoSamp") {return(rbind(parametric1,parametric2))}
  })
  
  #------------------MODEL---------------------MODEL-------------------MODEL---------------------------------
  # Another selective inputs for k means clustering, give us options to select atributes from dataset
  output$ColumnSelector_x <- renderUI({
    selectInput("SelectedColumn_x","X Variable", choices = colnames(new_data$data),
                selected=names(new_data$data)[[1]])
  })
  output$ColumnSelector_y <- renderUI({
    selectInput("SelectedColumn_y","Y Variable", choices = colnames(new_data$data),
                selected=names(new_data$data)[[2]])
  })
  # Clustering the selected columns from imported dataset
  selectedData <- reactive({
    new_data$data[, c(input$SelectedColumn_x, input$SelectedColumn_y)]
  })
  
  # Clustering in number of classes inputed via numeric input
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  # Ploting the clustered data
  plot_model2 <- renderPlot({
    if (is.null(new_data$data)){return(NULL)}
    else{
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    }
  })
  output$plot_model <- plot_model2
  
  select_model2 = function() {
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  }
  
  #----------------------COMMUNICATE--------------COMMUNICATE-----------COMMUNICATE---------------------------
  #this is to download a plot
  output$down_plot <- downloadHandler(
    filename =  function() {
      paste(input$SelectedColumn, '-histogram', Sys.Date(), input$var3, sep=".")
    },
    content = function(file) {
      if(input$var3 == "png")
        png(file, type="cairo") # open the png device
      else
        pdf(file) # open the pdf device
      select_plot2()
      dev.off()  # turn the device off
    })
  output$savePlot <- plot_variable
  
  output$down_model <- downloadHandler(
    filename =  function() {
      paste(input$SelectedColumn_x, '-', input$SelectedColumn_y, '-model',input$clusters, Sys.Date(), input$var5, sep=".")
    },
    content = function(file) {
      if(input$var5 == "png")
        png(file, type="cairo") # open the png device
      else
        pdf(file) # open the pdf device
      select_model2()
      dev.off()  # turn the device off
    })
  output$saveModel <- plot_model2
  
  #to get the extension for table
  fileext <- reactive({
    switch (input$var4,
            "Excel (CSV)" = "csv",
            "Text (TSV)" = "txt",
            "Text (space separated)" = "txt",
            "Doc" = "doc")
  })
  
  #function for download button for plot
  output$down_data <- downloadHandler(
    filename =  function() {
      if (input$own == FALSE) {paste(input$dataset, '-data', Sys.Date(), fileext(), sep=".")}
      else {paste(input$file$name, '-data', Sys.Date(), fileext(), sep=".")}
    },
    content = function(file) {
      sep <- switch (input$var4,
                     "Excel (CSV)" = ",",
                     "Text (TSV)" = "\t",
                     "Text (space separated)" = " ",
                     "Doc" = " "
      )
      write.table(new_data$data, file, sep = sep, row.names = FALSE)
    })

  output$contents2 <- renderTable({head(new_data$data)})
  
  observeEvent(input$save_button_code,{
    if (input$own == FALSE) {
      myCode$data <<- paste(myCode$data, "write.csv(myData, \"", input$dataset, "-modified.csv\")\n\n", sep="")
    }
    else {
      myCode$data <<- paste(myCode$data, "write.csv(myData, ", input$file$name, "-modified.csv)\n\n", sep="")
    }
    updateAceEditor(session, "myEditor", myCode$data)
  })
  
  #-------------EDITOR-------------------EDITOR------------------------------EDITOR-------------------------
  myCode <- paste("\n")
  observeEvent(input$tidy_button, {
    myCode <- paste(myCode, "tidy", collapse = "\n")
    updateAceEditor(session, "myEditor", myCode)
  })
  observeEvent(input$save_button, {
    myCode <- paste(myCode, "save", collapse = "\n")
    updateAceEditor(session, "myEditor", myCode)
  })
  myCode <- reactiveValues(data = "library(ggplot2)\nlibrary(tidyverse)\n\n")
  observeEvent(input$save_button,{
    if (input$own == FALSE) {switch(input$dataset,
                                    "Iris" = myCode$data <<- paste(myCode$data, "myData <- read.csv(\"./Data/Iris.csv\")\n\n", sep = ""),
                                    "Abalone" = myCode$data <<- paste(myCode$data, "myData <- read.csv(\"./Data/Iris.csv\")\n\n", sep = ""),
                                    "Wine" = myCode$data <<- paste(myCode$data, "myData <- read.csv(\"./Data/Iris.csv\")\n\n", sep = ""))
    } else {myCode$data <<- paste(myCode$data, "myData <- read.csv(", input$file, ") #please manually input the path\n\n", sep = "")}
    #myCode$data <<- paste(myCode$data, "save\n\n")
    updateAceEditor(session, "myEditor", myCode$data)
  })
  observeEvent(input$tidy_button,{
    if(input$own == TRUE){
      myCode$data <<- paste(myCode$data, "myData <- read.csv(", input$file, "),\n", sep = "")
      myCode$data <<- paste(myCode$data, "                   header = ", input$header, ",\n", sep = "")
      myCode$data <<- paste(myCode$data, "                   sep = \"", input$sep, "\",\n", sep = "")
      myCode$data <<- paste(myCode$data, "                   quote = \"\\", input$quote, "\")\n\n", sep = "")
    }
    updateAceEditor(session, "myEditor", myCode$data)
  })
  observeEvent(input$visual_button,{
    if(input$plot_type == "Histogram"){
      x <- new_data$data[,input$SelectedColumn]
      b <- input$bins+1
      myCode$data <<- paste(myCode$data, "hist(myData[,'",input$SelectedColumn, "'], breaks=seq(", min(x),",", max(x),", length.out=", b, "), col='darkgray', border='white')\n", sep = "")
      myCode$data <<- paste(myCode$data, "dev.copy(png,'", input$SelectedColumn, "-", b,"-histogram.png')\n", sep="")
      myCode$data <<- paste(myCode$data, "dev.off()\n\n", sep="")
    }
    if(input$plot_type == "Numerical"){
      myCode$data <<- paste(myCode$data, "summary(myData[,'", input$SelectedColumn, "'])\n\n", sep = "")
    }
    if(input$plot_type == "T-Test"){
      if(input$sample == "oneSamp"){
        myCode$data <<- paste(myCode$data, "t.test(myData$", input$SelectedColumn, ", mu=", input$mu, ")\n\n", sep = "")
      }
      if(input$sample == "twoSamp"){
        myCode$data <<- paste(myCode$data, "t.test(myData$", input$SelectedColumn,", myData$", input$sample_two_selected, ")\n\n", sep = "")
      }
    }
    updateAceEditor(session, "myEditor", myCode$data)
    
  })
  
  observeEvent(input$model_button,{
    myCode$data <<- paste(myCode$data, "selectedData <- myData[, c('", input$SelectedColumn_x, "','" ,input$SelectedColumn_y, "')]\n", sep = "")
    myCode$data <<- paste(myCode$data, "clusters <- kmeans(selectedData, ", input$clusters, ")\n", sep="")
    myCode$data <<- paste(myCode$data, "palette(c(\"#E41A1C\", \"#377EB8\", \"#4DAF4A\", \"#984EA3\", \"#FF7F00\", \"#FFFF33\", \"#A65628\", \"#F781BF\", \"#999999\"))\n", sep="")
    myCode$data <<- paste(myCode$data, "par(mar = c(5.1, 4.1, 0, 1))\n", sep="")
    myCode$data <<- paste(myCode$data, "plot(selectedData, col = clusters$cluster, pch = 20, cex = 3)\n", sep="")
    myCode$data <<- paste(myCode$data, "points(clusters$centers, pch = 4, cex = 4, lwd = 4)\n", sep="")
    myCode$data <<- paste(myCode$data, "dev.copy(png,'", input$SelectedColumn_x, "-", input$SelectedColumn_y,"-",input$clusters, "cluster.png')\n", sep="")
    myCode$data <<- paste(myCode$data, "dev.off()\n\n", sep="")
    updateAceEditor(session, "myEditor", myCode$data)
  })

}
)

#
# Jenny Xiao
#
#
#

library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(knitr)
library(ggplot2)
library(caret)
library(plotly)
# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$centile),]

function(input, output, session) {
  
 
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(zipdata,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  newTable <- reactive({
    
    newdata <- cleantable %>% filter(State == input$Summery_states)%>% select("Score","Population","College","Income")
    
  })
  trainTable <- reactive(
    {
      traindata <-allzips
    }
  )
  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks
  
  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    hist(zipsInBounds()$centile,
         breaks = centileBreaks,
         main = "SuperZIP score (visible zips)",
         xlab = "Percentile",
         xlim = range(allzips$centile),
         col = '#00DD00',
         border = 'white')
  })
  
  output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size
    
    if (colorBy == "superzip") {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
      colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
      pal <- colorFactor("viridis", colorData)
    } else {
      colorData <- zipdata[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    }
    
    if (sizeBy == "superzip") {
      # Radius is treated specially in the "superzip" case.
      radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
    } else {
      radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
    }
    
    leafletProxy("map", data = zipdata) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })
  
  # Show a popup at the given location
  showZipcodePopup <- function(zipcode, lat, lng) {
    selectedZip <- allzips[allzips$zipcode == zipcode,]
    content <- as.character(tagList(
      tags$h4("Score:", as.integer(selectedZip$centile)),
      tags$strong(HTML(sprintf("%s, %s %s",
                               selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
      ))), tags$br(),
      sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
      sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
      sprintf("Adult population: %s", selectedZip$adultpop)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
  
  
  ## Data Explorer ###########################################
  
  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectizeInput(session, "cities", choices = cities,
                         selected = stillSelected, server = TRUE)
  })
  
  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
               is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectizeInput(session, "zipcodes", choices = zipcodes,
                         selected = stillSelected, server = TRUE)
  })
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Score >= input$minScore,
        Score <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df, outputId = "ziptable")
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  
  
  output$summerytable <- renderPrint({
    
    summary(newTable())
    
  })
  
  output$scatterCollegeIncome_state <- renderPlot({
    #scatter plot on income and college of the selected state
    print(xyplot(Income ~ College, data = newTable(), xlim = range(allzips$college), ylim = range(allzips$income)))
  })
  
  output$box <- renderPlot({
    x<-summary(newTable()[,input$var])
    boxplot(x,col="sky blue",border="purple",main=names(newTable()[input$var]))
  })
  
  output$info_s <- renderText({
    
    paste("In this page, you can review the numeric summary by selecting a state,\n",
          "and the numeric summary by selecting a variable in boxplot \n",
          "also, you can see the correlation between income and college")
  })
  
  pca_objects <- reactive({
                   the_data_subset <- allzips %>% select("adultpop","households","college","income")
                   
                     pca_output <- prcomp(
                     na.omit(the_data_subset),
                     center = input$center,
                     scale = input$scale_data
                     
                   )
    
  })
 
  output$SCREE_PLOT <- renderPlot({
    pca_output <- pca_objects()
    eig = (pca_output$sdev) ^ 2
    variance <- eig * 100 / sum(eig)
    cumvar <- paste(round(cumsum(variance), 1), "%")
    eig_df <- data.frame(eig = eig,
                         PCs = colnames(pca_output$x),
                         cumvar =  cumvar)
    
    num_PCS_to_plot = input$pc_range
    
    # limit to 10 PCs
    eig_df <- eig_df[1:num_PCS_to_plot,]
    eig <- eig[1:num_PCS_to_plot]
    cumvar <- cumvar[1:num_PCS_to_plot]
    
    ggplot(eig_df, aes(reorder(PCs,-eig), eig)) +
      geom_bar(stat = "identity",
               fill = "white",
               colour = "black") +
      geom_text(label = cumvar,
                size = 4,
                vjust = -0.4) +
      theme_bw(base_size = 14) +
      xlab("PC") +
      ylab("Variances") +
      ylim(0, (max(eig_df$eig) * 1.1))
  })
  
  # PC plot
  
  output$the_pcs_to_plot_x <- renderUI({
    pca_output <- pca_objects()$x
    
    # drop down selection
    selectInput(
      inputId = "the_pcs_to_plot_x",
      label = "X axis:",
      choices = colnames(pca_output),
      selected = 'PC1'
    )
  })
  
  output$the_pcs_to_plot_y <- renderUI({
    pca_output <- pca_objects()$x
    
    # drop down selection
    selectInput(
      inputId = "the_pcs_to_plot_y",
      label = "Y axis:",
      choices = colnames(pca_output),
      selected = 'PC2'
    )
  })
  
 
  # PC plot
  pca_biplot <- reactive({
    the_data_subset <- allzips %>% select("adultpop","households","college","income")
    pca_output <-  pca_objects()
    pcs_df <- cbind(the_data_subset, pca_output$x)
    
    var_expl_x <-
      round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_x))] ^
              2 / sum(pca_output$sdev ^ 2), 1)
    var_expl_y <-
      round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_y))] ^
              2 / sum(pca_output$sdev ^ 2), 1)
    labels <- rownames(pca_output$x)
    
    
    #TODO: separate the plot + legend since the legends can vary in size considerably
    
    
      pc_plot <<- ggplot(pcs_df,
                         aes_string(input$the_pcs_to_plot_x,
                                    input$the_pcs_to_plot_y))
   
    
    
      pc_plot = pc_plot + geom_point()
    
    
    pc_plot <- pc_plot +
      theme_gray(base_size = 14)
    
    
    
    if (input$draw_ellipse) {
      pc_plot = pc_plot + stat_ellipse(
        aes(fill = 'fill_'),
        geom = "polygon",
        alpha = 0.2,
        show.legend = FALSE
      )
    }
    
    pc_plot <- pc_plot +
      coord_equal() +
      xlab(paste0(
        input$the_pcs_to_plot_x,
        " (",
        var_expl_x,
        "% explained variance)"
      )) +
      ylab(paste0(
        input$the_pcs_to_plot_y,
        " (",
        var_expl_y,
        "% explained variance)"
      ))
    pc_plot
  })
  
  output$PCA_PLOT <- renderPlot({
    
    pca_biplot()
    
  })
 
  output$NumPredictors = renderUI({
    radioButtons("NumPredictor", label = "Include all the predictors",
                 choices = list("Yes" = 1, "No" = 2), 
                 selected = 1)
  })
  
  output$Predictors = renderUI(if(input$NumPredictor==2){
    selectInput(
      "Predictor", 
      label = "Chose Predictors",
      selected = "College",selectize=TRUE,multiple=TRUE,choices=names(newTable())
    )
  })
  
  output$ScatterAllPairs<-renderPlot({
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Plot will be displayed..Kindly wait......")
    pairs(trainTable() %>% select("centile","adultpop","households","college","income"), 
          main="Simple Scatterplot Matrix")      
    
  })
  

  output$Model = renderPrint({
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Processing is going on..Kindly wait")
    
    set.seed(7)
    trainControl <- trainControl(method="cv")
    
    #if(input$Go){
    #DF<-na.omit(zipdata %>% select("income","centile","college","adultpop","households"))
    DF<-na.omit(newTable())

    if (input$NumPredictor==1){
      Formula<-as.formula(paste("Income", ".", sep = " ~ "))
    }else{
      Formula<-as.formula(paste("Income", paste(input$Predictor, collapse = " + "), sep = " ~ "))
    }
    
    if(input$MLT==1){
      fit.linear<<- train(Formula, data=DF, method="lm", 
                          preProc=c("center", "scale"), trControl=trainControl)
      
      newdata = data.frame(College=72, Score=20, Population=85)
      y = predict(fit.linear, newdata)
      print(y)
      
      results <<- resamples(list(LinearRegression=fit.linear,LinearRegression=fit.linear))
      #LFC$Linear<-fit.linear
      print(fit.linear)
      print(varImp(fit.linear))
      summary(results)
    }else if(input$MLT==2){
      fit.rf<<- train(Formula, data=DF, method="rf", 
                      preProc=c("center", "scale"), trControl=trainControl,importance=T)
      
      results <- resamples(list(RandomForest=fit.rf,RandomForest=fit.rf))
      print(fit.rf)
      print(varImp(fit.rf))
      summary(results)    
    }
  }) 
  
  output$predictOutput = renderPrint(if(input$Go){
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    DF<-na.omit(newTable())
    set.seed(7)
    trainControl <- trainControl(method="cv")
    
    DFTest<-data.frame(College=input$collegepredictor,Population=input$populationpredictor)
    
    if(input$MLT==1){
      prefit.linear<<- train(Income ~ College + Population, data=DF, method="lm", 
                          preProc=c("center", "scale"), trControl=trainControl)
      
      print(predict(prefit.linear,DFTest))
      
    }
    
    if(input$PredictMLT==2){
      prefit.rf<<- train(Income ~ College + Population, data=DF, method="rf", 
                      preProc=c("center", "scale"), trControl=trainControl,importance=T)
      print(predict(prefit.rf,DFTest))
    }
    
    
  })
  
  ###start here
  
  output$Hist<-renderPlotly({
    
    DF<-na.omit(newTable())
    
    H <- hist(DF[,input$var], plot = FALSE)
    
    minimum<-min(H$breaks,na.rm=TRUE)
    maximum<-max(H$breaks,na.rm=TRUE)
    step<-H$breaks[2]-H$breaks[1]
    
    ggplot(DF,aes_string(x=input$var)) + 
      stat_bin(binwidth=step,colour="blue",fill="pink") +  
      stat_bin(binwidth=step, geom="text", aes(label=scales::percent((..count../sum(..count..)))), vjust=-1.5)+
      scale_x_continuous(breaks=seq(minimum,maximum, by=step))+theme_bw()
  })
    ### end here
  
  
}
  
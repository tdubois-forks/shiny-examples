library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- allzips2years |>
  filter(year == 2019)

zipdata <- zipdata[sample.int(nrow(zipdata), 10000),]
# zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$prc_pov),]
# colnames(zipdata2)

function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
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


  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks

  # output$histCentile <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  #
  #   hist(zipsInBounds()$centile,
  #     breaks = centileBreaks,
  #     main = "SuperZIP score (visible zips)",
  #     xlab = "Percentile",
  #     xlim = range(allzips$centile),
  #     col = '#00DD00',
  #     border = 'white')
  # })

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
      colorData <- zipdata[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
      # radius <- zipdata[[sizeBy]]*100
      if (sizeBy == "adultpop") {
        # Radius is treated specially in the "superzip" case.
        radius <- zipdata[[sizeBy]]
      }
      else {
        radius <- zipdata[[sizeBy]]*500
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
        Year == input$dataset,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df, outputId = "ziptable")
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })


  datasetInput <- reactive({
    switch(input$dataset,
           "2015" = cleantable |>
             filter(
               Year == 2015,
               is.null(input$states) | State %in% input$states,
               is.null(input$cities) | City %in% input$cities,
               is.null(input$zipcodes) | Zipcode %in% input$zipcodes) |>
             select(c(-Lat, -Long)),
           "2019" = cleantable |>
             filter(
               Year == 2019,
               is.null(input$states) | State %in% input$states,
               is.null(input$cities) | City %in% input$cities,
               is.null(input$zipcodes) | Zipcode %in% input$zipcodes) |>
             select(c(-Lat, -Long)))
  })

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )


#   NOTE: DOWNLOAD ONLY WORKS IF YOU OPEN THE APP IN A BROWSER, NOT IF YOU TRY IT FROM THE RSTUDIO WINDOW

# PASTED FROM data_analyser app


options(shiny.maxRequestSize=10*1024^2)

data_input <- reactive({
  req(input$csv_input)
  fread(input$csv_input$datapath)
})

observeEvent(data_input(),{
  choices <- c(not_sel,names(data_input()))
  updateSelectInput(inputId = "num_var_1", choices = choices)
  updateSelectInput(inputId = "num_var_2", choices = choices)
  updateSelectInput(inputId = "fact_var", choices = choices)
})

num_var_1 <- eventReactive(input$run_button,input$num_var_1)
num_var_2 <- eventReactive(input$run_button,input$num_var_2)
fact_var <- eventReactive(input$run_button,input$fact_var)

# plot

plot_1 <- eventReactive(input$run_button,{
  draw_plot_1(data_input())
})

output$plot_1 <- renderPlot(plot_1())

# 1-d summary tables

output$num_var_1_title <- renderText(paste("Num Var 1:",num_var_1()))

num_var_1_summary_table <- eventReactive(input$run_button,{
  create_num_var_table(data_input(), num_var_1())
})

output$num_var_1_summary_table <- renderTable(num_var_1_summary_table(),colnames = FALSE)

output$num_var_2_title <- renderText(paste("Num Var 2:",num_var_2()))

num_var_2_summary_table <- eventReactive(input$run_button,{
  create_num_var_table(data_input(), num_var_2())
})

output$num_var_2_summary_table <- renderTable(num_var_2_summary_table(),colnames = FALSE)

output$fact_var_title <- renderText(paste("Patient Distribution Among States"))

fact_var_summary_table <- eventReactive(input$run_button,{
  create_fact_var_table(data_input(), fact_var())
})

output$fact_var_summary_table <- renderTable(fact_var_summary_table(),colnames = FALSE)

# multi-d summary table

combined_summary_table <- eventReactive(input$run_button,{
  create_combined_table(data_input(), num_var_1(), num_var_2(), fact_var())
})

output$combined_summary_table <- renderTable(combined_summary_table())

}

library(leaflet)
library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)

# Choices for drop-downs
vars <- c(
  # "Is SuperZIP?" = "superzip",
  # "Centile score" = "centile",
  "Population" = "adultpop",
  "College education" = "college",
  "Low education" = "prc_educ_ltHS",
  "Median income" = "income",
  "Income" = "income",
  "Renter" = "prc_renterocc_hh",
  "Transit" = "prc_trans_tran",
  "Employed" = "prc_employed",
  "Poverty" = "prc_pov",
  "Black" = "prc_NHBlack",
  "Asian" = "prc_NHAsian",
  "Hispanic" = "prc_HispanicAn"

)

navbarPage("Lynch Lab Indices Explorer", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Zip Code Explorer"),

        selectInput("color", "Color", vars),
        selectInput("size", "Size", vars, selected = "adultpop"),
        conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
          numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        ),

        # plotOutput("histCentile", height = 200),
        plotOutput("scatterCollegeIncome", height = 250)
      ),

      tags$div(id="cite",
        'Data compiled by Tesla DuBois for Fox Chase Cancer Center, Division of Cancer Prevention and Control.'
      )
    )
  ),

  tabPanel("Data explorer",
    fluidRow(
      column(3,
        selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
        )
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
        )
      ),
      column(3,
             selectInput("dataset", "Year",
                         choices = c("2015", "2019"))
      )
    ),

    fluidPage(

      # App title ----
      titlePanel("Downloading Data"),

      # Sidebar layout with input and output definitions ----
      sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(

          # Input: Choose dataset ----
          # selectInput("dataset", "Choose a dataset:",
          #             choices = c("2015", "2019")),

          # Button
          downloadButton("downloadData", "Download")

        ),

        # Main panel for displaying outputs ----
        mainPanel(

          tableOutput("table")

        )

      )
    ),
    hr(),
    DT::dataTableOutput("ziptable")
  ),


  conditionalPanel("false", icon("crosshair")),

  tabPanel(
    title = "Analysis",
    titlePanel("Analysis"),
    sidebarLayout(
      sidebarPanel(
        title = "Inputs",
        fileInput("csv_input", "Select CSV File to Import", accept = ".csv"),
        selectInput("num_var_1", "Zip Code", choices = c(not_sel)),
        selectInput("num_var_2", "Patient Count", choices = c(not_sel)),
        selectInput("state", "State", choices = c(not_sel)),
        br(),
        actionButton("run_button", "Run Analysis", icon = icon("play"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = "Patient Poverty",
            fluidRow(
              column(width = 6, strong(textOutput("plot_1_title"))),
              column(width = 4, strong(textOutput("plot_2_title")))
            ),
            fluidRow(
              column(width = 6, plotOutput("plot_1")),
              column(width = 6, plotOutput("plot_2"))

          )),
          tabPanel(
            title = "Patient States",
            fluidRow(
              # column(width = 4, strong(textOutput("num_var_1_title"))),
              # column(width = 4, strong(textOutput("num_var_2_title"))),
              column(width = 4, strong(textOutput("state_title")))
            ),
            fluidRow(
              # column(width = 4, tableOutput("num_var_1_summary_table")),
              # column(width = 4, tableOutput("num_var_2_summary_table")),
              column(width = 4, tableOutput("state_summary_table"))
            # ),
            # fluidRow(
            #   column(width = 12, strong("Combined Statistics"))
            ),
            fluidRow(
              column(width = 12, tableOutput("combined_summary_table"))
            )

          )
        )

      )
    )
  )
)

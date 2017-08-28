# Shiny UI
library(shiny)
library(plotly)

#-- shiny --#
shinyUI(
  pageWithSidebar(
    headerPanel("Plate reader"),
    
    sidebarPanel(      
      fileInput("data_file", "Choose Excel File"),
      textInput("sheet_name", 
                label = "Sheet name", 
                value = "Sheet1")
    ),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Raw data table", DT::dataTableOutput('raw_tbl')), 
                  tabPanel("Std curve table", DT::dataTableOutput('std_curve_tbl')), 
                  tabPanel("Std curve plot", plotlyOutput('std_curve_plot')),
                  tabPanel("Samples", DT::dataTableOutput('conc_tbl')),
                  tabPanel("Table for 'dilute'", DT::dataTableOutput('conc_tbl_dil')),
                  tabPanel("Example data table", DT::dataTableOutput('example_tbl'))
      )
    )
  ))

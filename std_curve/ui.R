# Shiny UI
library(shiny)
library(plotly)

#-- shiny --#
shinyUI(
  pageWithSidebar(
    headerPanel("Plate reader"),
    
    sidebarPanel(      
      fileInput("data_file", "Choose Gen5 concentration table (Excel, csv, or txt)"),
      textInput("sheet_name_data", 
                label = "Sheet name (if excel)", 
                value = "Sheet1"),
      hr(),
      h4('(optional) Add sample names to concentrations'),
      fileInput("map_file", "A QIIME-formatted table of sample names (Excel, csv, or txt)"),
      textInput("sheet_name_map",
                label = "Sheet name (if excel)", 
                value = "Sheet1"),
      numericInput("sample_start", 
                   label = "Starting sample in mapping file",
                   value = '1', min=1),
      numericInput("sample_end", 
                   label = "Ending sample in mapping file",
                   value = '40', min=1),
      h6('Note: assuming sample order matches concentration table order')
    ),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Raw data table", DT::dataTableOutput('raw_tbl')), 
                  tabPanel("Std curve table", DT::dataTableOutput('std_curve_tbl')), 
                  tabPanel("Std curve plot", plotlyOutput('std_curve_plot')),
                  tabPanel("Samples", DT::dataTableOutput('conc_tbl')),
                  tabPanel("Table for 'dilute'", DT::dataTableOutput('conc_tbl_dil')),
                  tabPanel("Mapping table", DT::dataTableOutput('mapping_tbl')),
                  tabPanel("Example data table", DT::dataTableOutput('example_data_tbl')),
                  tabPanel("Example mapping table", DT::dataTableOutput('example_map_tbl'))
      )
    )
  ))

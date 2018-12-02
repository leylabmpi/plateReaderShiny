# Shiny UI
library(shiny)
library(plotly)

#-- shiny --#
shinyUI(
  navbarPage("PicoGreen",
    tabPanel("Input", 
      fluidPage(
        fluidRow(
          column(4,
                 h4("Gen5 exported table"),
                 fileInput("data_file", "Concentration table (Excel, csv, or txt)"),
                 textInput("sheet_name_data", 
                           label = "Sheet name (if excel)", 
                           value = "Sheet1")
          ),
          column(4,
                 h4("Mapping file (optional)"),
                 h5("Used to add sample IDs to the concentrations"),
                 fileInput("map_file", "A QIIME-formatted table of sample names (Excel, csv, or txt)"),
                 textInput("sheet_name_map",
                           label = "Sheet name (if excel)", 
                           value = "Sheet1"),
                 hr(),
                 textInput("masked_wells",
                           label = "Wells in the std curve to mask", 
                           value = ""),
                 numericInput("sample_start", 
                              label = "Starting sample in mapping file",
                              value = '1', min=1),
                 numericInput("sample_end", 
                              label = "Ending sample in mapping file",
                              value = '40', min=1)    
          ),
          column(4,
                 h4("Sample positions"),
                 textInput("TECAN_labware_name",
                           label = 'Labware name',
                           value = 'Sample plate'),
                 textInput("TECAN_labware_type",
                           label = 'Labware type',
                           value = 'PCR Adapter 96 Well and 96 Well Eppendorf TwinTec PCR'),
                 numericInput("TECAN_target_position_start", 
                              label = "Target position start",
                              value = '1', min=1),
                 numericInput("TECAN_target_position_end", 
                              label = "Target position end",
                              value = '96', min=1)       
          )
        )
      )
    ),
    tabPanel("Raw data", 
      tabsetPanel(
        tabPanel("Raw data table", DT::dataTableOutput('raw_tbl')), 
        tabPanel("Mapping table", DT::dataTableOutput('mapping_tbl'))
      )
    ),
    tabPanel("Std curve", 
      tabsetPanel(
        tabPanel("Std curve table", DT::dataTableOutput('std_curve_tbl')), 
        tabPanel("Std curve plot", plotlyOutput('std_curve_plot'))
      )
    ),
    tabPanel("Samples",
      tabsetPanel(
        tabPanel("Samples", DT::dataTableOutput('conc_tbl')),
        tabPanel("Table for 'dilute'", DT::dataTableOutput('conc_tbl_dil'))
      )
    ),
    tabPanel("Examples", 
      tabsetPanel("Examples",
                  tabPanel("Example data table", DT::dataTableOutput('example_data_tbl')),
                  tabPanel("Example mapping table", DT::dataTableOutput('example_map_tbl'))
      )
    )
  )
)

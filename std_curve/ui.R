# Shiny UI
library(shiny)
library(plotly)

#-- shiny --#
shinyUI(
  navbarPage("PicoGreen",
    tabPanel("Raw data",
      sidebarLayout(
        sidebarPanel(
                 numericInput("sample_start", 
                              label = "Starting sample in mapping file",
                              value = '1', min=1),
                 numericInput("sample_end", 
                              label = "Ending sample in mapping file",
                              value = '40', min=1),
                 width=3
               ),
        mainPanel(
          tabsetPanel(
            tabPanel("Destination plate 1",
              textAreaInput("dest_plate_1", 
                            "Paste your exported stats data into this box (tab-delimited)", 
                            "", 
                            width = "700px",
                            height = "500px")
            ),
            tabPanel("Destination plate 2",
              textAreaInput("dest_plate_2", 
                            "Paste your exported stats data into this box (tab-delimited)", 
                            "", 
                            width = "700px",
                            height = "500px")
            ),
            tabPanel("Destination plate 3",
              textAreaInput("dest_plate_3", 
                            "Paste your exported stats data into this box (tab-delimited)", 
                            "", 
                            width = "700px",
                            height = "500px")
            ),
            tabPanel("Sample IDs",
              textAreaInput("sample_ids", 
                            "Paste sample names into this box", 
                            "", 
                            width = "700px",
                            height = "500px")
            )
          )
        )
      )
    ),
    tabPanel("Std curve", 
      sidebarLayout(
        sidebarPanel(
           textInput("masked_wells_plate1",
                     label = "Plate1: Wells in the std curve to mask", 
                     value = ""),
           textInput("masked_wells_plate2",
                     label = "Plate2: Wells in the std curve to mask", 
                     value = ""),
           textInput("masked_wells_plate3",
                     label = "Plate3: Wells in the std curve to mask", 
                     value = ""),
           width=3
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Std curve plot", plotlyOutput('std_curve_plot', height='600px')),
            tabPanel("Std curve table", DT::dataTableOutput('std_curve_tbl'))
          )
        )
      )
    ),
    tabPanel("Samples",
      sidebarLayout(
        sidebarPanel(
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
                          value = '96', min=1),
          width=3
        ),
        mainPanel(      
          tabsetPanel(
            tabPanel("Table for 'dilute'", DT::dataTableOutput('conc_tbl_dil')),
            tabPanel("Samples", DT::dataTableOutput('conc_tbl')),
            tabPanel("Conc. heatmap (ng/ul)", plotlyOutput('conc_tbl_htmap')),
            tabPanel("RFU heatmap (ng/ul)", plotlyOutput('RFU_tbl_htmap'))
          )
        )
      )
    ),
    tabPanel("Examples", 
      tabsetPanel("Examples",
                  tabPanel("Example data table", DT::dataTableOutput('example_data_tbl')),
                  tabPanel("Example sample IDs", DT::dataTableOutput('example_map_tbl'))
      )
    )
  )
)

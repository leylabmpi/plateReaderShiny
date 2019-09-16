# Shiny UI
library(shiny)
library(plotly)

#-- shiny --#
shinyUI(
  navbarPage("PicoGreen",
    tabPanel("Raw data",
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
                            "Paste sample names into this box (one per line)", 
                            "", 
                            width = "700px",
                            height = "500px")
            )
          )
    ),
    tabPanel("Std curve", 
      sidebarLayout(
        sidebarPanel(
           h5('WARNING: do not use masking in the plate reader software! Mask wells using the options below.'),
           h6('For multiple wells, use comma-delimited lists (eg., "A1,B1")'),
           textInput("masked_wells_plate1",
                     label = "Plate1: Wells in the std curve to mask", 
                     value = ""),
           textInput("masked_wells_plate2",
                     label = "Plate2: Wells in the std curve to mask", 
                     value = ""),
           textInput("masked_wells_plate3",
                     label = "Plate3: Wells in the std curve to mask", 
                     value = ""),
           checkboxInput("set_intercept_zero",
                         label = "Set intercept to zero?", 
                         value = FALSE),
           width=3
        ),
        mainPanel(
          fluidRow(
            column(12,
                   h5('WARNING: this app assumes a dilution of 1:100 (sample : total_rxn_volume)')
            )
          ),
          tabsetPanel(
            tabPanel("Std curve plot", plotlyOutput('std_curve_plot', height='700px')),
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
          hr(),
          h4('Blank sample substraction'),
          h6('Subtracting out "blank" sample concentations. 
              The mean of all blank samples will be used.'),
          textInput("blank_samples",
                    label = "Blank samples: list Well IDs (comma-separated)", 
                    value = ""),
          hr(),
          h5('If large differences between replicates (eg., due to low DNA volumes), you can filter out low conc. replicates with this cutoff'),
          numericInput("CV_cutoff",
                        label = "CV cutoff", 
                        value = 25),
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
      tabsetPanel(tabPanel("Example data table",
                           DT::dataTableOutput('example_data_tbl')),
                  tabPanel("Example sample IDs",
                           DT::dataTableOutput('example_map_tbl'))
      )
    )
  )
)

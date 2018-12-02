# Shiny server
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(plotly)
source("../utils/io.R")
source("../utils/format.R")


#' Reading table 
read_table_file = function(infile, sheet_name){
  if(endsWith(infile, '.xlsx') | endsWith(infile, '.xls')){
    df = read_excel(infile, sheet=sheet_name)
  } else
  if(endsWith(infile, '.csv')){
    df = read.table(infile, sep=',', header=TRUE, 
                      comment.char='~', check.names=FALSE)
  } else
  if(endsWith(infile, '.txt')){
    df = read.table(infile, sep='\t', header=TRUE, 
                    comment.char='~', check.names=FALSE)
  } else {
    stop('Unknown input file format')
  }
  return(df)
}

#' fill in names
fill_names = function(Name){
  Name = Name %>% as.character
  last_name = NULL
  for(i in 1:length(Name)){
    x = Name[i]
    if(is.na(x) & !is.null(last_name)){
      Name[i] = last_name
    }
    if(!is.na(x)){
      last_name = x
    }
  }
  return(Name)
}

#' Loading concentration table (plate reader output)
read_conc = function(data_file, sheet_name){
  # read table
  if(is.null(data_file)){
    return(NULL)
  }
  infile = rename_tmp_file(data_file)
  df = read_table_file(infile, sheet_name)

  # formatting
  colnames(df) = gsub('[_/()% ]+', '_', colnames(df))
  colnames(df) = gsub('_+$', '', colnames(df))
  colnames(df)[5] = 'RFU'
  ## checking for required columns
  req_cols = c('Well_ID', 'Name', 'Well', 'RFU')
  for(x in req_cols){
    stopifnot(x %in% colnames(df))
  }
  ## formatting columns
  df = df %>%
    mutate(Mean = ifelse(grepl('^[?]+$', Mean), NA, Mean),
           Std_Dev = ifelse(grepl('^[?]+$', Std_Dev), NA, Std_Dev),
           Mean = Mean %>% as.Num,
           Std_Dev = Std_Dev %>% as.Num,
           RFU = RFU %>% as.Num,
           CV = CV %>% as.Num,
           Name = fill_names(Name))
  return(df)
}

#' Loading mapping file
read_map = function(map_file, sheet_name){
  # read table
  if(is.null(map_file)){
    return(NULL)
  }
  infile = rename_tmp_file(map_file)
  df = read_table_file(infile, sheet_name)
  return(df)
}

#' formatting linear regression equation for plotting
equation = function(x) {
  a = round(coef(x)[1], digits = 2)
  b = round(coef(x)[2], digits = 2)
  r2 = round(summary(x)$r.squared, digits = 2)
  sprintf("y = %0.2fx +  %.2f, R^2 = %.2f", b, a, r2)
}

#' calculating concentations based on linear regression of std curve
calc_conc = function(df, fit){
  a = coef(fit)[1]
  b = coef(fit)[2]
  df %>%
    filter(grepl('^SPL[0-9]+', Well_ID)) %>%
    mutate(Conc_Dil = (Mean - a) / b)
}

#' convert well IDs (eg., 'A3' or 'D12' to well locations (column-wise))
well2loc = function(x, plate_type='96-well'){
  plate_type = tolower(plate_type)
  
  x = strsplit(x, '')[[1]]
  well_row = toupper(x[1])
  well_col = as.Num(x[2])
  well_row = which(well_row == LETTERS)
  
  if(plate_type == '96-well'){
    n_per_col = 8
  } else
  if(plate_type == '384-well'){
    n_per_col = 16
  } else {
    stop('plate_type not recognized')
  }
  # well ID
  (well_col - 1) * n_per_col + well_row
}

#' formatting concentration table for dilution
conc_tbl_to_dilute = function(df, sample_labware='96 Well[001]', plate_reader_labware='96-well'){
  if(is.null(df)){
    return(NULL)
  }
  df = df %>%
    dplyr::select(Well_ID, Name, Well, Conc_Dil) %>%
    mutate(Sample_labware = sample_labware,
           Sample_location = sapply(Well, well2loc, plate_type=plate_reader_labware),
           Sample_concentration = Conc_Dil) %>%
    dplyr::select(-Conc_Dil, -Well)
  colnames(df) = gsub('_', ' ', colnames(df))
  return(df)
}

#' loading example plate reader file
load_ex_data_file = function(){
  read_excel('../data/picogreen1.xlsx')
}

#' loading example mapping file
load_ex_map_file = function(){
  read_excel('../data/mapping1.xlsx')
}

#' Adding sample names to plate reader output ()
#' Joining based on sample order
add_sample_names = function(df_data, df_map, sample_start = 1, 
                            sample_end = 1, plate_type='96 well'){
  # limiting sample range
  if(sample_start > nrow(df_map)){
    return(df_data)
  }
  if(sample_end > nrow(df_map)){
    sample_end = nrow(df_map)
  }
  
  df_data = df_data[1:(sample_end-sample_start+1),]
  x = df_map[sample_start:sample_end, c('#SampleID')] %>% 
    as.matrix %>% as.vector
  df_data$Name = x[1:nrow(df_data)]
  
  return(df_data)
}


#-- server --#
shinyServer(function(input, output, session) {
  
  # reading in data table
  data_tbl = eventReactive(input$data_file, {
    read_conc(input$data_file, input$sheet_name_data)
  })
  
  # loading mapping file
  map_tbl = eventReactive(c(input$map_file, input$data_file), {
    if(is.null(input$map_file)){
      return(NULL)
    }
    read_map(input$map_file, input$sheet_name_map)
  })
  
  # get standard curve values
  masked_wells = reactive({
    gsub(' +', '', input$masked_wells) %>%
      strsplit(',') %>%
      unlist
  })
  
  std_curve = reactive({
    if(is.null(data_tbl())){
      return(NULL)
    }
    data_tbl() %>%
        filter(Name == 'Standard curve') %>%
        filter(!Well %in% masked_wells())
  })
  
  # linear regression on standard curve
  std_curve_lm = reactive({
    if(is.null(std_curve())){
      return(NULL)
    }
    df = std_curve() %>%
      group_by(Conc_Dil) %>%
      summarize(mean_RFU = mean(RFU, na.rm=TRUE)) %>%
      ungroup()
    lm(mean_RFU ~ Conc_Dil, data = df)
  })
  
  # calculating concentrations
  data_tbl_conc = reactive({
    if(is.null(data_tbl()) | is.null(std_curve_lm())){
      return(NULL)
    }
    df = calc_conc(data_tbl(), std_curve_lm()) %>%
      mutate(Conc_Dil = round(Conc_Dil %>% as.Num, 3))
    # adding sample names
    if(!is.null(map_tbl())){
      df = df = add_sample_names(df, map_tbl(),
                               sample_start = input$sample_start,
                               sample_end = input$sample_end)
    }
    return(df)
  })
   
  #--- rendering ---#
  # Table of raw data
  output$raw_tbl = DT::renderDataTable(
    data_tbl(),
    filter = 'bottom',
    extensions = c('Buttons'),
    options = list(
      pageLength = 40,
      dom = 'Blfrtip',
      buttons = c('colvis', 'copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  
  # Table of standard curve values
  output$std_curve_tbl = DT::renderDataTable(
    std_curve(),
    filter = 'bottom',
    extensions = c('Buttons'),
    options = list(
      pageLength = 40,
      dom = 'Blfrtip',
      buttons = c('colvis', 'copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  
  # standard curve plot
  output$std_curve_plot = renderPlotly({
    # std curve data
    if(is.null(std_curve())){
      return(NULL)
    }
    
    # filtering out 'masked' values
    df_std_curve = std_curve() %>%
      filter(!grepl('^[*].+[*]$', RFU),
             !is.na(RFU)) %>%
      rename('Def_conc' = Conc_Dil)
    
    # linear regression data
    x_txt = quantile(df_std_curve$Def_conc, 0.6, na.rm=TRUE) 
    y_txt = quantile(df_std_curve$RFU, 0.9, na.rm=TRUE) 
    fit = std_curve_lm()
    
    # plotting
    p = ggplot(df_std_curve, aes(Def_conc, RFU)) +
      geom_smooth(method=lm) +
      geom_point(aes(test=Well)) +
      annotate("text", x=x_txt, y=y_txt, 
               label=equation(fit), 
               parse=TRUE, size=3) +
      labs(x='Defined conc.', y='RFU') +
      theme_bw() 
    ggplotly(p)
   })
  
  # Table of calculated concentrations
  output$conc_tbl = DT::renderDataTable(
    data_tbl_conc(),
    filter = 'bottom',
    extensions = c('Buttons'),
    options = list(
      pageLength = 40,
      dom = 'Blfrtip',
      buttons = c('colvis', 'copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  
  # Table formatted for colutions
  output$conc_tbl_dil = DT::renderDataTable(
    conc_tbl_to_dilute(data_tbl_conc()),
    filter = 'bottom',
    extensions = c('Buttons'),
    options = list(
      pageLength = 40,
      dom = 'Blfrtip',
      buttons = c('colvis', 'copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  
  # Mapping file table
  output$mapping_tbl = DT::renderDataTable(
    map_tbl(),
    filter = 'bottom',
    extensions = c('Buttons'),
    options = list(
      pageLength = 40,
      dom = 'Blfrtip',
      buttons = c('colvis', 'copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  
  # example data table
  output$example_data_tbl = DT::renderDataTable(
    load_ex_data_file(),
    extensions = c('Buttons'),
    options = list(
      pageLength = 50,
      dom = 'Brt',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  
  # example mapping table
  output$example_map_tbl = DT::renderDataTable(
    load_ex_map_file(),
    extensions = c('Buttons'),
    options = list(
      pageLength = 50,
      dom = 'Brt',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
})


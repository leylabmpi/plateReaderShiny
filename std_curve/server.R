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

#' reading table pasted in
read_table_pasted = function(x, header=TRUE){
  # read table
  if(is.null(x) | nchar(x) == 0){
    return(NULL)
  }
  read.delim(text=x, sep='\t', header=header)
}


#' Loading concentration table (plate reader output)
read_conc = function(txt, label, header=TRUE){
  # read table from pasted-in
  if(is.null(txt) | nchar(txt) == 0){
    return(NULL)
  }
  df = read_table_pasted(txt, header=header)

  # formatting
  colnames(df) = gsub('[._/()% ]+', '_', colnames(df))
  colnames(df) = gsub('_+$', '', colnames(df))
  colnames(df)[colnames(df) == 'X480_520'] = 'RFU'
  
  ## checking for required columns
  req_cols = c('Well_ID', 'Name', 'Well', 'RFU')
  for(x in req_cols){
    if(!x %in% colnames(df)){
      print(sprintf('%s not in header', x))
    }
  }
  ## formatting columns
  df = df %>%
    mutate(Mean = ifelse(grepl('^[?]+$', Mean), NA, Mean),
           Std_Dev = ifelse(grepl('^[?]+$', Std_Dev), NA, Std_Dev),
           Mean = Mean %>% as.Num,
           Std_Dev = Std_Dev %>% as.Num,
           RFU = RFU %>% as.Num,
           CV = CV %>% as.Num,
           Name = Name %>% as.character,
           Name = ifelse(lag(Name) != "", 
                         lag(Name), Name), #fill_names(Name),
           Plate_ID = label)
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
calc_conc = function(df, fit, int_zero=FALSE){
  a = coef(fit)[1]
  if(int_zero == TRUE){
    a = 0
  }
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
conc_tbl_to_dilute = function(df,
                              TECAN_labware_name,
                              TECAN_labware_type,
                              TECAN_target_position_start=1,
                              TECAN_target_position_end=384){
  if(is.null(df)){
    return(NULL)
  }
  TECAN_target_position = TECAN_target_position_start:TECAN_target_position_end
  TECAN_target_position = TECAN_target_position[1:nrow(df)]
  df = df %>%
    dplyr::select(Well_ID, Name, Well, Conc_Dil) %>%
    mutate(TECAN_labware_name = TECAN_labware_name,
           TECAN_labware_type = TECAN_labware_type,
           TECAN_target_position = TECAN_target_position,
           TECAN_sample_conc = Conc_Dil) %>%
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

# linear regression on standard curve
std_curve_lm = function(df_std_curve){
  if(is.null(df_std_curve) | nrow(df_std_curve) == 0){
    return(NULL)
  }
  df = df_std_curve %>%
    group_by(Conc_Dil) %>%
    summarize(mean_RFU = mean(RFU, na.rm=TRUE)) %>%
    ungroup()
  lm(mean_RFU ~ Conc_Dil, data = df)
}


#------------------ SERVER ----------------#
shinyServer(function(input, output, session) {
  
  # reading in data table(s)
  data_tbl = reactive({
    df1 = read_conc(input$dest_plate_1, label='Plate1')
    df2 = read_conc(input$dest_plate_2, label='Plate2')
    df3 = read_conc(input$dest_plate_3, label='Plate3')
    if(!is.null(df1) & !is.null(df2)){
      df1 = rbind(df1, df2)
    }
    if(!is.null(df1) & !is.null(df3)){
      df1 = rbind(df1, df3)
    }
    if(!is.null(df1)){
      cols = colnames(df1)
      cols = c('Plate_ID', setdiff(cols, 'Plate_ID'))
      df1 = df1[,cols]
    }
    return(df1)
  })
  
  # loading sample_IDs
  map_tbl = reactive({
    df = read_table_pasted(input$sample_ids, header=FALSE)
    if(is.null(df)){
      return(NULL)
    } else {
      colnames(df)[1] = '#SampleID'
    }
    return(df)
  })
  
  # get standard curve values
  masked_wells_plate1 = reactive({
    gsub(' +', '', input$masked_wells_plate1) %>%
      strsplit(',') %>%
      unlist
  })
  masked_wells_plate2 = reactive({
    gsub(' +', '', input$masked_wells_plate2) %>%
      strsplit(',') %>%
      unlist
  })
  masked_wells_plate3 = reactive({
    gsub(' +', '', input$masked_wells_plate3) %>%
      strsplit(',') %>%
      unlist
  })
  
  std_curve = reactive({
    if(is.null(data_tbl())){
      return(NULL)
    }
    data_tbl() %>%
        filter(Name == 'Standard curve') %>%
        filter(!(Plate_ID == 'Plate1' & Well %in% masked_wells_plate1()),
               !(Plate_ID == 'Plate2' & Well %in% masked_wells_plate2()),
               !(Plate_ID == 'Plate3' & Well %in% masked_wells_plate3()))
  })
  

  
  # calculating concentrations
  data_tbl_conc = reactive({
    if(is.null(data_tbl()) | is.null(std_curve())){
      return(NULL)
    }
    df1 = data_tbl() %>% 
      filter(Plate_ID == 'Plate1') %>%
      calc_conc(std_curve() %>% 
                  filter(Plate_ID == 'Plate1') %>%
                  std_curve_lm, input$set_intercept_zero) %>%
      mutate(Conc_Dil = round(Conc_Dil %>% as.Num, 3))
    df2 = data_tbl() %>% 
      filter(Plate_ID == 'Plate2') %>%
      calc_conc(std_curve() %>% 
                  filter(Plate_ID == 'Plate2') %>%
                  std_curve_lm, input$set_intercept_zero) %>%
      mutate(Conc_Dil = round(Conc_Dil %>% as.Num, 3))
    df3 = data_tbl() %>% 
      filter(Plate_ID == 'Plate3') %>%
      calc_conc(std_curve() %>% 
                  filter(Plate_ID == 'Plate3') %>%
                  std_curve_lm, input$set_intercept_zero) %>%
      mutate(Conc_Dil = round(Conc_Dil %>% as.Num, 3))
    # combining tables
    df1 = rbind(df1, df2)
    df1 = rbind(df1, df3)
    # adding sample names
    if(!is.null(map_tbl())){
      df1 = add_sample_names(df1, map_tbl(),
                             sample_start = input$sample_start,
                             sample_end = input$sample_end)
    }
    return(df1)
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
    x_txt = quantile(df_std_curve$Def_conc, 0.63, na.rm=TRUE) 
    y_txt = quantile(df_std_curve$RFU, 0.99, na.rm=TRUE) 
    fit_plate1 = std_curve() %>% 
      filter(Plate_ID == 'Plate1') %>%
      std_curve_lm
    fit_plate2 = std_curve() %>% 
      filter(Plate_ID == 'Plate2') %>%
      std_curve_lm
    fit_plate3 = std_curve() %>% 
      filter(Plate_ID == 'Plate3') %>%
      std_curve_lm
    ## data.frame of lm equations
    df_fit = data.frame(
      Plate_ID = c('Plate1', 'Plate2', 'Plate3'),
      Fit = c(lm2str(fit_plate1, input$set_intercept_zero),
              lm2str(fit_plate2, input$set_intercept_zero),
              lm2str(fit_plate3, input$set_intercept_zero)),
      x = x_txt,
      y = y_txt
    )
    
    # plotting
    p = ggplot(df_std_curve, aes(Def_conc, RFU)) +
      geom_smooth(method=lm) +
      geom_point(aes(test=Well)) +
      geom_text(data=df_fit, aes(x=x, y=y, label=Fit)) +
      labs(x='Defined conc.', y='RFU') +
      facet_wrap(~ Plate_ID, ncol=2) +
      theme_bw() 
    ggplotly(p)
   })
  
  # Table of calculated concentrations
  output$conc_tbl = DT::renderDataTable(
    data_tbl_conc() %>%
      dplyr::select(-Mean, -Std_Dev, -CV, -Count),
    filter = 'bottom',
    extensions = c('Buttons'),
    options = list(
      pageLength = 40,
      dom = 'Blfrtip',
      buttons = c('colvis', 'copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  
  # rendering heatmap of concentrations
  output$conc_tbl_htmap = renderPlotly({
    # WARNING: can only handle 96-well source plate of samples!!!
    df = data_tbl_conc()
    df = df[1:96,]
    df$RowID = rep(rev(1:8), 12)[1:nrow(df)]
    df$ColID = sapply(1:12, function(x) rep(x, 8)) %>% 
      as.vector() %>% .[1:nrow(df)]
    df = df %>%
      dplyr::select(RowID, ColID, Conc_Dil) %>%
      mutate(Conc_Dil = Conc_Dil %>% as.character %>% as.numeric) %>%
      spread(ColID, Conc_Dil)
    rownames(df) = df$RowID
    df$RowID = NULL
    plot_ly(z = as.matrix(df), 
            x = gsub('^', 'C', 1:12), 
            y = rev(LETTERS[1:8]),
            type = "heatmap")
  })
  
  # rendering heatmap of RFU
  output$RFU_tbl_htmap = renderPlotly({
    # WARNING: can only handle 96-well source plate of samples!!!
    df = data_tbl_conc()
    df = df[1:96,]
    df$RowID = rep(rev(1:8), 12)[1:nrow(df)]
    df$ColID = sapply(1:12, function(x) rep(x, 8)) %>% 
      as.vector() %>% .[1:nrow(df)]
    df = df %>%
      dplyr::select(RowID, ColID, RFU) %>%
      mutate(RFU = RFU %>% as.character %>% as.numeric) %>%
      spread(ColID, RFU)
    rownames(df) = df$RowID
    df$RowID = NULL
    plot_ly(z = as.matrix(df), 
            x = gsub('^', 'C', 1:12), 
            y = rev(LETTERS[1:8]),
            type = "heatmap")
  })
  
  # Table formatted for dilution app
  output$conc_tbl_dil = DT::renderDataTable(
    conc_tbl_to_dilute(data_tbl_conc(), 
                       input$TECAN_labware_name,
                       input$TECAN_labware_type,
                       input$TECAN_target_position_start,
                       input$TECAN_target_position_end),
    filter = 'bottom',
    extensions = c('Buttons'),
    options = list(
      pageLength = 40,
      lengthMenu = c(40, 80, 120),
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
      pageLength = 96,
      lengthMenu = c(48, 96, 384, 1536),
      dom = 'Blfrtip',
      buttons = c('colvis', 'copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  
  # example data table
  output$example_data_tbl = DT::renderDataTable(
    load_ex_data_file(),
    extensions = c('Buttons'),
    options = list(
      pageLength = 96,
      lengthMenu = c(96, 384, 1536),
      dom = 'Blfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  
  # example mapping table
  output$example_map_tbl = DT::renderDataTable(
    load_ex_map_file(),
    extensions = c('Buttons'),
    options = list(
      pageLength = 96,
      lengthMenu = c(96, 384, 1536),
      dom = 'Blfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
})


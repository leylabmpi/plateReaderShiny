# Shiny server
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(plotly)


as.Num = function(x){
  x %>% as.character %>% as.numeric
}

load_excel= function(input){
  # loading excel file of plate reader output
  infile = input$data_file
  if(is.null(infile)){
    return(NULL)
  }
  file_ext = gsub('.+(\\.[^.])$', '\\1', infile$name) 
  new_file = paste0(infile$datapath, file_ext)
  file.rename(infile$datapath,
              new_file)
  df = read_excel(new_file, 
                  sheet=input$sheet_name)
  # formatting
  colnames(df) = gsub('[_/()% ]+', '_', colnames(df))
  colnames(df) = gsub('_+$', '', colnames(df))
  colnames(df)[5] = 'RFU'
  df = df %>%
    mutate(Mean = ifelse(grepl('^[?]+$', Mean), NA, Mean),
           Std_Dev = ifelse(grepl('^[?]+$', Std_Dev), NA, Std_Dev),
           Mean = Mean %>% as.Num,
           Std_Dev = Std_Dev %>% as.Num,
           RFU = RFU %>% as.Num,
           CV = CV %>% as.Num)
  return(df)
}

# formatting linear regression equation for plotting
equation = function(x) {
  a = round(coef(x)[1], digits = 2)
  b = round(coef(x)[2], digits = 2)
  r2 = round(summary(x)$r.squared, digits = 2)
  sprintf("y = %0.2fx +  %.2f, R^2 = %.2f", b, a, r2)
}

# calculating concentations based on linear regression of std curve
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

#' loading example file
load_ex_file = function(){
  df = read.table('../data/picogreen1.csv', sep=',', header=TRUE, check.names=FALSE)
  return(df)
}


#-- server --#
shinyServer(function(input, output, session) {

  # Load excel
  reac = reactive({
    # reading in excel
    ret = list()
    ret[['table']] = load_excel(input)
    if(is.null(ret$table)){
      return(NULL)
    }
    # getting standard curve
    ret[['std_curve']] = ret$table %>%
      filter(Name == 'Standard curve',
             !is.na(Count))
    # linear regression
    #ret$std_curve %>% filter(!is.na(Mean)) %>% print
    ret[['std_curve_lm']] = lm(Mean ~ Conc_Dil, 
                               data=ret$std_curve %>% filter(!is.na(Mean)))
    # calculating concentrations
    ret[['conc']] = calc_conc(ret$table, ret$std_curve_lm) %>%
      mutate(Conc_Dil = round(Conc_Dil %>% as.Num, 3))
    
    # return 
    return(ret)
  })
  
  # Table of raw data
  output$raw_tbl = DT::renderDataTable(
    reac()$table,
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
    reac()$std_curve,
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
    df_std_curve = reac()$std_curve
    if(is.null(df_std_curve)){
      return(NULL)
    }
    df_std_curve = df_std_curve %>%
      filter(!grepl('^[*].+[*]$', RFU),
             !is.na(RFU)) %>%
      rename('Mean_RFU' = Mean,
             'Def_conc' = Conc_Dil)
    
    # linear regression data
    x_txt = quantile(df_std_curve$Def_conc, 0.6, na.rm=TRUE) 
    y_txt = quantile(df_std_curve$Mean_RFU, 0.9, na.rm=TRUE) 
    fit = reac()$std_curve_lm
    
    # plotting
    p = ggplot(df_std_curve, aes(Def_conc, Mean_RFU)) +
      geom_linerange(aes(ymin=Mean_RFU-Std_Dev,
                         ymax=Mean_RFU+Std_Dev)) +
      geom_smooth(method=lm, se=FALSE) +
      geom_point() +
      annotate("text", x=x_txt, y=y_txt, 
               label=equation(fit), 
               parse=TRUE, size=3) +
      labs(x='Defined conc.', y='RFU') +
      theme_bw() 
    ggplotly(p)
   })
  
  # Table of calculated concentrations
  output$conc_tbl = DT::renderDataTable(
    reac()$conc,
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
    conc_tbl_to_dilute(reac()$conc),
    filter = 'bottom',
    extensions = c('Buttons'),
    options = list(
      pageLength = 40,
      dom = 'Blfrtip',
      buttons = c('colvis', 'copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  
  
  # example data table
  output$example_tbl = DT::renderDataTable(
    load_ex_file(),
    extensions = c('Buttons'),
    options = list(
      pageLength = 200,
      dom = 'Brt',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
})


# app.R

# CaEGTA vs BoNT


library(shiny)
library(ggplot2)
library(data.table)

js_dim <- 'var dimension = [0, 0];
           $(document).on("shiny:connected", function(e) {
           dimension[0] = window.innerWidth;
           dimension[1] = window.innerHeight;
           Shiny.onInputChange("dimension", dimension);
           });
           $(window).resize(function(e) {
           dimension[0] = window.innerWidth;
           dimension[1] = window.innerHeight;
           Shiny.onInputChange("dimension", dimension);
           });'

dotplot <- function(df){
  
  # Function produces a plot of normalized intensities
  # Normalized intensities are depicted as dots
  # and grouped based on Experiment and Treatment condition
  # Medians of each group are shown as vertical bars
  # Means of each group are shown as white triangles 
  # Means of each treatment condition that are compared within
  # single experiment are connected using straight lines
  #
  # Arguments:
  # df: a data.table that contains following columns:
  # Norm.intensity: numeric, normalized intesities
  # ExperimentID: character, unique experiment name
  # Condition: character, name of the treatment
  # Gene.name, Amino.acid, Position, Multiplicity,
  # Protein, Protein.description: character columns,
  # needed for annotation
  #
  # Value:
  # ggplot object
  
  #df <- df[!is.na(df$Norm.intensity)]
  
  if(nrow(df) == 0) return(ggplot())
  
  expid <- c("CaEGTA_01", "CaEGTA_02", "BoNT_01", "BoNT_02")
  missing <- expid[!expid %in% df$ExperimentID]
  
  if(length(missing) != 0){
    
    temp <- data.table()
    for(i in 1:(6*length(missing))){
      
      temp <- rbind(temp, df[1])
      
    }
    
    for(i in seq_along(missing)){
      
      if(grepl("CaEGTA", missing[i])){
        
        temp[(i-1)*6 + (1:3), Condition := "Ca"]
        temp[(i-1)*6 + (4:6), Condition := "EGTA"]
        
      } else {
        
        temp[(i-1)*6 + (1:3), Condition := "Mock"]
        temp[(i-1)*6 + (4:6), Condition := "BoNT"]
        
      }
      
    }
    temp[, ExperimentID := rep(missing, each = 6)]
    temp[, Norm.intensity := NA]
    temp[, q.val := NA]  
    temp[, log2FC := NA]
    df <- rbind(df, temp)
    
  }
  df <- df[, ExperimentID := factor(ExperimentID, levels = expid)]
  df <- df[, Condition := factor(Condition, levels = c("Ca", "EGTA", "Mock", "BoNT"))]
  
  means <- df[, list(Mean = mean(Norm.intensity)), by = c("ExperimentID", "Condition")]
  
  g <- ggplot(data = df, aes(y = Norm.intensity, x = ExperimentID, fill = Condition))
  g <- g + geom_segment(data = means,
                        xend = as.integer(means$ExperimentID) + ((as.integer(means$Condition) + 1)%%2) * 0.375,
                        x    = as.integer(means$ExperimentID) + ((as.integer(means$Condition) + 1)%%2) * 0.375 - 0.375,
                        y = means$Mean, yend = means$Mean)
  g <- g + geom_point(aes(color = Condition), size = 3, alpha = 0.8, position = position_jitterdodge(jitter.width = 0.2))
  g <- g + geom_line(data = means, aes(y = Mean, group = ExperimentID), position = position_dodge2(width = 0.75))
  g <- g + geom_point(data = means,
                      aes(y = Mean, group = ExperimentID), position = position_dodge2(width = 0.75),
                      shape = 25, fill = "white")
  g <- g + scale_x_discrete(labels = c("Experiment1\nCa / EGTA", "Experiment2\nCa / EGTA",
                                       "Experiment3\nMock / BoNT", "Experiment4\nMock / BoNT"))
  g <- g + ggtitle(paste0(df$Gene.name[1], ": ", df$Amino.acid[1], df$Position[1]))
  g <- g + annotate("text", y = max(df$Norm.intensity, na.rm = TRUE) * 0.95, x = 1.0, hjust = 0,
                    label = paste0("log2FC (Ca / EGTA) = ", round(df$log2FC[df$ExperimentID == "CaEGTA_01"][1], 2), ";    ",
                                   "q-Value = ", round(df$q.val[df$ExperimentID == "CaEGTA_01"][1], 3), "\n",
                                   "log2FC (Mock / BoNT) = ", round(df$log2FC[df$ExperimentID == "BoNT_01"][1], 2), ";    ",
                                   "q-Value = ", round(df$q.val[df$ExperimentID == "BoNT_01"][1], 3)
                    ))
  g <- g + ylab("Normalized Intensity") + xlab("")
  suppressWarnings(print(g))
  
}

##### Load data #####

# load Rdata 

invisible({
lapply(list.files(pattern = ".Rdata$", full.names = TRUE), function(x) {
  
  load(file = x, envir = globalenv())
  
  })
})

####### UI ###################################################################################


ui = fluidPage(
  
  tags$head(tags$script(js_dim)),
  
  sidebarLayout(
    
    sidebarPanel(width = 4,
                 
                 # FR
                 fluidRow(
                   
                   #Choose ProteinID from a list
                   selectInput(inputId = "gn", 
                               label = "Gene name:", 
                               choices = "Aak1",
                               selected = "Aak1")
                   
                 ),
                 
                 # FR
                 fluidRow(
                   
                   #Proteins with significantly regulated sites only
                   checkboxInput(inputId = "sign_only",
                                 label   = "Contain significant sites",
                                 value   = TRUE)
                   
                 ),
                 
                 # FR
                 fluidRow(
                   
                   # Protein Accession
                   radioButtons(inputId = "acc",
                                label = "Uniprot Accession:",
                                choices = "",
                                inline = TRUE,
                                selected = "")
                   
                 ),
                 
                 # FR
                 fluidRow(
                   
                   # Protein Function
                   tableOutput("prot_func")
                   
                 ),
                 
                 
                 # FR
                 fluidRow(
                   
                   #Choose Position from a list
                   selectInput(inputId = "pos", 
                               label = "Position:", 
                               choices = 0)
                   
                 ),
                 
                 # FR
                 fluidRow(
                   
                   plotOutput("dotplot")
                   
                 )
                 
                 
    ),
    
    mainPanel(
      
      # Line plot Phosphosites
      fluidRow(
        
        HTML('<h4 style="margin-bottom: -2em; position: relative; z-index: 500;"> Ca<sup>2+</sup> vs EGTA </h4>'),
        column(12, htmlOutput("lineplot_sites_CaEGTA", inline = TRUE))
        
      ),
      
      fluidRow(
        
        HTML('<h4 style="margin-bottom: -2em; position: relative; z-index: 500;"> Mock- vs BoNT-Treated </h4>'),
        column(7, htmlOutput("lineplot_sites_BoNT"))
        
      )
      
    )
    
  )       
)           


####### Server ##############################################################################

server = function(input, output, session) {
  
  empty_file_path <- file.path("Lineplots_CaEGTA/_Empty.svg")
  
  # observe protein id input
  rv <- reactiveValues()
  
  # observe changes in  browser dimensions
  observe({
    
    rv$window_h <- input$dimension[2]
    rv$window_w <- input$dimension[1]
    
  })
  
  # Checkbox
  # offer proteins with significant sites only?
  observe({
    
    rv$sign_only <- input$sign_only
    
    if(!rv$sign_only){
      
      updateSelectInput(session, "gn",
                        choices = genes_all,
                        selected = genes_all[2])
      
    } else {
      
      updateSelectInput(session, "gn",
                        choices = genes_reg,
                        selected = genes_reg[2])
      
    }
    
    
  })
  
  # Radio buttons
  # Accessions to offer
  observe({
    
    rv$gn_inp  <- stringr::str_split(input$gn, pattern = "[ \t;,]")[[1]]
    rv$acc     <- unique(gn_acc$Accession[gn_acc$Gene.name == rv$gn_inp])
    
    updateRadioButtons(session, "acc",
                       label = "Uniprot Accession:",
                       choices = rv$acc,
                       selected = rv$acc[[1]],
                       inline = TRUE)
    
  })
  
  # Position selection
  # Which positions to offer
  observe({
    
    rv$acc_sel <- input$acc
    updateSelectInput(session, "pos",
                      label = "Position:",
                      choices = unlist(positions$Position[positions$Accession == rv$acc_sel]))
    
  })
  
  
  # Render Dot Plot
  output$dotplot <- renderPlot(expr = {
    
    dotplot(dat_int[dat_int$Accession == rv$acc_sel &
                      dat_int$Position == input$pos, ]
    )
    
  })
  
  # Render Protein Annotation table
  output$prot_func <- renderTable({
    
    if(rv$acc_sel == "") {
      
      data.table()
      
    } else {
      
      prot_name <- gn_acc$Protein.description[gn_acc$Accession == rv$acc_sel][1]
      df        <- gn_acc[Accession == rv$acc_sel, c("Function")][1,]
      names(df) <- prot_name
      df
      
    }
    
    
  })
  
  output$lineplot_sites_CaEGTA <- renderUI(expr = {
    
    acc <- rv$acc_sel
    file_path <- file.path(paste0("Lineplots_CaEGTA/", acc, ".svg"))
    if(!file.exists(file_path)){
      
      file_path <- empty_file_path
      scaling_factor <- 0.7
      
    } else {
      
      scaling_factor <- 1
      
    } 
    
    html <- xml2::read_html(file_path)
    
    new_w = paste0(round(rv$window_w * (0.79375 / 3) * 8/12 * 1.02 * scaling_factor, 2), "mm")
    new_h = paste0(round(rv$window_h * (0.79375 / 3) * 0.48 * 1.02 * scaling_factor, 2), "mm")
    
    xml2::xml_set_attr(xml2::xml_find_all(html, ".//svg"), attr = "preserveAspectRatio", value = "xMinYMin meet")
    xml2::xml_set_attr(xml2::xml_find_all(html, ".//svg"), attr = "width", value = new_w)
    xml2::xml_set_attr(xml2::xml_find_all(html, ".//svg"), attr = "height", value = new_h)
    xml2::xml_set_attr(xml2::xml_find_all(html, ".//text"), attr = "text-rendering", value = "optimizeLegibility")
    xml2::xml_set_attr(xml2::xml_find_all(html, ".//svg"), attr = "shape-rendering", value = "geometricPrecision")
    
    HTML(as.character(html))
    
  })
  
  output$lineplot_sites_BoNT <- renderUI(expr = {
    
    acc <- rv$acc_sel
    file_path <- file.path(paste0("Lineplots_BoNT/", acc, ".svg"))
    if(!file.exists(file_path)){ 
      
      file_path <- empty_file_path
      scaling_factor <- 0.7
      
    }  else {
      
      scaling_factor <- 1
      
    }
    
    html <- xml2::read_html(file_path)
    
    
    new_w = paste0(round(rv$window_w * (0.79375 / 3) * 8/12 * 1.02 * scaling_factor, 2), "mm")
    new_h = paste0(round(rv$window_h * (0.79375 / 3) * 0.48 * 1.02 * scaling_factor, 2), "mm")
    
    xml2::xml_set_attr(xml2::xml_find_all(html, ".//svg"), attr = "preserveAspectRatio", value = "xMinYMin meet")
    xml2::xml_set_attr(xml2::xml_find_all(html, ".//svg"), attr = "width", value = new_w)
    xml2::xml_set_attr(xml2::xml_find_all(html, ".//svg"), attr = "height", value = new_h)
    xml2::xml_set_attr(xml2::xml_find_all(html, ".//text"), attr = "text-rendering", value = "optimizeLegibility")
    xml2::xml_set_attr(xml2::xml_find_all(html, ".//svg"), attr = "shape-rendering", value = "geometricPrecision")
    
    HTML(as.character(html))
    
  })
  
  
}

suppressWarnings(
  shinyApp(ui = ui, server = server)
)
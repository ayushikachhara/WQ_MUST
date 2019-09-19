# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {

  ## creating reactivevalues ####
  vals <- reactiveValues(p1=NULL,p2=NULL,p3 = NULL, p4 = NULL, t1=NULL)
  legendtable <- reactiveValues(dfcu = NULL,dfzn = NULL)
  ## once the actionbutton is clicked, change the window to the output tab automatically ####
  observeEvent(input$do, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "output")
  })
  
  ## text output summing up landuse to 100% ####
  output$total <- renderText({
    total.lu <- sum(c(input$reslowl,
                      input$resmedl,
                      input$reshighl,
                      input$commsbl,
                      input$commcbdl,
                      input$industryl,
                      input$highwayl,
                      input$pasturel,
                      input$exoticforestl,
                      input$nativeforestl,
                      input$horticulturel))
    return(total.lu)
    
  })
  
  # unless landuse is 100%, disable the calculate button ####
  observe({
    total.lu <- sum(c(input$reslowl,
                      input$resmedl,
                      input$reshighl,
                      input$commsbl,
                      input$commcbdl,
                      input$industryl,
                      input$highwayl,
                      input$pasturel,
                      input$exoticforestl,
                      input$nativeforestl,
                      input$horticulturel))
    if (total.lu != 100 | input$area == 0) {
      shinyjs::disable("do")
      h3("Land use must add up to 100 and Catchment area must be greater than 0")

    } else {
      shinyjs::enable("do")
      print(input$do)
    }
  })
  
  
  ### preparing input table for predictions script. ####
  tablefunc <- eventReactive(input$do, {
    
    withProgress(message = 'Preparing input tables',
                 detail = 'This may take a while...', value = 0, {
                   key <- c("ResidentialLow", "ResidentialMed",  "ResidentialHigh", "CommercialSub", 
                            "CommercialCBD",  "Industrial", "Highways",       
                            "Pasture", "ExoticForest",  "NativeForest", "Horticulture")
                   
                   lu.frac <- c(input$reslowl,
                                input$resmedl,
                                input$reshighl,
                                input$commsbl,
                                input$commcbdl,
                                input$industryl,
                                input$highwayl,
                                input$pasturel,
                                input$exoticforestl,
                                input$nativeforestl,
                                input$horticulturel)
                   table.landuse <- cbind.data.frame(Key = key, Area.frac = lu.frac*0.01) ## final output
                   incProgress(1/6)
                   sc.opt.msc <- c(input$reslowmlc,
                                   input$resmedmlc,
                                   input$reshighmlc,
                                   input$commsbmlc,
                                   input$commcbdmlc,
                                   input$industrymlc,
                                   input$highwaymlc,
                                   0,0,0,0)
                   sc.opt.msc <- ifelse(sc.opt.msc == 0, FALSE, TRUE)
                   incProgress(1/6)
                   
                   sc.opt.li <-  c(input$reslowmli,
                                   input$resmedmli,
                                   input$reshighmli,
                                   input$commsbmli,
                                   input$commcbdmli,
                                   input$industrymli,
                                   0,0,0,0,0)
                   sc.opt.li <- ifelse(sc.opt.li == 0, FALSE, TRUE)
                   table.scopt <-  cbind.data.frame(Key = key, MSC = sc.opt.msc, LI = sc.opt.li) ## final output
                   incProgress(1/6)
                   swt.opt.conv <- c(input$reslows,
                                     input$resmeds,
                                     input$reshighs,
                                     input$commsbs,
                                     input$commcbds,
                                     input$industrys,
                                     input$highways,
                                     0,0,0,0)
                   swt.opt.conv = as.numeric(swt.opt.conv)
                   swt.opt.conv <- ifelse(swt.opt.conv == 1, TRUE, FALSE)
                   incProgress(1/6)
                   
                   swt.opt.gi <-  c(input$reslows,
                                    input$resmeds,
                                    input$reshighs,
                                    input$commsbs,
                                    input$commcbds,
                                    input$industrys,
                                    input$highways,
                                    0,0,0,0)
                   swt.opt.gi = as.numeric(swt.opt.gi)
                   swt.opt.gi <- ifelse(swt.opt.gi == 2, TRUE, FALSE)
                   incProgress(1/6)
                   
                   table.swt.opt <-  cbind.data.frame(Key = key, Conv = swt.opt.conv, GI = swt.opt.gi) ## final output
                   
                   swt.frac <- c(input$reslowt,
                                 input$resmedt,
                                 input$reshight,
                                 input$commsbt,
                                 input$commcbdt,
                                 input$industryt,
                                 input$highwayt,
                                 0,0,0,0)
                   table.swt.frac <-  cbind.data.frame(Key = key, Treatment.frac = swt.frac*0.01) ## final output
                   incProgress(1/6)
                   
                   area <- as.data.table(input$area)
                   
                 })
    
    
    
    return(list(as.data.table(table.landuse), 
                as.data.table(table.scopt), 
                as.data.table(table.swt.opt), 
                as.data.table(table.swt.frac))) ## final output
  })

  ## run prediction function after getting userinputs ####
  predictionfunction <- eventReactive(input$do,{
    # start.time = Sys.time()
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.3, {
                   table.list <- tablefunc()
                   incProgress(0.4)
                   catch.area = input$area*10000
                   LU.breakdown = table.list[1]
                   SC.opts = table.list[2]
                   SWT.opts = table.list[3]
                   SWT.fracs = table.list[4]
                   final.vec <- PredictConcentrations(catch.area, LU.breakdown, 
                                                      SC.opts, SWT.opts, SWT.fracs)
                   incProgress(0.5)
                   
                 })
    ## get all user inputs in appropriate variable names ####
   
   
   return(final.vec)
   
  })
  
  ##tableOutput ####
  output$table1 <- renderTable({
    
    pred.concs <- predictionfunction()
    withProgress(message = 'Outputting table',
                 detail = 'This may take a while...', value = 0.5, {
                   cu.med <- round(median(pred.concs$Cu_Median, na.rm = T),1)
                   zn.med <- round(median(pred.concs$Zn_Median, na.rm = T),0)
                   cu.95 <- round(median(pred.concs$Cu_Pctile95, na.rm = T),1)
                   zn.95 <- round(median(pred.concs$Zn_Pctile95, na.rm = T),0)
                   df <- cbind.data.frame(Metal = c("Dissolved Copper",
                                                    "Dissolved Zinc"), Median = c(cu.med,zn.med), 
                                          `95th Percentile` = c(cu.95,zn.95))
                   
                   names(df) <- c("Metal", 
                                  paste0("Median (", "\u03BC", "g/L)"),
                                  paste0("`95th Percentile (", "\u03BC", "g/L)"))
                   incProgress(0.6)
                 })
    vals$t1 <- df
    df
  })
  
  ### Copper median ####
  output$plot1 <- renderPlotly({
   
                   pred.concs <- predictionfunction()
                   withProgress(message = 'Plot Output',
                                detail = 'This may take a while...', value = 0.6, {
                   pred.concs <- as.data.frame(pred.concs$Cu_Median)
                   colnames(pred.concs) <- "pred.concs"
                   
                   results.dist = ecdf(pred.concs$pred.concs)
                   DGVs = c(isolate(input$tcu1),isolate(input$tcu2),
                            isolate(input$tcu3), isolate(input$tcu4))
                   DGVs.yints = results.dist(DGVs)
                   input$changekey
                   legendKey <-  c(isolate(input$t1), isolate(input$t2), 
                                   isolate(input$t3), isolate(input$t4))
                   
                   p1 <- ggplot(pred.concs)  +
                     stat_ecdf(aes(sort(pred.concs))) +
                     geom_segment(aes(x = DGVs[1], y = 0, xend = DGVs[1], yend = DGVs.yints[1], 
                                      color = legendKey[1]), lty = 2) +
                     geom_segment(aes(x = 0, y = DGVs.yints[1], xend = DGVs[1], yend = DGVs.yints[1], 
                                      color = legendKey[1]), lty = 2) +
                     
                     geom_segment(aes(x = DGVs[2], y = 0, xend = DGVs[2], yend = DGVs.yints[2], 
                                      color = legendKey[2]), lty = 2) +
                     geom_segment(aes(x = 0, y = DGVs.yints[2], xend = DGVs[2], yend = DGVs.yints[2], 
                                      color = legendKey[2]), lty = 2) +
                     
                     geom_segment(aes(x = DGVs[3], y = 0, xend = DGVs[3], yend = DGVs.yints[3], 
                                      color = legendKey[3]), lty = 2) +
                     geom_segment(aes(x = 0, y = DGVs.yints[3], xend = DGVs[3], yend = DGVs.yints[3], 
                                      color = legendKey[3]), lty = 2) +
                     
                     geom_segment(aes(x = DGVs[4], y = 0, xend = DGVs[4], yend = DGVs.yints[4], 
                                      color = legendKey[4]), lty = 2) +
                     geom_segment(aes(x = 0, y = DGVs.yints[4], xend = DGVs[4], yend = DGVs.yints[4], 
                                      color = legendKey[4]), lty = 2) +
                     scale_colour_manual(name="", 
                                         values = c("yellow", "darkorange4", "red", "brown4")) +
                     scale_y_continuous(breaks = c(0,0.1,0.25,0.5,0.75,0.9,0.95,1),
                                        labels =c(0,10,25,50,75,90,95,100), expand =c(0,0)) +
                     scale_x_continuous(expand = c(0,0)) +
                     theme_bw()
                   
                   incProgress(0.7)
                 })
    
    vals$p1 <- p1 +  labs(y = "Probability of meeting threshold (%)", 
                          x=bquote("Predicted median Cu concentrations ("*mu*"g/L)"))
    legendtable$dfcu <- cbind.data.frame(Thresholds = legendKey,
                                         Copper = DGVs)
    p2 <- ggplotly(p1) %>% 
      layout(xaxis = list(title = "Predicted Median Cu Concentrations (&mu;g/L)"),
             yaxis = list(title = "Probability of meeting threshold (%)"),
             autosize = F) %>%
      style(hoverinfo = "x+y")
   
   
  })
 
  
  ## Zinc median ####
  output$plot2 <- renderPlotly({
  
    pred.concs <- predictionfunction()
    withProgress(message = 'Plot Output',
                 detail = 'This may take a while...', value = 0.7, {
                   pred.concs <- as.data.frame(pred.concs$Zn_Median)
                   colnames(pred.concs) <- "pred.concs"
                   results.dist = ecdf(pred.concs$pred.concs)
                   DGVs = c(isolate(input$tzn1),isolate(input$tzn2),
                            isolate(input$tzn3), isolate(input$tzn4))
                   DGVs.yints = results.dist(DGVs)
                   input$changekey
                   legendKey <-  c(isolate(input$t1), isolate(input$t2), 
                                   isolate(input$t3), isolate(input$t4))
                   p1 <- ggplot(pred.concs) +
                     stat_ecdf(aes(sort(pred.concs))) +
                     geom_segment(aes(x = DGVs[1], y = 0, xend = DGVs[1], yend = DGVs.yints[1], 
                                      color = legendKey[1]), lty = 2) +
                     geom_segment(aes(x = 0, y = DGVs.yints[1], xend = DGVs[1], yend = DGVs.yints[1], 
                                      color = legendKey[1]), lty = 2) +
                     
                     geom_segment(aes(x = DGVs[2], y = 0, xend = DGVs[2], yend = DGVs.yints[2], 
                                      color = legendKey[2]), lty = 2) +
                     geom_segment(aes(x = 0, y = DGVs.yints[2], xend = DGVs[2], yend = DGVs.yints[2], 
                                      color = legendKey[2]), lty = 2) +
                     
                     geom_segment(aes(x = DGVs[3], y = 0, xend = DGVs[3], yend = DGVs.yints[3], 
                                      color = legendKey[3]), lty = 2) +
                     geom_segment(aes(x = 0, y = DGVs.yints[3], xend = DGVs[3], yend = DGVs.yints[3], 
                                      color = legendKey[3]), lty = 2) +
                     
                     geom_segment(aes(x = DGVs[4], y = 0, xend = DGVs[4], yend = DGVs.yints[4], 
                                      color = legendKey[4]), lty = 2) +
                     geom_segment(aes(x = 0, y = DGVs.yints[4], xend = DGVs[4], yend = DGVs.yints[4], 
                                      color = legendKey[4]), lty = 2) +
                     scale_colour_manual(name="", 
                                         values = c("yellow", "darkorange4", "red", "brown4")) +
                     scale_y_continuous(breaks = c(0,0.1,0.25,0.5,0.75,0.9,0.95,1),
                                        labels =c(0,10,25,50,75,90,95,100), expand =c(0,0)) +
                     scale_x_continuous(expand = c(0,0)) +
                     theme_bw()
                   incProgress(0.8)
                 })
    
    
    vals$p2 <- p1 +  labs(y = "Probability of meeting threshold (%)", 
                          x=bquote("Predicted median Zn concentrations ("*mu*"g/L)"))
    legendtable$dfzn <- cbind.data.frame(Thresholds = legendKey,
                                         Zinc = DGVs)
    p2 <- ggplotly(p1) %>% layout(xaxis = list(title = "Predicted Median Zn Concentrations (&mu;g/L)"),
                                  yaxis = list(title = "Probability of meeting threshold (%)"),
                                  autosize = F) %>%
      style(hoverinfo = "x+y")
    
  })

  ### Copper 95th Percentile ####
  output$plot3 <- renderPlotly({
    pred.concs <- predictionfunction()
    withProgress(message = 'Plot Output',
                 detail = 'This may take a while...', value = 0.8, {
                   pred.concs <- as.data.frame(pred.concs$Cu_Pctile95)
                   colnames(pred.concs) <- "pred.concs"
                   
                   results.dist = ecdf(pred.concs$pred.concs)
                   DGVs = c(isolate(input$tcu1),isolate(input$tcu2),
                            isolate(input$tcu3), isolate(input$tcu4))
                   DGVs.yints = results.dist(DGVs)
                   input$changekey
                   legendKey <-  c(isolate(input$t1), isolate(input$t2), 
                                   isolate(input$t3), isolate(input$t4))
                   p1 <- ggplot(pred.concs) +
                     stat_ecdf(aes(sort(pred.concs))) +
                     geom_segment(aes(x = DGVs[1], y = 0, xend = DGVs[1], yend = DGVs.yints[1], 
                                      color = legendKey[1]), lty = 2) +
                     geom_segment(aes(x = 0, y = DGVs.yints[1], xend = DGVs[1], yend = DGVs.yints[1], 
                                      color = legendKey[1]), lty = 2) +
                     
                     geom_segment(aes(x = DGVs[2], y = 0, xend = DGVs[2], yend = DGVs.yints[2], 
                                      color = legendKey[2]), lty = 2) +
                     geom_segment(aes(x = 0, y = DGVs.yints[2], xend = DGVs[2], yend = DGVs.yints[2], 
                                      color = legendKey[2]), lty = 2) +
                     
                     geom_segment(aes(x = DGVs[3], y = 0, xend = DGVs[3], yend = DGVs.yints[3], 
                                      color = legendKey[3]), lty = 2) +
                     geom_segment(aes(x = 0, y = DGVs.yints[3], xend = DGVs[3], yend = DGVs.yints[3], 
                                      color = legendKey[3]), lty = 2) +
                     
                     geom_segment(aes(x = DGVs[4], y = 0, xend = DGVs[4], yend = DGVs.yints[4], 
                                      color = legendKey[4]), lty = 2) +
                     geom_segment(aes(x = 0, y = DGVs.yints[4], xend = DGVs[4], yend = DGVs.yints[4], 
                                      color = legendKey[4]), lty = 2) +
                     scale_colour_manual(name="", 
                                         values = c("yellow", "darkorange4", "red", "brown4")) +
                     scale_y_continuous(breaks = c(0,0.1,0.25,0.5,0.75,0.9,0.95,1),
                                        labels =c(0,10,25,50,75,90,95,100), expand =c(0,0)) +
                     scale_x_continuous(expand = c(0,0)) +
                     theme_bw()
                   incProgress(0.9)
                   
                 })
    
    
    vals$p3 <- p1 +  labs(y = "Probability of meeting threshold (%)", 
                          x=bquote("Predicted 95th percentile Cu concentrations ("*mu*"g/L)"))
    
    p2 <- ggplotly(p1) %>% layout(xaxis = list(title = "Predicted 95th percentile Cu Concentrations (&mu;g/L)"),
                                  yaxis = list(title = "Probability of meeting threshold (%)"),
                                  autosize = F) %>% style(hoverinfo = "x+y")
    
  })

  ### Zinc 95th percentile ####
  output$plot4 <- renderPlotly({
    pred.concs <- predictionfunction()
    withProgress(message = 'Plot Output',
                 detail = 'This may take a while...', value = 0.9, {
                   pred.concs <- as.data.frame(pred.concs$Zn_Pctile95)
                   colnames(pred.concs) <- "pred.concs"
                   
                   results.dist = ecdf(pred.concs$pred.concs)
                   DGVs = c(isolate(input$tzn1),isolate(input$tzn2),
                            isolate(input$tzn3), isolate(input$tzn4))
                   DGVs.yints = results.dist(DGVs)
                   input$changekey
                   legendKey <-  c(isolate(input$t1), isolate(input$t2), 
                                   isolate(input$t3), isolate(input$t4))
                   p1 <- ggplot(pred.concs) +
                     stat_ecdf(aes(sort(pred.concs))) +
                     geom_segment(aes(x = DGVs[1], y = 0, xend = DGVs[1], yend = DGVs.yints[1], 
                                      color = legendKey[1]), lty = 2) +
                     geom_segment(aes(x = 0, y = DGVs.yints[1], xend = DGVs[1], yend = DGVs.yints[1], 
                                      color = legendKey[1]), lty = 2) +
                     
                     geom_segment(aes(x = DGVs[2], y = 0, xend = DGVs[2], yend = DGVs.yints[2], 
                                      color = legendKey[2]), lty = 2) +
                     geom_segment(aes(x = 0, y = DGVs.yints[2], xend = DGVs[2], yend = DGVs.yints[2], 
                                      color = legendKey[2]), lty = 2) +
                     
                     geom_segment(aes(x = DGVs[3], y = 0, xend = DGVs[3], yend = DGVs.yints[3], 
                                      color = legendKey[3]), lty = 2) +
                     geom_segment(aes(x = 0, y = DGVs.yints[3], xend = DGVs[3], yend = DGVs.yints[3], 
                                      color = legendKey[3]), lty = 2) +
                     
                     geom_segment(aes(x = DGVs[4], y = 0, xend = DGVs[4], yend = DGVs.yints[4], 
                                      color = legendKey[4]), lty = 2) +
                     geom_segment(aes(x = 0, y = DGVs.yints[4], xend = DGVs[4], yend = DGVs.yints[4], 
                                      color = legendKey[4]), lty = 2) +
                     scale_colour_manual(name="", 
                                         values = c("yellow", "darkorange4", "red", "brown4")) +
                     scale_y_continuous(breaks = c(0,0.1,0.25,0.5,0.75,0.9,0.95,1),
                                        labels =c(0,10,25,50,75,90,95,100), expand =c(0,0)) +
                     scale_x_continuous(expand = c(0,0)) +
                     theme_bw()
                   Sys.sleep(3)
                   incProgress(1)
                 })
    
    
    vals$p4 <- p1 +  labs(y = "Probability of meeting threshold (%)", 
                          x=bquote("Predicted 95th percentile Zn concentrations ("*mu*"g/L)"))
    
    p2 <- ggplotly(p1) %>% layout(xaxis = list(title = "Predicted 95th percentile Zn Concentrations (&mu;g/L)"),
                                  yaxis = list(title = "Probability of meeting threshold (%)"),
                                  autosize = F) %>%
      style(hoverinfo = "x+y")
   
  })
  ##### final report generator #####
  output$report <- downloadHandler(
    filename = paste0(input$name, '_MUST_report.pdf'),
    content = function(file) {
      ## adding the logo at the bottom ####
      img <- readPNG(paste0(getwd(),"/www/niwa_logo.png"))
      letterhead_panel <- grobTree(rectGrob(gp=gpar(fill="steelblue", lwd=0)),
                                   textGrob(input$name,gp=gpar(fontsize=45,font=3)))
      bottom_panel <- rasterGrob(img, width = unit(3,"in"), height=unit(1.5,"in"))
      
      lay1 <- rbind(c(1,1),
                    c(2,3),
                   c(4,NA),
                   c(5,6),
                   c(7,8),
                   c(NA,9))
      ## all the text: 
      text1 <- textGrob(paste0("Estimates of in-stream dissolved copper and zinc concentrations are given in the table.",
                               "\nThis gives the 'expected value' (mid-point) concentration estimate."),
                        gp=gpar(fontsize=18), just = "left", x = unit(0,"npc"))
      
      ## table theme
      tt <- ttheme_default(base_size = 25, base_colour = "black",
                           colhead=list(fg_params = list(parse=TRUE)))
      tbl <- tableGrob(vals$t1, rows=NULL, theme=tt)
      
      ## displayed thresholds
      thresholds <- merge(legendtable$dfcu,legendtable$dfzn, by = "Thresholds")
      names(thresholds) <- c("Threshold", 
                       paste0("Copper (", "\u03BC", "g/L)"),
                       paste0("Zinc (", "\u03BC", "g/L)"))
      tbl2 <- tableGrob(thresholds, rows=NULL, theme=tt)
      
      ## final output
      pdf(file, width = 20, height = 18)
      
      grid.arrange(letterhead_panel,
                   tbl, text1, tbl2,
                   vals$p1,vals$p2,
                   vals$p3,vals$p4,
                   bottom_panel,
                   bottom = paste("MUST OUTPUT generated on ", Sys.Date()),
                   layout_matrix = lay1,
                   heights = c(1,2,2,2,2,0.5))
      dev.off()
    }
  )
  
  
}


source("loadlibraries.R")
source("loadfunctions.R")

# Define UI ----
ui <- navbarPage(
  theme = "bootstrap.min.css",
  title = h1("Metals in Urban Streams Tool (MUST)"),

  ####### about page ######
  tabPanel(
    h2(tags$b("About")), 
    h3("Overview:"),
    h4("MUST estimates concentrations of dissolved copper and zinc in an urban stream based on its 
       catchment land use and stormwater management characteristics."),
    br(),
    h4("Users are asked to enter details about the proportion of the catchment in each of a number 
       of broad land use classes. One or more generic stormwater source control and treatment options 
       can be selected. By varying inputs, users can re-run the tool to allow comparison of copper and 
       zinc concentration estimates under different scenarios."),
    br(),
    h4("MUST generates estimates of the median and 95th percentile concentrations of dissolved copper 
       and zinc and reports these relative to the respective Australian and New Zealand water quality 
       guideline values."),
    br(),
    h4("MUST estimates the probability that a concentration of dissolved copper or zinc meets a given 
       threshold, rather than generating a single number. This probabilistic approach reflects uncertainty 
       in the estimation of catchment loads and stream concentrations of copper and zinc that derives from:"), 
    br(),
    tags$ul(
      tags$li(h4("Variability in the proportion of contaminant sources (roofs, roads etc) in urban land use classes;")),
      
      tags$li(h4("Uncertainty in estimates of copper and zinc yields derived from sampling and modelling of stormwater 
                 runoff from urban contaminant sources;")),
      
      tags$li(h4("Variability in the performance of stormwater treatment devices; and")),
      
      tags$li(h4("Uncertainty in the relationship between modelled catchment loads 
                 of copper and zinc and measured in-stream concentrations of these metals."))
      
    ),
    
    br(),
    h4("While MUST has been developed from water quality monitoring data collected in Auckland, 
       Christchurch and Wellington, it can be used for screening-level assessments throughout New Zealand. 
       In particular, MUST is useful for investigating the relative difference between copper and zinc concentrations 
       that result from different land use and/or stormwater management configurations."),
    br(),
    h4("However, users are cautioned that MUST relies on relationships developed from data in catchments in which the 
       source of stream flow and contaminants is well-defined and well-matched. In strongly spring-fed streams these 
       relationships break down, with greater dilution occurring. MUST is likely to over-estimate concentrations of 
       dissolved copper and zinc in these situations."),
    br(),
    br(),
    hr(),
    h3("Suggested citation:"),
    div(h4(HTML(paste0("Gadd, J., Yalden, S., Moores, J., Semadeni-Davies, A. and Kachhara, A. (2019). 
       Metals in Urban Streams Tool (MUST): An interactive online tool for estimating concentrations of copper and 
       zinc in urban streams. NIWA, Auckland.", a(href="https://shiny.niwa.co.nz/MUST/", 
                                                  h4("https://shiny.niwa.co.nz/MUST/")))))),
    hr(),
    
    h3("Contact Us:"),
    div(h4(HTML(paste0('Please email us at ',a(href = 'mailto:stormwater@niwa.co.nz', 'stormwater@niwa.co.nz'),
                       ' for more information about MUST or to provide feedback. We welcome comments 
                       about how to make this webtool more useful.')))),
    br(),
    hr(),
    h3("Acknowledgements"),
    h4("This webtool was developed by NIWA’s Urban Aquatic Environments Group, with funding from NIWA's ‘Causes 
       and effects of water quality degradation’ Programme. Development of the tool relied on State of the 
       Environment river and stream water quality monitoring data collected by Auckland Council, Christchurch City 
       Council and Greater Wellington Regional Council."),
    br(),
    hr(),
    h3("Disclaimer"),
    h4("Whilst NIWA has used all reasonable endeavours to ensure that the information contained in this website is 
       accurate, NIWA does not give any express or implied warranty as to the accuracy of the information contained 
       herein. MUST is intended to be used for screening-level assessments and should not be used to replace 
       site-specific studies of river and stream water quality. This website has been reviewed internally by NIWA 
       and meets NIWA standards for website delivery."),
    br(),
    hr()
    ),
  
  ##################
  tabPanel(
    h3(tags$b("Tool")),
    tags$head(
      tags$style(
        HTML('#name{height: 30px}
             #area{height: 30px}
             #reslowl{height: 30px}
             #reslowl{font-size: 14px: 30px}
             #resmedl{height: 30px}
             #reshighl{height: 30px}
             #commsbl{height: 30px}
             #commcbdl{height: 30px}
             #industryl{height: 30px}
             #highwayl{height: 30px}
             #pasturel{height: 30px}
             #exoticforestl{height: 30px}
             #nativeforestl{height: 30px}
             #horticulturel{height: 30px}
             #reslowmlc{height: 30px}
             #resmedmlc{height: 30px}
             #reshighmlc{height: 30px}
             #commsbmlc{height: 30px}
             #commcbdmlc{height: 30px}
             #industrymlc{height: 30px}
             #highwaymlc{height: 30px}
             #reslowmli{height: 30px}
             #resmedmli{height: 30px}
             #reshighmli{height: 30px}
             #commsbmli{height: 30px}
             #commcbdmli{height: 30px}
             #industrymli{height: 30px}
             #reslows{height: 30px}
             #resmeds{height: 30px}
             #reshighs{height: 30px}
             #commsbs{height: 30px}
             #commcbds{height: 30px}
             #industrys{height: 30px}
             #highways{height: 30px}
             #reslowt{height: 30px}
             #resmedt{height: 30px}
             #reshight{height: 30px}
             #commsbt{height: 30px}
             #commcbdt{height: 30px}
             #industryt{height: 30px}
             #highwayt{height: 30px}
             #total{font-size: 16px}
             ')
      )),
  tags$head(tags$style(".row1{height:40px;}")),
  tags$style(type = "text/css", "label { font-size: 14px; }"),
  tags$style('.navbar-default {background-color: #75B5E5; color: #FF3368;'), 
  tags$style('.navbar-default .navbar-brand {                       
             color: #000204;}'),
  tags$style('.nav.navbar-nav.navbar-right li a {
    color: blue;}'),
  useShinyjs(),
        tabsetPanel(
          id = "tabs",
          tabPanel(title = h3("Inputs"), value = "input", id = "inTabset",
                   hr(),
                   fluidRow(column(4,
                                   tags$div(title ="Unique name identifying the location and scenario",
                                            textInput('name', h3("Catchment Name:"),value = "Catchment Scenario A"))),
                            column(4,
                                   tags$div(title = "Enter the total catchment or project area in hectares",
                                            numericInput("area",h3("Catchment Area (hectares)"), min = 0, max = 100, value = 0)))
                            ),
                   hr(),
                   #### column names ####
                   fluidRow(class = "row1",
                            br(),
                            column(3,
                                   tags$div(title = "Land Use categories",
                                            h3("Category"))),
                            column(1,
                                   tags$div(title = "Enter the proportion of the total area \nin each of the broad land use classes (must add up to 100)",
                                            h3("Land Use %"))),
                            column(3,
                                   tags$div(title = "select which (if any) of the following \ncontaminant source control and stormwater treatment options apply",
                                            h3("Contaminant source control"))),
                            column(3,
                                   tags$div(title = "Stormwater treatment (one or none can be selected)", 
                                            h3("Stormwater treatment"))),
                            column(1,
                                   tags$div(title = "If a stormwater treatment option is selected, \nspecify the proportion (%) of the area in each land use class \nwhich is treated (default 100%)",h3("% Area treated"))),
                            column(1, h4(""))
                            
                   ),
                   hr(),
                   
                   ### Residential - Low Density #####
                   fluidRow(class = "row1",
                            column(3,
                                   h4("Residential - Low Density")),
                            column(1,
                                   numericInput("reslowl", "", value = 0, max = 100)),
                            column(1,
                                   checkboxInput("reslowmlc", "Low Cu/Zn", value= F)),
                            column(2,
                                   checkboxInput("reslowmli", "Low Imperviousness", value= F)),
                            column(3,
                                   radioButtons("reslows", "", choices = list("Conventional" = 1,
                                                                              "Green Infrastructure" = 2,
                                                                              "None" = 3),
                                                selected = 3, inline = T)),
                            column(1,
                                   numericInput("reslowt", "", value = 100, max = 100)),
                            column(1, h4(""))
                   ),
                  
                   hr(),
                   ### Residential - Medium Density #####
                   fluidRow(class = "row1",
                            column(3,
                                   h4("Residential - Medium Density")),
                            column(1,
                                   numericInput("resmedl", "", value = 0, max = 100)),
                            column(1,
                                   checkboxInput("resmedmlc", "Low Cu/Zn", value= F)),
                            column(2,
                                   checkboxInput("resmedmli", "Low Imperviousness", value= F)),
                            column(3,
                                   radioButtons("resmeds", "", choices = list("Conventional" = 1,
                                                                              "Green Infrastructure" = 2,
                                                                              "None" = 3), 
                                                selected = 3, inline = T)),
                            column(1,
                                   numericInput("resmedt", "", value = 100, max = 100)),
                            column(1, h5(""))
                            
                   ),
                   hr(),
                   ### Residential - High Density ####
                   fluidRow(class = "row1",
                            column(3,
                                   h4("Residential - High Density")),
                            column(1,
                                   numericInput("reshighl", "", value = 0, max = 100)),
                            column(1,
                                   checkboxInput("reshighmlc", "Low Cu/Zn", value= F)),
                            column(2,
                                   checkboxInput("reshighmli", "Low Imperviousness", value= F)),
                            column(3,
                                   radioButtons("reshighs", "", choices = list("Conventional" = 1,
                                                                               "Green Infrastructure" = 2,
                                                                               "None" = 3), 
                                                selected = 3, inline = T)),
                            column(1,
                                   numericInput("reshight", "", value = 100, max = 100)),
                            column(1, h5(""))
                   ),
                   hr(),
                   ### Commercial - suburban ####
                   fluidRow(class = "row1",
                            column(3,
                                   h4("Commercial - Suburban")),
                            column(1,
                                   numericInput("commsbl", "", value = 0, max = 100)),
                            column(1,
                                   checkboxInput("commsbmlc", "Low Cu/Zn", value= F)),
                            column(2,
                                   checkboxInput("commsbmli", "Low Imperviousness", value= F)),
                            column(3,
                                   radioButtons("commsbs", "", choices = list("Conventional" = 1,
                                                                              "Green Infrastructure" = 2,
                                                                              "None" = 3), 
                                                selected = 3, inline = T)),
                            column(1,
                                   numericInput("commsbt", "", value = 100, max = 100)),
                            column(1, h5(""))
                   ),
                   hr(),
                   ### Commercial - CBD #####
                   fluidRow(class = "row1",
                            column(3,
                                   h4("Commercial - CBD")),
                            column(1,
                                   numericInput("commcbdl", "", value = 0, max = 100)),
                            column(1,
                                   checkboxInput("commcbdmlc", "Low Cu/Zn", value= F)),
                            column(2,
                                   checkboxInput("commcbdmli", "Low Imperviousness", value= F)),
                            column(3,
                                   radioButtons("commcbds", "", choices = list("Conventional" = 1,
                                                                               "Green Infrastructure" = 2,
                                                                               "None" = 3),
                                                selected = 3, inline = T)),
                            column(1,
                                   numericInput("commcbdt", "", value = 100, max = 100)),
                            column(1, h5(""))
                   ),
                   hr(),
                   ### Industrial ####
                   fluidRow(class = "row1",
                            column(3,
                                   h4("Industrial")),
                            column(1,
                                   numericInput("industryl", "", value = 0, max = 100)),
                            column(1,
                                   checkboxInput("industrymlc", "Low Cu/Zn", value= F)),
                            column(2,
                                   checkboxInput("industrymli", "Low Imperviousness", value= F)),
                            column(3,
                                   radioButtons("industrys", "", choices = list("Conventional" = 1,
                                                                                "Green Infrastructure" = 2,
                                                                                "None" = 3), 
                                                selected = 3, inline = T)),
                            column(1,
                                   numericInput("industryt", "", value = 100, max = 100)),
                            column(1, h5(" "))
                   ),
                   hr(),
                   ### Highways (motorways/major arterial roads) ####
                   fluidRow(class = "row1",
                            column(3,
                                   h4("Highways (motorways/major arterial roads)")),
                            column(1,
                                   numericInput("highwayl", "", value = 0, max = 100)),
                            column(1,
                                   checkboxInput("highwaymlc", "Low Cu/Zn", value= F)),
                            column(2, h5("")),
                            column(3,
                                   radioButtons("highways", "", choices = list("Conventional" = 1,
                                                                               "Green Infrastructure" = 2,
                                                                               "None" = 3),
                                                selected = 3, inline = T)),
                            column(1,
                                   numericInput("highwayt", "", value = 100, max = 100)),
                            column(1, h5(""))
                   ),
                   hr(),
                   ### Pasture ####
                   fluidRow(class = "row1",
                            column(3,
                                   h4("Pasture")),
                            column(1,
                                   numericInput("pasturel", "", value = 0, max = 100)),
                            column(8, h5(""))
                   ),
                   hr(),
                   ## Exotic Forest #####
                   fluidRow(class = "row1",
                            column(3,
                                   h4("Exotic Forest")),
                            column(1,
                                   numericInput("exoticforestl", "", value = 0, max = 100)),
                            column(8, h5(""))
                   ),
                   hr(),
                   #### Native Forest ####
                   fluidRow(class = "row1",
                            column(3,
                                   h4("Native Forest")),
                            column(1,
                                   numericInput("nativeforestl", "", value = 0, max = 100)),
                            column(8, h5(""))
                   ),
                   hr(),
                   ##### Horticulture ####
                   fluidRow(class = "row1",
                            column(3,
                                   h4("Horticulture")),
                            column(1,
                                   numericInput("horticulturel", "", value = 0, max = 100)),
                            column(8, h5("")),
                            br()
                   ),
                   hr(),
                   fluidRow(
                            column(3,
                                   h3(tags$b("Total"))),
                            column(1,
                                   verbatimTextOutput("total")),
                            column(8, h5(""))
                   ),
                   hr(),
                   ##### final action button ####
                   fluidRow(column(5,
                                   tags$div(title = "Land Use must sum up to 100 and catchment area must be greater than 0",
                                            disabled(
                                              actionButton("do",h3(tags$b("Calculate Cu & Zn concentrations")), 
                                                         width = "600px",class="btn btn-primary btn-lg btn-block"))
                                            )
                                   )),
                   fluidRow(),
                   
                   hr()), 
          tabPanel(title = h3("Outputs"), value = "output", id = "inTabset",
                   
                   br(),
                   tags$head(tags$style(type="text/css",
                                          paste0("
                                                 #loadmessage {
                                                 position: fixed;
                                                 top: 0px;
                                                 left: 0px;
                                                 width: 100%;
                                                 padding: 5px 0px 5px 0px;
                                                 text-align: center;
                                                 font-weight: bold;
                                                 font-size: 300%;
                                                 color: ", "black",";
                                                 background-color: ", "grey",";
                                                 z-index: 205;
                                                 }
                                                 "))),
                   conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                    tags$div("Please wait...",id="loadmessage")),
                   downloadButton("report",h2("Generate Report"),
                                  width = "600px",class="btn btn-primary btn-lg btn-block"),
                   hr(),
                   fluidRow(column(6, h3("Estimates of in-stream dissolved copper and zinc concentrations are given in the 
                                         table below. This gives the 'expected value' (mid-point) concentration estimate."),
                                   div(tableOutput("table1") %>% withSpinner(), style = "font-size:200%"))),
                                   hr(),
                   hr(),
                   fluidRow(column(6,
                                   h3("Comparison with water quality thresholds, guidelines or standards"),
                                   h4("The plots below show estimates of the probability that median and 95th percentile concentrations of dissolved 
                                      copper or zinc will exceed a given value. As a default, the plots are annotated with concentrations associated with 80%, 
                                      90%, 95% and 99% levels of protection given in the Australian and New Zealand Guidelines for Fresh and Marine Water Quality 
                                      (2018). Users are able to compare the concentration estimates with other water quality thresholds")
                                   )
                                   ),
                   hr(),
                   fluidRow(column(2,h3("ANZ Guidelines/ Thresholds")),
                            column(1,h3("Copper")),
                            column(1,h3("Zinc"))),
                   
                   fluidRow(column(2, textInput("t1","", "99% level of protection")),
                            column(1, numericInput("tcu1","", value = 1.0, step = 0.1)),
                            column(1, numericInput("tzn1","", value = 2.4, step = 1))),
                   
                   fluidRow(column(2,textInput("t2","", "95% level of protection")),
                            column(1, numericInput("tcu2","", value = 1.4, step = 0.1)),
                            column(1, numericInput("tzn2","", value = 8, step = 1))),
                   
                   fluidRow(column(2,textInput("t3","", "90% level of protection")),
                            column(1, numericInput("tcu3","", value = 1.8, step = 0.1)),
                            column(1, numericInput("tzn3","", value = 15, step = 1))),
                   
                   fluidRow(column(2,textInput("t4","", "80% level of protection")),
                            column(1, numericInput("tcu4","", value = 2.5, step = 0.1)),
                            column(1, numericInput("tzn4","", value = 31, step = 1)),
                            hr()),
                   fluidRow(column(1, actionButton("changekey","Update Legend", 
                                                   width = "600px",class="btn btn-primary btn-lg btn-block")),
                            hr()),
                                   
                   fluidRow(column(6,plotlyOutput("plot1",height = "500px", width = "580px")%>% withSpinner()),
                            column(6, plotlyOutput("plot2",height = "500px", width = "580px")%>% withSpinner())
                            ),
                   fluidRow(column(6, plotlyOutput("plot3",height = "500px", width = "580px")%>% withSpinner()),
                            column(6, plotlyOutput("plot4",height = "500px", width = "580px")%>% withSpinner())
                            ),
                   hr()
                   )
          )
        )
  )
  


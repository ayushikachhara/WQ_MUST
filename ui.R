source("loadlibraries.R")

# Define UI ----
ui <- navbarPage(
  ####### title, name and catchment area ######
  title = "Metals in Urban Stream Tool (MUST)",
  tabPanel(
    tags$b("Introduction"), 
    p("MUST estimates concentrations of dissolved copper and zinc in an urban stream 
      based on its catchment land use and stormwater management characteristics."),
    
    p("Users are asked to enter details about the proportion of the catchment in each 
      of a number of broad land use classes. One or more generic stormwater source control 
      and treatment options can be selected. By varying inputs, users can re-run the tool to 
      allow comparison of copper and zinc concentration estimates under different scenarios. "),
    
    p("MUST generates estimates of the median and 95th percentile concentrations of dissolved copper 
      and zinc and reports these relative to the respective Australian and New Zealand water quality 
      guideline values."),
    
    p("MUST estimates the probability that a concentration of dissolved copper or zinc exceeds a given 
      threshold, rather than generating a single number. This probabilistic approach reflects uncertainty 
      in the estimation of catchment loads and stream concentrations of copper and zinc that derives from:"), 
    tags$ul(
      tags$li("Variability in the proportion of contaminant sources (roofs, roads etc) in urban land use classes;"),
      
      tags$li("Uncertainty in estimates of copper and zinc yields derived from sampling and modelling of stormwater 
      runoff from urban contaminant sources;"),
      
      tags$li("Variability in the performance of stormwater treatment devices; and"),
      
      tags$li("Uncertainty in the relationship between modelled catchment loads of copper and zinc and measured 
      in-stream concentrations of these metals.")
      
    ),
    
    br(),
    p("While MUST has been developed from water quality monitoring data collected in Auckland, Christchurch 
      and Wellington, it can be used for screening-level assessments throughout New Zealand. In particular, 
      MUST is useful for investigating the relative difference between copper and zinc concentrations that 
      result from different land use and/or stormwater management configurations."),
    
    p("However, users are cautioned that MUST relies on relationships developed from data in catchments in 
      which the source of stream flow and contaminants is well-defined and well-matched. In strongly spring-fed 
      streams these relationships break down, with greater dilution occurring. MUST is likely to over-estimate 
      concentrations of dissolved copper and zinc in these situations."),
    
    a(href="https://www.niwa.co.nz/", tags$b(h3("Find out more"))),
    hr()
    ),
  
  tabPanel(
    tags$b("Input Options"),
    fluidRow(column(8,
                    h2("Instructions:"),
                    tags$ol(
                      tags$li("Enter a unique name identifying the location and scenario to be 
                              investigated, for instance “Catchment Name, Scenario A”"),
                      tags$li("Enter the total catchment or project area in hectares, then enter the proportion (%) 
                              of the total area in each of the broad land use classes listed below. These must add up to 100%."),
                      tags$li("For each urban land use class, select which (if any) of the following contaminant source control 
                              and stormwater treatment options apply:"),
                      tags$ul(tags$li("Contaminant source control (both, one or none can be selected)"),
                              tags$ul(tags$li("Low copper- and zinc-yielding materials. The concentration of copper
                                              in vehicle brake pads is reduced by 90%. Unpainted and poorly-painted 
                                              galvanised steel roofs are replaced with coated steel."),
                                      tags$li("Low imperviousness. The total area of roofs, paved areas and minor roads 
                                              is 20% less than in conventional urban development. Not available for highways")),
                              tags$li("Stormwater treatment (one or none can be selected)"),
                              tags$ul(tags$li("Conventional treatment methods, typically stormwater ponds constructed at a catchment outlet."),
                                      tags$li("	Green Infrastructure (GI), using distributed treatment trains of devices such
                                              as vegetated swales, bioretention (rain gardens) and wetlands. "))),
                      tags$li("If a stormwater treatment option is selected, specify the proportion (%) of the area in each land 
                              use class which is treated (default 100%)"),
                      tags$li("Click on “Calculate Cu and Zn concentrations”.")
                    )
                  
    )),
    
    hr(),
    
    fluidRow(column(8,
                    textInput("name", h3("Name"), 
                              value = "Catchment Name, Scenario A"),
                    numericInput("area", "Catchment Area (ha)", value = 100)
    )),
    
    hr(),
    
    fluidRow(column(2,
                    h4("")),
             column(2,
                    h4("Category")),
             column(2,
                    h4(" Land Use %")),
             column(2,
                    h4("Contaminant source control")),
             column(2,
                    h4("Stormwater treatment")),
             column(2,
                    h4("% stormwater treated"))
    ),
    hr(),
    
    ### Residential - Low Density #####
    fluidRow(column(2,
                    h5("")),
             column(2,
                    h5("Residential - low density")),
             column(2,
                    numericInput("reslowl", "", value = 50, max = 100)),
             column(2,
                    checkboxGroupInput("reslowc", "", choices = list("Low Cu/Zn" = 1,
                                                                     "Low imperv" = 2), selected = 1, inline = T)),
             column(2,
                    radioButtons("reslows", "", choices = list("Conv." = 1,
                                                               "GI" = 2), selected = 1, inline = T)),
             column(2,
                    numericInput("reslowt", "", value = 50, max = 100))
    ),
    ### Residential - Medium Density #####
    fluidRow(column(2,
                    h5("")),
             column(2,
                    h5("Residential - Medium Density")),
             column(2,
                    numericInput("resmedl", "", value = 50, max = 100)),
             column(2,
                    checkboxGroupInput("resmedc", "", choices = list("Low Cu/Zn" = 1,
                                                                     "Low imperv" = 2), selected = 1, inline = T)),
             column(2,
                    radioButtons("resmeds", "", choices = list("Conv." = 1,
                                                               "GI" = 2), selected = 1, inline = T)),
             column(2,
                    numericInput("resmedt", "", value = 50, max = 100))
    ),
    
    ### Residential - High Density ####
    fluidRow(column(2,
                    h5("")),
             column(2,
                    h5("Residential - High Density")),
             column(2,
                    numericInput("reshighl", "", value = 50, max = 100)),
             column(2,
                    checkboxGroupInput("reshighc", "", choices = list("Low Cu/Zn" = 1,
                                                                      "Low imperv" = 2), selected = 1, inline = T)),
             column(2,
                    radioButtons("reshighs", "", choices = list("Conv." = 1,
                                                                "GI" = 2), selected = 1, inline = T)),
             column(2,
                    numericInput("reshight", "", value = 50, max = 100))
    ),
    
    ### Commercial - suburban ####
    fluidRow(column(2,
                    h5("")),
             column(2,
                    h5("Commercial - suburban")),
             column(2,
                    numericInput("commsbl", "", value = 50, max = 100)),
             column(2,
                    checkboxGroupInput("commsbc", "", choices = list("Low Cu/Zn" = 1,
                                                                     "Low imperv" = 2), selected = 1, inline = T)),
             column(2,
                    radioButtons("commsbs", "", choices = list("Conv." = 1,
                                                               "GI" = 2), selected = 1, inline = T)),
             column(2,
                    numericInput("commsbt", "", value = 50, max = 100))
    ),
    
    ### Commercial - CBD #####
    fluidRow(column(2,
                    h5("")),
             column(2,
                    h5("Commercial - CBD")),
             column(2,
                    numericInput("commcbdl", "", value = 50, max = 100)),
             column(2,
                    checkboxGroupInput("commcbdc", "", choices = list("Low Cu/Zn" = 1,
                                                                      "Low imperv" = 2), selected = 1, inline = T)),
             column(2,
                    radioButtons("commcbds", "", choices = list("Conv." = 1,
                                                                "GI" = 2), selected = 1, inline = T)),
             column(2,
                    numericInput("commcbdt", "", value = 50, max = 100))
    ),
    
    ### Industrial ####
    fluidRow(column(2,
                    h5("")),
             column(2,
                    h5("Industrial")),
             column(2,
                    numericInput("industryl", "", value = 50, max = 100)),
             column(2,
                    checkboxGroupInput("industryc", "", choices = list("Low Cu/Zn" = 1,
                                                                       "Low imperv" = 2), selected = 1, inline = T)),
             column(2,
                    radioButtons("industrys", "", choices = list("Conv." = 1,
                                                                 "GI" = 2), selected = 1, inline = T)),
             column(2,
                    numericInput("industryt", "", value = 50, max = 100))
    ),
    
    ### Highways (motorways/major arterial roads) ####
    fluidRow(column(2,
                    h5("")),
             column(2,
                    h5("Highways (motorways/major arterial roads)")),
             column(2,
                    numericInput("highwayl", "", value = 50, max = 100)),
             column(2,
                    checkboxGroupInput("highwayc", "", choices = list("Low Cu/Zn" = 1), selected = 1, inline = T)),
             column(2,
                    radioButtons("highways", "", choices = list("Conv." = 1,
                                                                "GI" = 2), selected = 1, inline = T)),
             column(2,
                    numericInput("highwayt", "", value = 50, max = 100))
    ),
    
    ## Exotic Forest #####
    fluidRow(column(2,
                    h5("")),
             column(2,
                    h5("Exotic Forest")),
             column(2,
                    numericInput("exoticforestl", "", value = 50, max = 100))
    ),
    #### Native Forest ####
    fluidRow(column(2,
                    h5("")),
             column(2,
                    h5("Native Forest")),
             column(2,
                    numericInput("nativeforestl", "", value = 50, max = 100))
    ),
    
    ##### Horticulture ####
    fluidRow(column(2,
                    h5("")),
             column(2,
                    h5("Horticulture")),
             column(2,
                    numericInput("horticulturel", "", value = 50, max = 100))
    ),
    hr(),
    ##### final action button ####
    actionButton("do",h3(tags$b("Calculate Cu & Zn concentrations")), 
                 width = "600px"),
   
    hr()
  ),
  
  
  tabPanel(
    tags$b("Output")
  ), theme = shinytheme("sandstone")
    )


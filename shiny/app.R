### TO DO LIST:
### Stand alone application 
### Report output


library(shiny)
library(shinyjs)
library(shinyMatrix)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(orca)
library(leaflet)
library(sf)
library(jsonlite)
library(rjson)
library(terra)
library(tidyterra)
library(stringi)
library(nimble)
library(latticeExtra)
#library(popbio)

## important for spatial visualization
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1Ijoic2hpbnlkZWVydGltZSIsImEiOiJjbTAxOW1pdGQwMDJ3MnJxM3FodTA3cWp1In0.McVBwxd4mZNcjPNvD62Nhw')


## Some color stuff
myCSS <- "
/* Main header */
.skin-blue .main-header .navbar {
  background-color: #192841; /* Color for the top header line */
}

/* Main header (Title) */
.skin-blue .main-header .logo {
  background-color: #140000; /* Color for the title */
  font-weight: bold;
  font-size: 24px;
}

/* Menu header when collapsed */
.skin-blue .main-sidebar, .left-side {
  background-color: #192841; /* Color for the sidebar */
}

/* Side panel where tabs are located */
.skin-blue .sidebar-menu > li.active > a {
  border-left-color: #353839; /* Indigo color for the active tab */
}

.skin-blue .sidebar-menu > li > a {
  color: white; /* White text color for better readability */
}

/* Boxes around sections with rounded corners and a light gray background */
.box-section {
  border: 1px solid #ddd; /* Light gray border */
  border-radius: 8px; /* Rounded corners */
  padding: 10px; /* Padding inside the box */
  margin-bottom: 15px; /* Space between sections */
  background-color: #f9f9f9; /* Light gray background */
}

/* Vertical line between columns */
.vertical-line {
  border-top: 2px solid #ccc; /* Solid line on the top of a column */
  padding-top: 20px; /* Space under the line */
}

/* Making progress bar larger */
.shiny-notification {
              height: 100px;
              width: 500px;
              position:fixed;
              top: calc(50% - 50px);;
              left: calc(50% - 200px);;
            }

/* Custom CSS to adjust the text size and alignment for menu items */
      .treeview-menu > .conditional-item > a {
        font-size: 50px; /* Adjust the font size as needed */
        text-align: center; /* Center align the text */
        line-height: 40px; /* Adjust line height to vertically center the text */
      }
      
/* Custom CSS to hide the arrows */
 .treeview > a > .fa-angle-left {
  display: none !important;
"

## Grab functions from other script
source('cwd_mods.R')
scenarios <- read.csv('Scenarios.csv')
cell_pts <- xyFromCell(dem, 1:prod(dim(dem)[1:2]))
N0_dd_s1     <<- dget('n0_AR2.txt')
studyarea <- vect('study_area.shp')
crs(studyarea) <- 'EPSG:26915'
#studyarea <- crop(studyarea, ext(473665.5541, 533665.5541, 3972203.5355, 4000203.5355))
#studyarea <- st_as_sf(studyarea)
#studyarea$Area <- c('Erbie', 'Gene Rush', 'Tyler Bend')
counties <- readRDS('small_AR_counties.rds')
counties$Area <- counties$NAMELSAD
#new_outlines <- st_union(counties, studyarea)
zone2 <- readRDS('zone2.RDS')
counties <- crop(as_spatvector(counties), zone2)
# Define user side
ui <- dashboardPage(
  dashboardHeader(
    title = 'CWD Simulator'
  ),
  dashboardSidebar( collapsed = TRUE,
    sidebarMenuOutput("sidebarmenu")
  ),
  dashboardBody(
    useShinyjs(),  
    tags$head(tags$style(HTML(myCSS))),
    tags$head(tags$style(HTML('
      .main-header .sidebar-toggle:before {
        content: "\\Menu";}'))),
    tabItems(
      #### Stochastic spatial ####
      tabItem(tabName = "DD_S",
              fluidPage(
                # Define the checkbox
                checkboxInput('show_inputs', label = 'Show additional inputs', value = FALSE),
                fluidRow(
                    # column(3,
                    #        div(class = "box-section",
                    #            h3("Initial Population", style = "margin-top: -5px;"),
                    #            matrixInput("N0_dd_s",
                    #                        value = init_pop,
                    #                        row = list(names = TRUE),
                    #                        cols = list(names = TRUE),
                    #                        class = 'numeric') #end matrix
                    #        )), #end div and column
                    # column(3,
                    #        div(class = "box-section",
                    #            numericInput("q_dd_s", label = "Fawn sex ratio (Proportion Male)", value = 0.5, min = 0, max = 1, step = .05)
                    #        )),#end div and column
                    column(3,
                           div(class = "box-section",
                               numericInput("nocc_dd_s", label = "Years (max 10)", value = 5, min = 2, max = 10, step = 1)
                           )),
                    column(2,
                           div(class = "box-section",
                               numericInput("BL_dd_s", label = "Antlered Bag Limit (0 - 3)", 
                                            value = 2, min = 0, max = 3, step = 1)
                                                          )),
                    column(2,
                           div(class = "box-section",
                               numericInput("DL_dd_s", label = "Non-antlered Bag Limit (0 - 4)", 
                                            value = 3, min = 0, max = 4, step = 1)
                           )),
                    column(2,
                           div(class = "box-section",
                               checkboxInput('APR_dd_s', label = 'APR in Effect?', value = FALSE)
                           )),
                    column(2, 
                           div(class = 'box-section',
                               actionButton("do", "Go!", class = "btn-success")
                               )
                           ),
                    column(2, div(class = 'box-section',
                                  downloadButton("report", "Generate report")))
                           
                    ), #end row
                fluidRow(
                   column(3,
                          conditionalPanel(
                            condition = "input.show_inputs == true",
                  #          div(class = "box-section",
                  #              numericInput("eta_dd_s", label = "Intercept for infection (hazard scale)", value = -5, step = .01, min = 0)
                  #          ),
                  #          div(class = "box-section",
                  #              numericInput("eta2_dd_s", label = "Effect of contact rates on infection (hazard scale)", value = .04, step = .01, min =0)
                  #          ),
                           div(class = "box-section",
                               checkboxInput('posfawns_dd_s', label = 'CWD+ moms produce CWD+ fawns', value = FALSE)
                           )
                         ) # end conditionalPanel
                  ),
                  column(6,
                         conditionalPanel(
                           condition = "input.show_inputs == true",
                           div(class = "box-section",
                               matrixInput("Survival",
                                           label = "Annual survival",
                                           value = init_phi_a,
                                           row = list(names = TRUE),
                                           cols = list(names = TRUE),
                                           class = 'numeric')
                           )
                         ) # end conditionalPanel
                  ),
                  column(6,
                         conditionalPanel(
                           condition = "input.show_inputs == true",
                           div(class = "box-section",
                               matrixInput("sigmas_dd_s",
                                           label = "Home range parameter sigma (movement)",
                                           value = init_sigma_a,
                                           row = list(names = TRUE),
                                           cols = list(names = TRUE),
                                           class = 'numeric')
                         ) # end div
                  ) #end panel
                )
              ), # end fluidRow

              fluidRow(
                column(4, class = 'vertical-line',
                       div(class = "box-section",
                           plotlyOutput('totpop_dd_s'))),
                column(4, class = 'vertical-line',
                       div(class = "box-section",
                           plotlyOutput('prev_dd_s'))),
                column(4, class = 'vertical-line',
                       div(class = "box-section",
                           plotlyOutput('agepop_dd_s')))
              ), #end row
              fluidRow(
                column(4, class = 'vertical-line',
                       div(class = "box-section",
                           plotlyOutput('surv_dd_s'))),
                column(4, class = 'vertical-line',
                       div(class = "box-section",
                           plotlyOutput('sexratio_dd_s'))),
                column(4, class = 'vertical-line',
                        div(class = "box-section",
                            plotlyOutput('prev_lam'))),
              ), #end Row 
                fluidRow(
                column(5, class = 'vertical-line',
                       div(class = "box-section",
                           #plotlyOutput(outputId = "Raster"))),
                           plotOutput(outputId = 'Raster'))),
                column(3,
                       uiOutput("yearslide")),
                column(4, class = 'vertical-line',
                       div(class = 'box-section',
                           plotlyOutput('lambda_dd_s')))
                
               ), #end row
              fluidRow(
                column(5, class = 'vertical-line',
                       div(class = "box-section",
                           plotOutput(outputId = 'PrevRast'))),
                column(3,
                       uiOutput("yearslide2")),
                column(4, class = 'vertical-line',
                       div(class = 'box-section',
                           plotlyOutput('agedetail_dd_s')))
                
              )
              #  fluidRow(
              #    column(5, class = 'vertical-line',
              #           div(class = "box-section",
              #               plotlyOutput('multi_sim_s'))),
              #    column(3, class = 'vertical-line',
              #           div(class = "box-section",
              #               h4("Simulate this model 10 more times (slow)", style = "margin-top: -5px;"),
              #               actionButton("goButton", "Go!", class = "btn-success")))
              #  ) #end row
              )#end fluidPage
      ) #end fancy spatial model
      ))
) #end UI
  
#### Define server logic ####
server <- function(input, output, session) {
  output$sidebarmenu <- renderMenu({
    sidebarMenu(
      #menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Spatial Model", tabName = "DD_S", icon = icon("arrow-up-right-dots"))
    )})
                       

  #### Spatial stochastic w/ two sex and DD in transmission ####
  observeEvent(input$do, {
    req(input$nocc_dd_s >= 2); req(input$nocc_dd_s <= 10); req(all(!is.null(input$Survival))); req(all(!is.null(input$sigmas_dd_s))); req(input$BL_dd_s >= 0 & input$BL_dd_s <= 3); req(input$DL_dd_s >= 0 & input$DL_dd_s <=4)#; req(input$BL_dd_s ==2)
    
    q_dd_s1      <<- .5
    nocc_dd_s1   <<- input$nocc_dd_s
    posfawns_dd_s1 <<- input$posfawns_dd_s
    sigs_dd_s1 <<- input$sigmas_dd_s
    phi_dd_s1 <<- input$Survival
    BL_dd_s1 <<- input$BL_dd_s
    DL_dd_s1 <<- input$DL_dd_s
    APR_dd_s1 <<- input$APR_dd_s*1
    pixArea <<-  6.178
    
    Myinputs_r <<- reactive({
      paste0("Occasions Selected: ", input$nocc_dd_s, "  ", "Buck Limit Selected: " , input$BL_dd_s, "  ", 
             "Doe Limit Selected: ", input$DL_dd_s, "  ", "APR in Effect? ", input$APR_dd_s, "  ")
    })

  req(input$nocc_dd_s)

  withProgress(message = 'Calculating', value = 0, {
    # Update progress every 10% during computation
    for (i in seq(1, 5)) {
      incProgress(0.1, detail = paste("..."))
      Sys.sleep(0.10)  # Simulate some processing time in each step (optional)
    }
    
  stoch_dd_s <<- I_2sex(n0 = N0_dd_s1, #initial population (counts)
                         eta = -4.4, 
                         eta2 = 0,
                         nocc = nocc_dd_s1, #time steps,
                         nPixels = nPix, #total pixels
                         gridDis = gridDis, #distance between pixels
                         pixArea = 6.178, #sq miles per pixel
                         sigmas= c(t(sigs_dd_s1)),
                         phis = c(t(phi_dd_s1)),
                         s.r = q_dd_s1, #sex ratio for fawns
                         posfawns = posfawns_dd_s1,
                         quickrun =  FALSE,
                        scenarios = scenarios,
                        APR = APR_dd_s1,
                        DoeLim = DL_dd_s1,
                        BuckLim = BL_dd_s1,
                        grid0 = grid0)
  })
  maxDens <<- max(stoch_dd_s$Nq, na.rm = T)/6.178 + .1
  print(maxDens)
  #print(mean(stoch_dd_s$eta_d[,,1]))
  #print(mean(stoch_dd_s$eta_d[,,14]))
  print(rowMeans(stoch_dd_s$Pq/(stoch_dd_s$Nq+.00001)))
  popcrash <<- ifelse(all(stoch_dd_s$N >0), nocc_dd_s1, min(which(stoch_dd_s$N == 0))-1)
  
  #print(stoch_dd_s$Ratios)
  #dput(stoch_dd_s, "sim_cwd.txt")

  TotalPopulation <<- reactive({
    stoch_dd_s$Totpop
  })
    # #Graph of total, total pos and total neg
    TotPopGraph <<- reactive({
     pppp <-  plot_ly(stoch_dd_s$Totpop,
              x = ~t,
              y = ~N,
              color = ~group,
              type = 'scatter',
              mode = 'lines',
              colors = c('CWD Positive' = '#C30013', 'All' = '#528148','CWD Negative' = '#79A7D8')) %>%
        layout(title = "Total Population",
               xaxis = list(title = 'Year'),
               yaxis = list(title = 'Density per Square Mile'),
               legend = list(x =.5, y=.9))
      if (min(which(stoch_dd_s$N == 0)) <= nocc_dd_s1) {
          # Add a dotted line when the population dies out
          #pppp <- pppp 
          pppp <-   pppp %>% layout(
            shapes=list(type='line', x0=popcrash, x1=popcrash, y0=0, y1=max(stoch_dd_s$N), line = list(dash = 'dash'))
          )
      }
     pppp
    })
    
    output$totpop_dd_s <- renderPlotly({
      req(TotPopGraph())
      TotPopGraph()
    })
    
    # #Graph of prevalence 
    #Average across entire area is in stoch_dd_s$Ratios. Specific pixel level is in stoch_dd_s$PixPrevs 
    prevs1 <<- reactive({stoch_dd_s$Ratios[stoch_dd_s$Ratios$group != 'Adult Sex Ratio',]})
    prevs2 <<- reactive({stoch_dd_s$PixPrevs})
    
    prev_dd_graph <<- reactive({
      plotme <- stoch_dd_s$Ratios[stoch_dd_s$Ratios$group != 'Adult Sex Ratio',]
      plotme2 <- stoch_dd_s$PixPrevs
      plotme2$group <- 'Pixel Level Prevalence'
      plot_ly(plotme,
              x = ~t,
              y = ~Prop,
              type = 'scatter',
              mode = 'lines',
              color = ~group, 
              colors = c('pink', 'red'),
              name = ~group) %>%
        add_trace(data = plotme2 ,
                  x = ~t,
                  y = ~Prop,
                  type = 'scatter',
                  mode = 'markers',
                  name = 'Pixel-level Prevalence',
                  marker = list(color = 'orange',
                                size = 3,
                                opacity = .05)) %>%
        layout(title = "Average CWD Prevalence",
               xaxis = list(title = 'Year'),
               yaxis = list(title = 'Prevalence'),
               legend = list(x =.5, y=.94))
    })
    output$prev_dd_s <- renderPlotly({
      req(prev_dd_graph())
      prev_dd_graph()
    })

    # #Graph of sex ratio 
    output$sexratio_dd_s <- renderPlotly({
      plot_ly(stoch_dd_s$Ratios[stoch_dd_s$Ratios$group == 'Adult Sex Ratio',],
              x = ~t,
              y = ~Prop,
              type = 'scatter',
              mode = 'lines',
              color = ~ group,
              colors = '#5B497A') %>%
        layout(title = "Adult Sex Ratio (Proportion Female)",
               xaxis = list(title = 'Year'),
               yaxis = list(title = 'Proportion Female'),
               legend = list(x =.5, y=.9))
    })
    #Graph of population by stage
    output$agedetail_dd_s <- renderPlotly({
      plot_ly(stoch_dd_s$Agepop2,
              x = ~t,
              y = ~N2,
              color = ~group2,
              type = 'scatter',
              mode = 'lines') %>%
        layout(title = 'Population by Stage',
               xaxis = list(title = 'Year'),
               yaxis = list(title = 'Abundance'),
               legend = list(x =.6, y=.95))

    })
    
    Age_r <<- reactive({stoch_dd_s$SimpleAge})
    
    #Graph of population age structure
    output$agepop_dd_s <- renderPlotly({
      plot_ly(data = stoch_dd_s$SimpleAge, 
              x = ~t, 
              y = ~N, 
              color = ~group,
              colors = c('#30586C', '#549EC3','#6BC9E3'),
              type = 'bar') %>%
        layout(title = "Population Age Structure",
               xaxis = list(title = "Time (t)"),
               yaxis = list(title = "Proportion"),
               barmode = 'stack',  # Stacked bar chart
               showlegend = TRUE)
      
    })

    ### Survival for average landscape covariate value:
    #plogis(phi0+phi_infect*infected+phi_age[age] + phi_sex*sex)
    # beta_mat_phi_s <- data.frame(int = phi0_dd_s1,
    #                              sex = rep(c(1,0), each = 6)*phi_sex_dd_s1,
    #                              age = rep(c(stoch_dd_s$phi_age), 4),
    #                              infected = rep(rep(c(0,1), each = 3),2)*rep(phi_infect_dd_s1,4))
    # print(stoch_dd_s$phi)

    phi_df_s <- data.frame(p = stoch_dd_s$phi,
                                Stage = c("F- Fawns","F- Yearlings", "F- Adults",
                                          "F+ Fawns","F+ Yearlings", "F+ Adults",
                                          "M- Fawns","M- Yearlings", "M- Adults",
                                          "M+ Fawns","M+ Yearlings", "M+ Adults"),
                                Sex = rep(c("F", "M"), each = 6))
    phi_df_s$Stage <- factor(phi_df_s$Stage, levels = c("F- Fawns","F+ Fawns","F- Yearlings",
                                                        "F+ Yearlings","F- Adults","F+ Adults",
                                                        "M- Fawns","M+ Fawns","M- Yearlings",
                                                        "M+ Yearlings","M- Adults","M+ Adults"))
    output$surv_dd_s <- renderPlotly({
      plot_ly(phi_df_s,
              x = ~Stage,
              y = ~p,
              color = ~Sex,
              colors = c('#5B497A','#49A2A8'),
              type = 'bar') %>%
        layout(title = 'Survival Probability (Average Pixel)',
               xaxis = list(title = 'Stage'),
               yaxis = list(title = 'Yearly Survival')
        )
    })
    
    
    
    # Prev_lambda <- stoch_dd_s$Prev_lam
    # #xys <- coef(lm(Lambda ~ Prop, Prev_lambda))
    # range_prev <- seq(0, max(stoch_dd_s$Prev_lam$Prop), length = 20)
    # #Lam_vals <- xys[1] + xys[2]*range_prev
    # Lam_vals <- predict.lm(lm(Lambda ~ Prop, Prev_lambda), data.frame(Prop = range_prev), interval = 'confidence')
    # Lam_vals <- cbind(Lam_vals, data.frame(Prevalence = range_prev))
    # names(Lam_vals) <- c('Median', "LCI", "UCI", "Prevalence")
    # 
    # output$prev_lam <- renderPlotly({
    # ggplotly(
    #   ggplot(Lam_vals, aes(x = Prevalence, y = Median))+
    #     geom_line()+
    #     geom_ribbon(aes(ymin = LCI, ymax = UCI), lty = 2, fill = 'tan', alpha = .5)+
    #     geom_hline(yintercept = 1, lty =2)+
    #     theme_bw()+
    #     xlab('Prevalence')+
    #     ylab('Realized Growth Rate')
    # )
    # 
    # })
    
    leslie_mats <- array(c(.04*stoch_dd_s$s.r, stoch_dd_s$phi[1], 0, 
                           1.25*stoch_dd_s$s.r, 0, NA, 
                           1.725*stoch_dd_s$s.r, 0, NA), c(3,3,13))
    lambda <- array(NA, 13)
    pert_lam <- array(NA, c(13, 2)) 
    vals <- c(.95, 1.05) #5% drop, 5% increase
    for(kk in 1:13){
    leslie_mats[3,2,kk]  <- stoch_dd_s$phi[2]*(1-.05*(kk-1)) + stoch_dd_s$phi[5]*(.05*(kk-1))
    leslie_mats[3,3,kk]  <- stoch_dd_s$phi[3]*(1-.05*(kk-1)) + stoch_dd_s$phi[6]*(.05*(kk-1))
    lambda[kk] <- Re(eigen(leslie_mats[,,kk])$values)[1] ## Growth rate
    temp_lm <- leslie_mats[,,kk] 
    for(j in 1:2){
      temp_lm[3,2:3] <- leslie_mats[3,2:3,kk]*vals[j]
      pert_lam[kk,j] <- Re(eigen(temp_lm)$values)[1] ## Growth rate
    }
    }
    
    
    Lam_vals <- data.frame(Prevalence = seq(0, .6, by = .05),
                           Median = lambda,
                           LCI = pert_lam[,1],
                           UCI = pert_lam[,2]
                           )
    
    Prev_lambda <- stoch_dd_s$Prev_lam
    names(Prev_lambda) <- c('t', 'Prevalence', 'Median')
    
    output$prev_lam <- renderPlotly({
    ggplotly(
      ggplot()+
        geom_line(data = Lam_vals, aes(x = Prevalence, y = Median))+
        geom_ribbon(data = Lam_vals, aes(x = Prevalence, ymin = LCI, ymax = UCI), lty = 2, fill = 'tan', alpha = .5)+
        geom_hline(yintercept = 1, lty =2)+
        #geom_point(data = Prev_lambda, aes(x = Prevalence, y = Median))+
        theme_bw()+
        ggtitle('Population Growth (Without Hunting)')+
        xlab('Prevalence')+
        ylab('Population Growth Rate') 
    )

    })
    


    ### Lambda over time:
    Lambda_dd_s <- data.frame(t = 2:popcrash,
                              lambda = stoch_dd_s$N[2:popcrash]/stoch_dd_s$N[1:(popcrash-1)])
    #abline_l <- data.frame(x=1:nocc_dd_s1, y=1)

    lambda_r <<- reactive({
      plot_ly(Lambda_dd_s,
              x = ~t,
              y = ~lambda,
              type = 'scatter',
              mode = 'lines',
              name = 'Projected'  # Label the projected trace
      ) %>%
        add_trace(
          x = ~t, 
          y = rep(1, length(Lambda_dd_s$t)),  # y = 1 across the time range
          type = "scatter",
          mode = "lines",  # Ensure it’s a line
          line = list(dash = 'dot'),  # Make the line dotted
          name = 'Stable Population'  # Label the dotted line
        ) %>%
        layout(title = 'Realized Population Growth Rate',
               xaxis = list(title = 'Time', range = list(2, nocc_dd_s1)),
               yaxis = list(title = 'Lambda',
               legend = list(x =.5, y=.9))
        )
    })
    output$lambda_dd_s <- renderPlotly({
      req(lambda_r())
      lambda_r()
    })

    #### Adjustable 'map' of population and abundance ####
    #print(input$graphyear)
    output$yearslide <- renderUI({
      req(input$nocc_dd_s)
      sliderInput(inputId = "graphyear", label = "Year", min = 1, max = input$nocc_dd_s, value = 1, step = 1)
    })

    densityplot_r <<- reactive({stoch_dd_s$Nq})
    print(densityplot_r())
    plot_data <- reactive({
      req(stoch_dd_s$Nq)  # Make sure the data is available
      req(input$graphyear)      # Ensure that the input is provided
      #prevmap <- stoch_dd_s$prev_map
      prevmap <- stoch_dd_s$Nq
      grid0_plot <- grid0
      #grid0_plot$Prevalence <- subset(prevmap, prevmap$time ==  input$graphyear)$prev
      grid0_plot$Density <- prevmap[input$graphyear,]/6.158
      grid0_plot$Density[stoch_dd_s$noGo] <- NA
      
      county_levels <- as.data.frame(grid0_plot %>% 
                                       group_by(County) %>% 
                                       summarize(Density = mean(Density, na.rm = T)))[c(1:3,6:7,9:10),]
      county_levels$Density <- round(county_levels$Density)
      county_levels$x <- c(561570.5,489570.5,459570.5,
                           531470.5,481570.5,520570.5,570570.5)
      county_levels$y <- c(3997732, 4023732, 4031732,
                           4005732,3981732,3975732, 3977732)
      #grid0_plot$Prevalence <- cut(grid0_plot$Prions, breaks = c(0, seq(.1, .9, by = .2), 1), include.lowest = T)
      #levs <- cut(seq(0,1, by = .1), breaks = c(0, seq(.1, .9, by = .2), 1), include.lowest = T)
      ggplot() +
        geom_tile(data= grid0_plot, aes(x = x, y = y, fill = Density), alpha = .8) + 
        #scale_fill_viridis_d(option = 'B', alpha = .8, limits = unique(levs))+
        scale_fill_gradient2(low = 'black', mid  = 'pink',midpoint = .25, high = 'firebrick', 
                             na.value = 'white', 
                             limits = c(0,max(stoch_dd_s$Nq, na.rm = T)/6.178 + .4)
                             )+
        theme_minimal() + 
        geom_label(data = county_levels, aes(x = x, y=y, label = Density))+
        theme(
          strip.text = element_text(size = 10),  # Adjust facet labels
          title = element_text(size = 20),
          axis.title = element_blank(),
          legend.text = element_text(size =18),
          axis.text = element_blank(),
          panel.grid = element_blank()
        )+
        #geom_sf(data = studyarea, fill = NA, col = 'black', lwd = .5, lty = 2)+
        #geom_sf_text(data = studyarea, aes(label = Area), size = 3, color = "black")+
        geom_spatvector(data = counties, fill = NA, col = 'grey80', lwd = .5, lty = 2)+
        geom_spatvector(data = zone2, fill = NA, col = 'grey80', lwd = 1)+
        ggtitle("Deer Density per Sq Mile")
    })
    
    # Convert ggplot to plotly within the render function
    output$Raster <- renderPlot({
      req(plot_data())  # Ensure plot data is ready
      plot_data()
      # # Convert ggplot to plotly and return it
      # ggplotly(plot_data(), tooltip = 'fill') %>% layout(margin = list(t = 0, r = 0, b = 0, l = 0))
    })
    
    #### Prevalence Map ####
    output$yearslide2 <- renderUI({
      req(input$nocc_dd_s)
      sliderInput(inputId = "graphyear2", label = "Year", min = 1, max = input$nocc_dd_s, value = 1, step = 1)
    })
    plot_data2 <<- reactive({
      req(stoch_dd_s$prev_map)  # Make sure the data is available
      req(input$graphyear2)      # Ensure that the input is provided
      print(input$graphyear2)
      prevmap <- stoch_dd_s$prev_map
      grid0_plot <- grid0
      myyear <- subset(prevmap, prevmap$time ==  input$graphyear2)
      grid0_plot$Prevalence <- myyear$AvgPrev
      grid0_plot$Prevalence[stoch_dd_s$noGo] <- NA
      # grid0_plot <- as.data.frame(grid0_plot %>%
      #   group_by(County) %>%
      #   mutate(AvgPrev = mean(Prevalence, na.rm = TRUE)) %>%
      #   ungroup())
      county_levels <- as.data.frame(myyear %>% 
                                       group_by(county) %>% 
                                       summarize(Prev = mean(AvgPrev, na.rm = T)))[c(1:3,6:7,9:10),]
      county_levels$Prev <- round(county_levels$Prev, 2)
      county_levels$x <- c(561570.5,489570.5,459570.5,
                           531470.5,481570.5,520570.5,570570.5)
      county_levels$y <- c(3997732, 4023732, 4031732,
                           4005732,3981732,3975732, 3977732)
      ### TEMP
      #grid0_plot$Prevalence[grid0_plot$Prevalence >0] <- 1
      
      ggplot() +
        geom_tile(data= grid0_plot, aes(x = x, y = y, fill = Prevalence), alpha = .8) +
         scale_fill_viridis_c(option = 'B', alpha = .8, direction = 1,
                              limits = c(0,max(prevmap$AvgPrev, na.rm = T) + .1))+
        theme_minimal() + 
        theme(
          strip.text = element_text(size = 10),  # Adjust facet labels
          title = element_text(size = 20),
          legend.text = element_text(size =18),
          axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank()
        )+
        #geom_spatvector(data = counties, fill = NA, col = 'grey80', lwd = .5, lty = 2)+
        geom_spatvector(data = zone2, fill = NA, col = 'grey80', lwd = 1)+
        geom_label(data = county_levels, aes(x = x, y=y, label = Prev))+
        ggtitle("CWD Prevalence")
    })
    
    output$PrevRast <- renderPlot({
      req(plot_data2())  # Ensure plot data is ready
      plot_data2()
    })
    
    prevmap_r <<- reactive({stoch_dd_s$prev_map})
    prevmap_alt_r <<- reactive({stoch_dd_s$prev_map1})
    grid0_plotr <<- reactive({stoch_dd_s$grid0})
  }) #end observe
  
#### Report Out Information ####  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_file.Rmd")
      file.copy("report_file.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(initPrms = Myinputs_r(),
                     TotPop = TotalPopulation(),
                     TotPopGraph = TotPopGraph(),
                     prev1 = prevs1(),
                     prev2 = prevs2(),
                     PrevsGraph = prev_dd_graph(),
                     PrevMap = prevmap_r(),
                     PrevMapAlt = prevmap_alt_r(),
                     grid =grid0_plotr(),
                     Lambda = lambda_r(),
                     Age = Age_r(),
                     Densplot = densityplot_r()) 
      print(params)
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
    
}

# Run the application 
shinyApp(ui = ui, server = server)

#column(6, class = 'vertical-line',
#div(class = 'box-section',
#    plotOutput('totpop_det_ns')))
library(shiny)
library(shinyauthr)
library(shinyjs)
library(datasets)
library(tidyverse)
library(sf)
library(sp)
library(tmaptools)
library(noncensus)
library(leaflet)
library(tools)
library(raster)
library(grid)
library(DT)
library(devtools)
library(noncensus)
data("zip_codes")
library(shinycssloaders)

library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)

library(dashboardthemes)
library(leaflet)
library(leaflet.extras)
library(RMySQL)
library(plotly)
library(heatmaply)
library(shinyHeatmaply)
library(shinyWidgets)


#changes life exp
library("gplots")
library(HMDHFDplus)
library(reshape2)
library(RColorBrewer)


options(HMD_user = "bh2722@columbia.edu")
options(HMD_password = "password999")

logotitle <- shinyDashboardLogoDIY(boldText = "", mainText = "MortalityViz", textSize = 14, badgeText = "BETA",  
                                    badgeTextColor = "black", badgeTextSize = 2, badgeBackColor = "#DCDCDC", badgeBorderRadius = 2)

sysfonts::font_add_google("Roboto Condensed", regular.wt = 400, bold.wt = 700)

theme_custom <- shinyDashboardThemeDIY(
    
    ### general
    appFontFamily = "Roboto Condensed"
    ,appFontColor = "rgb(0,0,0)"
    ,primaryFontColor = "rgb(0,0,0)"
    ,infoFontColor = "rgb(0,0,0)"
    ,successFontColor = "rgb(0,0,0)"
    ,warningFontColor = "rgb(0,0,0)"
    ,dangerFontColor = "rgb(0,0,0)"
    ,bodyBackColor = "rgb(236,242,247)"
    
    ### header
    ,logoBackColor = "rgb(53,64,83)" #mortalityviz background
    
    ,headerButtonBackColor = "rgb(255,255,255)" #white background for lines button 
    ,headerButtonIconColor = "rgb(125,132,145)" #grey three lines 
    ,headerButtonBackColorHover = "rgb(21, 154, 251)" #bright blue
    ,headerButtonIconColorHover = "rgb(0,0,0)" #black hover three lines = black lines
    
    ,headerBackColor = "rgb(255,255,255)"  #rest of header back color = white
    ,headerBoxShadowColor = "rgb(232,232,232)" 
    ,headerBoxShadowSize = "2px 2px 2px"
    
    ### sidebar
    ,sidebarBackColor = "rgb(53,64,83)"
    ,sidebarPadding = 0 #left side selected tab
    
    ,sidebarMenuBackColor = "rgb(53,64,83)" #"transparent" for sidebar collapse menu
    ,sidebarMenuPadding = 0
    ,sidebarMenuBorderRadius = 20
    
    ,sidebarShadowRadius = "2px 2px 2px" #sidebar shadow
    ,sidebarShadowColor = "#aaaaaa"
    
    ,sidebarUserTextColor = "rgb(0,0,0)"
    
    ,sidebarSearchBackColor = "rgb(55,72,80)"
    ,sidebarSearchIconColor = "rgb(153,153,153)"
    ,sidebarSearchBorderColor = "rgb(55,72,80)"
    
    #for unselected sidebars = dark blue shading and grey text
    ,sidebarTabTextColor = "rgb(125,132,145)" #grey text color
    ,sidebarTabTextSize = 14
    ,sidebarTabBorderStyle = "none none none none" #border clockwise from top
    ,sidebarTabBorderColor = ""
    ,sidebarTabBorderWidth = 0
    
    #selected sidebar = darker blue shading, w/ white text
    ,sidebarTabBackColorSelected = "rgb(46,58,72)" #selected even darker blue
    ,sidebarTabTextColorSelected = "rgb(255,255,255)" #white text
    ,sidebarTabRadiusSelected = "0px 0px 0px 0px" #rectangular
    
    #hovering sidebar = darker blue shading w/ grey text
    ,sidebarTabBackColorHover = "rgb(46,58,72)" #hover same as selected = even darker blue
    ,sidebarTabTextColorHover = "rgb(125,132,145)" #but hover not selected = grey text
    ,sidebarTabBorderStyleHover = "none none none solid"
    ,sidebarTabBorderColorHover = "rgb(21, 154, 251)" #bright blue hovering left padding
    ,sidebarTabBorderWidthHover = 5
    ,sidebarTabRadiusHover = "0px 0px 0px 0px"
    
    ### boxes
    ,boxBackColor = "rgb(255,255,255)"
    ,boxBorderRadius = 5
    ,boxShadowSize = "0px 1px 1px"
    ,boxShadowColor = "rgba(0,0,0,.1)"
    ,boxTitleSize = 16
    ,boxDefaultColor = "rgb(111,125,150)" #grey
    ,boxPrimaryColor = "rgb(71,186,193)" #teal
    ,boxInfoColor = "rgb(21, 154, 251)" #bright blue
    ,boxSuccessColor = "rgb(67, 138, 250)"
    ,boxWarningColor = "rgb(244,156,104)"
    ,boxDangerColor = "rgb(111,125,150)" #grey
    
    ,tabBoxTabColor = "rgb(255,255,255)"
    ,tabBoxTabTextSize = 14
    ,tabBoxTabTextColor = "rgb(0,0,0)"
    ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
    ,tabBoxBackColor = "rgb(255,255,255)" 
    ,tabBoxHighlightColor = "rgb(21, 154, 251)" #"rgb(111,125,150)" #rim of tab boxes of non seelected
    ,tabBoxBorderRadius = 0
    
    ### inputs
    ,buttonBackColor = "rgb(245,245,245)"
    ,buttonTextColor = "rgb(0,0,0)"
    ,buttonBorderColor = "rgb(200,200,200)"
    ,buttonBorderRadius = 5
    
    ,buttonBackColorHover = "rgb(235,235,235)"
    ,buttonTextColorHover = "rgb(100,100,100)"
    ,buttonBorderColorHover = "rgb(200,200,200)"
    
    ,textboxBackColor = "rgb(255,255,255)"
    ,textboxBorderColor = "rgb(200,200,200)"
    ,textboxBorderRadius = 5
    ,textboxBackColorSelect = "rgb(245,245,245)"
    ,textboxBorderColorSelect = "rgb(200,200,200)"
    
    ### tables
    ,tableBackColor = "rgb(255,255,255)"
    ,tableBorderColor = "rgb(240,240,240)"
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1
)

convertMenuItem <- function(mi,tabName) {
    mi$children[[1]]$attribs['data-toggle']="tab"
    mi$children[[1]]$attribs['data-value'] = tabName
    if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
        mi$attribs$class=NULL
    }
    mi
}

dbHeader <- dashboardHeader(title = tags$a(href = "javascript:void(window.open('https://google.com', '_blank'))",
                                       tags$img(src='MortalityViz.png', class = 'logo-lg',
                                                style = "float: center",height='45x',width='160px')),
                            tags$li(a(href = "javascript:void(window.open('https://github.com/bhsu4', '_blank'))",
                                      icon("github", "fa-1.5x"),
                                      title = "Visit my Github"),
                                      class = "dropdown", 
                                      style="color: #000; background-color: #b3b3b3; 
                                                  font-size:135% ;"),
                            tags$li(a(href = "javascript:void(window.open('https://sps.columbia.edu/academics/masters/actuarial-science', '_blank'))",
                                      img(src = 'ColumbiaLogo.png',
                                          title = "Contact Us", height = "20px", width = "20px")),
                                    class = 'dropdown') 
                            )


ui = dashboardPage(
    
    
    title = "MortalityViz",
    
    # Dashboard Page Setup ----------------------------------------------------
    dbHeader,
    #dashboardHeader(
    ### changing logo
    # title = logo_blue_gradient),
    
    # Dashboard Sidebar -------------------------------------------------------
    dashboardSidebar(

        sidebarUserPanel("Mushu",
                         subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                         # Image file should be in www/ subdir
                         image = "https://fiverr-res.cloudinary.com/t_profile_original,q_auto,f_auto/attachments/profile/photo/a8ece8a3bb5951b6a9ffe9a19063327a-1537916496823/Mushu%20Glasses.jpg"
        ),
        sidebarMenu(
           # id = "tabs",
            #convertMenuItem(
            menuItem("MORTALITY DECOMP", #tabName = "tab_le", 
                         icon = icon("desktop"), startExpanded = TRUE,
                menuSubItem("Overview", tabName = "tab_le", icon = icon("clipboard-list")),
                menuSubItem("Decomposition by Age", tabName = "LEAge", icon = icon("chevron-right")), #icon("hand-holding-heart")),
                menuSubItem("NULL", tabName = "LPAge", icon = icon("chevron-right"))), #icon("hand-holding-usd"))), #, "tab_le"),
            menuItem("ABOUT", tabName = "tab_about", icon = icon("gear")) #info
            
           
        )
    ),
    
    # Dashboard Body ----------------------------------------------------------
    dashboardBody(
    
        tags$script(HTML("$('body').addClass('sidebar-mini');")),
        
        theme_custom,
        
        #tabItems(
        tags$head(tags$style(HTML('.main-sidebar { font-size: 20px; font-weight: bold;}
                        .skin-blue .main-header .logo:hover {  background-color: #47b2ff; }
                        .skin-blue .main-header .navbar {  background-color: "rgb(255,255,255)"; }   ')),
                  #'.main-sidebar {font-weight: bold; font-family: Source Sans Pro
                     # font-size: 30px; ')),
                  
                  tags$link(rel = "shortcut icon", 
                            href = "https://www.columbia.edu/content/themes/custom/columbia/favicon-crown.png"), 
                  ),
        
    tabItems(
        # About - tab_life exp -------------------------------------------------------
        tabItem(
            "tab_le", 
            fluidRow(
                # About - About Me - start ------------------------------------------------
                box(
                    title = tags$strong(HTML("<br>&nbsp;&nbsp;&nbsp;DECOMPOSITION DESCRIPTION<br><br>")),
                    status = "danger",
                    width = 6, solidHeader = TRUE,
                    tags$style(HTML('color: #ffffff')),
                    tags$br(),
                    tags$p(
                        class = "text-left",
                        HTML('&nbsp;'), HTML('&nbsp;'), tags$strong(HTML("FUNCTION TITLE")), 
                        HTML('<span style="padding: 0 20px">&nbsp;</span>'), "Decomposition of Mortality by Age", HTML('<hr/>')
                    ),
                    tags$p(
                        class = "text-left",
                        HTML('&nbsp;'), HTML('&nbsp;'), tags$strong(HTML("METRIC")), 
                        HTML('<span style="padding: 0 44px">&nbsp;</span>'), "Change in Life Expectancy, Life Preparancy", HTML('<hr/>')
                    ), 
                    tags$p(
                        class = "text-left",
                        HTML('&nbsp;'), HTML('&nbsp;'), tags$strong(HTML("EXPERIENCE")), 
                        HTML('<span style="padding: 0 30px">&nbsp;</span>'), "Click functionality in barchart", HTML('<hr/>')
                    ),
                    tags$p(
                        HTML('<div style="display:flex; justify-content:space-around; content-align: left; ">
                        <p><b>&nbsp;&nbsp;&nbsp;&nbsp;DESCRIPTION</b><p>
                        <span style = "padding: 0 65px; display:inline-block; word-wrap:break-word;"> The Decomposition of Mortality by Age is responsible of observing the life expectancy changes over a time interval for a select country.</span>
                        </div><hr/>')
                    ),
                    tags$p(
                        HTML('<div style="display:flex; justify-content:space-around; content-align: left; ">
                        <p><b>&nbsp;&nbsp;&nbsp;&nbsp;ERRORS</b><p>

                        <span style = " padding: 0 95px; display:inline-block; word-wrap:break-word;">Australia is working country for all years. Need to figure out why conditional panels dont work for other countries. </span>
                        
                        </div><hr/>'
                        )
                    ),
                    
                    tags$head(tags$style(HTML('.box-header h3.box-title { font-weight: bold; font-size: 18px; color: rgb(0,144,197);} 
                                              p {color: #5e697d}'))) 
                    
                )
            )
        ),
        tabItem(
            "LEAge", 
            fluidRow(
                column(width = 12,
                       box(width = NULL, status = "warning",
                           fluidRow(
                               column(width = 3,
                                   fluidRow(
                                       column(width = 10,
                                        selectInput("heatCountry", "Selected Country", c(Choose = ""), selectize = TRUE)
                                        ), 
                                       column(width = 2, 
                                       actionButton(
                                           inputId = "heatQA",
                                           label = "",
                                           icon = icon("question-circle"),
                                           style="color: #fff; background-color: #b3b3b3; 
                                                  border-color: #b3b3b3; padding:2px; font-size:78% ; 
                                                  width: 20px; height: 20px"
                                       ))
                                       
                                   ),
                                   fluidRow(
                                       column(width = 9,
                                          checkboxGroupButtons(
                                              inputId = "heatGender", label = "Gender", 
                                              choices = c("Male", "Female"), selected = "Male", 
                                              justified = TRUE, status = "primary")),
                                       
                                       column(width = 3, 
                                              
                                              materialSwitch(
                                                  inputId = "heatAggregate", label = div(style = "margin-top:0px; font-weight: bold ; margin-bottom: -10px", HTML("Aggregate<br><br>")), 
                                                  right = FALSE, value = FALSE, status = "primary")
                                              )

                                   )
                               
                               ), 
                              column(width = 9, 
                                  # Input: Specification of range within an interval ----
                                   wellPanel( 
                                       conditionalPanel(
                                           condition = "input.heatCountry == 'Australia'",
                                           sliderInput("range_t",
                                                       label = "Years Selected",
                                                       min = 1921, max = 2018, value = c(1921, 2018))
                                       )
                             ) # wellPanel
                           ) # column
                         ), #fluidrow
                         
                       )
                     ), 
                column(width = 12,
                       
                       tabBox(width = NULL, title = tagList(shiny::icon("calculator"), "By Age"), 
                              tabPanel(title = "Change in Life Expectancy", 
                                              
                                       fluidRow(
                                           column(width = 6,
                                                  plotlyOutput("BarplotLE", height = 300)
                                           ), 
                                           column(width = 6, 
                                                  plotlyOutput("BarplotLE_specific", height = 300)

                                           )
                                       )
                                       , 
                                       tags$br(), tags$br(),
                                       
                                       fluidRow(
                                           column(width = 12, 
                                                  withSpinner(plotlyOutput("HeatMap", height = 600), proxy.height = "20px")
                                           )
                                       )
                              ),
                              tabPanel("LE2", "Tab content 2")
                       )
                )
                
                    
             
            )

                
            
        
        ),
                    
        
        # About - tab_about -------------------------------------------------------
        tabItem(
            "tab_about",
            
            
            fluidRow(
                # About - About Me - start ------------------------------------------------
                box(
                    title = "About me",
                    status = "danger",
                    width = 3,
                    tags$p(
                        class = "text-center",
                        tags$img(class = "img-responsive img-rounded center-block", src = "me.png", style = "max-width: 150px;")
                    ),
                    tags$p(
                        class = "text-center",
                        tags$strong("Hi! I'm Ben!"),
                        HTML(paste0("(", tags$a(href = "https://www.linkedin.com/in/benjamin-hsu-10b33a97/", "Benjamin Hsu"), ")"))
                    ),
                    tags$p(
                        "I am a Master of Science student in Actuarial Science at Columbia University.",
                        "I graduated from the University of Rochester with a Bachelor's degree in Statistics",
                        "with a Certificate in Actuarial Science."
                    ),
                    tags$p(
                        "I like to work on projects with",
                        "statistical and machine learning. You can find more of my work on my website",
                        HTML(paste0(tags$a(href = "https://benjaminhsu.netlify.com", "benjaminhsu.com", target = "a()"), "."))
                    ),
                    tags$p(
                        "Get in touch with me on LinkedIn at",
                        HTML(paste0("(", tags$a(href = "https://www.linkedin.com/in/benjamin-hsu-10b33a97/", "Benjamin Hsu", target = "a()"), "),")),
                        "online at",
                        HTML(paste0(tags$a(href = "https://benjaminhsu.netlify.com", "benjaminhsu.com", target = "a()"), ",")),
                        "or by email at",
                        HTML(paste0(tags$a(href = "mailto:bh2722@columbia.edu", "bh2722@columbia.edu"), "."))
                    )
                )
            )
        )
    )
        
    )    
        
)
    



change5x1 <- function(cntry, t1, t2){
    
    Males_5x1 <- readHMDweb(CNTRY = cntry, item = "mltper_5x1", username = getOption("HMD_user"), password = getOption("HMD_password"))
    Females_5x1 <- readHMDweb(CNTRY = cntry, item = "fltper_5x1", username = getOption("HMD_user"), password = getOption("HMD_password"))
    labels <- levels(cut(unique(Males_5x1$Age), breaks = c(0, 1, 5, seq(10, max(Males_5x1$Age), 5)), right = FALSE))
    labels_m <- paste0(levels(cut(unique(Males_5x1$Age), breaks = c(0, 1, 5, seq(10, max(Males_5x1$Age), 5)), right = FALSE)), "M")
    labels_f <- paste0(levels(cut(unique(Females_5x1$Age), breaks = c(0, 1, 5, seq(10, max(Females_5x1$Age), 5)), right = FALSE)), "F")
    
    #create heatmap labels
    age_interest <- gsub(",.*$", "", gsub("\\[|\\)", "", labels)) #gsub the brackets out, and then take lower value before comma
    age_contribution <- paste("Contribution Age", age_interest)
    age_le <- paste("LE Age", age_interest)
    
    #barplot labels
    age_range <- gsub(",", "-", gsub("\\[|\\)", "", labels)) #dash range labels
    
    #start of calculation
    lx_matrix_M<-dcast(Males_5x1,Year~Age,value.var = "lx")
    ex_matrix_M<-dcast(Males_5x1,Year~Age,value.var = "ex")
    lx_matrix_F<-dcast(Females_5x1,Year~Age,value.var = "lx")
    ex_matrix_F<-dcast(Females_5x1,Year~Age,value.var = "ex")
    age_groups<-dim(lx_matrix_M)[2]-1
    
    ## Definition of vectors to be used
    changes_per_age_M<-matrix(0,nrow=age_groups-1,ncol=age_groups-1)
    changes_per_age_F<-matrix(0,nrow=age_groups-1,ncol=age_groups-1)
    changes_per_age_MF <- matrix(0, nrow = age_groups, ncol = age_groups)
    
    for(k in 1:(age_groups-1)){
        d_x_12_M<-matrix(0,nrow=age_groups,ncol=1)
        d_x_21_M<-matrix(0,nrow=age_groups,ncol=1)
        d_x_12_F<-matrix(0,nrow=age_groups,ncol=1)
        d_x_21_F<-matrix(0,nrow=age_groups,ncol=1)
        ##we redefine the vectors lx by dividing them by the respective l0
        ##Case of males
        lx_t1_M<-lx_matrix_M[lx_matrix_M$Year==t1,-1]/lx_matrix_M[lx_matrix_M$Year==t1,k+1]
        lx_t2_M<-lx_matrix_M[lx_matrix_M$Year==t2,-1]/lx_matrix_M[lx_matrix_M$Year==t2,k+1]
        ex_t1_M<-ex_matrix_M[ex_matrix_M$Year==t1,-1]
        ex_t2_M<-ex_matrix_M[ex_matrix_M$Year==t2,-1]
        ##Case of females
        lx_t1_F<-lx_matrix_F[lx_matrix_F$Year==t1,-1]/lx_matrix_F[lx_matrix_F$Year==t1,k+1]
        lx_t2_F<-lx_matrix_F[lx_matrix_F$Year==t2,-1]/lx_matrix_F[lx_matrix_F$Year==t2,k+1]
        ex_t1_F<-ex_matrix_F[ex_matrix_F$Year==t1,-1]
        ex_t2_F<-ex_matrix_F[ex_matrix_F$Year==t2,-1]
        
        
        #<-dim(lx_t1)[2]
        for(x in k:(age_groups-1)){##
            ##Calculation for males
            d_x_12_M[x]<-as.double(lx_t1_M[x])*(as.double(ex_t1_M[x])-as.double(ex_t2_M[x]))-as.double(lx_t1_M[x+1])*(as.double(ex_t1_M[x+1])-as.double(ex_t2_M[x+1]))
            d_x_21_M[x]<-as.double(lx_t2_M[x])*(as.double(ex_t2_M[x])-as.double(ex_t1_M[x]))-as.double(lx_t2_M[x+1])*(as.double(ex_t2_M[x+1])-as.double(ex_t1_M[x+1]))  
            changes_per_age_M[x,k]<-0.5*(d_x_21_M[x]-d_x_12_M[x])
            ##calculation for females
            d_x_12_F[x]<-as.double(lx_t1_F[x])*(as.double(ex_t1_F[x])-as.double(ex_t2_F[x]))-as.double(lx_t1_F[x+1])*(as.double(ex_t1_F[x+1])-as.double(ex_t2_F[x+1]))
            d_x_21_F[x]<-as.double(lx_t2_F[x])*(as.double(ex_t2_F[x])-as.double(ex_t1_F[x]))-as.double(lx_t2_F[x+1])*(as.double(ex_t2_F[x+1])-as.double(ex_t1_F[x+1]))  
            changes_per_age_F[x,k]<-0.5*(d_x_21_F[x]-d_x_12_F[x])
        }
    }

    rownames(changes_per_age_F) <- age_contribution
    colnames(changes_per_age_F) <- age_le
    rownames(changes_per_age_M) <- age_contribution
    colnames(changes_per_age_M) <- age_le
    
    both_genders = cbind(changes_per_age_M, changes_per_age_F)
    total_change_in_age <- apply(both_genders, 2, sum)
    res <- rbind(both_genders, total_change_in_age)
    #add labels
    colnames(res) <- c(labels_m, labels_f)
    row.names(res) <- c(labels, "Total")
    ##The following two quantities are used for a plot
    total_male <- apply(changes_per_age_M, 2, sum)
    total_female <- apply(changes_per_age_F, 2, sum)
    #creating a matrix w/ M/F
    changes_per_age_MF[lower.tri(changes_per_age_MF)] <- changes_per_age_M[lower.tri(changes_per_age_M, diag = TRUE)]
    changes_per_age_MF[upper.tri(changes_per_age_MF)] <- t(changes_per_age_F)[upper.tri(t(changes_per_age_F), diag = TRUE)]
    diag(changes_per_age_MF) <- NA
    rownames(changes_per_age_MF) <- c("", age_contribution)
    colnames(changes_per_age_MF) <- c(age_le, " ")
    
    
    ##Removal of the upper part of matrix since it is always zero
    changes_per_age_M[upper.tri(changes_per_age_M)] <- NA
    changes_per_age_F[upper.tri(changes_per_age_F)] <- NA
    
    
    return(list(changes_per_age_M, changes_per_age_F, changes_per_age_MF))
}

create_hover_MF <- function(x){
    my_rep <- vector() ; my_rep2 <- vector()
    for(k in 24:1){
        times <- 24 - k
        if(k == 24){ curr_rep <- c(rep(rev(x)[k], k)) }
        else{ curr_rep <- c(rep(rev(x)[k+1], times), rep(rev(x)[k], k)) }
        my_rep = c(my_rep, curr_rep)
    }
    return(my_rep)
}

server <- shinyServer(function(input, output, session){ 
    
        
    
        observeEvent(input$heatQA, {
            show_alert(
                title = NULL,
                text = tags$span(
                    tags$h3("Decomposition of Mortality by Age",
                            style = "color: steelblue;"), tags$br(),
                    "Select country and time period along with gender", 
                    tags$br(), tags$br(),
                    "If Aggregate", tags$b("OFF"), "then male and female decomposition will be performed separately",
                    tags$br(), tags$br(),
                    "If Aggregate", tags$b("ON"), "both genders then population (male + female) decomposition will be performed",
                    tags$br(), tags$br(),
                    icon("far fa-smile")
                ),
                html = TRUE
            )
        })
    
        country_info <- reactive({
            read.csv("List_Countries.csv")
        })
    
        res <- reactive({
            req(input$heatCountry)
            chosen_country <- as.character(country_info()$Code[which(country_info()$Country == input$heatCountry)])
            res <- change5x1(chosen_country, input$range_t[1], input$range_t[2])
            return(res)
        })
        
        observe({
            updateSelectInput(session, inputId = 'heatCountry', label = 'Selected Country',
                              choices = unique(country_info()$Country), 
                              selected = input$heatCountry)
        })
        
        country_years <- reactive({
            chosen_country <- as.character(country_info()$Code[which(country_info()$Country == input$heatCountry)])
            males5x1 <- readHMDweb(CNTRY = chosen_country, item = "mltper_5x1", username = getOption("HMD_user"), password = getOption("HMD_password"))
            return(males5x1$Year)
        })
        
        #heatmap
        output$HeatMap <- renderPlotly({
            chosen_country <- as.character(country_info()$Code[which(country_info()$Country == input$heatCountry)])
            scale_colors <- brewer.pal(n=9, name = "YlOrRd") #selection of
            
            if (length(input$heatGender) == 1){
                if (input$heatGender == "Male" ){ res_gender <- res()[[1]] }
                else if (input$heatGender == "Female" ){ res_gender <- res()[[2]]}

                par(mar=c(5.1,2.1,1,2.1))
                dim1 <- dim(res_gender)[[1]]
                hover_text <- matrix(paste0((sapply(colnames(res_gender), function(x) rep(x, dim1))), "<br>", 
                                            rep(rownames(res_gender), dim1), "<br>", 
                                            rep(input$heatGender, dim1*2), "<br>"),
                                     byrow = FALSE, ncol = dim1)
                hover_text2 <- matrix(paste(hover_text, round(res_gender, 4), sep="Change: "), dim1, dim1)
                
                heatmaply(res_gender, dendrogram = "none", Rowv = FALSE, Colv = FALSE, 
                          cexRow = 0.9, cexCol = 0.9, col = scale_colors,  
                          plot_method = c("plotly"), main = paste0("Changes in Life Expectancy (", 
                                                                    input$range_t[1], "-", input$range_t[2], ", ",
                                                                    input$heatGender, ", ", input$heatCountry, ")"), 
                          font = list(size = 8), custom_hovertext = hover_text2, 
                          key.title = "Changes in Years", colorbar_xpos = 30, colorbar_ypos = 10) %>% 
                    layout(xaxis = list(ticktext = as.numeric(gsub("LE Age ", "", colnames(res_gender))), title = "Age", 
                                        showgrid = F, tickangle = 0, showticklabels = TRUE), 
                           yaxis = list(ticktext = as.numeric(gsub("Contribution Age ", "", rev(rownames(res_gender)))),
                                        title = "Contribution", showgrid = F, showticklabels = TRUE))
            
            }
            else{ 
                if (input$heatAggregate == "FALSE"){ 
                    res_gender <- res()[[3]]
                    par(mar=c(5.1,2.1,1,2.1))
                    dim1 <- dim(res_gender)[[1]]
                 
                    #create hover text 
                    row_contr <- create_hover_MF(c(rownames(res_gender)[-1], " ")) #byrow
                    col_le <- create_hover_MF(colnames(res_gender)) #bycol
                  
                    mat1 <- matrix(row_contr, byrow = TRUE, ncol = dim1)
                    mat2 <- matrix(col_le, byrow = FALSE, ncol = dim1)
                    mat3 <- ifelse(lower.tri(mat2), "Male", "Female")
                  
                    mat4 <- matrix(paste(paste0(mat1, "<br>", mat2, "<br>", mat3, "<br>"), 
                                       round(res_gender, 4), sep = "Change: "), dim1, dim1)
                    diag(mat4) <- NA #remove diag hover text
                  
                    heatmaply(res_gender, dendrogram = "none", Rowv = FALSE, Colv = FALSE, #sepwidth=c(1.5, 1.5),
                            cexRow = 0.65, cexCol = 0.65, 
                            col = scale_colors, xlab = "", ylab = "", plot_method = c("plotly"),
                            main = paste0("Changes in Life Expectancy (", 
                                          input$range_t[1], "-", input$range_t[2], ", ",
                                          paste(input$heatGender, collapse = "/"), ", ", input$heatCountry, ")"),
                            font = list(size = 8), custom_hovertext = mat4,
                            key.title = "Changes in Years", 
                            colorbar_xpos = 30, colorbar_ypos = 10) %>% 
                            layout(shapes = list(type = 'line', x0 = 0, x1 = 25, y0 =25, y1 = 0, line = list(width = 1.5)),
                                   xaxis = list(title = "Age", showgrid = F, showticklabels = FALSE), 
                                   yaxis = list(title = "Contribution", showgrid = F, showticklabels = FALSE))
                }
                else {
                   res_gender <- res()[[1]] + res()[[2]] 
                   par(mar=c(5.1,2.1,1,2.1))
                   dim1 <- dim(res_gender)[[1]]
                   hover_text <- matrix(paste0((sapply(colnames(res_gender), function(x) rep(x, dim1))), "<br>", 
                                                rep(rownames(res_gender), dim1), "<br>", 
                                                rep("Aggregate", dim1*2), "<br>"),
                                         byrow = FALSE, ncol = dim1)
                   hover_text2 <- matrix(paste(hover_text, round(res_gender, 4), sep="Change: "), dim1, dim1)
                    
                   heatmaply(res_gender, dendrogram = "none", Rowv = FALSE, Colv = FALSE, 
                             cexRow = 0.9, cexCol = 0.9, col = scale_colors,  
                             plot_method = c("plotly"), main = paste0("Changes in Life Expectancy (", 
                                                                       input$range_t[1], "-", input$range_t[2], ", ",
                                                                       input$heatGender, ", ", input$heatCountry, ")"), 
                              font = list(size = 8), custom_hovertext = hover_text2, 
                              key.title = "Changes in Years", colorbar_xpos = 30, colorbar_ypos = 10) %>% 
                             layout(xaxis = list(ticktext = as.numeric(gsub("LE Age ", "", colnames(res_gender))), title = "Age", 
                                                showgrid = F, tickangle = 0, showticklabels = TRUE), 
                                   yaxis = list(ticktext = as.numeric(gsub("Contribution Age ", "", rev(rownames(res_gender)))),
                                                title = "Contribution", showgrid = F, showticklabels = TRUE))
                }
            }
        })
        
        
        # for maintaining the state of drill-down variables
        BarplotLE <- reactiveVal()
        BarplotLE_specific <- reactiveVal()

        # when clicking on a category, 
        observeEvent(event_data("plotly_click", source = "BarplotLE"), {
            BarplotLE(event_data("plotly_click", source = "BarplotLE")$x)
            BarplotLE_specific(NULL)
            #updateSelectInput(session, inputId = 'heatAge', selected = gsub("Age ", "", BarplotLE()))
        })
        
        
        output$BarplotLE <- renderPlotly({
            #gender select
            if (length(input$heatGender) == 1){
                if (input$heatGender == "Male"){ res_gender <- res()[[1]] }
                else if (input$heatGender == "Female"){ res_gender <- res()[[2]] }
                else if (input$heatGender == "Population"){ res_gender <- res()[[1]] + res()[[2]] }
            
            
            #ecalculation of total
            print(BarplotLE())
            if (is.null(BarplotLE())){
                Total_male <- data.frame(le = apply(res_gender, 2, sum, na.rm = TRUE), row.names = colnames(res_gender)) %>% 
                    rownames_to_column() %>% mutate(curr_col = "#FDB863")
            }
            else{
                Total_male <- data.frame(le = apply(res_gender, 2, sum, na.rm = TRUE), row.names = colnames(res_gender)) %>% 
                    rownames_to_column() %>% mutate(curr_col = if_else(gsub("LE Age ", "", rowname) %in% BarplotLE(), "#8073AC", "#FDB863"))
            }
                
                p <- plot_ly(Total_male, x = ~gsub("LE Age ", "", rowname), y = ~le, type = "bar", 
                             source = "BarplotLE", marker = list(color = ~curr_col)) %>% 
                    config(displayModeBar = FALSE)
                p <- layout(p, barmode="overlay",
                            title = paste0("Changes in Life Expectancy (", 
                                    input$range_t[1], "-", input$range_t[2], ", ",
                                    input$heatGender, ", ", input$heatCountry, ")"),
                            font = list(size = 8),
                            xaxis = list(title = "Age", categoryarray = names(Total_male), 
                                         categoryorder = "array", size = 8, tickangle = 0), 
                            yaxis = list(title = "Change (Years)"))
                p 
                
            }
            else{ 
                if (input$heatAggregate == "FALSE"){ 
                    res_gender1 = res()[[1]]
                    res_gender2 = res()[[2]]
                    
                    print(BarplotLE())
                    if (is.null(BarplotLE())){
                        Total_male <- data.frame(le = apply(res_gender1, 2, sum, na.rm = TRUE), row.names = colnames(res_gender1)) %>% 
                            rownames_to_column() %>% mutate(curr_col = "#FDB863")
                        Total_female <- data.frame(le = apply(res_gender2, 2, sum, na.rm = TRUE), row.names = colnames(res_gender2)) %>% 
                            rownames_to_column() %>% mutate(curr_col = "#FD6363")
                    }
                    else{
                        Total_male <- data.frame(le = apply(res_gender1, 2, sum, na.rm = TRUE), row.names = colnames(res_gender1)) %>% 
                            rownames_to_column() %>% mutate(curr_col = if_else(gsub("LE Age ", "", rowname) %in% BarplotLE(), "#8073AC", "#FDB863"))
                        Total_female <- data.frame(le = apply(res_gender2, 2, sum, na.rm = TRUE), row.names = colnames(res_gender2)) %>% 
                            rownames_to_column() %>% mutate(curr_col = if_else(gsub("LE Age ", "", rowname) %in% BarplotLE(), "#92CCDE", "#FD6363"))
                    }
                    
                    p <- plot_ly(Total_male, x = ~gsub("LE Age ", "", rowname), y = ~le, type = "bar", name = 'Male',
                                 source = "BarplotLE", marker = list(color = ~curr_col)) %>%  config(displayModeBar = FALSE) %>% 
                          add_trace(data = Total_female, x = ~gsub("LE Age ", "", rowname), y = ~le, type = "bar", 
                                     name = 'Female', marker = list(color = ~curr_col)) %>% 
                          layout(barmode="group", title = paste0("Changes in Life Expectancy (", 
                                                                 input$range_t[1], "-", input$range_t[2], ", ",
                                                                 paste(input$heatGender, collapse = "/"), ", ", input$heatCountry, ")"), 
                                 font = list(size = 8),
                                 xaxis = list(title = "Age", categoryarray = names(Total_male), 
                                              categoryorder = "array", size = 8, tickangle = 0), 
                                 yaxis = list(title = "Change (Years)"))
                    p
                }
                else{
                    res_gender = res()[[1]] + res()[[2]]
                    print(BarplotLE())
                    if (is.null(BarplotLE())){
                        Total_male <- data.frame(le = apply(res_gender, 2, sum, na.rm = TRUE), row.names = colnames(res_gender)) %>% 
                            rownames_to_column() %>% mutate(curr_col = "#FDB863")
                    }
                    else{
                        Total_male <- data.frame(le = apply(res_gender, 2, sum, na.rm = TRUE), row.names = colnames(res_gender)) %>% 
                            rownames_to_column() %>% mutate(curr_col = if_else(gsub("LE Age ", "", rowname) %in% BarplotLE(), "#8073AC", "#FDB863"))
                    }
                    
                    p <- plot_ly(Total_male, x = ~gsub("LE Age ", "", rowname), y = ~le, type = "bar", 
                                 source = "BarplotLE", marker = list(color = ~curr_col)) %>% 
                        config(displayModeBar = FALSE)
                    p <- layout(p, barmode="overlay",
                                title = paste0("Changes in Life Expectancy (", 
                                               input$range_t[1], "-", input$range_t[2], ", ",
                                               "Aggregate", ", ", input$heatCountry, ")"),
                                font = list(size = 8),
                                xaxis = list(title = "Age", categoryarray = names(Total_male), 
                                             categoryorder = "array", size = 8, tickangle = 0), 
                                yaxis = list(title = "Change (Years)"))
                    p 
                    
                    
                }
            }
        })
        
        
        
        output$BarplotLE_specific <- renderPlotly({
            if (length(input$heatGender) == 1){
                if (input$heatGender == "Male"){ res_gender <- res()[[1]] }
                else if (input$heatGender == "Female"){ res_gender <- res()[[2]]}
                else if (input$heatGender == "Population") { res_gender <- res()[[1]] + res()[[2]] }
                
                if (is.null(BarplotLE())){  
                    p <- plotly_empty(type = "scatter", mode = "markers") %>%
                          config(displayModeBar = FALSE) %>%
                          layout(title = list(text = "Click on Each Bar for Decomposition Details", yref = "paper", y = 0.5))
                    return(p)
                }
                
                data.frame(contribution = res_gender[, paste("LE Age", BarplotLE())]) %>% 
                    rownames_to_column() %>% mutate(curr_color = "#8073AC") %>%
                    plot_ly(x = ~gsub("Contribution Age", "", rowname), y = ~contribution, source = "BarplotLE_specific", 
                            type = "bar", marker = list(color = ~curr_color)) %>% 
                    config(displayModeBar = FALSE) %>% 
                    layout(barmode="overlay",
                           title = paste0("Contribution of Change in Life Expectancy (", 
                                          input$range_t[1], "-", input$range_t[2], ", ",
                                          input$heatGender, ", ", BarplotLE(), ", ", 
                                          input$heatCountry, ")"), 
                           font = list(size = 8),
                           xaxis = list(title = "Contribution Age", categoryarray = ~rowname, 
                                        categoryorder = "array", size = 8, tickangle = 0),
                           yaxis = list(title = "Contribution (Years)"))
                }
            else{
                if (input$heatAggregate == "FALSE"){ 
                    if (is.null(BarplotLE())){
                        p <- plotly_empty(type = "scatter", mode = "markers") %>%
                            config(displayModeBar = FALSE) %>%
                            layout(title = list(text = "Click on Each Bar for Decomposition Details", yref = "paper", y = 0.5))
                        return(p)
                    } 
                    
                    res_gender1 = res()[[1]]
                    res_gender2 = res()[[2]]
                    
                    d1 <- data.frame(contribution = res_gender1[, paste("LE Age", BarplotLE())]) %>% 
                        rownames_to_column() %>% mutate(curr_color = "#8073AC") 
                    d2 <- data.frame(contribution = res_gender2[, paste("LE Age", BarplotLE())]) %>% 
                        rownames_to_column() %>% mutate(curr_col = "#92CCDE") 
                    
                    p <- plot_ly(data = d1, x = ~gsub("Contribution Age", "", rowname), y = ~contribution, source = "BarplotLE_specific", 
                                 type = "bar", marker = list(color = ~curr_color), name = "Male") %>%  config(displayModeBar = FALSE)  %>% 
                         add_trace(data = d2, x = ~gsub("Contribution Age", "", rowname),
                                  y = ~contribution, type = "bar", name = "Female", marker = list(color = ~curr_col)) %>% 
                         layout(barmode="group",
                                title = paste0("Contribution of Change in Life Expectancy (", 
                                               input$range_t[1], "-", input$range_t[2], ", ",
                                               paste0(input$heatGender, collapse = "/"), ", ", BarplotLE(), ", ", 
                                               input$heatCountry, ")"), 
                                font = list(size = 8),
                                xaxis = list(title = "Contribution Age", categoryarray = ~rowname, 
                                             categoryorder = "array", size = 8, tickangle = 0),
                                yaxis = list(title = "Life Expectancy Change (Years)"))
                    p
                }
                else{
                    if (is.null(BarplotLE())){  
                        p <- plotly_empty(type = "scatter", mode = "markers") %>%
                            config(displayModeBar = FALSE) %>%
                            layout(title = list(text = "Click on Each Bar for Decomposition Details", yref = "paper", y = 0.5))
                        return(p)
                    }
                    res_gender <- res()[[1]] + res()[[2]]
                    
                    data.frame(contribution = res_gender[, paste("LE Age", BarplotLE())]) %>% 
                        rownames_to_column() %>% mutate(curr_color = "#8073AC") %>%
                        plot_ly(x = ~gsub("Contribution Age", "", rowname), y = ~contribution, source = "BarplotLE_specific", 
                                type = "bar", marker = list(color = ~curr_color)) %>% 
                        config(displayModeBar = FALSE) %>% 
                        layout(barmode="overlay",
                               title = paste0("Contribution of Change in Life Expectancy (", 
                                              input$range_t[1], "-", input$range_t[2], ", ",
                                              "Aggregate", ", ", BarplotLE(), ", ", 
                                              input$heatCountry, ")"), 
                               font = list(size = 8),
                               xaxis = list(title = "Contribution Age", categoryarray = ~rowname, 
                                            categoryorder = "array", size = 8, tickangle = 0),
                               yaxis = list(title = "Contribution (Years)"))
                }
            }
                
         })
        
        
        
        
       # outputOptions(output, 'HeatMap', suspendWhenHidden=FALSE)
        
        
        
        
        
        

    
})


shinyApp(ui, server)


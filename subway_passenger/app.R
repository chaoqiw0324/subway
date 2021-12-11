library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(plotly)
library(shinyTime)
library(gganimate)
library(gifski)

# color
line_color <- function(linename){
    color = case_when(
        linename %in% c("A","C","E") ~"blue",
        linename %in% c("S") ~"azure4",
        linename %in% c("B","D","F","M") ~"orange",
        linename %in% c("G") ~"green",
        linename %in% c("L") ~"gray",
        linename %in% c("J","Z") ~"brown",
        linename %in% c("N","Q","R","W") ~"gold",
        linename %in% c("1","2","3") ~"red",
        linename %in% c("4","5","6") ~"forestgreen",
        linename %in% c("7") ~"purple",
        linename %in% c("A","C","E") ~"blue",
    )
    
    return(color)
}




# 4 hour period passenger data
passenger1 <- read_csv("data/passenger_timeI.csv") 
passenger2 <- read_csv("data/passenger_timeII.csv")
passenger3 <- read_csv("data/passenger_timeIII.csv") 
passenger4 <- read_csv("data/passenger_timeIV.csv") 

passenger<- rbind(passenger1,passenger2,passenger3,passenger4)

# daily passenger data
passenger_df <- read_csv("data/passenger.csv") %>% 
    mutate(
        date = ymd(date)
    )



# crime data
subwaycrime_with_station <- 
    read_csv("data/subwaycrime_with_station.csv")  %>% 
    filter(
        cmplnt_to_dt != "NA"
    ) %>% 
    mutate(
        date = mdy(cmplnt_to_dt),
        linename = closest_line,
        station = closest_station,
        station_line = str_c(closest_station,closest_line,sep = " line:"),
    ) %>% 
    select(station_line,station,linename,ofns_desc,pd_desc,law_cat_cd,date)

# station data
subway_info_final3 <- read_csv("data/subway_info_final3.csv")

# ui
ui <- navbarPage(
    title = "Subway Passenger",
    # first Page
    tabPanel(
        "Main Page",
        mainPanel(
            h1("Introduction"),
            h4("This is a shiny app for Subway passenger flow animation and info lookup and a more detailed app on each line"),
            tags$hr(),
            h1("Instruction"),
            h4("This shiny app has two page,each page provide different functions"),
            h3("Single Date Page"),
            h4("In this page, you can choose the subway line, the date and specific time.After that you can get four return"),
            h5("1. A map show the station of the line, the round of the marker is based on the total passenger entering and exiting"),
            h5("2. A ordered barplot show the total passenger of each station"),
            h5("3. Total passenger of the line and each station"),
            h5("4. Crime reported on the date you choose"),
            h3("Date Ranger Page"),
            h4("In this page, you can choose the subway line, the date range. After that you can get four return"),
            h5("1. A plot of daily passenger of the line you choose (which will load slowly)"),
            h5("2. Daily total passenger of the line you choose "),
            h5("3. A barplot of top 10 busiest station each day (which will load slowly)"),
            h5("4. Daily total passenger of the station in the line"),
            tags$hr(),
            h1("More"),
            h4("You can get more information in our main",tags$a("website",href ="https://zheyanliu99.github.io/SubwayCrime/"))
        )
    ),
    tabPanel(
        "Single Date",
        sidebarLayout( 
            sidebarPanel(
                h2("Choose Subway and Time"),
                selectInput("subwayline", label = h4("Subway Lines"),
                            choices = list("1" = 1,"2" = 2, "3" = 3,"4" = 4,"5" = 5,"6" = 6,"7" = 7,"A" = "A","B" = "B","C" = "C","D" = "D","E" = "E","F" = "F", "G" = "G","J" = "J","L" = "L","M" = "M","N" = "N","Q" = "Q","R" = "R","S" = "S","W" = "W","Z" = "Z"),
                            selected = 1),
                selectInput("input_type", "Input type",
                            c("slider","date")),
                uiOutput("ui"),
                timeInput("time", "Time:", value = Sys.time()),
                actionButton("update","Update"),
                plotOutput("clock")
            ), 
            mainPanel(
                h2("Single Line of Subway Passenger"),
                tabsetPanel( 
                    tabPanel("Plot",  
                             h3("Daily Passenger of Line in Map"),
                             p("This is the map daily passenger of the line you are interested in "),
                             leafletOutput(outputId = "mymap1",width = "100%",height = "800")), 
                    tabPanel("Passenger(Plot)", 
                             h3("Daily Passenger of Each Station"),
                             p("This is the plot daily passenger of each station in the line you are interested in "),
                             plotlyOutput(outputId = "station"),
                    ),
                    tabPanel("Passenger(Table)", 
                             wellPanel(
                                 h3("Daily Passenger of Line"),
                                 DT::dataTableOutput(outputId = "mytable1"),
                                 h3("Daily Passenger of Each Station"),
                                 DT::dataTableOutput(outputId = "mytable2")
                             )
                    ),
                    tabPanel("Crime", 
                             h3("Daily Passenger of Each Station"),
                             p("This is the table of crime report in the line you are interested in "),
                             DT::dataTableOutput(outputId = "mytable3"))
                )
            )
        )
    ),
    tabPanel(
        "Date Range",
        sidebarLayout( 
            sidebarPanel(
                h2("Choose Subway and Time"),
                selectInput("subwayline1", label = h4("Subway Lines"),
                            choices = list("1" = 1,"2" = 2, "3" = 3,"4" = 4,"5" = 5,"6" = 6,"7" = 7,"A" = "A","B" = "B","C" = "C","D" = "D","E" = "E","F" = "F", "G" = "G","J" = "J","L" = "L","M" = "M","N" = "N","Q" = "Q","R" = "R","S" = "S","W" = "W","Z" = "Z"),
                            selected = 1),
                selectInput("input_type1", "Input type",
                            c("slider","daterange")),
                uiOutput("ui1"),
                actionButton("update1","Update")
            ), 
            mainPanel(
                h2("Single Line of Subway Passenger"),
                tabsetPanel( 
                    tabPanel("Line Animation",  
                             h3("Daily Passenger of Line"),
                             p("This is the animation of daily passenger of the line you are interested in "),
                             imageOutput(outputId = "animation1")), 
                    tabPanel("Datatable(line)", 
                             h3("Daily Passenger of Line"),
                             p("This is the datatable of daily passenger of the line you are interested in "),
                             DT::dataTableOutput("mytable4")),
                    tabPanel("Station Animation", 
                             h3("Daily Passenger of Each Station"),
                             p("This is the animation of daily passenger of each station in the line you are interested in "),
                             imageOutput(outputId = "animation2")),
                    tabPanel("Datatable(Station", 
                             h3("Daily Passenger of Each Station"),
                             p("This is the table of daily passenger of each station in the line you are interested in "),
                             DT::dataTableOutput("mytable5"))
                )
            )
        )
    )
    
)



server <- function(input, output) {
    # choose of input
    output$ui <- renderUI({
        if (is.null(input$input_type))
            return()
        
        switch(input$input_type,
               "slider" = sliderInput("dates", "Date",
                                      min = ymd("2021-01-01"),
                                      max = ymd("2021-10-31"),
                                      value = ymd("2021-01-01")),
               "date" = dateInput("dates", "Date",
                                  min = ymd("2021-01-01"),
                                  max = ymd("2021-10-31"),
                                  value = ymd("2021-01-01")
               )
               
        )
    })
    
    # input
    specific_date <- reactive({ymd(input$dates)})
    
    
    # action button
    specific_line <- eventReactive(input$update,{
        as.character(input$subwayline)
        })  
    
    specific_time <- eventReactive(input$update,{
        hour(hms(strftime(input$time,"%T")))*3600 + minute(hms(strftime(input$time,"%T"))) + second(hms(strftime(input$time,"%T")))
    })
    
    
    passenger_date <- eventReactive(input$update,{
        passenger %>% 
            filter(
                date == specific_date(),
                start_time < specific_time(),
                specific_time() < end_time,
                str_detect(linename,specific_line())
            ) %>% 
            mutate(
                total = entry_data + exit_data
            ) %>% 
            relocate(station,linename,date)
        
    }
    )
    
    subway_crime <- eventReactive(input$update,{
        subwaycrime_with_station %>% 
            filter(date == specific_date(),
                   str_detect(linename,specific_line()))
    })
    
    
    # clock
    output$clock <- renderPlot({
        
        time <- 
            data.frame(
                list(
                    x = c(
                        ifelse(hour(seconds_to_period(specific_time())) >= 12, 
                               hour(seconds_to_period(specific_time())) - 12 + (minute(seconds_to_period(specific_time())))/60, 
                               hour(seconds_to_period(specific_time()))      + (minute(seconds_to_period(specific_time())))/60 ), 
                        (minute(seconds_to_period(specific_time())))*0.2 
                    ), 
                    y=c(.9, 1.3)
                )
            )
        
        ggplot(time, aes(xmin = x, xmax = x+0.1, ymin = 0, ymax = y))+
            geom_rect(aes(alpha=0.5))+
            scale_x_continuous(limits=c(0,11.98333), breaks=0:11, 
                               labels=c(12, 1:11))+
            scale_y_continuous(limits=c(0,1.3)) + 
            scale_alpha() + 
            theme_bw()+
            coord_polar() + 
            theme(axis.text.y=element_blank(), axis.ticks=element_blank(), 
                  panel.grid.major=element_blank(), 
                  strip.background = element_rect(colour = 'white'),
                  legend.title = element_blank(),
                  legend.position = "none")
        
    })
    
    
    # maymap1: daily passenger in map
    
    output$mymap1 <- renderLeaflet({
        
        passenger_date <- 
            left_join(passenger_date(),subway_info_final3,by=c("station","linename")) %>% 
            mutate(linecolor = specific_line())
        
        if (specific_time() >= 7*3600 & specific_time() < 17*3600 ){
            passenger_date %>% 
                mutate(
                    click_label = 
                        str_c("<b>Station:", station, "</b><br>Line:", linename, " <br>Entry:", round(entry_data)," <br>Exit:", round(exit_data),"<br>Total:", round(total))
                ) %>% 
                leaflet() %>% 
                addProviderTiles(providers$CartoDB.Positron) %>% 
                addCircleMarkers(~long, 
                                 ~lat, 
                                 radius = ~log(total)/2, 
                                 color = ~line_color(linecolor), 
                                 popup = ~click_label)
        } else {
            passenger_date %>% 
                mutate(
                    click_label = 
                        str_c("<b>Station:", station, "</b><br>Line:", linename, " <br>Entry:", round(entry_data)," <br>Exit:", round(exit_data),"<br>Total:", round(total))
                ) %>% 
                leaflet() %>% 
                addProviderTiles(providers$CartoDB.DarkMatter) %>% 
                addCircleMarkers(~long, 
                                 ~lat, 
                                 radius = ~log(total)/2, 
                                 color = ~ line_color(linecolor), 
                                 popup = ~click_label)
        }
        
        
    })
    
    # station: plot of daily station passenger
    output$station <- renderPlotly({
        passenger_date <- 
            left_join(passenger_date(),subway_info_final3,by = c("station","linename")) %>% 
            mutate(linecolor = specific_line())
        
        passenger_date %>% 
            mutate(
                entry_single = round(entry_data/nchar(linename)),
                exit_single  = round(exit_data/nchar(linename)),
                total        = entry_single + exit_single,
                station_line      = fct_reorder(station_line,total)
            ) %>% 
            plot_ly(
                x = ~station_line, y = ~total,type = "bar",alpha = 0.5, color = ~line_color(linecolor), width = 800, height = 800
            ) %>% 
            layout(autosize = F,showlegend = FALSE) 
        
    })
    
    #mytable 1: daily passenger table of line passenger
    output$mytable1 <- DT::renderDataTable({
        
        Datatable <- 
            passenger_date() %>% 
            mutate(
                entry_single = round(entry_data/nchar(linename)),
                exit_single = round(exit_data/nchar(linename)),
                total = entry_single + exit_single
                
            ) %>% 
            group_by(date,start_time,end_time) %>% 
            summarize(Entry = sum(entry_single,na.rm = TRUE),
                      Exit  = sum(exit_single,na.rm = TRUE),
                      Total = sum(total,na.rm = TRUE),
            ) %>% 
            mutate(Entry = unlist(Entry),
                   Exit  = unlist(Exit),
                   Total = unlist(Total)) %>% 
            ungroup() %>% 
            mutate(
                start_time = str_c(start_time/3600,":00:00"),
                end_time = str_c(end_time/3600,":00:00")
            )
        
        
        DT::datatable(Datatable, 
                      options = list(lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')),pageLength = 5) 
        )
    })
    
    
    
    
    
    
    #mytable 2: daily passenger table of passenger
    output$mytable2 <- DT::renderDataTable({
        
        Datatable <- 
            passenger_date() %>% 
            mutate(
                entry_single = round(entry_data/nchar(linename)),
                exit_single = round(exit_data/nchar(linename)),
                total = entry_single + exit_single
            ) %>% 
            group_by(date,station,start_time,end_time) %>% 
            summarize(
                Entry = sum(entry_single,na.rm = TRUE),
                Exit  = sum(exit_single,na.rm = TRUE),
                Total = sum(total,na.rm = TRUE),
            ) %>% 
            mutate(
                Entry = unlist(Entry),
                Exit  = unlist(Exit),
                Total = unlist(Total)
            ) %>% 
            ungroup()%>% 
            mutate(
                start_time = str_c(start_time/3600,":00:00"),
                end_time = str_c(end_time/3600,":00:00")
            )
        
        
        
        DT::datatable(Datatable, 
                      options = list(lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),pageLength = 15) 
        )
    })
    
    
    #mytable 3: daily passenger table of crime ({
    output$mytable3 <- DT::renderDataTable({
        Datatable3 <- 
            subway_crime() %>%
            select(-station_line) %>% 
            arrange(date) %>% 
            relocate(date)
        
        
        
        
        DT::datatable(Datatable3, 
                      options = list(lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),pageLength = 15) 
        )
    })
    
# second page
    # choose of input
    output$ui1 <- renderUI({
        if (is.null(input$input_type1))
            return()
        
        switch(input$input_type1,
               "slider" = sliderInput("daterange", "Date",
                                      min = ymd("2021-01-01"),
                                      max = ymd("2021-10-31"),
                                      value = c(ymd("2021-01-01"),ymd("2021-01-02"))),
               "daterange" =dateRangeInput("daterange", label = h4("Date range"),
                                           start  = "2021-01-01",
                                           end    = "2021-01-02",
                                           min    = "2021-01-01",
                                           max    = "2021-10-31"),
        )
    })
    
    # input 
    specific_line1 <- reactive({as.character(input$subwayline1)})  
    start_date <- reactive({ymd(input$daterange[1])}) 
    end_date <- reactive({ ymd(input$daterange[2])})
    
    # action button
    p <- eventReactive(input$update1,{
        passenger_df %>% 
            filter(start_date() <= date & date <= end_date(),
                   str_detect(linename,specific_line1())
            ) %>% 
            mutate(
                entry_single = round(entry_data/nchar(linename)),
                exit_single = round(exit_data/nchar(linename)),
                total = entry_single + exit_single
            )
    })
    
    # daily passenger gif of line
    output$animation1 <- renderImage({ 
        
        
        outfile <- tempfile(fileext = '.gif')
        
        animation <- p() %>% 
            group_by(date) %>% 
            summarize(
                passenger = sum(total,na.rm = TRUE)
            ) %>%  
            ggplot(aes(x = date,y = passenger)) +
            geom_line() +
            transition_reveal(date) +
            ease_aes('linear')
        
        
        
        anim_save("outfile.gif", animate(animation, duration = 5, fps = 20, renderer = gifski_renderer()))
        
        list(src = "outfile.gif",
             contentType = 'image/gif',
             width = 700,
             height = 700
             # alt = "This is alternate text"
        )
    },
    deleteFile = TRUE)
    
    # daily passenger table of line passenger
    output$mytable4 <- DT::renderDataTable ({
        
        Datatable <- 
            p() %>% 
            group_by(date) %>% 
            summarize(Entry = sum(entry_single,na.rm = TRUE),
                      Exit  = sum(exit_single,na.rm = TRUE),
                      Total = sum(total,na.rm = TRUE),
            ) %>% 
            mutate(Entry = unlist(Entry),
                   Exit  = unlist(Exit),
                   Total = unlist(Total)) %>% 
            ungroup()
        
        
        DT::datatable(Datatable, 
                      options = list(lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),pageLength = 15) 
        )
    })
    
    
    
    # daily passenger gif of station
    output$animation2 <- renderImage({
        
        outfile <- tempfile(fileext='.gif')
        
        animation <- 
            p() %>% 
            group_by(date) %>% 
            mutate(
                rank = rank(-total,ties.method = "max"),
                Value_lbl = paste0(" ", round(total/1e3))
            ) %>% 
            filter(
                rank <= 10
            ) %>% 
            ungroup() %>% 
            ggplot(aes(rank, group = station, fill = as.factor(station_line), colour = as.factor(station_line))) +
            # 绘制柱子
            geom_tile(aes(y = total/2, height = total, width = 0.8), alpha = 0.8, colour = NA) +
            # 添加地区名称
            geom_text(aes(y = 0, label = paste(station, " ")), vjust = 0.2, hjust = 1, size = 8) +
            # 添加柱子数值
            geom_text(aes(y = total, label = Value_lbl, hjust = 0), size = 8) +
            # 交换坐标轴，让柱子横向“赛跑”
            coord_flip(clip = "off", expand = FALSE) +
            scale_x_reverse() +
            # 调整各种主题细节
            guides(colour = "none", fill = "none") +
            theme(axis.line = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  legend.position = "none",
                  panel.background = element_blank(),
                  panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_line(),
                  panel.grid.minor.x = element_line(),
                  plot.title = element_text(size = 32, hjust = 0.5, face = "bold", colour = "black", vjust = -1),
                  plot.subtitle=element_text(size = 32, hjust = 0.5, face = "italic", colour = "black"),
                  plot.caption = element_text(size = 16, hjust = 0.5, face = "italic", colour = "black"),
                  plot.background = element_blank(),
                  plot.margin = margin(1, 3, 1, 6.5, "cm")) +
            transition_states(date, transition_length = 2, state_length = 1) +
            view_follow(fixed_y = TRUE)  + 
            labs(title = "Passenger: {closest_state}",
                 subtitle = " ",
                 caption = "Passenger in 1000")
        
        
        anim_save("outfile.gif", animate(animation), duration = 30, fps = 20, renderer = gifski_renderer())
        
        list(src = "outfile.gif",
             contentType = 'image/gif',
             width = 700,
             height = 700
             # alt = "This is alternate text"
        )}, 
        deleteFile = TRUE)
    
    
    # daily passenger table of passenger
    output$mytable5 <- DT::renderDataTable ({
        
        Datatable <- 
            p() %>% 
            group_by(date,station) %>% 
            summarize(Entry = sum(entry_single,na.rm = TRUE),
                      Exit  = sum(exit_single,na.rm = TRUE),
                      Total = sum(total,na.rm = TRUE),
            ) %>% 
            mutate(Entry = unlist(Entry),
                   Exit  = unlist(Exit),
                   Total = unlist(Total)) %>% 
            ungroup()
        
        
        DT::datatable(Datatable, 
                      options = list(lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),pageLength = 15) 
        )
    })
    
    
    
    
    
    
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
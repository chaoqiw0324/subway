library(shiny)
library(tidyverse)
library(lubridate)
library(gganimate)

# passenger data
passenger <- read_csv("data/passenger.csv") %>% 
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
  select(station_line,linename,ofns_desc,pd_desc,law_cat_cd,date)


ui <- navbarPage(
  title = "Subway Passenger",
  tabPanel(
    "Line",
  sidebarLayout( 
    sidebarPanel(
      h2("Choose Subway and Time"),
      selectInput("subwayline", label = h4("Subway Lines"),
                             choices = list("1" = 1,"2" = 2, "3" = 3,"4" = 4,"5" = 5,"6" = 6,"7" = 7,"A" = "A","B" = "B","C" = "C","D" = "D","E" = "E","F" = "F", "G" = "G","J" = "J","L" = "L","M" = "M","N" = "N","Q" = "Q","R" = "R","S" = "S","W" = "W","Z" = "Z"),
                             selected = 1),
      dateRangeInput("dates", label = h4("Date range"),
                     start  = "2021-01-01",
                     end    = "2021-01-02",
                     min    = "2021-01-01",
                     max    = "2021-10-31")
    ), 
    mainPanel(
      h2("Single Line of Subway Passenger"),
      tabsetPanel( 
        tabPanel("Line Animation",  
                 h3("Daily Passenger of Line"),
                 p("This is the animation of daily passenger of the line you are interested in "),
                 imageOutput(outputId = "daily")), 
        tabPanel("Datatable", 
                 h3("Daily Passenger of Line"),
                 p("This is the datatable of daily passenger of the line you are interested in "),
                 DT::dataTableOutput("mytable1")),
        tabPanel("Animation", 
                 h3("Daily Passenger of Each Station"),
                 p("This is the animation of daily passenger of each station in the line you are interested in "),
                 imageOutput(outputId = "station")),
        tabPanel("Datatable", 
                 h3("Daily Passenger of Each Station"),
                 p("This is the table of daily passenger of each station in the line you are interested in "),
                 DT::dataTableOutput("mytable2"))
        )
      )
    )
  ),
  tabPanel(
    "Crime",
    sidebarLayout( 
      sidebarPanel(
        h2("Choose Subway and Time"),
        selectInput("subwayline", label = h4("Subway Lines"),
                    choices = list("1" = 1,"2" = 2, "3" = 3,"4" = 4,"5" = 5,"6" = 6,"7" = 7,"A" = "A","B" = "B","C" = "C","D" = "D","E" = "E","F" = "F", "G" = "G","J" = "J","L" = "L","M" = "M","N" = "N","Q" = "Q","R" = "R","S" = "S","W" = "W","Z" = "Z"),
                    selected = 1),
        dateRangeInput("dates", label = h4("Date range"),
                       start  = "2021-01-01",
                       end    = "2021-01-02",
                       min    = "2021-01-01",
                       max    = "2021-10-31")
      ), 
      mainPanel(
        h2("Single Line of Subway Crime"),
        tabPanel("Datatable", 
                 DT::dataTableOutput("mytable3")
        )
      )
    )
  ),
  tabPanel(
    "Home",
    href= "http://www.git.com"
  )
)



server <- function(input, output) {
  specific_line <- reactive({as.character(input$subwayline)})  
  start_date <- reactive({ymd(input$dates[1])}) 
  end_date <- reactive({ ymd(input$dates[2])})

  # daily passenger gif of line
  output$daily <- renderImage({ 

    
    outfile <- tempfile(fileext='.gif')
    
    p <- passenger %>% 
      filter(start_date() <= date & date <= end_date(),
             str_detect(linename,specific_line())
      ) %>% 
      mutate(
        entry_single = entry_data/nchar(linename),
        exit_single = exit_data/nchar(linename),
        total = entry_single + exit_single
      ) %>% 
      group_by(date) %>% 
      summarize(
        passenger = sum(total,na.rm = TRUE)
      ) %>%  
      ggplot(aes(x = date,y = passenger)) +
      geom_line() +
      transition_reveal(date) +
      ease_aes('linear')
                       
    
    anim_save("outfile.gif", animate(p))
    
    list(src = "outfile.gif",
         contentType = 'image/gif',
         width = 700,
         height = 700
         # alt = "This is alternate text"
         )
    },
    deleteFile = TRUE)
  
  # daily passenger table of line passenger
  output$mytable1 <- DT::renderDataTable ({
    
    Datatable <- 
      passenger %>% 
      filter(start_date() <= date & date <= end_date(),
             str_detect(linename,specific_line())
      ) %>% 
      mutate(
        entry_single = entry_data/nchar(linename),
        exit_single = exit_data/nchar(linename),
        total = entry_single + exit_single
      ) %>% 
      group_by(date) %>% 
      summarize(passenger = sum(total,na.rm = TRUE)) %>% 
      mutate(passenger = unlist(passenger)) %>% 
      ungroup()

    
    DT::datatable(Datatable, 
                  options = list(lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),pageLength = 15) 
    )
  })
  

  
  # daily passenger gif of station
  output$station <- renderImage({
    
    outfile <- tempfile(fileext='.gif')
    
    p <- 
      passenger %>% 
      filter(start_date() <= date & date <= end_date(),
             str_detect(linename,specific_line())
      ) %>% 
      mutate(
        entry_single = entry_data/nchar(linename),
        exit_single = exit_data/nchar(linename),
        total = entry_single + exit_single
      ) %>% 
      group_by(date,station) %>% 
      summarize(
        passenger = sum(total,na.rm = TRUE)
      ) %>% 
      ungroup() %>% 
      mutate(
        rank = order(order(passenger, decreasing = TRUE)),
        station = fct_reorder(station, passenger)
        )%>% 
      ggplot(aes(x = station,y = passenger)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Year: {frame_time}")+
      transition_time(date) +
      ease_aes('linear')

    
    anim_save("outfile.gif", animate(p))
    
    list(src = "outfile.gif",
         contentType = 'image/gif',
         width = 700,
         height = 700
         # alt = "This is alternate text"
    )}, 
    deleteFile = TRUE)


# daily passenger table of passenger
  output$mytable2 <- DT::renderDataTable ({
  
    Datatable <- 
      passenger %>% 
    filter(start_date() <= date & date <= end_date(),
           str_detect(linename,specific_line())
    ) %>% 
    mutate(
      entry_single = entry_data/nchar(linename),
      exit_single = exit_data/nchar(linename),
      total = entry_single + exit_single
    ) %>% 
    group_by(date,station) %>% 
    summarize(passenger = sum(total,na.rm = TRUE)) %>% 
    mutate(passenger = unlist(passenger)) %>% 
    ungroup()

  
  DT::datatable(Datatable, 
                options = list(lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),pageLength = 15) 
  )
  })
  

# daily passenger table of crime ({
  output$mytable3 <- DT::renderDataTable ({
  Datatable3 <- 
    subwaycrime_with_station %>% 
    filter(start_date() <= date & date <= end_date(),
           str_detect(linename,specific_line())
    ) %>% 
    arrange(date,station_line)

    

  
  DT::datatable(Datatable3, 
                options = list(lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),pageLength = 15) 
  )
})
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
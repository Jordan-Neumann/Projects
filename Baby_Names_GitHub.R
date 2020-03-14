if(!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, tidyverse, data.table, dplyr, ggplot2, shiny)


#Girl Names
girl_names <- read_xlsx('Top100_Popular_Baby_Names.xlsx', sheet = 1)
#Dropping Rows
girl_names <- girl_names[,-1]
girl_names <- girl_names[-c(1:4,6:7),]
girl_names <- girl_names[-1,]
#Dropping Columns
girl_names <- girl_names[,-c(seq(from = 4, to = 193, by =3))]
girl_names <- girl_names[,-1]
girl_names <- girl_names[,-c(seq(from = 2, to = 130, by =2))]
#Year Names
colnames(girl_names) <- c(1954:2018)



#Boy Names
boy_names <- read_xlsx('Top100_Popular_Baby_Names.xlsx', sheet = 2)
#Dropping Rows
boy_names <- boy_names[,-1]
boy_names <- boy_names[-c(1:4,6:7),]
boy_names <- boy_names[-1,]
#Droppingi Columns
boy_names <- boy_names[,-c(seq(from = 4, to = 193, by =3))]
boy_names <- boy_names[,-1]
boy_names <- boy_names[,-c(seq(from = 2, to = 130, by =2))]
#Year Names
colnames(boy_names) <- c(1954:2018)



#Girls Names For Chart Output
girl_names_chart <- read_xlsx('Top100_Popular_Baby_Names.xlsx', sheet = 1)
#Dropping Rows
girl_names_chart <- girl_names_chart[,-1]
girl_names_chart <- girl_names_chart[-c(1:6),]
#Dropping Columns
girl_names_chart <- girl_names_chart[,-c(seq(from = 4, to = 193, by =3))]
girl_names_chart <- girl_names_chart[,-1]

colnames(girl_names_chart) <- c(1:130) #Changing all column names
girl_names_chart <- girl_names_chart[-c(101:105),] #Removing NAs
new_girl_counts <- girl_names_chart[,-c(seq(from = 1,to = 130, by = 2))] #Remove names
new_girl_counts <- data.frame("2"=unlist(new_girl_counts, use.names = FALSE)) #All count data is now in 1 column
new_girl_names <- girl_names_chart[,-c(seq(from = 2,to = 130, by = 2))] #Remove names
new_girl_names <- data.frame("V2"=unlist(new_girl_names, use.names = FALSE)) #All name data is now in 1 column
new_girl <- data.frame(new_girl_names, new_girl_counts) #Two columns above combined
colnames(new_girl) <- (c("Name", "Count")) #New column names
n <- 100
year_column <- data.frame(rep(1954:2018, each = n)) #Column of years
colnames(year_column) <- c("Year") #Changing column name
new_girl_final <- data.frame(year_column, new_girl) #Combining
all_girls_names <- unique(new_girl$Name)
all_girls_names <- all_girls_names %>% sort(decreasing = FALSE)
new_girl_final$Count <- as.numeric(new_girl_final$Count) #Count to numeric
str(new_girl_final)



#Boy Names Chart
boy_names_chart <- read_xlsx('Top100_Popular_Baby_Names.xlsx', sheet = 2)
#Removing Rows
boy_names_chart <- boy_names_chart[,-1]
boy_names_chart <- boy_names_chart[-c(1:6),]
#Removing Columns
boy_names_chart <- boy_names_chart[,-c(seq(from = 4, to = 193, by =3))]
boy_names_chart <- boy_names_chart[,-1]

colnames(boy_names_chart) <- c(1:130) #Changing all column names
boy_names_chart <- boy_names_chart[-c(101:105),] #Removing NAs
new_boy_counts <- boy_names_chart[,-c(seq(from = 1,to = 130, by = 2))] #Remove names
new_boy_counts <- data.frame("2"=unlist(new_girl_counts, use.names = FALSE)) #All count data is now in 1 column
new_boy_names <- boy_names_chart[,-c(seq(from = 2,to = 130, by = 2))] #Remove names
new_boy_names <- data.frame("V2"=unlist(new_boy_names, use.names = FALSE)) #All name data is now in 1 column
new_boy <- data.frame(new_boy_names, new_boy_counts) #Two columns above combined
colnames(new_boy) <- (c("Name", "Count")) #New column names
n <- 100
boy_year_column <- data.frame(rep(1954:2018, each = n)) #Column of years
colnames(boy_year_column) <- c("Year")  #Changing column name
new_boy_final <- data.frame(year_column, new_boy) #Combining
all_boy_names <- unique(new_boy$Name)
all_boy_names <- all_boy_names %>% sort(decreasing = FALSE)
new_boy_final$Count <- as.numeric(new_boy_final$Count) #Count to numeric





ui <- fluidPage(
  
  tabsetPanel(   
     
    
    tabPanel("Popular Girl Names",
             sidebarPanel(selectInput("ID", "Select Year", choices = 1954:2018)),
             mainPanel(h4('Top 10 Names Per Year'), align = "center", tableOutput("girls"))), 
    
    tabPanel("Popular Boy Names", 
             sidebarPanel(selectInput("BID", "Select Year", choices = 1954:2018)),
             mainPanel(h4('Top 10 Names Per Year'), align = "center", tableOutput("boys"))),
    
    
    tabPanel("Girl Name Trends", 
             sidebarPanel(selectInput("GID","Select Name", choices = all_girls_names)),
             mainPanel(plotOutput("Plot"))),
    
    tabPanel("Boy Names Trends",
             sidebarPanel(selectInput("Last","Select Name", choices = all_boy_names)),
             mainPanel(plotOutput("Plot2")))
    
    
    
    
  )
  
)


server <- function(input, output){
  
  output$girls <-renderTable(head(girl_names[input$ID], n = 10), align = 'c', striped = TRUE, table.title = "HeyNow")
  
  output$boys <- renderTable(head(boy_names[input$BID], n = 10), align = 'c', striped = TRUE)
  
  output$Plot <- renderPlot({
    df <- subset(new_girl_final, new_girl_final$Name == input$GID)
    
    ggplot(data = df, aes(Year,Count)) + geom_point(shape = "triangle") + ggtitle("Girl Names Over Time") + 
      theme(plot.title = element_text(size=18, hjust = .5),
            axis.title = element_text(size = 14),
            axis.title.x = element_text(margin=margin(t = 10)),
            axis.title.y = element_text(margin=margin(r = 10))) + 
      scale_y_continuous(breaks = c(1, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 510), limits = c(0,510)) +
      scale_x_continuous("Year", breaks = c(1954,1960,1970,1980,1990,2000,2010,2018), limit = c(1954,2018))})  
?scale_x_continuous
    
  output$Plot2 <- renderPlot({
      df2 <- subset(new_boy_final, new_boy_final$Name == input$Last)
      
    ggplot(data = df2, aes(Year,Count)) + geom_point(shape = "triangle") + ggtitle("Boy Names Over Time") +
      theme(plot.title = element_text(size=18, hjust = .5),
            axis.title = element_text(size = 14),
            axis.title.x = element_text(margin=margin(t = 10)),
            axis.title.y = element_text(margin=margin(r = 10))) +
      scale_y_continuous(breaks = c(1, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 510), limits = c(0,510)) +
      scale_x_continuous("Year", breaks = c(1954,1960,1970,1980,1990,2000,2010,2018), limit = c(1954,2018)) 
        
    
    
  })
  
}


shinyApp(ui = ui, server = server)



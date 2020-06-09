library(tidyverse)
library(jsonlite)
library(shiny)
library(plotly)
library(httr)
library(rvest)

##########
r <- GET("https://ghibliapi.herokuapp.com/films/")
stop_for_status(r)


json <- content(r, as = "text", encoding = "UTF-8")
mydata1 <- fromJSON(json) %>% select(id, title, description, director, producer, release_date, rt_score, url)
mydata1$title <- gsub("\u00A0", " ", mydata1$title , fixed =TRUE)

score <- mydata1 %>% 
    select(rt_score) %>% 
    type.convert()

mydata2 <- select(mydata1, -c(rt_score))
mydata <- cbind(mydata2, score)

##########
SGhtml <- read_html("https://www.studioghibli.com.au/")

links <- SGhtml %>%
    html_nodes("div.poster-holder") %>%
    html_nodes("a") %>%
    html_attr("href")

titles <- SGhtml %>%
    html_nodes("div.poster-holder") %>%
    html_nodes("a") %>%
    html_attr("title")

titles <- sub(titles[6], mydata$title[13], titles)
titles <- sub(titles[7], mydata$title[1], titles)
titles <- sub(titles[13], mydata$title[4], titles)
titles <- sub(titles[19], mydata$title[14], titles)
titles <- sub(titles[22], mydata$title[17], titles)


links_table <- tibble(title = titles, links)
movies <- left_join(mydata, links_table, by = "title", na.rm())

##########
RThtml <- read_html("https://editorial.rottentomatoes.com/guide/all-studio-ghibli-movies-ranked-by-tomatometer/")

RTlinks <- RThtml %>%
    html_nodes("div.article_movie_title") %>%
    html_nodes("div") %>%
    html_nodes("h2") %>%
    html_nodes("a") %>%
    html_attr("href")

RTnames <- RThtml %>%
    html_nodes("div.article_movie_title") %>%
    html_nodes("div") %>%
    html_nodes("h2") %>%
    html_nodes("a") %>%
    html_text()

RT_titles <- str_replace(RTnames, " \\(.*\\)", "")

RT_titles <- sub(RT_titles[1], mydata$title[14], RT_titles)
RT_titles <- sub(RT_titles[4], mydata$title[17], RT_titles)
RT_titles <- sub(RT_titles[16], mydata$title[16], RT_titles)

RTlinks_table <- tibble(title = RT_titles, RTlinks)

movies_full <- left_join(movies, RTlinks_table, by = "title", na.rm())

##########


youtube_ids <- character(length(movies_full$links))
for (i in 1:length(movies_full$links)){
    movie_page <- read_html(movies_full$links[i])
    
    youtube_ids[i] <- movie_page %>%
        html_nodes("div.film-cta-buttons") %>%
        html_node("a") %>%
        html_attr("data-video-id")
    
}



youtube <- cbind(movies_full, youtube_ids)

##########
rating_data <- movies_full %>% 
    mutate(rt_score, score_rating = case_when(rt_score %in% (90:100) ~ "90-100",
                                              rt_score %in% (80:89) ~ "80-89",
                                              rt_score %in% (70:79) ~ "70-79",
                                              rt_score %in% (0:70) ~ "Less than 70"))

####################################################################################################

ui <- fluidPage(
    
    titlePanel(title=div(img(src="Studio_Ghibli_logo.png", height = 200, width = 400))),
    
    tabsetPanel(
        
        # tab 1
        tabPanel("Studio Ghibli Collection", 
                 sidebarPanel(
                     selectInput(inputId = "movie_director",
                                 label = "Select movie's director:",
                                 choices = unique(movies_full$director)
                                 ),
                     uiOutput("director_out")
                     ),
                 
                 mainPanel(dataTableOutput("movie_info"))
                 ),
        
        # tab 2
        tabPanel("Movies' Scores",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(inputId = "movie_score",
                                     label = "Select score range:",
                                     choices = unique(rating_data$score_rating)
                         )
                     ),
                     
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Score Details", dataTableOutput("movierating_info")),
                             tabPanel("Director Distribution Based on Rating Score", plotlyOutput(outputId = "pieChart"))
                             )
                         )
                     )
                 ),
        
        # tab 3
        tabPanel("Movies' Trailers",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(inputId = "movie_name",
                                     label = "Select movie:",
                                     choices = unique(youtube$title)
                                     )
                         ),
                     mainPanel(uiOutput("video"))
                     )
                 )
        
    )
)



####################################################################################################

server <- function(input, output, session) {
    
    #Filter based on input
    director_data <- reactive({
        movies_full %>% filter(director == input$movie_director)
    })
    
    producer_data <- reactive({
        director_data() %>% filter(producer == input$movie_producer)
    })
    
    movie_data <- reactive({
        youtube %>% filter(title == input$movie_name)
    })
    
    score_data <- reactive({
        rating_data %>% filter(score_rating == input$movie_score)
    })
    
    # Render data table for selected director
    
    output$director_out <- renderUI({
        selectInput(inputId = "movie_producer",
                    label = "Select movies's producer", 
                    c('-', director_data() %>% pull(producer) %>% unique() %>% sort()))
    
    })
    
    
    # tab 1    
    output$movie_info <- renderDataTable({ 
        
        if(is.null(input$movie_producer) || input$movie_producer == "-"){
            my_table <- director_data() %>%
                select(title, director, producer, release_date, description, links) %>% 
                mutate(links = paste0("<a href='", links,"' target='_blank'>", links,"</a>")) %>%
                rename(Title = title, 'Release Year' = release_date, Description = description, Director = director, Producer = producer, Link = links)
            
            my_table
        } else {
            my_table <- producer_data() %>%
                select(title, director, producer, release_date, description, links) %>% 
                mutate(links = paste0("<a href='", links,"' target='_blank'>", links,"</a>")) %>%
                rename(Title = title, 'Release Year' = release_date, Description = description, Director = director, Producer = producer, Link = links)
            
            my_table
        }
        
    }, escape = FALSE)       
    
    
    # tab 2
    output$movierating_info <- renderDataTable({
        my_ratingtable <- score_data() %>%
            select(title, release_date, rt_score, RTlinks) %>% 
            mutate(RTlinks = paste0("<a href='", RTlinks,"' target='_blank'>", RTlinks,"</a>")) %>%
            rename(Title = title, 'Release Year' = release_date, 'Rotten Tomato Score' = rt_score, 'Rotten Tomato Link' = RTlinks)
        
        my_ratingtable
        
        }, escape = FALSE)
    
    
    output$pieChart <- renderPlotly({
        fig <- plot_ly(score_data(), labels = ~director, type = 'pie') %>% 
            layout(title = paste('Piechat of Director Based on', input$movie_score, 'Score Range'),
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    
    # tab 3
    output$video <- renderUI({
        req(movie_data())
        
        select_movie = input$movie_name
        urls = movie_data()$youtube_ids
        url = urls[select_movie[length(select_movie)]] 
        
        HTML(paste0('<iframe width="800" height="400" src="https://www.youtube.com/embed/',urls,'" frameborder="0" allowfullscreen></iframe>'))

    })
    
    
    
    
}


####################################################################################################
shinyApp(ui = ui, server = server)

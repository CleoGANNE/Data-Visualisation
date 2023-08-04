library(DT)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)

load("linkedin_au.RData")

ui <- dashboardPage(
  dashboardHeader(title = "AU Talents on Linkedin", titleWidth = 300),
  dashboardSidebar(
    
    radioButtons("radio_gender", label = ("Select gender"),
                 choices = list("Female" = "female", 
                                "Male" = "male",
                                # "Unknown" = "unknown", 
                                "All" = "all"), 
                 selected = "all"),
    
    sliderInput("slider_age", label = ("Select age"), min = min(linkedin_au$age), 
                max = max(linkedin_au$age), value = range(linkedin_au$age)),
    
    sliderInput("slider_followers", label = ("User with the number of followers"), 
                min = 0, 
                max = 10000, value = c(0,10000)),
    
    checkboxInput("check_premium", label = "Premium account", value = FALSE),
    
    checkboxGroupInput("check_company_size", label = "Company size", 
                       choices = levels(linkedin_au$company_scale_categories),
                       selected = levels(linkedin_au$company_scale_categories)),
    
    radioButtons("radio_still_in_job", label = "Still in job", 
                 choices = c("Yes", "Non", "All"), selected = "All")
    
  ),
  dashboardBody(
    
    
    fluidRow(
      infoBoxOutput("infobox_number_people"),
      infoBoxOutput("infobox_duration_job"), 
      infoBoxOutput("infobox_premium")
      
    ),
    
    HTML("<br>"),
    
    tabsetPanel(type = "pills",
                
                tabPanel("Dashboard", 
                         
                         HTML("<br>"),
                         
                         fluidRow(
                           column(width = 6,
                                  box(width = NULL,
                                      solidHeader = TRUE, status = "primary",
                                      title = "Age Distribution",
                                      plotlyOutput("age_distribution", height=300)
                                  )
                           ),
                           column(width = 6,
                                  box(width = NULL,
                                      solidHeader = TRUE, status = "primary",
                                      title = "Gender Distribution",
                                      plotlyOutput("gender_distribution", height=300)
                                  )
                           )
                         ),
                         
                         fluidRow(
                           
                           column(width = 6,
                                  box(width = NULL,
                                      solidHeader = TRUE, status = "primary",
                                      title = "Duration Job Distribution",
                                      plotlyOutput("duration_job_distribution", height=300)
                                  )
                           ),
                           
                           column(width = 6,
                                  box(width = NULL,
                                      solidHeader = TRUE, status = "primary",
                                      title = "Company scale",
                                      plotlyOutput("plot_company_scale", height=300)
                                  )
                           )
                         )
                ),
                tabPanel("Company ranking",
                         
                         HTML("<br>"),
                         
                         fluidRow(
                           
                           dataTableOutput("table_company")
                           
                         )
                )
    )
    
    
  )
)


server <- function(input, output){
  
  # Filtered table reactive to the input on the sidebar
  mydata_filtered <- reactive({
    select_gender <- input$radio_gender
    if(select_gender == "all"){
      select_gender = levels(linkedin_au$gender)
    }
    
    mydata <- linkedin_au %>% 
      filter(gender %in% select_gender,
             between(age, min(input$slider_age), max(input$slider_age)),
             between(followers, 
                     min(input$slider_followers), 
                     max(max(input$slider_followers),max(linkedin_au$followers))),
             company_scale_categories %in% input$check_company_size)
    
    if(input$check_premium){
      mydata <- mydata %>%
        filter(premium == 1)
    }
    
    if(input$radio_still_in_job == "Yes"){
      mydata <- mydata %>%
        filter(still_in_job)
    }else if(input$radio_still_in_job == "No"){
      mydata <- mydata %>%
        filter(!still_in_job)
    }
    
    mydata
  })
  
  mydata_persona_filtered <- reactive({
    mydata_filtered() %>%
      select(id_picture, gender, age, followers, premium) %>%
      unique()
  })
  
  mydata_company_filtered <- reactive({
    mydata_filtered() %>%
      select(starts_with("company")) %>%
      unique() %>% 
      left_join(mydata_filtered() %>% 
                  count(company, sort = TRUE, name = "nb_jobs"),
                by = "company") %>% 
      left_join(mydata_filtered() %>% 
                  filter(still_in_job) %>%
                  count(company, sort = TRUE, name = "nb_current_jobs"),
                by = "company")
  })
  
  output$infobox_premium <- renderInfoBox({
    infoBox(
      "Premium Account",
      paste0(round(100*mean(mydata_persona_filtered()$premium), 1),"%"),
      icon = icon("linkedin"),
      width = 4
    )
  })
  
  output$infobox_duration_job <- renderInfoBox({
    infoBox(
      "Median of Job Duration",
      paste(round(median(mydata_filtered()$duration_job, na.rm = TRUE),2), "years"),
      icon = icon("hourglass-half"),
      width = 4
    )
  })
  
  output$infobox_number_people <- renderInfoBox({
    infoBox(
      "Number of people",
      paste(n_distinct(mydata_persona_filtered()$id_picture), "people"),
      icon = icon("user-friends"),
      width = 4
    )
  })
  
  output$age_distribution <- renderPlotly({
    my_table <- mydata_persona_filtered()
    my_plot <- ggplot(my_table) +
      geom_histogram(aes(x=age,
                         text = paste(..count.., "people from", x-1,"to",x+1,"years old")), fill = "#0e76a8", 
                     breaks = seq(floor(min(my_table$age)),
                                  floor(max(my_table$age)),
                                  by = 2)) +
      theme_classic() +
      ylab("")
    ggplotly(my_plot, tooltip = "text")
  })
  
  output$gender_distribution <- renderPlotly({
    my_plot <- mydata_persona_filtered() %>%
      count(gender) %>%
      ggplot() +
      geom_col(aes(x=gender, y = n, fill = gender,
                   text = paste(n, gender))) +
      scale_fill_manual(values = c("male" = "blue", "female" = "#ffc0cb", "unknown" = "grey")) +
      theme_classic() +
      theme(legend.position='none') +
      ylab("")
    ggplotly(my_plot, tooltip="text")
  })
  
  output$duration_job_distribution <- renderPlotly({
    my_table <- mydata_filtered()
    my_plot <- ggplot(my_table) +
      geom_histogram(aes(x = duration_job,
                         text = paste(..count.., "jobs between", x-1,"and",x+1,"years")), fill = "#0e76a8", 
                     breaks = seq(floor(min(my_table$duration_job)),
                                  floor(max(my_table$duration_job)),
                                  by = 2)) +
      xlab("Duration job (in years)") +
      ylab("Count") +
      theme_classic()
    ggplotly(my_plot, tooltip = "text")
  })
  
  output$plot_company_scale <- renderPlotly({
    my_plot <- ggplot(mydata_company_filtered()) +
      geom_point(aes(x=company_scale/1000, y=nb_jobs, text = company), color = "#0e76a8") +
      theme_classic() +
      xlab("Number of employees (in thousands)") +
      ylab("Number of jobs on LinkedIn")
    ggplotly(my_plot, tooltip = "text")
  })
  
  output$table_company <- renderDataTable({
    my_table <- mydata_company_filtered() %>%
      arrange(desc(nb_jobs)) %>%
      select("Company name" = company,
             "Number of jobs" = nb_jobs,
             "Number of current jobs" = nb_current_jobs,
             "Company size" = company_scale_categories,
             "Number of employees" = company_scale)
    DT::datatable(my_table, options = list(lengthMenu = c(10, 50, 100), pageLength = 10))
  })
  
  
}

shinyApp(ui = ui, server = server)
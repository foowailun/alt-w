# Description -------------------------------------------------------------
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# Library -----------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)

# Data Source -------------------------------------------------------------
# Group link
# https://drive.google.com/drive/folders/1apxDlz7v0g_zXQiprWdlTnciGQ5bueQC
# https://rstudio.github.io/shinydashboard/structure.html

# Data Source
# Calories Data
food_calories_table <- fread("food_calories_table.csv")
# documentation_source <- fread("Documentation.txt",header = F)
# contact_us_source <- fread("Documentation.txt", header = F)
food_calories_table[, recommend := 0]
my_choices <- food_calories_table[, food_short_name]
food_products_order <- food_calories_table[, .(mean_calorie = mean(calorie_in_Kcal)), products][order(mean_calorie)][, products]

# UI ---------------------------------------------------------------
# Define UI for application
ui <- dashboardPage(title = "My Calories", skin = "green", 
                    
        dashboardHeader(title = "Dashboard", dropdownMenu(type = "tasks", badgeStatus = "success",
                                                      taskItem(value = 90, color = "green",
                                                               "Documentation"
                                                      ))),
        
        dashboardSidebar(sidebarMenu(id = "sidebar",
          menuItem("My Profile", tabName = "dashboard", icon = icon("user"),
                   badgeLabel = "Input", badgeColor = "green"),
          # menuItem("My Meal", icon = icon("spoon"), tabName = "widgets",
          #          badgeLabel = "Input", badgeColor = "green"),
          menuItem("Analysis", icon = icon("bar-chart-o"), tabName = "analysis",
                   badgeLabel = "Analysis", badgeColor = "purple"),
          menuItem("Documentation", icon = icon("book"), tabName = "documentation",
                   badgeLabel = "Info", badgeColor = "yellow"),
          menuItem("Source code", icon = icon("code"), 
                   href = "https://github.com/rstudio/shinydashboard/",newtab = F,
                   badgeLabel = "GitHub", badgeColor = "red"), 
          menuItem("Contact us", icon = icon("comment"), 
                   href = "https://github.com/rstudio/shinydashboard/",newtab = F,
                   badgeLabel = "Info", badgeColor = "yellow")
        )),
        
        dashboardBody(
          
          tabItems(
            tabItem(tabName = "dashboard",
                    wellPanel(radioButtons(inputId = "user_gender", label = "My Gender", choices = c("Male", "Female"), selected = NULL, inline = T),
                              numericInput(inputId = "user_age", label = "My Age", min = 0, max = 150, step = 1, value = 0),
                              numericInput(inputId = "user_height", label = "My Height (cm)", min = 0, max = 200, step = 1, value = 0),
                              numericInput(inputId = "user_weight", label = "My Weight (kg)", min = 0, max = 300, step = 1, value = 0),
                              actionButton(label = "Save & Continue", inputId = "next_button"))
            ),
            
            tabItem(tabName = "widgets",
                    wellPanel(

                    
                    )
  
            ),
            
            tabItem(tabName = "analysis",
                    wellPanel(
                      
                      fluidRow(
                        
                        box(verbatimTextOutput("recommended_calories"),title = "Recommended Daily Calories Per Meal (Kcal)"),
        
                        box(actionButton(label = "Generate a random meal for me", inputId = "Generate_button"), title = "What are the recommended food for today?"),
                        
                        box(tableOutput("recommended_food"),title = "Recommended for you"),
                        
                        box(selectInput(choices =  my_choices, inputId = "user_food_intake",
                                        label = "Customize your meal", selectize = T, multiple = T,
                                        selected = NULL), background = "olive"),
                        
                        box(verbatimTextOutput("balance_calories"),title = "Balance Remaining (Kcal)"),
                        
                        
                        box(tableOutput("stats"), title = "Analysis", collapsible = T, solidHeader = T, background = "olive"),
                        
                        box(plotOutput("coord_polar"), title = "Visualisation", collapsible = T, background = "olive")
                        
                        
                        
                        
                      )
                      
                    )
                    
            ),
            
            tabItem(tabName = "documentation",
                    h2("How to use")
            )
            
          )
        )   
        
)
    
# Server ------------------------------------------------------------
# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$Generate_button,  {
          
          quota <- user_max_intake()
          print(quota)
          food_calories_table[, recommend := 0]
          
          for (i in 1:length(food_products_order)) {
          food_list <- food_calories_table[products == food_products_order[i], ]
            
            if (quota >= 0) {
              food_selected <- food_list[sample(1:nrow(food_list), size = 1), food_short_name]
              food_calories_table[food_short_name %in% food_selected, recommend := 1]
              quota <- quota - food_list[food_short_name == food_selected, calorie_in_Kcal]
            } else {
              food_selected <- "Not recommended"
              quota <- quota
            }
          
          print(paste(food_products_order[i], food_selected))
            
          }
          
    output$stats <- renderTable({
        food_calories_table[recommend == 1, .(Kcal = sum(calorie_in_Kcal)), .(Products = products)]
    })
    
    # coord_polar 
    output$coord_polar <- renderPlot({
        ggplot(food_calories_table[recommend == 1], aes(fill = food_short_name, y = calorie_in_Kcal, x = "")) + geom_bar(width = 1, stat = "identity")  + coord_polar("y", start=0) + facet_wrap(~products)
    })
    
    # coord_polar 
    output$recommended_food <- renderTable({
      food_calories_table[recommend == 1,.(food_short_name, calorie_in_Kcal)]
    })
    
    # coord_polar 
    updateSelectInput(session, inputId = "user_food_intake", selected = food_calories_table[recommend == 1,food_short_name])
    
    
    })  
  
  output$balance_calories <- renderText({
    isolate(
      ifelse (user_gender_data() == "Male", 
              10 * user_weight_data() + 6.25 * user_height_data() - 5 * user_age_data() + 5, 
              10 * user_weight_data() + 6.25 * user_height_data() - 5 * user_age_data() - 161)
    ) - food_calories_table[food_short_name %in% input$user_food_intake, sum(calorie_in_Kcal), ]
  })
  
  # documentation
  output$documentation <- renderTable({
    documentation_source
  })

   # user_gender data
   user_gender_data <- reactive({
     input$user_gender
   })
   
   # user_age data
   user_age_data <- reactive({
     input$user_age
   })
   
   # user_weight data
   user_weight_data <- reactive({
     input$user_weight
   })
   
   # user_height data
   user_height_data <- reactive({
     input$user_height
   })
   
   # user_max_intake data
   user_max_intake <- reactive({
     ifelse (user_gender_data() == "Male",
             10 * user_weight_data() + 6.25 * user_height_data() - 5 * user_age_data() + 5,
             10 * user_weight_data() + 6.25 * user_height_data() - 5 * user_age_data() - 161)
     
   })
   
   # recommended_calories
   observeEvent(input$next_button,  {
     output$recommended_calories <- renderText({
       isolate(
         ifelse (user_gender_data() == "Male",
                 10 * user_weight_data() + 6.25 * user_height_data() - 5 * user_age_data() + 5,
                 10 * user_weight_data() + 6.25 * user_height_data() - 5 * user_age_data() - 161)
       )
     })

   })
   
   
   observeEvent(input$next_button, {
     updateTabItems(session, "sidebar", "analysis")
   }) 
}


# Run Shiny App -----------------------------------------------------------
shinyApp(ui = ui, server = server)

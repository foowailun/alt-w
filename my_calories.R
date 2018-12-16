# Description -------------------------------------------------------------
# This is a Shiny web application. You can run the application by clicking the 'Run App' button above.

# Library -----------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)

# Data Source -------------------------------------------------------------
# Main data source
food_calories_table <- fread("https://raw.githubusercontent.com/foowailun/alt-w/master/food_calories_table.csv")
food_calories_table[, recommend := 0]
food_products_order <- food_calories_table[, .(mean_calorie = mean(calorie_in_Kcal)), products][order(mean_calorie)][, products]
activity_table <- as.data.table(cbind(activity_level = c("Sedentary (little or no exercise)", 
                                                         "Lightly active (light exercise/sports 1-3 days/week)",
                                                         "Moderately active (moderate exercise/sports 3-5 days/week)",
                                                         "Very active (hard exercise/sports 6-7 days a week)",
                                                         "Extra active (very hard exercise/sports & physical job or 2x training)"), 
                                      activity_multiplier = c(1.2,1.375,1.55,1.725,1.9)))



# UI ---------------------------------------------------------------
# Define UI for application
ui <- dashboardPage(title = "My Calories", skin = "green", 
                    
                    # Dashboard header                        
                    dashboardHeader(title = "My Calories"),
                    
                    # Dashboard sidebar
                    dashboardSidebar(sidebarMenu(id = "sidebar",
                                                 
                                                 menuItem(text = "My Profile", tabName = "dashboard", icon = icon("user"),
                                                          badgeLabel = "Input", badgeColor = "green"),
                                                 
                                                 menuItem(text = "My Meal", icon = icon("bar-chart-o"), tabName = "recommendation_tab",
                                                          badgeLabel = "Analysis", badgeColor = "aqua"),
                                                 
                                                 menuItem(text = "Documentation", icon = icon("book"), tabName = "documentation",
                                                          badgeLabel = "Info", badgeColor = "yellow"),
                                                 
                                                 menuItem(text = "Source Code", icon = icon("code"), 
                                                          href = "https://github.com/foowailun/alt-w/blob/master/my_calories.R",newtab = F,
                                                          badgeLabel = "GitHub", badgeColor = "red")
                                                 
                    )
                    ),
                    
                    # Dashboard body
                    dashboardBody(
                      
                      tabItems(
                        tabItem(tabName = "dashboard",
                                box(radioButtons(inputId = "user_gender", label = "My Gender", choices = c("Male", "Female"), selected = "Male", inline = T),
                                    numericInput(inputId = "user_age", label = "My Age", min = 0, max = 150, step = 1, value = 0),
                                    numericInput(inputId = "user_height", label = "My Height (cm)", min = 0, max = 300, step = 1, value = 0),
                                    numericInput(inputId = "user_weight", label = "My Weight (kg)", min = 0, max = 500, step = 1, value = 0),
                                    selectInput(label = "Activity Factor", inputId = "user_activity", choices = activity_table[, activity_level], selected = activity_table[1, activity_level]),
                                    actionButton(label = "Save & Continue", inputId = "next_button"),
                                    actionButton(label = "Reset", inputId = "reset_button"), background = NULL, width = NULL 
                                )
                        ),
                        
                        tabItem(tabName = "recommendation_tab",
                                
                                fluidRow(
                                  

                                  box(box(verbatimTextOutput("recommended_calories", placeholder = T), 
                                          actionButton(label = "Generate Random Meal For Me", inputId = "generate_button"),
                                          title = "Calories Per Meal (Kcal)", width = NULL),        

                                    
                                      box(selectInput(choices =  food_calories_table[, food_short_name], inputId = "user_food_intake",
                                                      label = NULL, selectize = T, multiple = T,
                                                      selected = NULL), background  = NULL, width = NULL, title = "Or Customize Your Own Meal"),
                                      box(tableOutput("diet_breakdown"), background = "aqua",  title = "Breakdown"),
                                      box(verbatimTextOutput("balance_calories", placeholder = T), title = "Deficit / Excess (Kcal)",  background = "aqua"),        
                                      title = "Recommended For You", collapsible = T, solidHeader = T, background = NULL),
                                  
                                  box(plotOutput("coord_polar"), title = "Calorie Charts", collapsible = T, background = "aqua")
                                )
                                
                        ),
                        
                        tabItem(tabName = "documentation",
                                h1("How to use this App"),
                                h4("Step 1"),
                                h4("Step 2"),
                                h4("Step 3"),
                                h4("Step 4")
                        )
                      )
                    )   
)

# Server ------------------------------------------------------------
# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$generate_button,  {
    
    quota <- user_max_intake()
    food_calories_table[, recommend := 0]
    
    for (i in 1:length(food_products_order)) {
      food_list <- food_calories_table[products == food_products_order[i], ]
      
      if (quota >= 0) {
        food_selected <- food_list[calorie_in_Kcal <= quota]
        
        if (nrow(food_selected) != 0){
          
          if (i == length(food_products_order)) {
            food_selected <- food_selected[calorie_in_Kcal >= quantile(calorie_in_Kcal, 0.9)] 
            food_selected <- food_selected[sample(1:nrow(food_selected) , size = 1) , food_short_name]
          } else {
            food_selected <- food_selected[sample(1:nrow(food_selected) , size = 1) , food_short_name]  
          }
          
          food_calories_table[food_short_name %in% food_selected, recommend := 1]
          quota <- quota - food_list[food_short_name == food_selected, calorie_in_Kcal]
          
        } else {
          food_selected <- "Not recommended"
          quota <- quota
        }
        
        
      } else {
        food_selected <- "Not recommended"
        quota <- quota
      }
      
      print(paste(food_products_order[i], food_selected))
      
    }
    
    # render table for diet breakdown      
    output$diet_breakdown <- renderTable({
      food_calories_table[recommend == 1, .(Kcal = sum(calorie_in_Kcal)), .(Products = products)]
    })
    
    # coord_polar 
    output$coord_polar <- renderPlot({
      ggplot(food_calories_table[recommend == 1], aes(fill = food_short_name, y = calorie_in_Kcal, x = "")) + geom_bar(width = 1, stat = "identity")  + coord_polar("y", start=0) + facet_wrap(~products)
    })
    
    # coord_polar 
    updateSelectInput(session, inputId = "user_food_intake", selected = food_calories_table[recommend == 1,food_short_name])
    
  })  
  
  output$balance_calories <- renderText({
    (isolate(
      ifelse (user_gender_data() == "Male", 
              round(0.33 * user_activity_data() * (10 * user_weight_data() + 6.25 * user_height_data() - 5 * user_age_data() + 5)), 
              round(0.33 * user_activity_data() * (10 * user_weight_data() + 6.25 * user_height_data() - 5 * user_age_data() - 161)))
    ) - food_calories_table[food_short_name %in% input$user_food_intake, sum(calorie_in_Kcal), ]) *-1
  })
  
  observeEvent(input$user_food_intake,  {
    
    # render table for diet breakdown      
    output$diet_breakdown <- renderTable({
      food_calories_table[food_short_name %in% input$user_food_intake, .(Kcal = sum(calorie_in_Kcal)), .(Products = products)]
    })
    
    # coord_polar 
    output$coord_polar <- renderPlot({
      ggplot(food_calories_table[food_short_name %in% input$user_food_intake], aes(fill = food_short_name, y = calorie_in_Kcal, x = "")) + geom_bar(width = 1, stat = "identity")  + coord_polar("y", start=0) + facet_grid(.~products) + 
        theme(legend.position = "bottom", axis.title = element_blank(), axis.ticks = element_blank(), legend.title = element_blank(), legend.direction = "vertical", legend.justification = "left")

    })
    
    
  })  
  
  
  # get user_gender data
  user_gender_data <- reactive({
    input$user_gender
  })
  
  # get user_age data
  user_age_data <- reactive({
    input$user_age
  })
  
  # get user_weight data
  user_weight_data <- reactive({
    input$user_weight
  })
  
  # get user_height data
  user_height_data <- reactive({
    input$user_height
  })
  
  # get user_activity data
  user_activity_data <- reactive({
    activity_table[activity_level  == input$user_activity, as.numeric(activity_multiplier)] 
  })
  
  # get user_max_intake data
  # Mifflin-St Jeor Equation formula - 3 meals per day
  user_max_intake <- reactive({
    ifelse (user_gender_data() == "Male",
            round(0.33 * user_activity_data() * (10 * user_weight_data() + 6.25 * user_height_data() - 5 * user_age_data() + 5)),
            round(0.33 * user_activity_data() * (10 * user_weight_data() + 6.25 * user_height_data() - 5 * user_age_data() - 161)))
  })
  
  # when next_button is clicked
  observeEvent(input$next_button,  {
    
    # update recommendation_calories 
    output$recommended_calories <- renderText({
      isolate(
        # Mifflin-St Jeor Equation formula - 3 meals per day
        ifelse (user_gender_data() == "Male",
                round(0.33 * user_activity_data() * (10 * user_weight_data() + 6.25 * user_height_data() - 5 * user_age_data() + 5)),
                round(0.33 * user_activity_data() * (10 * user_weight_data() + 6.25 * user_height_data() - 5 * user_age_data() - 161)))
      )
    })
    
    # update recommendation_tab
    updateTabItems(session = session, inputId = "sidebar", selected = "recommendation_tab")
    
  })
  
  # when reset_button is clicked
  observeEvent(input$reset_button,  {
    
    # update numericInput fields
    updateNumericInput(session = session, inputId = "user_height", value = 0)
    updateNumericInput(session = session, inputId = "user_weight", value = 0)
    updateNumericInput(session = session, inputId = "user_age", value = 0)
    updateSelectInput(session = session, inputId = "user_activity", selected = activity_table[1, activity_level])
  })
  
}


# Run Shiny App -----------------------------------------------------------
shinyApp(ui = ui, server = server)
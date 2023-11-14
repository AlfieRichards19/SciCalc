#Imports needed libraries
library(shiny)
library(bslib)

#Creates a fluid page
ui <- fluidPage(
  #Uses a theme on the entire fluid page
  theme = bs_theme(bootswatch = "darkly"),
  
  #creates a sidebar layout
  sidebarLayout(
    #this sets the title as "Scientific calculator", as well as creates the two numeric inputs and assigns a unique id to then, and sets their default value as 0.
    sidebarPanel(titlePanel("Scientific Calculator"), numericInput("Num1","Enter the First Number",0), numericInput("Num2","Enter the Second Number",0)),
  
    #this creates the main panel.
    mainPanel(
      
      #fluid rows are created to position elements correctly
      fluidRow(column(width = 10, div(style = "height:155px;"))),
      #all of the buttons for calculation for the first number, each with an assigned unique id. 
      actionButton("sqrt1", "Sqrt"),  actionButton("Expo1", "Expo"), actionButton("Sin1", "Sin"), actionButton("Sin11", "Sin-1"), actionButton("Cos1", "Cos"), actionButton("Cos11", "Cos-1"), actionButton("Tan1", "Tan"),  actionButton("Tan11", "Tan-1"), actionButton("In1", "In"), actionButton("Log1", "Log"),
      
      #fluid rows are created to position elements correctly
      fluidRow(column(width = 10, div(style = "height:50px;"))),
      #all of the buttons for calculation for the second number, each with an assigned unique id. 
      actionButton("sqrt2", "Sqrt"),  actionButton("Expo2", "Expo"), actionButton("Sin2", "Sin"), actionButton("Sin12", "Sin-1"), actionButton("Cos2", "Cos"), actionButton("Cos12", "Cos-1"), actionButton("Tan2", "Tan"),  actionButton("Tan12", "Tan-1"), actionButton("In2", "In"), actionButton("Log2", "Log"),
      
      #fluid rows are created to position elements correctly
      fluidRow(column(width = 10, div(style = "height:30px"))),
      
      #a select input which allows the user to choose which operator they would like to use.
      selectInput("Operators","Choose the Operator",choices = c("ADD","SUB","MUL","DIV","^")),
      
      #the output for the value of num1 and num2, using the operator between the two numbers. 
      textOutput("Output"),
    )
  )
)

#initializes the server and switches between the values in the select input, where it does the specified calculation of num1 and num2. It then sends the value to the output text area.
server <- function(input, output, session) {
  output$Output <- renderText({
    switch(input$Operators,
            "ADD" = input$Num1 + input$Num2,
            "SUB" = input$Num1 - input$Num2,
            "MUL" = input$Num1 * input$Num2,
            "DIV" = input$Num1 / input$Num2,
            "^" = input$Num1 ^ input$Num2)
  })
  
  #observe event which states that when the button is clicked(which button is shown by the id), the value of the input field changes by said calculation.
  #an if statement which checks if the number is not available, meaning when the input field has no number, that no function occurs and causes the program to crash. 
  #For the sqrt, it runs with an if statement, which states that in num1 is < 0, then to treat num1 as a complex number so as not to get a NaN's error. If num1 is greater than 0, then it runs as normal.
  observeEvent(input$sqrt1, {
    num1 <- input$Num1
    if (is.na(num1)){
      
    }
    else if (num1 <= 0){
      updated_value <- sqrt(as.complex(num1))
      updateNumericInput(session, "Num1", value = updated_value)
    }
    else{
      updated_value <- sqrt(num1)
      updateNumericInput(session, "Num1", value = updated_value) 
    }
    })
  
  #observe event which states that when the button is clicked(which button is shown by the id), the value of the input field changes by said calculation.
  #For the sqrt, it runs with an if statement, which states that in num2 is < 0, then to treat num1 as a complex number so as not to get a NaN's error. If num2 is greater than 0, then it runs as normal.
  observeEvent(input$sqrt2, {
    num2 <- input$Num2
    if (is.na(num2)){
      
    }
    else if (num2 <= 0){
      updated_value <- sqrt(as.complex(num2))
      updateNumericInput(session, "Num2", value = updated_value)
    }
    else{
      updated_value <- sqrt(num2)
      updateNumericInput(session, "Num2", value = updated_value) 
    }
  })
  
  #observe event which states that when the button is clicked(which button is shown by the id), the value of the input field changes by said calculation.
  observeEvent(input$Expo1, {
    num1 <- input$Num1
    updated_value <- exp(num1)
    updateNumericInput(session, "Num1", value = updated_value)
  })
  
  #observe event which states that when the button is clicked(which button is shown by the id), the value of the input field changes by said calculation.
  observeEvent(input$Expo2, {
    num2 <- input$Num2
    updated_value <- exp(num2)
    updateNumericInput(session, "Num2", value = updated_value)
  })
  
  #observe event which states that when the button is clicked(which button is shown by the id), the value of the input field changes by said calculation.
  observeEvent(input$Sin1, {
    num1 <- input$Num1
    updated_value <- sin(num1)
    updateNumericInput(session, "Num1", value = updated_value)
  })
  
  #observe event which states that when the button is clicked(which button is shown by the id), the value of the input field changes by said calculation.
  observeEvent(input$Sin2, {
    num2 <- input$Num2
    updated_value <- sin(num2)
    updateNumericInput(session, "Num2", value = updated_value)
  })
  
  #observe event which states that when the button is clicked(which button is shown by the id), the value of the input field changes by said calculation.
  #as the values of asin can only be between 1 & -1, an if statement is used to check if the num is within the correct range, if not, then no value is produced as it is NaN.
  observeEvent(input$Sin11, {
    num1 <- input$Num1
    if (is.na(num1)){
      
    }
    else if (num1 >= -1 && num1 <= 1){
      updated_value <- asin(num1)
      updateNumericInput(session, "Num1", value = updated_value) 
    }
    else{
      updateNumericInput(session, "Num1", value = "NaN") 
    }
  })
  
  #observe event which states that when the button is clicked(which button is shown by the id), the value of the input field changes by said calculation.
  #as the values of asin can only be between 1 & -1, an if statement is used to check if the num is within the correct range, if not, then no value is produced as it is NaN.
  observeEvent(input$Sin12, {
    num2 <- input$Num2
    if (is.na(num2)){
      
    }
    else if (num2 >= -1 && num2 <= 1){
      updated_value <- asin(num2)
      updateNumericInput(session, "Num2", value = updated_value) 
    }
    else{
      updateNumericInput(session, "Num2", value = "NaN") 
    }
  })
  
  #observe event which states that when the button is clicked(which button is shown by the id), the value of the input field changes by said calculation.
  observeEvent(input$Cos1, {
    num1 <- input$Num1
    updated_value <- cos(num1)
    updateNumericInput(session, "Num1", value = updated_value)
  })
  
  #observe event which states that when the button is clicked(which button is shown by the id), the value of the input field changes by said calculation.
  observeEvent(input$Cos2, {
    num2 <- input$Num2
    updated_value <- cos(num2)
    updateNumericInput(session, "Num2", value = updated_value)
  })
  
  #observe event which states that when the button is clicked(which button is shown by the id), the value of the input field changes by said calculation.
  #as the values of acos can only be between 1 & -1, an if statement is used to check if the num is within the correct range, if not, then no value is produced as it is NaN.
  observeEvent(input$Cos11, {
    num1 <- input$Num1
    if (is.na(num1)){
      
    }
    else if (num1 >= -1 && num1 <= 1){
      updated_value <- acos(num1)
      updateNumericInput(session, "Num1", value = updated_value) 
    }
    else{
      updateNumericInput(session, "Num1", value = "NaN") 
    }
    })

  #observe event which states that when the button is clicked(which button is shown by the id), the value of the input field changes by said calculation.
  #as the values of acos can only be between 1 & -1, an if statement is used to check if the num is within the correct range, if not, then no value is produced as it is NaN.
  observeEvent(input$Cos12, {
    num2 <- input$Num2
    if (is.na(num2)){
      
    }
    else if (num2 >= -1 && num2 <= 1){
      updated_value <- acos(num2)
      updateNumericInput(session, "Num2", value = updated_value) 
    }
    else{
      updateNumericInput(session, "Num2", value = "NaN") 
    }
  })
  
  #observe event which states that when the button is clicked(which button is shown by the id), the value of the input field changes by said calculation.
  observeEvent(input$Tan1, {
    num1 <- input$Num1
    updated_value <- tan(num1)
    updateNumericInput(session, "Num1", value = updated_value)
  })
  
  #observe event which states that when the button is clicked(which button is shown by the id), the value of the input field changes by said calculation.
  observeEvent(input$Tan2, {
    num2 <- input$Num2
    updated_value <- tan(num2)
    updateNumericInput(session, "Num2", value = updated_value)
  })
  
  #observe event which states that when the button is clicked(which button is shown by the id), the value of the input field changes by said calculation.
  observeEvent(input$Tan11, {
    num1 <- input$Num1
    updated_value <- atan(num1)
    updateNumericInput(session, "Num1", value = updated_value)
  })
  
  #observe event which states that when the button is clicked(which button is shown by the id), the value of the input field changes by said calculation.
  observeEvent(input$Tan12, {
    num2 <- input$Num2
    updated_value <- atan(num2)
    updateNumericInput(session, "Num2", value = updated_value)
  })
  
  #observe event which states that when the button is clicked(which button is shown by the id), the value of the input field changes by said calculation.
  observeEvent(input$In1, {
    num1 <- input$Num1
    updated_value <- log(num1)
    updateNumericInput(session, "Num1", value = updated_value)
  })
  
  #observe event which states that when the button is clicked(which button is shown by the id), the value of the input field changes by said calculation.
  observeEvent(input$In2, {
    num2 <- input$Num2
    updated_value <- log(num2)
    updateNumericInput(session, "Num2", value = updated_value)
  })
  
  #observe event which states that when the button is clicked(which button is shown by the id), the value of the input field changes by said calculation.
  observeEvent(input$Log1, {
    num1 <- input$Num1
    updated_value <- log10(num1)
    updateNumericInput(session, "Num1", value = updated_value)
  })
  
  #observe event which states that when the button is clicked(which button is shown by the id), the value of the input field changes by said calculation.
  observeEvent(input$Log2, {
    num2 <- input$Num2
    updated_value <- log10(num2)
    updateNumericInput(session, "Num2", value = updated_value)
  })
}

#initializes the entire shiny application.
shinyApp(ui = ui, server = server)

#creates a dialog window that can't be resized to prevent any resizing errors. 
runGadget(ui, server, viewer = dialogViewer("Scientific calulator", width = 1200, height = 800))

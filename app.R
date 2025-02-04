## ======================================================== ##

# Purpose: UI and Server for intensity-duration shiny app
# Author: Tim Fulton
# Date: January 25, 2025

## ======================================================== ##

# Source Scripts ----------------------------------------------------------
source("utils.R")


# tags ----------------------------------------------------------
link_github <- tags$a(
  href = "https://github.com/timfulton1/intensity_duration_app",
  target = "_blank",  # Open in a new tab
  icon("github"),     # Use Font Awesome GitHub icon
  style = "color: white; font-size: 20px;"
)


# UI ----------------------------------------------------------
ui <- page_navbar(
  header = tags$head(
    tags$style(HTML("
      .form-control:focus {
      border-color: #A0B3B7 !important; 
      box-shadow: 0 0 0.15rem 0.15rem #A0B3B7 !important;
      }
      .popover {
      max-width: 650px;
      }
    "))
  ),
  title = "Intensity-Duration Analysis",
  bg = "#013440",
  nav_spacer(),
  # Power Tab ----------
  tabPanel(
    "Critical Power",
    layout_sidebar(
      sidebar = sidebar(
        bg = "#F7F7F7",
        width = 350,
        title = NULL,
        fill = FALSE,
        card(
          card_header(
            "Performance Data",
            popover(
              bs_icon("question-circle", size = "1.3em"),
              title = "Instructions",
              HTML("Enter power and duration data for at least 3 performances. 
              <br><br>
              Each performance can be either a time to exhaustion test at a constant power or a fixed distance time trial (e.g., 4 km). 
              If using a fixed distance time trial, power may vary throughout the effort, so use the average power over the entire distance.
              <br><br>
              For the most accurate estimation of Critical Power and W Prime, choose performances lasting 
              between ~3 and ~20 minutes, spread across this range. For example, durations of 5, 10, and 15 minutes would provide a better  
              estimation than durations of 4, 6, and 8 minutes.")
            ),
            class = "d-flex justify-content-between"
          ),
          div(p("Trial 1"), style = "margin-bottom: -20px;"),
          layout_column_wrap(
            width = 1/2,
            generate_power_input(1), generate_duration_input(1),
          ),
          div(p("Trial 2"), style = "margin-bottom: -20px; margin-top: 10px;"),
          layout_column_wrap(
            width = 1/2,
            generate_power_input(2), generate_duration_input(2),
          ),
          div(p("Trial 3"), style = "margin-bottom: -20px; margin-top: 10px;"),
          layout_column_wrap(
            width = 1/2,
            generate_power_input(3), generate_duration_input(3),
          ),
          div(p("Trial 4"), style = "margin-bottom: -20px; margin-top: 10px;"),
          layout_column_wrap(
            width = 1/2,
            generate_power_input(4), generate_duration_input(4),
          )
        ),
        card(
          input_task_button("fit_button_power", "Fit your data", type = "default"),
          input_task_button("demo_button_power", "Use demo data", type = "default")
        )
      ),
      layout_column_wrap(
        fill = FALSE,
        width = 1/4,
        value_box(
          title = "Critical Power (W)",
          value = textOutput("cp_estimate"),
          theme = value_box_theme(bg = "#A0B3B7", fg = "black"), 
          max_height = 100,
        ),
        value_box(
          title = "W Prime (J)",
          value = textOutput("w_prime_estimate"),
          theme = value_box_theme(bg = "#A0B3B7", fg = "black"), 
          max_height = 100
        ),
        value_box(
          title = "5 min Power (W)",
          value = textOutput("five_estimate"),
          theme = value_box_theme(bg = "#A0B3B7", fg = "black"), 
          max_height = 100
        ),
        value_box(
          title = "20 min Power (W)",
          value = textOutput("twenty_estimate"),
          theme = value_box_theme(bg = "#A0B3B7", fg = "black"), 
          max_height = 100
        )
      ),
      card(
        fill = FALSE,
        height = 505,
        card_header("Power-Duration Plot", style = "background-color: #F7F7F7;"),
        withSpinner(
          plotlyOutput("plot_power"), 
          type = 5,
          color = "#013440",
          size = 1
        )
      )
    )
  ),
  # Speed Tab ----------
  tabPanel(
    "Critical Speed",
    layout_sidebar(
      sidebar = sidebar(
        bg = "#F7F7F7",
        width = 350,
        title = NULL,
        fill = FALSE,
        card(
          card_header(
            "Performance Data",
            popover(
              bs_icon("question-circle", size = "1.3em", class = "info-icon"),
              title = "Instructions",
              HTML("Enter distance and duration data for at least 3 performances. 
              <br><br>
              For the most accurate estimation of Critical Speed and D Prime, choose performances lasting 
              between ~3 and ~20 minutes, spread across this range. For example, durations of 5, 10, and 15 minutes would provide a better  
              estimation than durations of 4, 6, and 8 minutes.")
            ),
            class = "d-flex justify-content-between"
          ),
          div(p("Trial 1"), style = "margin-bottom: -20px"),
          layout_column_wrap(
            width = 1/2,
            generate_distance_input(1), generate_duration_input(5),
          ),
          div(p("Trial 2"), style = "margin-bottom: -20px; margin-top: 10px;"),
          layout_column_wrap(
            width = 1/2,
            generate_distance_input(2), generate_duration_input(6),
          ),
          div(p("Trial 3"), style = "margin-bottom: -20px; margin-top: 10px;"),
          layout_column_wrap(
            width = 1/2,
            generate_distance_input(3), generate_duration_input(7),
          ),
          div(p("Trial 4"), style = "margin-bottom: -20px; margin-top: 10px;"),
          layout_column_wrap(
            width = 1/2,
            generate_distance_input(4), generate_duration_input(8),
          )
        ),
        card(
          input_task_button("fit_button_speed", "Fit your data", type = "default"),
          input_task_button("demo_button_speed", "Use demo data", type = "default")
        )
      ),
      layout_column_wrap(
        fill = FALSE,
        width = 1/4,
        value_box(
          title = "Critical Speed (m/s)",
          value = textOutput("cs_estimate"),
          theme = value_box_theme(bg = "#A0B3B7", fg = "black"), 
          max_height = 100,
        ),
        value_box(
          title = "D Prime (m)",
          value = textOutput("d_prime_estimate"),
          theme = value_box_theme(bg = "#A0B3B7", fg = "black"), 
          max_height = 100
        ),
        value_box(
          title = "Mile",
          value = textOutput("mile_estimate"),
          theme = value_box_theme(bg = "#A0B3B7", fg = "black"), 
          max_height = 100
        ),
        value_box(
          title = "5,000 m",
          value = textOutput("five_thousand_estimate"),
          theme = value_box_theme(bg = "#A0B3B7", fg = "black"), 
          max_height = 100
        )
      ),
      card(
        fill = FALSE,
        height = 505,
        card_header("Speed-Duration Plot", style = "background-color: #F7F7F7;"),
        withSpinner(
          plotlyOutput("plot_speed"), 
          type = 5,
          color = "#013440",
          size = 1
        )
      )
    )
  ),
  tabPanel(
    "About",
    HTML(markdown("
      #### Background

      The sustainable duration of exercise is dependent upon the intensity (i.e., power or speed) at which the exercise is performed. 
      This fundamental concept is known as the intensity-duration relationship, and for exercise durations of ~2–40 minutes, can be modeled 
      by a two-parameter hyperbolic equation (see methods below). The critical intensity (power or speed) is the highest intensity that can be sustained 
      primarily by aerobic metabolism while still achieving a metabolic steady state. The critical intensity is also a metabolic threshold 
      such that exercising at intensities (powers or speeds) above the threshold will cause increased reliance on anaerobic metabolism and a greater accumulation of 
      fatigue-inducing metabolites. For a given intensity above the threshold, the time until task failure is dependent on the magnitude of the curvature constant
      (W' or D'), suggesting that curvature constant can be viewed as a fatigue buffer (or fatigue constant). 


      #### Cycling Methods
      The data are fit using a two parameter hyperbolic model according to the equation below:")),

      withMathJax("$$t = \\frac{W'}{P - CP}$$"), 
      
      HTML(markdown("
      where `t` is the performance time (s), `W'` is the curvature constant (Joules), `P`is the performance power (W), and `CP` is the Critical Power (W). 
      
      Two additional variables that are calculated in the application are:
      
      - **5 minute Power** - An estimate of the highest power that can be sustained for 5 minutes.
      - **20 minute Power** - An estimate of the highest power that can be sustained for 20 minutes.


      #### Running Methods
      The data are fit using a two parameter hyperbolic model according to the equation below:")),
    
      
      withMathJax("$$t = \\frac{D'}{S - CS}$$"),  
    
      HTML(markdown("where <code>t</code> is the performance time (s), <code>D'</code> is the curvature constant (m), <code>S</code> is the average speed (m/s), and
              <code>CS</code> is the critical speed (m/s).
      
      Two additional variables that are calculated in the application are:
      
      - **Mile** - Estimated one mile performance.
      - **5,000 m** - Estimated 5K performance.
      
      <br>
      
      "))
  ),
  nav_item(link_github)
)


# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  ## Power ---------------------------------------------------------------
  # Reactive values list to store the inputs from UI
  reactive_values <- reactiveValues(
    power = c(power_1 = NA, power_2 = NA, power_3 = NA, power_4 = NA),
    duration = c(duration_1 = NA, duration_2 = NA, duration_3 = NA, duration_4 = NA)
  )
  
  # Update the reactive values when the fit button is clicked
  observeEvent(input$fit_button_power, {
    
    for (i in 1:4) {
      # Get the input values for power and duration
      power_input <- input[[paste0("power_", i)]]
      duration_input <- input[[paste0("duration_", i)]]
      
      # Update reactive_values with user input or otherwise update with NA
      # This helps in scenarios where a user would fit the demo data, and then delete or replace the demo values
      # There could be a better solution available but it works 
      if (!is.na(power_input) && power_input != "") {
        reactive_values$power[i] <- power_input
      } else {
        reactive_values$power[i] <- NA  
      }
      
      if (!is.na(duration_input) && duration_input != "") {
        reactive_values$duration[i] <- duration_input
      } else {
        reactive_values$duration[i] <- NA  
      }
    }
    
    # Validate that there are at least 3 valid power and duration values
    valid_power_count <- sum(!is.na(reactive_values$power))
    valid_duration_count <- sum(!is.na(reactive_values$duration))
    
    if (valid_power_count < 3 || valid_duration_count < 3) {
      # Show a modal with an error message
      showModal(modalDialog(
        title = "Input Error",
        paste("Please enter Power and Duration values for at least 3 trials."),
        easyClose = FALSE,
        footer = modalButton("Close")
      ))
    }
    
  })
  
  
  # Update the reactive values and UI when the demo button is clicked
  observeEvent(input$demo_button_power, {
    
    # create demo data
    demo_power <- list(
      power = c(487, 416, 375, 360),
      duration = c(180, 330, 650, 1000)
    )
    
    # Update reactive_values
    reactive_values$power <- demo_power$power
    reactive_values$duration <- demo_power$duration
    
    # Update the UI inputs
    for (i in 1:4) {
      updateTextInput(session, paste0("power_", i), value = demo_power$power[i])
      updateTextInput(session, paste0("duration_", i), value = demo_power$duration[i])
    }
    
  })
  
  
  # Reactive expression to get the user data if at least 3 inputs for power are there
  user_data <- reactive({
    
    req(sum(!is.na(reactive_values$power)) >= 3 && sum(!is.na(reactive_values$duration)) >= 3)
    
    tibble(
      Power = as.numeric(reactive_values$power),
      Seconds = as.numeric(reactive_values$duration),
    ) %>% 
      mutate(Minutes = round(Seconds / 60, 2)) %>%
      filter(!is.na(Power))
    
  })
  
  
  # Value box outputs for Power
  # Put these all in a reactive with validation to control for model errors
  # This way nothing will show up in the boxes, but a model box, which is handled in the plot render function will show up 
  value_box_outputs <- reactive({
    
    validate(need(try(fit_the_data(user_data()) != "error"), ""))
    
    validate(need(try(fit_the_data(user_data())[[2]] > 0), ""))  # puts "" in boxes if the model runs but gives negative value
    
    
    cp_estimate <- fit_the_data(user_data())[[2]]
    w_prime_estimate <- fit_the_data(user_data())[[3]]
    five_estimate <- fit_the_data(user_data())[[4]]
    twenty_estimate <- fit_the_data(user_data())[[5]]
    
    list(cp_estimate, w_prime_estimate, five_estimate, twenty_estimate)
    
  })
  
  output$cp_estimate <- renderText(value_box_outputs()[[1]])
  output$w_prime_estimate <- renderText(value_box_outputs()[[2]])
  output$five_estimate <- renderText(value_box_outputs()[[3]])
  output$twenty_estimate <- renderText(value_box_outputs()[[4]])
  
  
  # Plot output for Power
  output$plot_power <- renderPlotly({
    
    req(user_data())  # so that the modal box for the fit error doesn't populate on start up
    
    # Check if the fit data function returns an error
    fit_data_result <- length(fit_the_data(user_data()))
    
    if (fit_data_result == 1) {
      # Show a modal with an error message if fitting fails
      showModal(modalDialog(
        title = "Fit Error",
        "Unable to fit the data. Please refer to the instructions and make sure your data are physiologically relevant.",
        easyClose = FALSE,
        footer = modalButton("Close")
      ))
      return(NULL)  # Return NULL to prevent rendering of the plot if there's an error
    }
    
    fit_the_data(user_data())[[6]]
    
  })
  
  
  
  ## Speed ------------------------------------------------------------------
  # Reactive values list to store the inputs from UI
  reactive_values_speed <- reactiveValues(
    distance = c(distance_1 = NA, distance_2 = NA, distance_3 = NA, distance_4 = NA),
    duration = c(duration_1 = NA, duration_2 = NA, duration_3 = NA, duration_4 = NA)
  )
  
  # Update the reactive values when the fit button is clicked
  observeEvent(input$fit_button_speed, {
    
    for (i in 1:4) {
      # Get the input values for power and duration
      distance_input <- input[[paste0("distance_", i)]]
      duration_input <- input[[paste0("duration_", i+4)]]
      
      # Only update reactive_values if the input is not NULL or empty, otherwise update with NA
      # This helps in scenarios where a user would fit the demo data, and then delete or replace the demo values
      # There could be a better solution available but it works 
      if (!is.null(distance_input) && distance_input != "") {
        reactive_values_speed$distance[i] <- distance_input
      } else {
        reactive_values_speed$distance[i] <- NA  
      }
      
      if (!is.null(duration_input) && duration_input != "") {
        reactive_values_speed$duration[i] <- duration_input
      } else {
        reactive_values_speed$duration[i] <- NA  
      }
    }
    
    # Validate that there are at least 3 valid distance and duration values
    valid_distance_count <- sum(!is.na(reactive_values_speed$distance))
    valid_duration_count <- sum(!is.na(reactive_values_speed$duration))
    
    if (valid_distance_count < 3 || valid_duration_count < 3) {
      # Show a modal with an error message
      showModal(modalDialog(
        title = "Input Error",
        paste("Please enter Distance and Duration values for at least 3 trials."),
        easyClose = FALSE,
        footer = modalButton("Close")
      ))
    }
    
  })
  
  
  # Update the reactive values and UI when the demo button is clicked
  observeEvent(input$demo_button_speed, {
    
    # create demo data
    demo_speed <- list(
      distance = c(1200, 1500, 3000, 5000),
      duration = c(180, 230, 483, 820)
    )
    
    # Update reactive_values
    reactive_values_speed$distance <- demo_speed$distance
    reactive_values_speed$duration <- demo_speed$duration
    
    # Update the UI inputs
    for (i in 1:4) {
      updateTextInput(session, paste0("distance_", i), value = demo_speed$distance[i])
      updateTextInput(session, paste0("duration_", i+4), value = demo_speed$duration[i])
    }
    
  })
  
  
  # Reactive expression to get the user data if there are at least 3 inputs
  user_data_speed <- reactive({
    
    req(sum(!is.na(reactive_values_speed$distance)) >= 3 && sum(!is.na(reactive_values_speed$duration)) >= 3)
    
    tibble(
      Distance = as.numeric(reactive_values_speed$distance),
      Seconds = as.numeric(reactive_values_speed$duration),
    ) %>% 
      mutate(
        Minutes = round(Seconds / 60, 2),
        Speed = Distance / Seconds
      ) %>%
      filter(!is.na(Speed))
    
  })
  
  
  # Value box outputs for Speed
  # Put these all in a reactive with validation to control for model errors
  # This way nothing will show up in the boxes, but a model box, which is handled in the plot render function will show up 
  value_box_outputs_speed <- reactive({
    
    validate(need(try(fit_the_data_speed(user_data_speed()) != "error"), ""))  # puts "" in boxes if error occurs
    
    validate(need(try(fit_the_data_speed(user_data_speed())[[2]] > 0), ""))  # puts "" in boxes if the model runs but gives negative value
    
    cs_estimate <- fit_the_data_speed(user_data_speed())[[2]]
    d_prime_estimate <- fit_the_data_speed(user_data_speed())[[3]]
    mile_estimate <- fit_the_data_speed(user_data_speed())[[4]]
    five_thousand_estimate <- fit_the_data_speed(user_data_speed())[[5]]
    
    list(cs_estimate, d_prime_estimate, mile_estimate, five_thousand_estimate)
    
  })
  
  output$cs_estimate <- renderText(value_box_outputs_speed()[[1]])
  output$d_prime_estimate <- renderText(value_box_outputs_speed()[[2]])
  output$mile_estimate <- renderText(value_box_outputs_speed()[[3]])
  output$five_thousand_estimate <- renderText(value_box_outputs_speed()[[4]])
  
  # Plot output for speed
  output$plot_speed <- renderPlotly({
    
    req(user_data_speed())  # so that the modal box for the fit error doesn't populate on start up
    
    # Check if the fit data function returns an error
    fit_data_result <- length(fit_the_data_speed(user_data_speed()))
    
    if (fit_data_result == 1 | fit_the_data_speed(user_data_speed())[[2]] < 0) {
      # Show a modal with an error message if fitting fails
      showModal(modalDialog(
        title = "Fit Error",
        "Unable to fit the data. Please refer to the instructions and make sure your data are physiologically relevant.",
        easyClose = FALSE,
        footer = modalButton("Close")
      ))
      return(NULL)  # Return NULL to prevent rendering of the plot if there's an error
    }
    
    fit_the_data_speed(user_data_speed())[[6]]
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

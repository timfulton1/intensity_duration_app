# Purpose: Provide helper functions for intensity-duration shiny app
# Author: Tim Fulton
# Date: January 25, 2025


# Load Packages -----------------------------------------------------------
library(shiny)
library(bslib)
library(bsicons)
library(shinycssloaders)
library(dplyr)
library(glue)
library(ggplot2)
library(plotly)
library(scales)


# Input Functions ----------------------------------------------------------
generate_power_input <- function(index = 1) {
  textInput(
    inputId = paste0("power_", index),
    label = NULL,
    width = 120,
    placeholder = "Power (W)"
  )
}

generate_distance_input <- function(index = 1) {
  textInput(
    inputId = paste0("distance_", index),
    label = NULL,
    width = 120,
    placeholder = "Distance (m)"
  )
}

generate_duration_input <- function(index = 1) {
  textInput(
    inputId = paste0("duration_", index),
    label = NULL,
    width = 120,
    placeholder = "Duration (s)"
  )
}

convert_seconds_to_mmssms <- function(seconds) {
  # Extract minutes and seconds
  minutes <- floor(seconds / 60)
  secs <- seconds %% 60
  secs_int <- floor(secs)
  secs_fraction <- round((secs - secs_int) * 100)
  
  # Format without leading zero
  formatted_time <- sprintf("%d:%02d.%02d", minutes, secs_int, secs_fraction)
  
  return(formatted_time)
}

# Modeling Functions ----------------------------------------------------------

# Power
fit_the_data <- function(data){
  
  tryCatch({
    # Fit using non-linear function
    model_fit <- nls(Seconds ~ w_prime / (Power - critical_power),
                     data = data,
                     start = list(critical_power = min(data$Power) - 50, w_prime = 10000))
    
    # Extract coefficients
    cp_estimate <- summary(model_fit)$parameters[1, 1]
    w_prime_estimate <- summary(model_fit)$parameters[2, 1]
    
    # Prepare fitted data for the plot
    model_data_df <- data.frame(Seconds = seq(120, 1800, by = 1)) %>%
      mutate(
        Power = round(w_prime_estimate / Seconds + cp_estimate, 2),
        Minutes = round(Seconds / 60, 2)
      )
    
    # Calculate 5 min power
    five_minute_power <- round((w_prime_estimate/300) + cp_estimate)
    
    # Calculate 20 min power
    twenty_minute_power <- round((w_prime_estimate/1200) + cp_estimate)
    
    plot <- plot_the_data(data, model_data_df)
    
    model_data_list <- list(model_data_df, round(cp_estimate), prettyNum(round(w_prime_estimate), big.mark = ","), five_minute_power, twenty_minute_power, plot)
    
    return(model_data_list)
  }, error = function(e){
    return("error")
  })
}

# Speed
fit_the_data_speed <- function(data){
  
  tryCatch({
    # Fit using non-linear function
    model_fit <- nls(Seconds ~ d_prime / (Speed - critical_speed),
                     data = data,
                     start = list(critical_speed = min(data$Speed) - 2, d_prime = 100))
    
    # Extract coefficients
    cs_estimate <- summary(model_fit)$parameters[1, 1]
    d_prime_estimate <- summary(model_fit)$parameters[2, 1]
    
    # Prepare fitted data for the plot
    model_data_df <- data.frame(Seconds = seq(120, 1800, by = 0.1)) %>%
      mutate(
        Speed = d_prime_estimate / Seconds + cs_estimate,
        Minutes = round(Seconds / 60, 2),
        Distance = Seconds * Speed
      )
    
    #index_mile <- which.min(abs(model_data_df$Distance - 1609))
    
    # Calculate Mile time
    mile_time <- convert_seconds_to_mmssms(model_data_df$Seconds[which.min(abs(model_data_df$Distance - 1609))])
    
    # Calculate 25k Time
    five_time <- convert_seconds_to_mmssms(model_data_df$Seconds[which.min(abs(model_data_df$Distance - 5000))])
    
    # Plot
    plot <- plot_the_data_speed(data, model_data_df)
    
    model_data_list <- list(model_data_df, round(cs_estimate, 2), prettyNum(round(d_prime_estimate), big.mark = ","), mile_time, five_time, plot)
    
    return(model_data_list)
  }, error = function(e){
    return("error")
  })
}

# Plotting Functiona ----------------------------------------------------------

plot_the_data <- function(user_data, model_data){
  
  plot <- ggplot(user_data, aes(x = Seconds, y = Power, label = Minutes)) +
    geom_point(
      shape = 21, 
      size = 3, 
      color = "black", 
      fill = "white"
    ) +
    geom_line(
      data = model_data, 
      aes(x = Seconds, y = Power), 
      color = "#FEA524",
      linewidth = 0.75
    ) +
    labs(
      x = "Time (min)", 
      y = "Power (W)"
    ) +
    scale_x_continuous(
      breaks = seq(120, 1800, 240),
      labels = seq(2, 30, 4)
    ) +
    scale_y_continuous(breaks = pretty_breaks(n = 5)) +
    theme_classic() +
    theme(
      axis.title = element_text(size = 14, face = "bold", color = "black"),
      axis.text = element_text(size = 12, color = "black"),
      axis.line = element_line(linewidth = 1.5, color = "black")
    )
  
  plotly_fig <- ggplotly(plot) %>%
    layout(
      yaxis = list(
        title = list(standoff = 20)  # Increase the space between the y-axis title and ticks
      )
    )
  
  return(plotly_fig)
}

plot_the_data_speed <- function(user_data, model_data){
  
  plot <- ggplot(user_data, aes(x = Seconds, y = Speed, label = Minutes)) +
    geom_point(
      shape = 21, 
      size = 3, 
      color = "black", 
      fill = "white"
    ) +
    geom_line(
      data = model_data, 
      aes(x = Seconds, y = Speed), 
      color = "#FEA524",
      linewidth = 0.75
    ) +
    labs(
      x = "Time (min)", 
      y = "Speed (m/s)"
    ) +
    scale_x_continuous(
      breaks = seq(120, 1800, 240),
      labels = seq(2, 30, 4)
    ) +
    scale_y_continuous(breaks = pretty_breaks(n = 5)) +
    theme_classic() +
    theme(
      axis.title = element_text(size = 14, face = "bold", color = "black"),
      axis.text = element_text(size = 12, color = "black"),
      axis.line = element_line(linewidth = 1.5, color = "black")
    )
  
  plotly_fig <- ggplotly(plot) %>%
    layout(
      yaxis = list(
        title = list(standoff = 20)  # Increase the space between the y-axis title and ticks
      )
    )
  
  return(plotly_fig)
}

# Load libraries
library(shiny)
library(EpiEstim)
library(ggplot2)
library(incidence)
library(cluster.datasets)

# Define UI for application
ui <- fluidPage(navbarPage("WHO / PAHO",
                           tabPanel("Tab Name",
                               sidebarPanel([inputs for the first tab]),
                               mainPanel([outputs for the first tab])
                           ),
                           
                           tabPanel("Second tab name",
                                    sidebarPanel([inputs for the second tab]),
                                    mainPanel([outputs for the second tab])
                           )
    
))

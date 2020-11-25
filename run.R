# Title:   Web App Executable
# Name:    run.R
# Author:  Stijn Middelhuis
# Project: Regulate bed occupation in the clinic
#
# Description:
# This file contains the code to run the Web Application
# From a windows .bat file.

require(shiny)
folder_address = "A:\\MMC\\Building_Blocks\\Planners\\PlanApp\\WebApp - AutoRefresh" 
runApp(folder_address, launch.browser = TRUE)
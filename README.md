# PREP Oyster Water Filtration Experiment

This Shiny application was created to track and visualize turbidity measurements for an educational experiment comparing water cleaning rates between tanks with and without oysters.

## About the Experiment

This experiment demonstrates how oysters naturally filter and clean water. By comparing turbidity (cloudiness) measurements between tanks with and without oysters over time, students can observe the water-filtering capabilities of these important bivalves.

## Features

- Simple data entry interface for school name, tank type, and turbidity readings
- Real-time visualization of turbidity measurements over time
- Automatic data storage in CSV format
- Interactive plot with tooltips showing detailed measurement information

## How to Use

### Running the App Locally

1. Clone this repository
2. Make sure you have R installed with the required packages
3. Run the app with:
   ```r
   shiny::runApp()
   ```

### Required R Packages

- shiny
- ggplot2
- readr
- dplyr
- scales
- shinyjs
- plotly
- lubridate

## Project Structure

- `app.R` - Main Shiny application code
- `data/observations.csv` - Data storage file
- `www/` - Directory for static assets like images

## Results and Data Access

After the experiment is complete, this repository will be updated with:
- Summary of findings
- Final visualization of results
- Access to the complete dataset
- Instructions for educators on how to run similar experiments

## Special thanks to
- UNH Jackson Estuarine Lab for providing equipment
- Fox Point Oysters and Laura Brown for supplying oysters for the experiment
- UNH Water Quality Analysis Lab for lending a water quality meter
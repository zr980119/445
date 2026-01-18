# IJC445 Data Visualisation Coursework

This repository contains the code and outputs for the IJC445 Data Visualisation coursework.

## Project Overview

The project analyses Spotify audio features of Billboard Hot-100 songs to explore how musical characteristics relate to chart performance and how these features vary over time.

The analysis focuses on:
- Distribution changes of audio features over time
- Comparison of audio features across chart ranking groups
- Multidimensional feature patterns
- Correlations between audio features

## Files in This Repository

- `code.R`  
  Main R script used for data cleaning, analysis, and visualisation.

- `IJC445 Report.docx`  
  Final coursework report describing the analysis, visualisation design, and findings.

- `figure1.png`  
  Ridgeline plots showing the evolution of music feature distributions (2000–2004).

- `figure2.png`  
  Radar charts comparing average audio features by chart ranking category.

- `figure3.png`  
  Parallel coordinates plot showing multidimensional audio features.

- `figure4.png`  
  Network graph visualising correlations between audio features.

## Data

The analysis uses a dataset of Billboard Hot-100 songs with Spotify audio features  
(`billboard_24years_lyrics_spotify.csv`).

The dataset is not included in this repository.

## How to Run

1. Open `code.R` in RStudio  
2. Ensure all required libraries are installed  
3. Place the dataset in the working directory  
4. Run the script to reproduce the figures

## Libraries Used

tidyverse, ggridges, fmsb, GGally, igraph, ggraph, viridis, scales

## Notes

This project was completed using **R only**, in line with the module requirements.

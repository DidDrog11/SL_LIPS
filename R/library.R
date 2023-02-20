# We load packages we need
if (!require("pacman")) install.packages("pacman")
pkgs = 
  c("lubridate",
    "here",
    "tidyverse",
    "sf",
    "readxl")
pacman::p_load(pkgs, character.only = T)

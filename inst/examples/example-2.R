##================================================================================
## This file is part of R/Repast
##
## Example of using R/Repast with Predator-Prey model with PARALLEL MULTI-CORE
## execution
##
## (C) 2016, 2017, 2018, 2019 - Antonio Prestes Garcia <@>
## For license terms see DESCRIPTION and/or LICENSE
##
## @file: example-2.R
##
## This file is very similar to ‘example-1.R’ and contains an R script for running 
## the Predator-Prey example model included in the 'Repast Simphony' distribution 
## but showing how to use the MULTI-CORE feature of R/Repast. This feature allows 
## the parallel execution of model replications. It is required to build and install 
## the model on your target system in order to run it using R/Repast. A pre-built 
## installer for the Predator-Prey model can be downloaded from the following 
## link http://goo.gl/cJ5z2r.
##================================================================================

## Initialization
rm(list=ls())
library(rrepast)
set.seed(2718282)

## TODO: Change the value below to match the directory where the model is installed.
modelpath<- "/usr/models/PredatorPrey" 

Easy.Setup(modelpath, multicore=TRUE)

## The following call run the model for 600 time ticks with four replications 
## and returns the “Agent Counts” dataset
v<- Easy.Run(modelpath, "Agent Counts", 600, 4)


## Sample of the model output dataset "Agent Counts"
## > head(v)
##   run tick Sheep.Count Wolf.Count
## 1   1    1          51         92
## 2   1    2          53         91
## 3   1    3          52         88
## 4   1    4          54         88
## 5   1    5          56         91
## 6   1    6          55         87


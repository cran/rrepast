##================================================================================
## This file is part of the rrepast package - R/Repast interface API
## 
## (C)2015 Antonio Prestes Garcia <@>
## For license terms see DESCRIPTION and/or LICENSE
##
## $Id$
##================================================================================


# ------------------------------------------------------------
# .onLoad shows the library disclaimer
#
# ------------------------------------------------------------
.onLoad<- function(libname, pkgname) {
  #packageStartupMessage("rrepast - interface API to Repast Simphony, version 0.1\n")
  assign("pkg.globals", new.env(), envir=parent.env(environment()))
  
  # Internal variables
  assign("pkg.basedir", NA, pkg.globals)
  assign("pkg.modeldir", NA, pkg.globals)
  assign("pkg.scenariodir", NA, pkg.globals)
  assign("pkg.modellibdir", NA, pkg.globals)
  assign("pkg.id", NA, pkg.globals)
  
  # global simulation results
  assign("pkg.parameters", data.frame(), pkg.globals)
  assign("pkg.results", data.frame(), pkg.globals)
  
  # default values for model
  assign("pkg.outputdir",paste0(Sys.getenv("TMP"),"/repast-output/"), pkg.globals)
  assign("pkg.repastlibdir", "/repast.simphony/", pkg.globals)
  assign("pkg.java.parameters","-server -Xms512m -Xmx1024m", pkg.globals)
  
  # default key for Repast random seed 
  assign("pkg.randomSeed","randomSeed", pkg.globals)
  
  
  # The Random Seed. You may want to change this.
  set.seed(exp(1)*10^6)
}

# ----- internal functions 

# Returns the wrapper classes jar file location. 
enginejar<- function() {

  # Try to guess the rrepast-engine.jar location  
  for(p in .libPaths()) {
    f0<- paste0(p,"/rrepast/java/rrepast-engine.jar")
    f1<- paste0(p,"/rrepast/inst/java/rrepast-engine.jar")
    if(file.exists(f0)) {
      f<- f0
      break
    } else if(file.exists(f1)) {
        f<- f1
        break
    }
  }
  return(f)
}

# Return the name of repast engine class name. 
engineclazz<- function() {
  return("org.haldane.rrepast.RepastEngine")
}

# Returns the repast simphony library dir
simphonylib<- function() {
  b<- get("pkg.basedir", pkg.globals)
  l<- get("pkg.repastlibdir", pkg.globals)
  return(paste0(b,l))
}

# Configure all model directories based on default installation values
configModelDirs<- function(s) {
  d<- basename(s)
  setBaseDir(s)
  setModelDir(paste0(paste0(s,"/"),d))
  setScenarioDir(paste0(paste0(paste0(getModelDir(),"/"),d),".rs"))
  setModelLibDir(paste0(getModelDir(),"/lib"))
}

# Setters and Getters ----------

#' @title Sets the model name
#' @description  Set the name of the model currently instantiated.
#' 
#' @param s -- The model name
#' 
#' @export
setId<- function(s) {
  assign("pkg.id", s, pkg.globals)      
}

#' @title Gets the model name
#' @description  Provides the name of the model currently instantiated.
#' 
#' @export
getId<- function() {
  return(get("pkg.id", pkg.globals))
}

#' @title Sets Repast randomSeed name
#' @description Configures a non-default value for Repast randomSeed
#' parameter name.
#' 
#' @param k -- The string with an alternative name for randomSeed 
#' 
#' @export
setKeyRandom<- function(k){
  assign("pkg.randomSeed",k, pkg.globals)
}

#' @title Gets Repast randomSeed name
#' @description Returns the Repast randomSeed parameter name.
#' 
#' @return A string value holding the randomSeed name.
#' 
#' @export
getKeyRandom<- function() {
  return(get("pkg.randomSeed", pkg.globals))
}

#' @title Sets output directory
#' 
#' @description Configure the desired directoy to save model 
#' output data.
#' 
#' @param s -- The full path for output directory
#' 
#' @export
setOutputDir<- function(s) {
  assign("pkg.outputdir", s, pkg.globals)    
}

#' @title Gets output directory
#' 
#' @description Returns the value of module variable for 
#' storing the current output directory. 
#' 
#' @export
getOutputDir<- function() {
  return(get("pkg.outputdir", pkg.globals))
}

#' @title Create output directory
#' 
#' @description A simple function to make a directory to save the 
#' model's data.
#' 
#' @details Create the, if required, the directory to save the 
#' output data generate by the model. It is intended for internal 
#' use.
#' 
#' @export 
createOutputDir<- function() {
  d<- getOutputDir()
  if(!dir.exists(d)) {
    dir.create(d)
  }
}

# Set the directory where repast model is installed
setBaseDir<- function(s) {
  assign("pkg.basedir", s, pkg.globals)  
}

# Gets the directory where repast model is installed
getBaseDir<- function() {
  return(get("pkg.basedir", pkg.globals))  
}

# Sets the directory where repast model is installed which normally
# is a subdirectory bellow installation base directory
setModelDir<- function(s) {
  assign("pkg.modeldir", s, pkg.globals)  
}

# Sets the directory where repast model is installed which normally
# is a subdirectory bellow installation base directory
getModelDir<- function() {
  return(get("pkg.modeldir", pkg.globals))  
}

# Sets the model's scenario directory
setScenarioDir<- function(s) {
  assign("pkg.scenariodir", s, pkg.globals)  
}

# Gets the model's scenario directory
getScenarioDir<- function() {
  return(get("pkg.scenariodir", pkg.globals))  
}

# Sets the model's lib directory
setModelLibDir<- function(s) {
  assign("pkg.modellibdir", s, pkg.globals)  
}

# Gets the model's lib directory
getModelLibDir<- function() {
  return(get("pkg.modellibdir", pkg.globals))  
}

# Traverse the lib dir to build up the classpath
repastlibs<- function() {
  libdir<- simphonylib()
  for(d in dir(libdir)) {
    # On bin dir we expect unpackaged class files
    bin<- paste0(libdir,paste0(d,"/bin"))
    lib<- paste0(libdir,paste0(d,"/lib"))
    # adding the bin dir to rjava classpath
    .jaddClassPath(bin)
    
    repastjars(paste0(libdir,d))
    repastjars(lib)
    repastjars(getModelLibDir())
  }
}

# Search for jar files inside lib dir and then add it to classpath
repastjars<- function(lib) {
  jars<- dir(lib,pattern="*.jar")
  for(j in jars) {
    jar<- paste0(paste0(lib,"/"),j)
    # adding jar file to classpath
    .jaddClassPath(jar)
  }
}


# ----- Exposed package API functions


#' Configures the jvm parameters
#' 
#' Set the underlying parameters for java virtual machine. The default 
#' values are "-server -Xms512m -Xmx1024m". These defaults can be changed 
#' to fit the model requirements.
#' 
#' @param s -- The paramter string to be passed to the underlying JVM 
#' 
#' @examples \dontrun{
#'    jvm.set_parameters("-server -Xms512m -Xmx2048m")}
#'    
#' @export 
jvm.set_parameters<- function(s) {
  assign("pkg.java.parameters", s, pkg.globals)  
}

#' Gets the current java virtual machine parameters
#' @return A string with JVM parameters. 
#' @export
jvm.get_parameters<- function() {
  return(get("pkg.java.parameters", pkg.globals))  
}

#' @title Init R/JVM environment
#' 
#' @description Initialize rJava and repast environment with classpath. This function
#' is called internally and it is not meant to be used directlly.
#' 
#' @details The default parameters can be changed as needed calling the 
#' primitive \code{\link{jvm.set_parameters}} befor instantiating the model 
#' engine.
#' 
#' @examples \dontrun{
#'      jvm.init()}
#' 
#' @references
#' [1] rJava: Low-Level R to Java Interface. Low-level interface to Java VM 
#' very much like .C/.Call and friends. Allows creation of objects, 
#' calling methods and accessing fields.
#' @import rJava
jvm.init<- function() {
  # The default parameters can be changed as needed
  .jinit(parameters= jvm.get_parameters() )
  .jaddClassPath(enginejar())
  .jaddClassPath(paste0(getModelDir(),"/bin"))
  # ----- Repast base libraries
  repastlibs()
}


# ----- Wrapper functions for Engine class method calls


#' Creates an instance of Engine
#' 
#' This function creates an instance of Repast model wrapper 
#' class. Before invoking the function Engine, make sure that 
#' environment was correctly initialized.
#' 
#' @return An onject instance of Engine class
#' @export
Engine<- function() {
  return(new(J(engineclazz())))
}

#' Loads the scenario files
#' 
#' This function loads the scenario of a Repast Model and 
#' initialize de model.
#' 
#' @param e -- An engine object instance
#' @param f -- The full path of scenario directory 
#' @export 
Engine.LoadModel<- function(e,f) {
  .jcall(e,"V", "LoadModel",f)  
}

#' Sets the model's dataset
#' 
#' Configure a dataset with the desired output values 
#' to be "drained" by the function Engine.GetModelOutput. 
#' 
#' @param e -- An engine object instance
#' @param k -- The repast model's data set name
#' @examples \dontrun{
#'    d<- "C:/usr/models/your-model-directory"
#'    m<- RepastModel(d)
#'    setAggregateDataSet(m,"dataset-name")}
#' @export
Engine.SetAggregateDataSet<- function(e,k) {
  .jcall(e,"V","ModelDataSet",k)
}

#' Parameter names
#' 
#' Returns the names of all declared model's parameters in
#' the parameter.xml file in the scenario directory.
#' 
#' @param e -- An engine object instance
#' @export
Engine.getParameterNames<- function(e) {
  names<- .jcall(e,"[S","getParameterNames")
  return(names)
} 

#' Get the value of model parameter s as java.lang.Object
#' @param e -- An engine object instance
#' @param k -- The parameter name
#' @return The parameter value
#' 
#' @export
Engine.getParameter<- function(e,k) {
  v<- .jcall(e,"Ljava/lang/Object;","getParameter",k)
  return(v)
}

#' Get the type of a model parameter
#' @param e -- An engine object instance
#' @param k -- The parameter name
#' @return The paramter type
#' 
#' @export 
Engine.getParameterType<- function(e,k) {
  v<- .jcall(e,"Ljava/lang/String;","getParameterType",k)
}

#' Get the value of model parameter s as java.lang.String
#' @param e -- An engine object instance
#' @param k -- The parameter name
#' @export
Engine.getParameterAsString<- function(e,k) {
  v<- .jcall(e,"Ljava/lang/String;","getParameterAsString",k)
  return(v)
}

#' Get the value of model parameter s as java.lang.Number
#' @param e -- An engine object instance
#' @param k -- The parameter name
#' @export
Engine.getParameterAsNumber<- function(e,k) {
  v<- .jcall(e,"Ljava/lang/Number;","getParameterAsNumber",k)
  return(v)
}

#' Get the value of model parameter s as java.lang.Double
#' @param e -- An engine object instance
#' @param k -- The parameter name
#' @export
Engine.getParameterAsDouble<- function(e,k) {
  v<- .jcall(e,"D","getParameterAsDouble",k)
  return(v)
}

#' Set the value of model parameter 
#' @param e -- An engine object instance
#' @param k -- The parameter name
#' @param v -- The parameter value
#' @export
Engine.setParameter<- function(e,k,v) {
  # Map the R type system to java object
  switch(typeof(v),
         character = {
           value<- new(J("java.lang.String"), v)  
         },
         
         double = {
           value<- new(J("java.lang.Double"), v)
         },
         
         integer = {
           value<- new(J("java.lang.Integer"), v)
         },
         
         logical = {
           value<- new(J("java.lang.Boolean"), v)
         })
  # Invoke the setParamter method
  .jcall(e,"V","setParameter",k,value)
}

#' Configure the maximun simulated time for the current model run
#' @param e -- An engine object instance
#' @param v -- The number of time ticks 
#' @export
Engine.endAt<- function(e,v) {
  .jcall(e,"V","endAt",v)
}

#' @title Returns the model id
#' @description This function provides a wrapper to the method getId() 
#' from repast context. The id is basically a String with the currently 
#' instantiated model name.
#' @param e -- An engine object instance
#' @export
Engine.getId<- function(e) {
  id<- .jcall(e,"S","getId")
  if(nchar(id) == 0) stop("Model not initilized.")
  return(id)
}

#' Run the model
#' @param e -- An engine object instance
#' @export
Engine.RunModel<- function(e) {
  .jcall(e,"V","RunModel")
}

#' Gets the model output data as a CSV String array
#' 
#' Call the engine method GetModelOutput to drain model output
#' data.
#' 
#' @param e -- An engine object instance
#' @return An array of strings containing the model's output
#' @examples \dontrun{
#'    d<- "C:/usr/models/your-model-directory"
#'    m<- Engine(d)
#'    csv<- RepastModel.GetOutput(m)}
#' @importFrom utils read.csv
#' @export
Engine.GetModelOutput<- function(e) {
  .jcall(e,"[S","GetModelOutput")
}

#' Performs a cleanup on engine instance.
#' 
#' Finalize and destroy repast controller data.
#' 
#' @param e -- An engine object instance
#' @export
Engine.Finish<- function(e) {
  .jcall(e,"V","cleanUpBatch")
}

#' @title Set the log level to INFO
#' @description Configures the underlying logging system
#' 
#' @export
Logger.setLevelInfo<- function() {
  logger<- J("org.haldane.rrepast.RepastEngineLogger")
  .jcall(logger,"V","setLevelInfo")
}

#' @title Set the log level to WARNING
#' @description Configures the underlying logging system
#' 
#' @export
Logger.setLevelWarning<- function() {
  logger<- J("org.haldane.rrepast.RepastEngineLogger")
  .jcall(logger,"V","setLevelWarning")
}

#' Prints the paths
#' 
#' Shows the directories currently used to load model scenario and lib. 
#' This function is informational only and can be used to check whether 
#' model data is being loaded properly from correct locations.
#' 
#' @examples \dontrun{
#'    ShowModelPaths()}
#' @export
ShowModelPaths<- function() {
  print(paste("Install dir.= ",getBaseDir()))
  print(paste("Model dir...= ",getModelDir()))
  print(paste("Scenario dir= ",getScenarioDir()))
  print(paste("Model lib...= ",getModelLibDir()))
}

#' Java Classpath
#' 
#' Returns the current setting of JVM classpath
#' 
#' @examples \dontrun{
#'    ShowClassPath()}
#' @export
ShowClassPath<- function() {
  .jclassPath()
}


#' @title The easy API for model initilization
#' @description Instantiate a repast model from the model dir without
#' loading the scenario file.
#' @details This is the entry point for model execution. Typically 
#' any model execution will start with this function which encapsulates 
#' all low level calls for model initialization. In order to perform 
#' simulations with repast from R code only \code{Model} and a 
#' few more function calls are required: \code{\link{Load}}, 
#' \code{\link{Run}}. Finally the output of model is managed with 
#' functions \code{\link{GetResults}} and \code{\link{SaveSimulationData}}.
#' 
#' @param modeldir The installation directory of some repast model
#' @param maxtime The total simulated time
#' @param dataset The name of any model aggregate dataset
#' 
#' @return Returns the instance of repast model
#' @examples \dontrun{
#'    d<- "C:/usr/models/your-model-directory"
#'    m<- Model(d)}
#'    
#' @references
#' [1] North, M.J., N.T. Collier, and J.R. Vos, "Experiences Creating Three Implementations of the Repast Agent Modeling Toolkit," ACM Transactions
#' on Modeling and Computer Simulation, Vol. 16, Issue 1, pp. 1-25, ACM,
#' New York, New York, USA (January 2006).
#' @export
Model<- function(modeldir="",maxtime=300,dataset="none") {
  if(dir.exists(modeldir)) {
    # Configure all required directory based on their default locations
    configModelDirs(modeldir)
    
    # Initilialized JVM
    # Setup classpath inferring default values from modeldir
    jvm.init()
    
    # Creating an engine instance
    e<- Engine()
    
    # Configure the total amount of time to be simulated
    Engine.endAt(e,maxtime)
    
    # Configure the dataset
    Engine.SetAggregateDataSet(e,dataset)
    
    return(e)
  } else {
    stop(paste0("The model directory does not exist: ", modeldir))
  }
}

#' @title The Scenario loader
#' @description Loads the model's scenario. This function must be 
#' called before running the model.
#' 
#' @examples \dontrun{
#'    d<- "C:/usr/models/your-model-directory"
#'    m<- Model(d)
#'    Load(m)}
#' 
#' @param e -- An engine object instance
#' 
#' @export
Load<- function(e) {
  Engine.LoadModel(e,getScenarioDir())
  setId(Engine.getId(e))
}

#' @title Run simulations
#' @description This function executes the time steps of an 
#' instantiated model. The number of replications of model 
#' runs can be specified by the function parameter. The seed 
#' parameter may be omitted and will be generated internally. 
#' If provided, the seed collection, must contain the same 
#' number of 'r' parameter. 
#'
#' @param e -- An engine object instance
#' @param r -- The number of experiment replications
#' @param seed - The random seed collection
#' 
#' @examples \dontrun{
#'    d<- "C:/usr/models/your-model-directory"
#'    m<- Model(d)
#'    Load(m)
#'    Run(m,r=2) # or Run(m,r=2,seed=c(1,2))}
#'    
#' @importFrom stats runif
#' @export
Run<- function(e,r=1,seed=c()) {
  # The default behaviour is if seed set was
  # not provided generate a suitable set of 
  # random seeds for the number of replications.
  if(length(seed) == 0) {
    seed= runif(r,-10^8,10^8)  
  } else if(length(seed) != r) {
    stop("The provided set of random numbers doesn't match replications!")
  }
  
  # Gets the current set of parameters
  p<- GetSimulationParameters(e)
  
  # Clear result repository
  ClearResults()
  
  SetResultsParameters(p)
  
  for(i in 1:r) {
    # Setting the random seed for experiment replication
    Engine.setParameter(e,getKeyRandom(),as.integer(seed[i]))
    
    Engine.RunModel(e)
    data<- GetOutput(e)
    print(paste0("run= ", i, " / rows=", nrow(data)))
    # Sets the current run number
    data$run<- i
    
    # Data data to replications
    AddResults(data)
  }
}

#' @title Run an experimental setup
#' 
#' @description Run the model multiple times for different parameters
#' given by design matrix function parameter.
#' 
#' @details The FUN function must return zero for perfect fit and values 
#' greater than zero otherwise.
#'
#' @param e -- An engine object instance
#' @param r -- The number of experiment replications
#' @param design -- The desing matrix holding parameter sampling
#' @param FUN -- THe calibration function.
#'
#' @export
RunExperiment<- function(e, r=1, design, FUN) {
  output<- c()
  
  for(i in 1:nrow(design)) {
    d<- design[i,]
    
    # -- Set parameters for next model 'Run'
    SetSimulationParameters(e, d)
    
    # -- Run model with current parameter set
    Run(e,r)
    
    results<- GetResults()
    
    # -- The user provided calibration function.
    # -- Calibration function must return 0 for perfect fit between 
    # -- observed and experimental data.
    deviation<- FUN(results)
    
    if(is.null(deviation)) {
      stop("Invalid user provided calibration function!")  
    }
    
    output<- rbind(output,cbind(deviation,d))
  } 
  return(output)
}

#' @title Gets the output
#' 
#' @description  Returns the results of a model a data.frame from the last
#' RUN. Should be used only if model replication is equal to 1,
#' otherwise GetResults must be used.
#' 
#' @param e -- An engine object instance
#' @return Returns a data.frame with output data
#' @examples \dontrun{
#'    d<- "C:/usr/models/your-model-directory"
#'    m<- RepastModel(d)
#'    ...
#'    data<- GetOutput(m)}
#'    
#' @importFrom utils read.csv
#' @export
GetOutput<- function(e) {
  c<- textConnection(Engine.GetModelOutput(e))
  read.csv(c)
}

#' @title Set parameters for running model
#' 
#' @description Modify the repast model parameters with 
#' values provided in parameter 'p' which is a data frame
#' with just one row.
#' 
#' @param e -- An engine object instance
#' @param p -- A data frame with simulation parameters
#' @export
SetSimulationParameters<- function(e, p) {
  if(is.null(e)) {
    stop("Engine object is null!")  
  }
  
  keys<- names(GetSimulationParameters(e))
  
  for(key in names(p)) {
    value<- p[1,key]
    if(is.factor(value)) {
      value<- levels(value)
    } 
    
    # Verify that "key" is a valid model parameter
    if(key %in% keys) {
      # Try to coerce the value to a type for safety
      switch(typeof(value),
             double = {
               #print(paste0("double", key,"<- ",value))
               value<- as.double(value)
             },
             integer = { 
               #print(paste0("integer", key,"<- ",value))
               value<- as.integer(value)
             },
             character = { 
               #print(paste0("character", key,"<- ",value))
               value<- as.character(value)
             })
      Engine.setParameter(e,key,value)          
    }
  }
}

#' @title Gets the simulation parameters
#' 
#' @description Returns a dataframe with the current set of input 
#' parameters for the last model run.
#' 
#' @param e -- An engine object instance
#' @return A data frame with simulation parameters
#'
#' @export
GetSimulationParameters<- function(e) {
  keys<- ""
  values<- ""
  names<- Engine.getParameterNames(e)
  for(n in names) {
    v<- Engine.getParameterAsString(e,n)
    if(nchar(keys) == 0){
      keys<- n
      values<- v
    } else {
      keys<- paste0(keys,",",n)
      values<- paste0(values,",",v)
    }
  }
  b<- rbind(keys,values)
  c<- textConnection(b)
  read.csv(c)
} 

#' @title Clear the results data.frame
#' 
#' @description This function is called automatically every
#' time Run method is called.
#'
#' @export
ClearResults<- function() {
  assign("pkg.results", data.frame(), pkg.globals)   
  assign("pkg.parameters", data.frame(), pkg.globals)   
}

#' Returns the model results
#'
#' @export
GetResults<- function() {
  return(get("pkg.results", pkg.globals))
}

#' Stores a data.frame 
#' 
#' @param d A data frame containing one replication data
#'
#' @export
SetResults<- function(d) {
  assign("pkg.results", d, pkg.globals)     
}

#' @title Concatenate results of multiple runs
#' 
#' @description This function stores the output 
#' of the last model execution and it is intended 
#' to be used internally.
#' 
#' @param d A data frame containing one replication data
#'
#' @export
AddResults<- function(d) {
  r<- GetResults()
  SetResults(rbind(r,d))
}

#' @title Gets the parameters
#' @description Returns the current set of paramters used 
#' for the last model run.
#'
#' @return A data.frame with parameters of the model.
#'
#' @export
GetResultsParameters<- function() {
  return(get("pkg.parameters", pkg.globals))
}

#' @title Sets the parameters
#' @description Save the current set of paramters used 
#' for the last model run.
#' 
#' @param d -- A data.frame with parameter values
#'
#' @export
SetResultsParameters<- function(d) {
  assign("pkg.parameters", d, pkg.globals)     
}

#' @title Saving simulation output
#' 
#' @description Saves the simulation results of last call to Run(e)
#' function.
#' 
#' @param as The desired output type, must be csv or xls
#' 
#' @importFrom xlsx write.xlsx
#' @importFrom digest digest
#' @importFrom utils write.csv
#' @export
SaveSimulationData<- function(as="csv") {
  # Creating output dir if needed
  createOutputDir()
  filename<- getId()
  if(is.na(filename)) {
    stop("Model was not initialized correctly!")
  }
  
  # The parameters of current simultation output
  parameters<- GetResultsParameters()
  
  # The results of simulation run
  results<- GetResults()
  
  hash <- digest(Sys.time(), algo="crc32")
  f0<- paste0(getOutputDir(),tolower(filename),"-",hash)
  f1<- paste0(getOutputDir(),tolower(filename),"-parameters-",hash)
  
  switch(as,
         csv = {
           f0<- paste0(f0,".csv")
           f1<- paste0(f1,".csv")
           write.csv(results, f0, row.names=FALSE)
           write.csv(parameters, f1, row.names=FALSE)
         },
         xls = { 
           f0<- paste0(f0,".xlsx")
           f1<- paste0(f1,".xlsx")
           write.xlsx(results, f0)
           write.xlsx(parameters, f1)
         })
}

##
## ----- Bellow sensitivity analysis functions
##

#' @title Adds a paramter to factor collection
#' 
#' @description Builds up the factor collection.
#' 
#' @param factors The current factor collection
#' @param lambda The function to apply FUN(p,min,max)
#' @param name The name of factor
#' @param min The minimun of parameter p
#' @param max The maximun of parameter p
#' 
#' @examples \dontrun{
#'    f<- AddFactor(name="Age",min=20,max=60)
#'    f<- AddFactor(factors=f, name="Weight",min=50,max=120)}
#' 
#' @return The collection of created factors
#'
#' @export
AddFactor<- function(factors=c(), lambda="qunif",name, min, max) {
  if(max < min) {
    stop("Invalid factor range!")
  }
  
  # if parameter already existe replace the current value
  rrow<- c(lambda=lambda,name=name,min=min,max=max)
  rownames(rrow)<- NULL
  if(length(factors) > 0 && factors[,"name"] == name) {
    i<- which(factors[,"name"] == name)  
    factors[i,]<- c(rrow)
  } else {
    factors<- rbind(factors,c(rrow))
  }
  return(factors)
}

#' @title Get the number of factors
#' 
#' @description Returns the total number of factors
#' 
#' @param factors -- A collection of factors created with AddFactor
#' 
#' @return The number of parameters in factors collection
#'
#' @export
GetFactorsSize<- function(factors) {
  n<- nrow(factors)
  if(is.null(n)) n<- 0
  return(n)
}

#' @title Corrects the LHS design matrix
#' 
#' @description Correct the LHS sampling matrix for a 
#' specific range applying the lambda function. The default
#' value of 'lambda' is 'qunif'.
#' 
#' @param design -- The LHS design matrix
#' @param factors -- THe collection of factors
#' 
#' @return The corrected design matrix
#'
#' @export
ApplyFactorRange<- function(design, factors) {
  k<- GetFactorsSize(factors)
  d<- sapply(1:k, function(p) {match.fun(factors[p,"lambda"])(design[,p],as.numeric(factors[p,"min"]),as.numeric(factors[p,"max"]))})
  d<- as.data.frame(d)
  names(d)<- factors[,"name"]
  return(d)
}

#' @title Generate a LHS sample for model parameters
#' 
#' @description Generate the LHS sampling for evaluating 
#' the parameters of a model.
#' 
#' @param n -- The number of samples
#' @param factors -- The model's parameters which will be evaluated
#' 
#' @return The LHS design matrix for provided parameters
#' 
#' @importFrom lhs randomLHS
#' @export
LatinHypercube<- function(n=10, factors=c()) {
  k<- GetFactorsSize(factors)
  if(k == 0) {
    stop("Empty factor collection!")
  }
  
  # --- Apply the desired range
  design<- ApplyFactorRange(randomLHS(n, k),factors)
  return(design)
}

#' @title Builds the simulation parameter set
#' 
#' @description Merges the design matrix with parameters which 
#' will be keep fixed along simulation runs.
#' 
#' @param design -- The experimental desing matrix for at least one factor
#' @param parameters -- All parameters of the repast model.
#' 
#' @return A data frame holding all parameters required for running the model
#' 
#' @examples \dontrun{
#'    modeldir<- "c:/usr/models/BactoSim(HaldaneEngine-1.0)"
#'    e<- Model(modeldir=modeldir,dataset="ds::Output")
#'    Load(e)
#'    
#'    f<- AddFactor(name="cyclePoint",min=40,max=90)
#     f<- AddFactor(factors=f, name="conjugationCost",min=1,max=80)
#'    
#'    p<- GetSimulationParameters(e)
#'    
#'    d<- LatinHypercube(factors=f)
#'    
#'    p1<- BuildParameterSet(d,p)}
#' 
#' @export
BuildParameterSet<- function(design, parameters) {
  v<- design
  tmp.p<- parameters
  for(n in names(design)) {
    # Drop parameters columns which are in design matrix
    tmp.p[n]<- NULL
  }
  
  # Now join two data frames
  for(i in 1:length(names(tmp.p))) {
    v<- cbind(tmp.p[i],v)      
  }
  return(v)
}


##> rm(list=ls())
##> library(raster)
##> my.cost<- function(r) { return(runif(1,0,10)) }
##> modeldir<- "c:/usr/models/BactoSim(HaldaneEngine-1.0)"
##> e<- Model(modeldir=modeldir,dataset="ds::Output")
##> Load(e)
##> my.cost<- function(r) { return(runif(1,0,10)) }
##> f<- AddFactor(name="cyclePoint",min=40,max=90)
##> f<- AddFactor(factors=f, name="conjugationCost",min=1,max=80)
##> d<- LatinHypercube(factors=f)
##> p<- GetSimulationParameters(e)
##> exp.design<- BuildParameterSet(d,p)
##> v<- RunExperiment(e,r=1,exp.design,my.cost)

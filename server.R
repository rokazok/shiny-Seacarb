#Interactive carbonate chemistry calculator using seacarb.
#Remy Okazaki
#Features:
#- smart variable ordering to get user inputs into seacarb in the right order
#- Uncertainty propagation
#- Interactive plots:
#-- single click to get information on nearest value
#-- brush to zoom-in; double-click to zoom out 
#- Upload your data, calculate carbonate chemistry, plot data, download data

# server.R
library(shiny); library(ggplot2); library(seacarb); library(data.table)
library(DT) #devtools::install_github('rstudio/DT') #Using the development version per: http://stackoverflow.com/questions/36656882/shiny-datatableproxy-function-doesnt-exist
options(datatable.integer64="character") #categorizes long id strings as characters

#Function
#data.table: Replace all -999 with NA #http://stackoverflow.com/a/7249454/4718512
f_dowle3 = function(DT, NA_vals) {   # either of the following for loops
  #for (j in names(DT))    set(DT,which( (DT[[j]]) == -999 ),j,NA)   # by name
  #for (j in seq_len(ncol(DT)))  set(DT, which( (DT[[j]]) %in% NA_vals_split ),j,NA)   # or by number (slightly faster than by name)
  NA_vals_split <- strsplit(NA_vals, split = " ")[[1]]
  for (j in seq_len(ncol(DT)))  set(DT, grep(paste(NA_vals_split, collapse = "|"), DT[[j]]),j,NA)
}


#Variables data.frame
var.units <- data.frame(var        = c("pH", "pCO2",    "ALK", "DIC", "CO2", "HCO3", "CO3"),
                        var.html   = c("pH", "pCO<sub>2</sub>", "ALK", "DIC", "CO<sub>2</sub>", "HCO<sub>3</sub><sup>-</sup>", "CO<sub>3</sub><sup>2-</sup>"),
                        comma     = c("", ", ", ", ", ", ", ", ", ", ", ", "),
                        html.units = c(  "", "&mu;atm", rep("&mu;mol kg<sup>-1</sup>", 5))
)
var.units[] <- lapply(var.units, as.character) #Convert each column from factor to character
var.units$selectable <- apply(var.units[, 2:4], 1, paste, collapse = "") #Combine HTML variable and units
#Seacarb flags data.frame.  This is used as a lookup table for the different combinations of input variables.
var.codes <-  as.data.frame(t(combn(x = c("pCO2", "pH", "CO2", "HCO3", "CO3", "ALK", "DIC"), m=2)))   #Unique combinations of 2 of variables
var.codes[] <- lapply(var.codes, as.character) #Convert each column from factor to character
var.codes$flag <- c(21,0,22,23,24,25,1,6,7,8,9,2,3,4,5,10,11,12,13,14,15) #pCO2-CO2 is not a valid combo, so label it 0 (easier than NA)

allvar.labels <- data.frame(var = c("S", "T", "Patm", "P", "pH", "CO2", "fCO2", "pCO2", "fCO2pot", "pCO2pot", "fCO2insitu", "pCO2insitu", "HCO3", "CO3", "DIC", "ALK", "OmegaAragonite", "OmegaCalcite"),
                          var.html = c("'salinity'", "'temperature'", "atmospheric~pressure", "'pressure'", "'pH'", "CO[2]", 
                                       rep(c("fCO[2]", "pCO[2]"), 3),
                                       "HCO[3]^'-'", "CO[3]^'2-'", "dissolved~inorganic~carbon", "total~alkalinity", "Omega[aragonite]~''", "Omega[calcite]~''"),
                          comma = c("", rep("*','~", 3), "", rep("*','~", 11), "", ""),
                          html.units = c("", "degree*C", "atm", "dbar", "", "mu*mol~kg^-1", 
                                         rep(c(paste0("mu*atm~", c("'('*T[italic('in situ')]~P[atm]*')'", "'('*T[potential]~P[atm]*')'", "'('*T[italic('in situ')]~P[total]*')'"))), each = 2), 
                                         rep("mu*mol~kg^-1", 4), "", ""),
                          leg.html = c("'S'~''", "'T'~''", "P[atm]~''", "'P'~''", "'pH'~''", "CO[2]~''", 
                                       rep(c("fCO[2]~''", "pCO[2]~''"),3), "HCO[3]^'-'", "CO[3]^'2-'", "'DIC'", "'TA'", "Omega[arag]~''", "Omega[calc]~''"),
                          leg.units = c("", "''~degree*C", "atm~''", "dbar~''", "", "mu*mol~kg^-1", rep("mu*atm~''", 6), rep("mu*mol~kg^-1", 4), "", "")
                )
temp <- allvar.labels   #Make more labels for simulated errors
temp <- within(temp, {
  var <- paste0("e.", var)
  var.html <- paste0("'simulated error in'~", var.html)
  leg.html <- paste0("'sim. err.'~", leg.html)
})
allvar.labels <- rbind(allvar.labels, temp); rm(temp)
allvar.labels[] <- lapply(allvar.labels, as.character) #Convert each column from factor to character
allvar.labels$selectable <- apply(allvar.labels[, 2:4], 1, paste, collapse = "") #Combine HTML variable and units

uploadColWidths <- c(2,6,2,2) #Upload tab renderUI column widths



shinyServer(function(input, output, session) {
  
  #Input carbon parameters need to be entered into a specific order in seacarb, but they can be entered in any order in Shiny.
  #To solve this problem, figure out which combination of input carbon parameters are selected, and select the corresponding seacarb::carb() flag and units.
  #VarA and VarB are the first and second variables in the ui, respectively.  They are later rearranged into var1 and var2 for seacarb

  #Lookup units for the selected parameters and print them so users know what units to enter their inputs in.
  output$WhatAreVarUnits <- renderUI({
    A.units <- var.units[var.units$var %in% input$varA.type, "selectable"]
    B.units <- var.units[var.units$var %in% input$varB.type, "selectable"]
    ( div(HTML(paste("Selected:", A.units , "and",  B.units) )) )
  })
  #Choose the N for the error simulations.  More = slower, but more resolution.
  output$Nsims <- renderUI({ 
    paste("Monte Carlo N =",     N() )
  })
  
  #Determine which flag to use for seacarb::carb()
  seacarbFlag <- reactive({
    #Match VarA and VarB inputs to lookup table V1 and V2 OR V2 and V1.
    var.codes[var.codes$V1 == input$varA.type & var.codes$V2 == input$varB.type |
              var.codes$V1 == input$varB.type & var.codes$V2 == input$varA.type, "flag"]
    #More efficient version of the above code: 
    #myvars <- c(input$varA.type, input$varB.type)
    #var.codes[var.codes$V1 %in% myvars & var.codes$V2 %in% myvars,]
  }) 
  
  N <- reactive(round(10^input$nsim)) #converted N simulations from decimal number into an integer

  #Determine if input value is a single value or range:
  #If values were entered as a range, choose values between min,max.  Otherwise, same value
  Single.or.Range <- function(input, N) {
    var.in <- as.numeric(unlist(strsplit(input, ",")))
    if(length(var.in) == 2) { 
      output <- runif(n = N, min= var.in[1], max= var.in[2]) #randomly choose values between min,max
      #output <- seq(from = var.in[1], to = var.in[2], length.out = N) #choose N evenly spaced values between min,max.  This feature only works well with 1 parameter range.
    } else {
      output <- rep(var.in[1], N)
    }
    return(output)
  }
  #Use the Single.or.Range function above to select input values
  varA     <- reactive({ Single.or.Range(input$varA, N())    })
  varB     <- reactive({ Single.or.Range(input$varB, N() ) })
  varS     <- reactive({ Single.or.Range(input$S, N() ) })
  varT.lab <- reactive({ Single.or.Range(input$T.lab, N() ) })
  varT.insitu <- reactive({ Single.or.Range(input$T.insitu, N() ) })
  varP     <- reactive({ Single.or.Range(input$P, N() ) })
  varPt    <- reactive({ Single.or.Range(input$Pt, N() ) })
  varSit   <- reactive({ Single.or.Range(input$Sit, N() ) })
  
  #Input data to use for seacarb
  sim.data <- reactive({
    #Based on the seacarb flag identified, assign user inputs A and B to seacarb::carb() ordered inputs 1 and 2
    var1.type <- var.codes[var.codes$flag == seacarbFlag(), "V1"] #V1 = first parameter
    var2.type <- var.codes[var.codes$flag == seacarbFlag(), "V2"] #V2 = second parameter
    #uncertainty of input variables. If removing, define uvA and uvB = 1.
    uvA <- as.numeric(input$u.varA)  #uncertainty with input variables
    uvB <- as.numeric(input$u.varB)
    if(input$varA.type == var1.type) { 
      var1 <- varA() 
      u1   <- uvA
      } else {
      if(input$varA.type == var2.type) {
        var2 <- varA() 
        u2 <- uvA
      }
    }
    if(input$varB.type == var1.type) { 
      var1 <- varB()
      u1 <- uvB
      } else {
      if(input$varB.type == var2.type) { 
        var2 <- varB() 
        u2 <- uvB
        }
      }
    #Convert umol kg-1 to mol kg-1 for seacarb ONLY IF the inputs are ALK, DIC, CO2, HCO3, or CO3
    if(var1.type %in% c("ALK", "DIC", "CO2", "HCO3", "CO3")) { 
      var1 <- var1 *1e-6 
      u1   <- u1 *1e-6
    }
    if(var2.type %in% c("ALK", "DIC", "CO2", "HCO3", "CO3")) { 
      var2 <- var2 *1e-6 
      u2  <- u2 *1e-6
    }
    return(list(c(u1, u2),
                data.frame(var1, var2, S = varS(), T.lab = varT.lab(), T.insitu = varT.insitu(), P = varP(), varPt = varPt(), varSit = varSit() ) 
                ) )
  })
  #CALCULATE INSITU pH
  #How to solve for pH insitu at temperature & pressure using seacarb:
  #1) Use DIC or TA (both conservative with T & P) to calculate the other variable at laboratory T and P(=0) using seacarb::carb()
  #2) Recalculate pH using TA and DIC at the in situ T & P.
  #DIC1 <- carb(8, c(8, 7.9, 7.6), 2300e-6)$DIC; DIC1 #Flag 8 = pH,ALK
  #carb(15, 2300e-6, DIC1, T=5, P=4500/10) #Flag 15 = ALK, DIC.
  #when pH.lab = 8.0, pH = 8.1412. Co2Sys pHinsitu = 8.1409
  #when pH.lab = 7.9, pH = 8.0361. Co2Sys pHinsitu = 8.0358
  #when pH.lab = 7.6, pH = 7.7111. Co2Sys pHinsitu = 7.7107
  #values calculated using recommended constants (Dickson et al., 2007)

  #Output from seacarb:::carb(input)
  #These are the calculations without any simulated errors
  #sim.data() is a list with uncertainty as item 1 and inputs as item 2
  outdata <- reactive({   #yy = lab (assume P=0 bar), zz = insitu
      yy <- carb(flag = seacarbFlag(), var1 = sim.data()[[2]]$var1, var2 = sim.data()[[2]]$var2, 
                 S= sim.data()[[2]]$S, T= sim.data()[[2]]$T.lab, P=0, Pt = sim.data()[[2]]$varPt*1e-6, Sit = sim.data()[[2]]$varSit*1e-6,
                 k1k2 = input$k1k2, kf = input$kf, ks = input$ks, pHscale = input$pHscale, b = input$b, gas = input$gas)
      zz <- carb(flag = 15, var1 = yy$ALK, var2 = yy$DIC, 
                 S= yy$S, T= sim.data()[[2]]$T.insitu, P=sim.data()[[2]]$P/10, Pt = sim.data()[[2]]$varPt*1e-6, Sit = sim.data()[[2]]$varSit*1e-6,
                 k1k2 = input$k1k2, kf = input$kf, ks = input$ks, pHscale = input$pHscale, b = input$b, gas = input$gas)
      yy[ , c("CO2", "HCO3", "CO3", "DIC", "ALK")] <- yy[ , c("CO2", "HCO3", "CO3", "DIC", "ALK")]*1e6  #Convert mol kg-1 back to umol kg-1
      zz[ , c("CO2", "HCO3", "CO3", "DIC", "ALK")] <- zz[ , c("CO2", "HCO3", "CO3", "DIC", "ALK")]*1e6  #Convert mol kg-1 back to umol kg-1
      names(yy) <- paste0(names(yy), ".lab")
      allData <- cbind(yy, zz)
      allData[, c("P.lab", "P")] <- allData[, c("P.lab", "P")] * 10  #Convert bar back to dbar
      return(allData)
  })
#IF removing uncertainty, comment out the below until the redefined 'final' dataframe
  #Monte Carlo simulation: uniformly distributed errors from uncertainty values.
  k <- 2 #coverage factor. U = expanded uncertainty = k * u = coverage factor * standard uncertainty.
  var.u <- reactive({  as.numeric(sim.data()[[1]] )  })  #Var1 and Var2 uncertainties
  u1 <- reactive(runif(N(), min = -1*k*var.u()[1], max = k*var.u()[1] ))
  u2 <- reactive(runif(N(), min = -1*k*var.u()[2], max = k*var.u()[2] ))
  u.S <-reactive(runif(N(), min = -1*k*as.numeric(input$u.S), max = k*as.numeric(input$u.S) ))
  u.T.lab <-reactive(runif(N(), min = -1*k*as.numeric(input$u.T.lab), max = k*as.numeric(input$u.T.lab) ))
  u.T.insitu <-reactive(runif(N(), min = -1*k*as.numeric(input$u.T.insitu), max = k*as.numeric(input$u.T.insitu) ))
  u.P <-reactive(runif(N(), min = -1*k*as.numeric(input$u.P), max = k*as.numeric(input$u.P) ))
  u.Pt <- reactive(runif(N(), min = -1*k*as.numeric(input$u.Pt), max = k*as.numeric(input$u.Pt) ))
  u.Sit <-reactive(runif(N(), min = -1*k*as.numeric(input$u.Sit), max = k*as.numeric(input$u.Sit) ))
  #Combine these Monte Carlo simulated errors to get alternative input dataset: sim.u
  sim.u <- reactive({
    all.u <- data.frame(u1 = u1(), u2 = u2(), S = u.S(), T.lab = u.T.lab(), T.insitu = u.T.insitu(), P = u.P(), Pt = u.Pt(), Sit = u.Sit() )
    return(sim.data()[[2]] + all.u)
  })
  
  #Residuals of simulated error values - actual values, cbind() with actual output.
  final <- reactive({  #yy = lab (assume P=0 bar), zz = insitu
    yy <- carb(flag = seacarbFlag(), var1 = sim.u()$var1, var2 = sim.u()$var2, 
               S= sim.u()$S, T= sim.u()$T.lab, P=0, Pt = sim.u()$Pt*1e-6, Sit = sim.u()$Sit*1e-6,
               k1k2 = input$k1k2, kf = input$kf, ks = input$ks, pHscale = input$pHscale, b = input$b, gas = input$gas)
    zz <- carb(flag = 15, var1 = yy$ALK, var2 = yy$DIC, 
               S= yy$S, T= sim.u()$T.insitu, P=sim.u()$P/10, Pt = sim.u()$Pt*1e-6, Sit = sim.u()$Sit*1e-6,
               k1k2 = input$k1k2, kf = input$kf, ks = input$ks, pHscale = input$pHscale, b = input$b, gas = input$gas)
    yy[ , c("CO2", "HCO3", "CO3", "DIC", "ALK")] <- yy[ , c("CO2", "HCO3", "CO3", "DIC", "ALK")]*1e6  #Convert mol kg-1 back to umol kg-1
    zz[ , c("CO2", "HCO3", "CO3", "DIC", "ALK")] <- zz[ , c("CO2", "HCO3", "CO3", "DIC", "ALK")]*1e6  #Convert mol kg-1 back to umol kg-1
    names(yy) <- paste0(names(yy), ".lab")
    simWrong <- cbind(yy, zz)
    simWrong[, c("P.lab", "P")] <- simWrong[, c("P.lab", "P")] * 10  #Convert bar back to dbar
    errorz <- simWrong - outdata()  #Residual error due to uncertainty = erroneous - actual
    names(simWrong) <- paste0("w.", names(simWrong))
    names(errorz) <- paste0("e.", names(errorz))
    final.cbind <- cbind(outdata(), simWrong, errorz)
    return(final.cbind )
  })
  #End of uncertainty simulation
  ##final <- reactive({ outdata() })  #Remove this if using uncertainty (currently commented out)
  
  #START SNIP
  #This line works locally, but shiny flags it as a potential source of error when I upload the app. Uploaded app does not work so I am commenting it out.
  #OLD: VarZ <- reactive(names(final() )[grep("\\bflag\\b|\\b.lab\\b", names(final()), invert=TRUE ) ] )
  VarZ <- reactive(names(final() )[grep("\\b(w.)|(flag)\\b|(.lab)\\b", names(final()), invert=TRUE ) ] ) #select inverse of words beginning in 'e.' OR ending in 'flag' OR ending in '.lab'
  VarResids <- reactive(names(final() )[grep("\\b(e.)", names(final()), invert=FALSE ) ] ) #select words beginning with 'r.'
  #Code below is an older workaround solution to select columns based on position instead of regex pattern.
  #VarZ <- reactive(names(final()[-(1:20)] ) )
  #END SNIP
  
  #Reactive UI for graphing
  output$plot.params <- renderUI({
    div(
      helpText("Select graphing parameters. Prefix 'e.' indicates simulated error."),
      fluidRow(column(class = "LP", 2, HTML("X:") ),
               column(class = "LP", 10, selectInput(inputId = "plot.x", label = NULL, choices =isolate(VarZ()), selected = isolate(input$varA.type), multiple = FALSE, selectize = TRUE, width = NULL, size = NULL) )
      ), #close fluidRow
      fluidRow(column(class = "LP", 2, HTML("Y:") ),
               column(class = "LP", 10, selectInput(inputId = "plot.y", label = NULL, choices = isolate(VarZ()), selected = "pCO2", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL) ) #isolate(paste0("e.", input$varA.type))
      ), #close fluidRow
#NOTE: Overylaying simulated errors is not as useful as a separate error plot
#fluidRow(HTML("(Optional): overlay simulated errors?"),
#               column(class = "LP", 2, HTML("Y2:") ),
#               column(class = "LP", 10, checkboxInput(inputId = "plot.y2", label = NULL, value = FALSE, width = NULL) ) 
#      ), #close fluidRow
      fluidRow(column(class = "LP", 2, HTML("color:") ),
               column(class = "LP", 10, selectInput(inputId = "plot.colour", label = NULL, choices = isolate(VarZ()), selected = isolate(input$varB.type), multiple = FALSE, selectize = TRUE, width = NULL, size = NULL) )
      )  #close fluidRow
    ) #close div
  }) #End output$plot.params
  #PLOT tab
  brush.ranges <- reactiveValues(x = NULL, y = NULL)
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      brush.ranges$x <- c(brush$xmin, brush$xmax)
      brush.ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      brush.ranges$x <- NULL
      brush.ranges$y <- NULL
    }
  })

  
  plotLabs.x <- reactive({ bquote( .( eval(parse(text = allvar.labels[allvar.labels$var %in% input$plot.x, "selectable"] )) ) ) })
  plotLabs.y <- reactive({ bquote( .( eval(parse(text = allvar.labels[allvar.labels$var %in% input$plot.y, "selectable"] )) ) ) })
  plotLabs.colour <- reactive({ bquote( atop( .( eval(parse(text = allvar.labels[allvar.labels$var %in% input$plot.colour, "leg.html" ])) ),
                                              .( eval(parse(text = allvar.labels[allvar.labels$var %in% input$plot.colour, "leg.units"])) )) 
                                        ) })

  output$PLOT <- renderPlot({
    if (!is.null(input$plot.x) & !is.null(input$plot.y)){
      myPlot <- ggplot(final(), aes_string(x = input$plot.x, y = input$plot.y)) + 
        geom_point(aes_string(colour = input$plot.colour)) + theme_bw() +
        labs(x = plotLabs.x(), y = plotLabs.y(), colour = plotLabs.colour())
      myPlot <- myPlot + coord_cartesian(xlim = brush.ranges$x, ylim = brush.ranges$y) 
      #      if(input$plot.y2 == TRUE) {   #Simulated error point overlays
      #      myPlot <- myPlot + geom_point(aes_string(y = paste0("e.", input$plot.y) ), colour = "grey50", alpha = 0.3) 
      #      }
      myPlot
      #Do not print plots if you want them to be interactive
    }
  })
  
  plotInput <- function() {
    myPlot <- ggplot(final(), aes_string(x = input$plot.x, y = input$plot.y)) + 
      geom_point(aes_string(colour = input$plot.colour)) + theme_bw() +
      labs(x = plotLabs.x(), y = plotLabs.y(), colour = plotLabs.colour())
    myPlot <- myPlot + coord_cartesian(xlim = brush.ranges$x, ylim = brush.ranges$y) 
    #    if(input$plot.y2 == TRUE) {
    #      myPlot <- myPlot + geom_point(aes_string(y = paste0("e.", input$plot.y) ), colour = "grey50", alpha = 0.3) 
    #    }
    myPlot
  }
  download_plotName <- reactive({ 
    fileName <- paste0(input$plot.y,"-",input$plot.x,".",input$plot_filetype)
    return(fileName)
  })
  output$plotDownloadButton <- downloadHandler(
    filename = download_plotName,
    content = function(file) {
      ggsave(file, plot = plotInput(), device = input$plot_filetype)
    })

  output$u.TABLE <- renderTable({ 
    errz <- final()[, grep("e\\.(?!flag|lab)", names(final() ), perl= TRUE )[] ]  
    #regex: Select columns with 'e.' prefix unless followed by (?!) 'flag' or 'lab'
    errz.names <- names(errz)
    errz.names <- gsub("e\\.(?!lab)", "", errz.names, perl= TRUE) #Remove the prefix 'e.'
    out.errz <- data.frame( parameter = errz.names,
                            sd = apply(errz, 2, sd), 
                            units = rep(c("", "&deg;C", "atm", "bar", "", "&mu;mol kg<sup>-1</sup>", rep("&mu;atm", 6), rep("&mu;mol kg<sup>-1</sup>", 4), "", ""), 2))
    row.names(out.errz) <- NULL  #&mu;mol kg<sup>-1</sup>
    return(out.errz)    
  }, digits = 3, sanitize.text.function = function(x) x)
  
  output$final.str <- renderDataTable( final() )
  #output$troubleshootin <- renderPrint( TEST()  )   #head(final() )
  
  output$plot_click_output <- renderTable({
    #Remove lab and flag columns
    VarMain <- reactive(names(final() )[grep("\\b(w.|e.)|(flag)\\b|(.lab)\\b", names(final()), invert=TRUE ) ] ) #Note: cannot click point when errors are plotted because these columns are excluded.
    NP <- nearPoints(final()[, VarMain() ],
                     xvar = input$plot.x, yvar = input$plot.y, input$plot_click, threshold = 10, maxpoints = 1, addDist = FALSE)
    output.NP <- data.frame(value = t(NP),
                            units = c("", "&deg;C", "atm", "dbar", "", "&mu;mol kg<sup>-1</sup>", rep("&mu;atm", 6), rep("&mu;mol kg<sup>-1</sup>", 4), "", "")
    )
    if(dim(output.NP)[2] == 2) {  names(output.NP) <- c("value", "units") } #If user clicks on data; output.NP will be 2 columns. Rename them.
    return(output.NP)
  }, digits = 3, sanitize.text.function = function(x) x)
  

  #UPLOAD tab
  #Process uploaded data
  dt <- reactive({if(is.null(input$inputFile)) return(NULL)
                        qq <- data.table::fread(input$inputFile$datapath, header = TRUE, na.strings = c("NA","#DIV/0!"))
                        #f_dowle3(qq, NA_vals = input$uplNAs)        #convert -999 to NA (see function above)
                        return(qq)
  })
  #Recode user-defined NA strings to NA
  observeEvent(  
    input$upl_f_dowle3, {   #when action button is pushed, then update the isolated code
    isolate({
      f_dowle3(dt(), NA_vals = input$uplNAs)        #convert user defined NA strings to NA (see function above)
      output$upl_str <- renderPrint({ str(dt()) })
     })} )
  
  dtNames <- reactive({ names(dt())  })
  
  output$upl_str <- renderPrint({ str(dt()) })

  output$troubleshooting <- renderPrint({list(input$uplNAs, input$upl_f_dowle3, dt()[1:3,1:7] )})#input$prDT_rows_selected })
  #Once data are uploaded, create a reactive UI with options selected from the uploaded data.
  #However, I want users to see the static part of the UI (column #, variable, unit) before they upload their data,
  #so I will make 2 UIs: one with static information and one reactive UI with dropdown selections based on uploaded data.
  #Reference for reactive table: http://stackoverflow.com/a/21610888/4718512
  #upload table has information on column order, variable, unit, and reactive selectInput
  upl_table <- data.frame(Column = c(1:9,rep("",7)), 
                          Variable = c("ID", "date/time", "Salinity (S)", "total phosphate (PO<sub>4</sub><sup>3-</sup>)", "total silicate (Si(OH)<sub>4</sub><sup></sup>)",
                                       "temperature input (T<sub>input</sub>)", "pressure input (P<sub>input</sub>)", "temperature in situ (T<sub>out</sub>)", "pressure in situ (P<sub>out</sub>)",
                                       "CO<sub>2</sub> partial pressure (pCO2)", "pH", "dissolved inorganic carbon (DIC)", "total alkalinity (ALK)", "CO<sub>3</sub><sup>2-</sup>", "HCO<sub>3</sub><sup>-</sup>", "CO<sub>2</sub>"),
                          Units = c(rep("", 3), rep("&mu;mol kg<sup>-1</sup>", 2), rep(c("&deg;C", "dbar"), 2), "&mu;atm", "", rep("&mu;mol kg<sup>-1</sup>", 5)),
                          Select = rep("", 16),
                          inputIDnames = paste0("upl_", c("ID", "datetime", "S", "Pt", "Sit", "Tin", "Pin", "Tout", "Pout", "pCO2", "pH", "DIC", "ALK", "CO3", "HCO3", "CO2"))
  )
  #do.call() requires a list, so break up upl_table into a list with each row as a sublist.
  #http://stackoverflow.com/questions/3492379/data-frame-rows-to-a-list
  upl_list <- split(upl_table[,1:4], seq(nrow(upl_table[,1:4])))
  upl_inputids <- setNames(split(as.character(upl_table$inputIDnames), seq(length(upl_table$inputIDnames))), rep("inputId", length(upl_table$inputIDnames)))
 
  myheader <- #reactive({  #Reactive not needed for the header
    tags$thead(
      tags$tr( lapply( c("Column", "Variable", "Units", "Select"), function( x ) tags$th( x ) ) )
    )
  #})

    mybody <- reactive({
      rows <- list()
      naymez <- isolate(dtNames())
      #Static UI with column #, variable, and unit
      for( i in 1:nrow( upl_table )) {
        rows[[i]] <- tags$tr( lapply( upl_table[i,1:4], function( x ) {  tags$td( HTML(as.character(x)) )  }) )
      }
      #rows <- tags$td( lapply(upl_list, function(x) { do.call( HTML, x ) }) ) 
      #Dynamic UI = static UI + select columns drop-down menu
      #Arguments: selectInput(inputId, label, choices, selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)
      if(!is.null(dt())) {
        for( i in 1:nrow( upl_table ) ) {
          rows[[i]][3]$children[[1]]$Select <- lapply( upl_inputids[[i]], function( rx ) {  
            tags$td( selectInput(rx, label=NULL, choices = c("", "", naymez), selected = naymez[as.numeric(as.character(upl_table$Column[i] ))], width = "100%")
                    #old code. doesn't work: do.call(selectInput, 
                    #         c(rx, label = NULL, 
                    #           choices = c("", naymez), 
                    #           selected = naymez[3],#[as.numeric(as.character(upl_table$Column[i] ))],
                    #           multiple = FALSE, selectize = TRUE, width = "30%", size = NULL
                    #           )) 
                     ) #close tags$td(  
            })  #close lapply function{
        } #close for loop
      }   #close if statement
    tags$tbody( rows )
  })   #close reactive
                           
  output$uplRenderUI <- renderUI({
    tags$div(class = ".zebraTable", 
      tags$table(
        myheader,
        mybody()
      )
    )
  })
  pr <- data.frame(dummy = NA)

  #Process the uploaded data: calculate carbonate chemistry using carb()
  pr <- reactive({  #pr = processed
    input$upl_calculate   #when action button is pushed, then update the isolated code
    isolate({
    #1) Figure out which flag to use based on selected inputs
    upl_var.codes <- var.codes[var.codes$V1 %in% input$upl_varAvarB & var.codes$V2 %in% input$upl_varAvarB, ] 
    #2) Figure out which order to enter variables into carb based on selected flag.
    #input is a list, so reference elements within input via doublebrackets, i.e. input[[listElement]]
    dt()[, UNO := as.numeric(get( input[[paste0("upl_",upl_var.codes$V1)]] ))]
    dt()[, DOS := as.numeric(get( input[[paste0("upl_",upl_var.codes$V2)]] ))]  

    #3) data.table operations to create columns out of input variables
    dt()[, id := get(input$upl_ID)]
    dt()[, timestamp := get(input$upl_datetime)]
    dt()[, Sin := as.numeric(get(input$upl_S))]
    dt()[, Tin := as.numeric(get(input$upl_Tin))]
    dt()[, Tout := as.numeric(get(input$upl_Tout))]
    dt()[, Pin.bar := as.numeric(get(input$upl_Pin))/10] #Calculate P in bar from dbar
    dt()[, Pout.bar := as.numeric(get(input$upl_Pout))/10] #Calculate P in bar from dbar
    dt()[, PT  := as.numeric(get(input$upl_Pt))*1e-6]  #Calculate phosphate in mol kg-1 from umol kg-1
    dt()[, SIT  := as.numeric(get(input$upl_Sit))*1e-6] 
    #If checkbox for nutrients NA = 0, then convert those rows with NA to 0
    if(input$upl_AssumeNuts0 == TRUE) {
      dt()[is.na(PT),  PT  := 0]
      dt()[is.na(SIT), SIT := 0]
    }

    #4) Create a subset of the original data.table where all NA values are discarded. Then combine with the full table.
    filterNAfrom_dt <- dt()[, .(Sin, Tin, Tout, Pin.bar, Pout.bar, PT, SIT, UNO, DOS)]  #or dt()[, c("colname1", "colname2", ...), with=FALSE]
      #This should create a new data.table. Use .Internal(inspect(filterNAfrom_dt)) to inspect for differences
    filterNAfrom_dt <- na.omit(filterNAfrom_dt) #Remove NAs
    setkeyv(dt(), names(filterNAfrom_dt)) #Set keys of larger data.table to allow the merge in the next step
    subset_dt <- dt()[filterNAfrom_dt]   #All remaining data after NA-filtering the important columns. #Equivalent to merge(filterNAfrom_dt, dt, all.x = TRUE)
    
    #5) Convert umol kg-1 to mol kg-1 for seacarb ONLY IF the inputs are ALK, DIC, CO2, HCO3, or CO3
    upl_var1 <- subset_dt[, UNO] #returns a vector. Can do this in one step by chaining: DT[, operation1][, operation2][,operation3]
    upl_var2 <- subset_dt[, DOS] #because there's no sorting  
    if(upl_var.codes$V1 %in% c("ALK", "DIC", "CO2", "HCO3", "CO3")) { 
      upl_var1 <- upl_var1 *1e-6 
    }
    if(upl_var.codes$V2 %in% c("ALK", "DIC", "CO2", "HCO3", "CO3")) { 
      upl_var2 <- upl_var2 *1e-6 
    }
    
    #6) Put the data into an INPUT data.frame to feed into carb()
    INPUT <- data.frame(id = subset_dt[, id], timestamp = subset_dt[, timestamp],
                       flag = upl_var.codes$flag, var1 = upl_var1, var2 = upl_var2,
                       S = subset_dt[, Sin], Tin = subset_dt[, Tin], Tout = subset_dt[, Tout],
                       Pin = subset_dt[, Pin.bar], Pout = subset_dt[, Pout.bar], Pt = subset_dt[, PT], Sit = subset_dt[, SIT])
    
    #7) calculate laboratory conditions (P = Pin). See code for data.table operation on named column.
    userUplData.lab <- carb(flag = INPUT$flag, var1 = INPUT$var1, var2 = INPUT$var2,
                        S = INPUT$S, T= INPUT$Tin, P = INPUT$Pin, Pt = INPUT$Pt, Sit = INPUT$Sit,
                        k1k2 = input$upl_k1k2, kf= input$upl_kf, ks = input$upl_ks, b= input$upl_b,
                        pHscale = input$upl_pHType, gas = input$upl_gasType)
    #8) calculate in situ conditions using TA and DIC (which do not change over T and P)
    userUplData.insitu <- carb(flag = 15, var1 = userUplData.lab$ALK, var2 = userUplData.lab$DIC, 
                           S= userUplData.lab$S, T= INPUT$Tout, P= INPUT$Pout, Pt = INPUT$Pt, Sit = INPUT$Sit,
                           k1k2 = input$upl_k1k2, kf = input$upl_kf, ks = input$upl_ks, b = input$upl_b, 
                           pHscale = input$upl_pHType, gas = input$upl_gasType)
    #9) combine the outputs and convert carb() units to common units
    userUplData.lab[ , c("CO2", "HCO3", "CO3", "DIC", "ALK")] <- userUplData.lab[ , c("CO2", "HCO3", "CO3", "DIC", "ALK")]*1e6  #Convert mol kg-1 back to umol kg-1
    userUplData.lab$P <- userUplData.lab$P * 10 #Convert bar to dbar
    userUplData.insitu[ , c("CO2", "HCO3", "CO3", "DIC", "ALK")] <- userUplData.insitu[ , c("CO2", "HCO3", "CO3", "DIC", "ALK")]*1e6  #Convert mol kg-1 back to umol kg-1
    userUplData.insitu$P <- userUplData.insitu$P * 10
    names(userUplData.lab) <- paste0(names(userUplData.lab), ".input")
    #IDcols <- as.data.frame(INPUT[, c("id", "timestamp", "Pt", "Sit"), with = FALSE])
    userProcessed <- cbind(userUplData.lab, userUplData.insitu) #cbind(IDcols, userUplData.lab, userUplData.insitu)
    userProcessed <- userProcessed[, !(names(userProcessed) %in% c("flag.input", "flag", "Patm", "S", "ALK", "DIC"))]
    
    #10) Merge input data with the rest of the retained original data
    subset_dt[, c("id", "timestamp", "Sin", "Tin", "Tout", "Pin.bar", "Pout.bar", "PT", "SIT", "UNO", "DOS") := NULL] #Delete multiple columns
    userProcessed <- cbind(as.data.frame(subset_dt), userProcessed) #First element determines the class() of cbind() output.    
    return(userProcessed)
    
    }) #Close isolate
  }) #close reactive
  
  #str() summary of processed data
  output$upl_test <-  renderPrint({ 
    if(is.data.table(dt()) == FALSE) { return(NULL) } else
      if(is.data.table(dt()) == TRUE & !is.null(pr()) ) 
      {
        str(pr())  
      }
  })

  #Check if uploaded data has been processed.
  #If it has, use the check to create a conditional tab with the output data.
  pr_check <- reactiveValues(check = FALSE)
  observeEvent(input$upl_calculate, {   #when action button is pushed, then update the isolated code
    if(!is.null(pr()) ){#&  all(c("fCO2insitu.input", "OmegaAragonite") %in% names(pr() )) ) {
      pr_check$check <- TRUE
    }
  }
  )

  output$upl_successfulProcessingCheck <- renderUI({
    input$upl_calculate   #when action button is pushed, then update the isolated code
    isolate({
    if(is.data.table(dt()) != FALSE  ) {
      div(
        HTML("<b>6)</b>"),
        downloadButton("upl_downloadData", "Download"),
        br(),
        HTML("<b>See \"Processed\" tab for raw data or download CSV file below."),
        br(),
        HTML("Preview processed data:<br>&nbsp;&nbsp; $Column  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; : variableType &nbsp;&nbsp;&nbsp; Row1 Row2 Row3 ... "),
        br(),
        verbatimTextOutput("upl_test"),
        br()
      )
    } else { div(br()) }
    })
  })
  

  output$upl_downloadData <- downloadHandler(
    filename = function() { 
      paste0("output_", format(Sys.Date(), "%Y%m%d"), ".csv") 
    },
    content = function(file) {
      write.table(pr(), file, sep=",", row.names=FALSE)
    }
  )


  #Add CONDITIONAL data.table tab
  #Add plotting + brush/zoom + label + plot download
  #PROCESSED TAB
  #Interactive datatable (DT): https://yihui.shinyapps.io/DT-info/
  # render the table (with row names)
  output$prDT <- DT::renderDataTable( pr(), filter = "top" )
  prProxy <- dataTableProxy("prDT")  #Create a proxy table for interacting via external widgets


  output$pr_plotOptions <- renderUI({
    if(pr_check$check == TRUE) {
      namez <- names(pr())
    div(
      fluidRow(column(3,
        flowLayout(
        selectInput(inputId = "pr_xvar", label = "X", choices = namez, selected = "pH" ),
        selectInput(inputId = "pr_yvar", label = "Y", choices = namez, selected = "fCO2" ),
        selectInput(inputId = "pr_colour", label = "color", choices = namez, selected = "T" ),
        actionButton("pr_resetSingleClick", label = "Clear selection"),
        helpText(HTML("Select = click on plot or table&nbsp;
                       Zoom In = drag and double-click&nbsp;
                       Zoom out = double-click"))
        )),
      column(9, 
        plotOutput("pr_ggplot", click = "pr_singleClick", height = 500, dblclick = "pr_doubleClick", brush = brushOpts(id = "pr_plotBrush", resetOnNew = TRUE, delay = 1000, delayType = c("debounce", "throttle")) )
      )
      ),
      DT::dataTableOutput("prDT")
      ) #close flowLayout and div
    }
  })

  filteredDF <- reactiveValues( somedata = NULL, allRows = NULL, newRow = NULL)
  observeEvent(pr_check$check,
          {
            if(pr_check$check == TRUE) { filteredDF$somedata <- pr() }
          })
  prNP <- reactive(nearPoints(df = filteredDF$somedata,
                              xvar = input$pr_xvar, yvar = input$pr_yvar, input$pr_singleClick, threshold = 10, maxpoints = 1, addDist = FALSE))
  
  observeEvent(
    prNP(),
    {
      filteredDF$newRow <- which(duplicated(rbind(filteredDF$somedata, prNP()), fromLast = TRUE)) #Choose the row number by finding the duplicate
      if(!is.null(filteredDF$newRow) & length(filteredDF$newRow) > 0) {
        if(filteredDF$newRow %in% filteredDF$allRows) {
          filteredDF$allRows <- filteredDF$allRows[!(filteredDF$allRows %in% filteredDF$newRow)]
        } else {
          filteredDF$allRows <- c(filteredDF$allRows, filteredDF$newRow)
        }
        selectRows(prProxy, as.numeric(filteredDF$allRows)) 
      }
    })
  observeEvent(
    input$pr_resetSingleClick,
    {
      filteredDF$allRows <- NULL
      filteredDF$newRow <- NULL
      selectRows(prProxy, NULL)
    })
  
  #Plot Zoom
  pr_brush.ranges <- reactiveValues(x = NULL, y = NULL)
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$pr_doubleClick, {
    brush1 <- input$pr_plotBrush
    if (!is.null(brush1)) {
      pr_brush.ranges$x <- c(brush1$xmin, brush1$xmax)
      pr_brush.ranges$y <- c(brush1$ymin, brush1$ymax)
    } else {
      pr_brush.ranges$x <- NULL
      pr_brush.ranges$y <- NULL
    }
  })
  
  output$pr_ggplot <- renderPlot({
    s1 = input$prDT_rows_current  # rows on the current page (not used in this graph)
    s2 = input$prDT_rows_all      # rows on all pages (after being filtered)
    s3 = input$prDT_rows_selected # rows selected on the plot OR in the data.table
    df <- pr()
    #all points (smaller)
    plot1 <- ggplot(df, aes_string(x=input$pr_xvar, y=input$pr_yvar, colour = input$pr_colour)) + 
      geom_point(aes(alpha = "all data") ) + theme_bw() +
      theme(legend.position="bottom", legend.key = element_blank(), legend.key.width = unit(x = c(.2), units = "npc"))
    #Points on the current page are larger
    leg.lab = c("all data" = 1, "current table" = 1, "selected" = 1)
    plot1 <- plot1 + geom_point(data = df[s1, ], aes(alpha = "current table"), size = 4 ) + geom_point(data = NULL, aes(alpha = "selected"))
    #Points on the current page are larger
    if (length(s3) > 0) { 
    plot1 <- plot1 + geom_point(data = df[s3, ], aes(alpha = "selected"), size = 6, shape = 21, fill = NA, colour = "red" ) 
    }
    plot1 <- plot1 + scale_alpha_manual(name=NULL, values = leg.lab, 
                               guide = guide_legend(override.aes=list(size = c(1,4,4), colour = c("black", "black", "red"), shape = c(19, 19, 21) ), keywidth = unit(1, units = "cm") ) )
    plot1 + coord_cartesian(xlim = pr_brush.ranges$x, ylim = pr_brush.ranges$y)
  })



  #META tab
  output$session.information <- renderPrint({ sessionInfo() })

  #TROUBLESHOOTING
  #output$troubleshooting <- renderPrint({list(paste(as.numeric(input$upl_calculate), pr_check$check), input$pr_plotBrush, pr_brush.ranges)})#input$prDT_rows_selected })


}) #Close shinyServer(function() 

                        



#Constants
colC <- c(5L,4L,3L) #Column widths for carbonate parameters, relative to 12.  Integer values only
colC1 <- c(3L,4L,2L,3L)  #Column widths for other parameters

#CSS formatting.  Nice templates here: https://css-tricks.com/complete-guide-table-element/
tableCSS <- list(tags$head(tags$style(HTML("
                .zebraTable body {
                  font: normal medium/1.4 sans-serif;
                }
                .zebraTable table {
                  border-collapse: collapse;
                  width: 100%;
                }
                .zebraTable th, td {
                  padding: 0.25rem;
                  text-align: left;
                  border: 1px solid #ccc;
                }
                .zebraTable tbody tr:nth-child(odd) {
                  background: #eee;
                }
              "))))

shinyUI(fluidPage(            
  tableCSS, #Load custom CSS into shiny
  
  #or specify CSS formatting here
  #class LP = less padding
  tags$head(tags$style(type="text/css", "
                            #WhatAreVarUnits{ color: black;
                                              
                                             }
                            .LP{padding: 1px;}
                            .selectize-input { font-size: 10px; 
                                               line-height: 10px;
                                             } 
                            .selectize-dropdown { font-size: 12px; 
                                                  line-height: 12px; 
                                                }
                            ") #close tags$style          
  ), #close tags$head
  
  navbarPage("Interactive Seacarb",
    tabPanel("Plot",
      sidebarLayout(       #Container for sidebarPanel() and mainPanel()
        sidebarPanel(width=3,      #Gray box with inputs
                    helpText("Select data and carbonate constants. Enter single values or range (separated by comma). u = standard uncertainty (assume uniform distribution). Expanded 95% uncertainty (k = 2) = 2*u."),
                    fluidRow(column(class = "LP", colC[1], HTML("min, max") ),
                    column(class = "LP", colC[2], HTML("param")    ), 
                    column(class = "LP", colC[3], HTML("<div align = \"right\">u</div>") )
           ),       
           #Variable A
           fluidRow(column(class = "LP", colC[1], textInput("varA", value = "7.7,8", label=NULL) ),
                    column(class = "LP", colC[2], selectInput(inputId = "varA.type", label = NULL, choices = c("pH", "DIC", "ALK", "pCO2", "HCO3", "CO3"), selected = "pH", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL) ), 
                    column(class = "LP", colC[3], textInput("u.varA", value = "0.01", label=NULL) )
           ),
           #Variable B
           fluidRow(column(class = "LP", colC[1], textInput("varB", value = 2100, label=NULL) ),
                    column(class = "LP", colC[2], selectInput(inputId = "varB.type", label = NULL, choices = c("pH", "DIC", "ALK", "pCO2", "HCO3", "CO3"), selected = "DIC", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL) ), 
                    column(class = "LP", colC[3], textInput("u.varB", value = "2", label= NULL) ) 
           ),
           uiOutput("WhatAreVarUnits"),
           br(),
           sliderInput(inputId = "nsim", label = HTML("Monte Carlo N = 10<sup>i</sup>. Select <b>i</b>"), min=1, max=5, value = 3, step = 0.1),
           uiOutput("Nsims"),
           br(),
           uiOutput("plot.params"),
           br(),
           #Other Parameters
           fluidRow(column(              colC1[1], HTML("") ),
                    column(class = "LP", colC1[2], HTML("min, max") ),
                    column(class = "LP", colC1[3], HTML("units")    ), 
                    column(class = "LP", colC1[4], HTML("<div align = \"right\">u</div>") )
           ),       
           
           fluidRow(column(              colC1[1], HTML("S")),
                    column(class = "LP", colC1[2], textInput("S", value = "35", label=NULL )),
                    column(class = "LP", colC1[3], HTML("")),
                    column(class = "LP", colC1[4], textInput("u.S", value = "0", label=NULL))
           ),
           HTML("<b>laboratory</b>"),
           fluidRow(column(              colC1[1], HTML("T")),
                    column(class = "LP", colC1[2], textInput("T.lab", value = "25", label=NULL )),
                    column(class = "LP", colC1[3], HTML("&deg;C")),
                    column(class = "LP", colC1[4], textInput("u.T.lab", value = "0", label=NULL))
           ),
           HTML("<b><i>in situ</i></b>"),
           fluidRow(column(              colC1[1], HTML("T")),
                    column(class = "LP", colC1[2], textInput("T.insitu", value = "25", label=NULL )),
                    column(class = "LP", colC1[3], HTML("&deg;C")),
                    column(class = "LP", colC1[4], textInput("u.T.insitu", value = "0", label=NULL))
           ),
           fluidRow(column(              colC1[1], HTML("P")),
                    column(class = "LP", colC1[2], textInput(inputId="P", value = "0", label=NULL )),
                    column(class = "LP", colC1[3], HTML("dbar")),
                    column(class = "LP", colC1[4], textInput("u.P", value = "0", label=NULL))
           ),
           fluidRow(column(              colC1[1], HTML("PO<sub>4</sub><sup>3-</sup>")), #total phosphate
                    column(class = "LP", colC1[2], textInput("Pt", value = "0", label=NULL )),
                    column(class = "LP", colC1[3], HTML("&mu;mol kg<sup>-1</sup>")),
                    column(class = "LP", colC1[4], textInput("u.Pt", value = "0", label=NULL))
           ),
           fluidRow(column(              colC1[1], HTML("Si")), #total silicate
                    column(class = "LP", colC1[2], textInput("Sit", value = "0", label=NULL )),
                    column(class = "LP", colC1[3], HTML("&mu;mol kg<sup>-1</sup>")),
                    column(class = "LP", colC1[4], textInput("u.Sit", value = "0", label=NULL))
           ),
           HTML("K<sub>H2CO3</sub>, K<sub>HCO3</sub>"),
           radioButtons(inputId = "k1k2", label = NULL, choices= c("Lueker ea '00" = "l",
                                                                   "Millero ea '06" = "m06",
                                                                   "Millero '10" = "m10",
                                                                   "Roy ea '93" = "r",
                                                                   "default" = "x"), selected = "x", inline = TRUE),
           HTML("K<sub>HF</sub>"),
           radioButtons(inputId = "kf", label = NULL, choices= c("Perez & Fraga '87" = "pf",
                                                                 "Dickson & Riley '79" = "dg",
                                                                 "default" = "x"), selected = "x", inline = TRUE),
           HTML("K<sub>HSO4-</sub>"),
           radioButtons(inputId = "ks", label = NULL, choices= c("Dickson '90" = "d",
                                                                 "Khoo ea '77" = "k",
                                                                 "default" = "x"), selected = "x", inline = TRUE),
           HTML("pH scale"),
           radioButtons(inputId = "pHscale", label = NULL, choices= c("total" = "T",
                                                                      "free" = "F",
                                                                      "seawater" = "SWS"), selected = "T", inline = TRUE),
           HTML("[B]"),
           radioButtons(inputId = "b", label = NULL, choices= c("Lee ea '10" = "l10",
                                                                "Uppstrom '74" = "u74"), selected = "u74", inline = TRUE),
           HTML("gas of input pCO<sub>2</sub>"),
           radioButtons(inputId = "gas", label = NULL, choices= c("in situ" = "insitu",
                                                                "potential" = "potential",
                                                                "standard" = "standard"), selected = "potential", inline = TRUE),
           
           
          #LAST ITEM
          br() #Dummy value to reduce code errors from putting a comma on the last element
        ),   #close sidebarPanel
        mainPanel(
          h2("Visualize seawater carbonate chemistry"),
          plotOutput("PLOT", click = "plot_click", dblclick = "plot_dblclick", brush = brushOpts(id = "plot_brush", resetOnNew = TRUE)),
          fluidRow(column( 6, HTML("")), #blank row
                   column( 3, downloadButton(outputId = "plotDownloadButton", label="Download Plot") ),
                   column( 3, selectInput(inputId = "plot_filetype", label = NULL, choices =c("eps", "jpeg", "pdf", "png"), selected = "pdf", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL))#,
                   #                column(class = "LP", colC1[4], textInput("u.T.lab", value = "0", label=NULL))
          ),
          HTML("<b>Click</b> = information on nearest point.<br>
                <b>Click and drag</b> = Zoom box.<br>
                <b>Double click</b> = Zoom-In on zoom box (zoom-out if no box)"),
          br(),
          tableOutput("plot_click_output"),
  
  
          br(),
            HTML("simulated errors/standard uncertainty table"),
            tableOutput("u.TABLE"),
        br()
        ) #close mainPanel()
      )   #close sidebarLayout
    ),    #close tabPanel "Plot"
  
  
    tabPanel("Upload",
      h2("Upload and process data"),
      verbatimTextOutput("troubleshooting"),
      HTML("<b>Instructions:</b><br>
           1) Upload data as a spreadsheet with the first row as a header. Each column should start with a letter and ideally have no spaces or special characters.<br>
           2) Indicate missing values (NA) in the text box, separated by spaces. Ex. -9, -999 \"NULL\".  Click Process. <br>
              Note: Rows with missing values are removed from the full dataset. An option exists to assume missing phosphate and silicate values are 0 &mu;mol kg<sup>-1</sup>.<br>
           3) Select columns corresponding to the required inputs. Organize the data according to the template below for faster processing.<br>
           4) Choose the two carbonate chemistry parameters for the calculations.<br>
           5) Choose constants.<br>
           6) Calculate.<br>
           7) Download processed data as a .CSV file.<br>
           8) <i>(Optional in Processed tab)</i> Visualize the processed data (X, Y, color)."),
      br(), br(), br(),
      fluidRow(column(6, fileInput("inputFile", "1) Browse for file")),  #Upload button
               tags$script('$( "#inputFile" ).on( "click", function() { this.value = null; });'), #Allow file input reset. http://stackoverflow.com/a/36296838/4718512
       # After file is uploaded, read the columns in the server function,
       # and create a responsive dropdown menu for plotting the variables
               column(6, checkboxInput(inputId = "uplPreview", label = "Preview data", value = FALSE))
       ),
      fluidRow(column(6,
        textInput(inputId = "uplNAs", label="Indicate missing values (NA)", value = "-9 -999 NULL")
        ),
        column(3, checkboxInput(inputId = "upl_AssumeNuts0", label = "Assume missing nutrients are 0 &mu;mol kg<sup>-1</sup>", value = TRUE)),
        column(3, actionButton("upl_f_dowle3", label = "Recode NAs"))
        ),
      br(),
      conditionalPanel(
       condition = "input.uplPreview == true",
       HTML("Preview uploaded data:<br>&nbsp;&nbsp; $Column  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; : variableType &nbsp;&nbsp;&nbsp; Row1 Row2 Row3 ... "),
       verbatimTextOutput("upl_str")
       ),
      HTML("<b>2) Select inputs</b><br>
          Organize the first nine columns as shown below for faster processing"),
      tags$div(class = "zebraTable",
              uiOutput("uplRenderUI") #Reactive dropdown menu for variable selection
       ),
     
       br(),
       radioButtons("upl_pHType", label = ("pH scale"), 
                          choices = list("total" = "T", "seawater" = "SWS", "free" = "F"),
                          selected = "T", inline= TRUE),
       HTML("<b>input gas type</b><br>If pCO<sub>2</sub> is one of the inputs, choose the reference pressure and temperature.<br>
            in situ =  <i>in situ</i> pressure and temperature<br>
            potential = 1 atm and potential temperature<br>
            standard = 1 atm pressure and <i>in situ</i> temperature<br>"),
       radioButtons("upl_gasType", label = NULL, #("gas type"), 
                          choices = list("in situ" = "insitu", "potential" = "potential", "standard" = "standard"), #UI = codedValue
                          selected = "potential", inline= TRUE),
      HTML("Note: Output CO<sub>2</sub> partial pressure (pCO2) and CO<sub>2</sub> fugacity (fCO2) have the corresponding suffixes, respectively: insitu, pot, &lt;no suffix&gt;."),
      br(), br(),
      checkboxGroupInput("upl_varAvarB", label = "3) Choose two carbonate parameters",  
                   choices = #list(HTML("pCO<sub>2</sub>") = "pCO2", "pH" = "pH", "TA" = "ALK", "DIC" = "DIC", HTML("CO<sub>3</sub><sup>2-</sup>") = "CO3", HTML("HCO<sub>3</sub><sup>-</sup>") = "HCO3", HTML("CO<sub>2</sub>") = "CO2"), #choices are : UI name = internalVariableName
                     list("pCO2" = "pCO2", "pH" = "pH", "DIC" = "DIC", "ALK" = "ALK", "CO3" = "CO3", "HCO3" = "HCO3", "CO2" = "CO2"),
                   selected = NULL, inline= TRUE),
      br(),
      HTML("<b>4) Choose constants</b>"), 
      br(),
      radioButtons(inputId = "upl_k1k2", label = HTML("K<sub>H2CO3</sub>, K<sub>HCO3</sub>"), choices= c("Lueker et al., 2000" = "l",
                                                              "Millero et al., 2006" = "m06",
                                                              "Millero, 2010" = "m10",
                                                              "Roy et al., 1993" = "r",
                                                              "default" = "x"), selected = "x", inline = TRUE),
      radioButtons(inputId = "upl_kf", label = HTML("K<sub>HF</sub>"), choices= c("Perez & Fraga, 1987" = "pf",
                                                            "Dickson & Riley, 1979" = "dg",
                                                            "default" = "x"), selected = "x", inline = TRUE),
      radioButtons(inputId = "upl_ks", label = HTML("K<sub>HSO4-</sub>"), choices= c("Dickson, 1990" = "d",
                                                            "Khoo et al., 1977" = "k"), selected = "d", inline = TRUE),
      radioButtons(inputId = "upl_b", label = HTML("[B]"), choices= c("Lee et al., 2010" = "l10",
                                                           "Uppstrom, 1974" = "u74"), selected = "u74", inline = TRUE),
      HTML("Defaults:<br>
            K<sub>1</sub> and K<sub>2</sub>: Lueker et al. (2000) for T 2-35&deg;C and S 19-43. Otherwise, Millero (2010).<br>
            K<sub>f</sub>: Perez and Fraga (1987) for T 9-33&deg;C and S 10-40. Otherwise, Dickson and Riley (1979)."),
      br(), br(), br(),
      HTML("<b>5)</b> "),
      actionButton("upl_calculate", label = "Calculate"),
      br(),
      HTML("Note 1: Missing values are removed. Check ID columns.<br>
            Note 2: pCO<sub>2</sub> estimates below 100 m are subject to considerable uncertainty."),
      br(),
      uiOutput("upl_successfulProcessingCheck"),
      
      br()       
    ),   #close tabPanel Upload
  
  
  
    tabPanel("Processed",
      conditionalPanel(
        condition = "pr_check.check != 'TRUE'",
        HTML("Processed data from the \"Upload\" tab will be displayed here.")),
      conditionalPanel(
        condition = "pr_check.check=='TRUE'",
          fluidRow(
            uiOutput("pr_plotOptions")        
          ),
          br()
        ),   #close conditionalPanel
      #verbatimTextOutput("troubleshooting"),
      br()
      ),    #close tabPanel
  
  
  
    tabPanel("Meta",
       HTML("<h2>Interactive <i>seacarb</i></h2><br>
            Remy Okazaki 2016<br>
            See Session Info for software information.<br>"),
       br(),
       a("Features", href = "#features"), br(),
       a("About", href = "#about"), br(), #same as: <a href= "#about">About</a>
       a("Session Info", href = "#sessionInfo"), br(),
       
       HTML("<a name=\"features\"></a>"), h2("Features"), br(),
       HTML("<b>Plot</b>: Visualize seawater carbonate chemistry"), br(),
       HTML("<b>Upload</b>: upload data, calculate seawater carbonate chemistry, download output"), br(),
       HTML("<b>Processed</b>: view processed uploaded data"), br(),
           HTML("<b>Error Propagation</b>: calculate how uncertainties propagate through calculations"), br(),

       a("", name="about"), h2("About"), br(),
       HTML("This CO<sub>2</sub> system calculator uses the <FONT face=\"Consolas,Monaco,Lucida Console,Liberation Mono,DejaVu Sans Mono,Bitstream Vera Sans Mono,Courier New\">carb()</FONT> function in the <i>seacarb</i><sup><a href=\"https://cran.r-project.org/web/packages/seacarb/index.html\">1</a></sup> package in R."),
       br(),
       HTML("<b>Default constants:</b> <br> 
             K<sub>1</sub>, K<sub>2</sub> = Lueker et al. (2000) for T &#91; 2,35&deg;C &#93; and S &#91;19,43&#93;. Outside these ranges, Millero (2010). <br>
             K<sub>f</sub> = Perez & Fraga (1987) for T &#91;9,33&deg;C&#93; and S &#91;10,40&#93;. Outside these ranges, Dickson and Riley (1979). <br>
             K<sub>s</sub> = Dickson (1990) for T &#91;0,45&deg;C&#93; and S &#91;5,45&#93;. <br>
             total boron = Uppstrom (1974) <br>
            <b>CO2 outputs</b> <br>
            pCO2 = \"standard\" pCO2, CO2 partial pressure computed at in situ temperature and atmospheric pressure (μatm) <br>
            fCO2 = \"standard\" fCO2, CO2 fugacity computed at in situ temperature and atmospheric pressure (μatm) <br>
            pCO2pot	= \"potential\" pCO2, CO2 partial pressure computed at potential temperature and atmospheric pressure (μatm) <br>
            fCO2pot	= \"potential\" fCO2, CO2 fugacity computed at potential temperature and atmospheric pressure (μatm) <br>
            pCO2insitu = \"in situ\" pCO2, CO2 partial pressure computed at in situ temperature and total pressure (atm + hydrostatic) (μatm) <br>
            fCO2insitu = \"in situ\" fCO2, CO2 fugacity computed at in situ temperature and total pressure (atm + hydrostatic) (μatm) <br>
             <b>References:</b> <br>
            Dickson A. G. and Riley J. P., 1979 The estimation of acid dissociation constants in seawater media from potentiometric titrations with strong base. I. The ionic product of water. Marine Chemistry 7, 89-99.<br>
            Dickson A. G., 1990 Standard potential of the reaction: AgCI(s) + 1/2H2(g) = Ag(s) + HCI(aq), and the standard acidity constant of the ion HSO4 in synthetic sea water from 273.15 to 318.15 K. Journal of Chemical Thermodynamics 22, 113-127.<br>
            Dickson A. G., Sabine C. L. and Christian J. R., 2007 Guide to best practices for ocean CO2 measurements. PICES Special Publication 3, 1-191.<br>
            Khoo H. K., Ramette R. W., Culberson C. H. and Bates R. G., 1977 Determination of Hydrogen Ion Concentration in Seawater from 5 to 40oC: Standard Potentials at Salinities from 20 to 45. Analytical Chemistry 22, vol49 29-34.<br>
            Lee K., Tae-Wook K., Byrne R.H., Millero F.J., Feely R.A. and Liu Y-M, 2010 The universal ratio of the boron to chlorinity for the North Pacific and North Atlantoc oceans. Geochimica et Cosmochimica Acta 74 1801-1811.<br>
            Lueker T. J., Dickson A. G. and Keeling C. D., 2000 Ocean pCO2 calculated from dissolved inorganic carbon, alkalinity, and equations for K1 and K2: validation based on laboratory measurements of CO2 in gas and seawater at equilibrium. Marine Chemistry 70 105-119.<br>
            Millero F. J., 1995. Thermodynamics of the carbon dioxide system in the oceans. Geochimica Cosmochimica Acta 59: 661-677.<br>
            Millero F. J., 2010. Carbonate constant for estuarine waters. Marine and Freshwater Research 61: 139-142.<br>
            Millero F. J., Graham T. B., Huang F., Bustos-Serrano H. and Pierrot D., 2006. Dissociation constants of carbonic acid in seawater as a function of salinity and temperature. Marine Chemistry 100, 80-84.<br>
            Orr J. C., Epitalon J.-M. and Gattuso J.-P., 2014. Comparison of seven packages that compute ocean carbonate chemistry. Biogeosciences Discussions 11, 5327-5397.<br>
            Perez F. F. and Fraga F., 1987 Association constant of fluoride and hydrogen ions in seawater. Marine Chemistry 21, 161-168.<br>
            Roy R. N., Roy L. N., Vogel K. M., Porter-Moore C., Pearson T., Good C. E., Millero F. J. and Campbell D. M., 1993. The dissociation constants of carbonic acid in seawater at salinities 5 to 45 and temperatures 0 to 45oC. Marine Chemistry 44, 249-267.<br>
            Uppstrom L.R., 1974 The boron/chlorinity ratio of the deep-sea water from the Pacific Ocean. Deep-Sea Research I 21 161-162.<br>
            Waters, J., Millero, F. J., and Woosley, R. J., 2014. Corrigendum to 'The free proton concentration scale for seawater pH', [MARCHE: 149 (2013) 8-22], Mar. Chem. 165, 66-67.<br>
            Weiss, R. F., 1974. Carbon dioxide in water and seawater: the solubility of a non-ideal gas, Mar. Chem., 2, 203-215.<br>
            Weiss, R. F. and Price, B. A., 1980. Nitrous oxide solubility in water and seawater, Mar. Chem., 8, 347-359.<br>
            Zeebe R. E. and Wolf-Gladrow D. A., 2001 CO2 in seawater: equilibrium, kinetics, isotopes. Amsterdam: Elsevier, 346 pp."),
       br(),br(),
       a("", name="sessionInfo"), h2("Session Info"), br(),
       verbatimTextOutput("session.information"),
       HTML("<b>References:</b><br>
            (Note: package versions subject to change. See Session Info for currently loaded package.)<br>
            R Core Team (2016). R: A language and environment for statistical computing. R Foundation for Statistical Computing. Vienna, Austria. http://www.R-project.org/<br>
            Jean-Pierre Gattuso, Jean-Marie Epitalon and Heloise Lavigne (2016). seacarb: Seawater Carbonate Chemistry. R package version 3.0.14. http://CRAN.R-project.org/package=seacarb<br>
            Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2016). shiny: Web Application Framework for R. R package version 0.13.2. http://CRAN.R-project.org/package=shiny<br>
            H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2009."),
       br()
    )#,  #Close tabPanel "Meta"
  ) #close navbarpage  
))  #End fluidPage, End shinyUI
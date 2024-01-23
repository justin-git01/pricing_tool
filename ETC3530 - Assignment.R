library(shiny)
library(shinythemes)
library(RCurl)
library(randomForest)

Lxs = c(10000.0000,9994.0000,9988.0636,9982.2006,9976.3909,9970.6346,9964.9313,9959.2613,9953.6144,9947.9807,9942.3402,9936.6730,9930.9694,9925.2094,9919.3535,9913.3821,9907.2655,9900.9645,9894.4299,9887.6126,9880.4540,9872.8954,9864.8688,9856.2863,9847.0510,9837.0661,9826.2060,9814.3359,9801.3123,9786.9534,
        9771.0789,9753.4714,9733.8865,9712.0728,9687.7149,9660.5021,9630.0522,9595.9715,9557.8179,9515.1040,9467.2906,9413.8004,9354.0040,9287.2164,9212.7143,9129.7170,9037.3973,8934.8771,8821.2612,8695.6199,8557.0118,8404.4916,8237.1329,8054.0544,7854.4508,7637.6208,7403.0084,7150.2401,6879.1673,6589.9258,
        6282.9803,5959.1680,5619.7577,5266.4604,4901.4789,4527.4960,4147.6708,3765.5998,3385.2479,3010.8395,2646.7416,2297.2976,1966.6499,1658.5545,1376.1906,1121.9889,897.5025,703.3242,539.0643,403.4023,294.2061,208.7060,143.7120,95.8476,61.7733,38.3796,22.9284,13.1359,7.1968,3.7596,1.8669,0.8784,0.3903,0.1632,0.0640,0.0234,
        0.0080,0.0025,0.0007,0.0002,0.0000,0.0000,0.0000,0.0000)

## Starting age is 17
#Wholelifeannuity for (x)
Wholelifeannuity = function(age,interestrate)
{discountrate = 1/(1 + interestrate)
survivalprobabilities = (Lxs[-(1:(age-16 + 1))]/Lxs[age-16 + 1])
alldiscountrates = discountrate^(1:length(survivalprobabilities))
Output = (sum(c((alldiscountrates * survivalprobabilities))))
return(Output)}

#Tempannuity for (x)
Tempannuity = function(age,interestrate,term)
{discountfactor = ((1/(1 + interestrate))^term) * (Lxs[(age-16)+1+term]/Lxs[(age-16)+1]) 
Output = Wholelifeannuity(age,interestrate) - discountfactor * Wholelifeannuity(age+term,interestrate)
return(Output)}

#Whole Life Assurance for (x)
Wholelifeassurance_X = function(age, interestrate) {
  Wholelifeannuity_arrears_X = function(age, interestrate){
    discountrate = 1 / (1+interestrate)
    survivalprobabilities = Lxs[-(1:(age-16+1))] / Lxs[age-16+1]
    alldiscountrate = discountrate ^ (1:length(survivalprobabilities))
    wlaa = sum(c(alldiscountrate * survivalprobabilities))
    return(wlaa)
  }
  d = interestrate / (1 + interestrate)
  WLA = 1 - d*(1 + Wholelifeannuity_arrears_X(age,interestrate))
  return(WLA)
}

#Term Assurance for (x)
Termassurance_X = function(age, interestrate, term){
  discountfactor = ((1/(1+interestrate)) ^ term) * (Lxs[age-16+1+term] / Lxs[age-16+1])
  ta_X = Wholelifeassurance_X(age, interestrate) - discountfactor * Wholelifeassurance_X(age+term, interestrate)
  return(ta_X)
}
  #Single Premium for Term Assurance for (x)
  Termassurance_X_P = function(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)
  {SinglePremium = ((1+ClaimExpenseRate) * Termassurance_X(age,interestrate,term))/(1-InitialExpenseRate)
  return (SinglePremium)}
  
  #Level Premium for Term Assurance for (x)
  Termassurance_X_LP = function(age,interestrate,term,InitialExpenseRate,PremiumExpenserate,ClaimExpenseRate)
  {LevelPremium = (Termassurance_X(age,interestrate,term) * (1+ClaimExpenseRate))/((Tempannuity(age,interestrate,term-1)+1)-InitialExpenseRate- PremiumExpenserate * ((Tempannuity(age,interestrate,term-1)+1)-1))
  return (LevelPremium)}
  
#Term Assurance for (x) immediately
Termassurance_X_im = function(age,interestrate,term)
{ta_im = ( 1 + interestrate )^0.5 * Termassurance_X(age,interestrate,term)
return (ta_im)}

  #Single premium for Term Assurance for (y) immediately
  Termassurance_X_im_P = function(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)
  {SinglePremium = (1 + interestrate)^0.5 * Termassurance_X_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)
  return(SinglePremium)}  

  #Level premium for Term Assurance for (y) immediately
  Termassurance_X_im_LP =function(age,interestrate,term,InitialExpenseRate,PremiumExpenseRate,ClaimExpenseRate)
  { LevelPremium = (1 + interestrate)^0.5 * Termassurance_X_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenseRate,ClaimExpenseRate)
  return(LevelPremium)}
  
#Pure Endowment for (x)
PureEndowment_X = function(age, interestrate,term) {
  discountfactor = ((1/(1+interestrate)) ^ term) * (Lxs[age-16+1+term] / Lxs[age-16+1])
  return(discountfactor)
}

  #Single Premium for Pure Endowment for (x) 
  PureEndowment_X_P = function(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)
  {SinglePremium = ( ( 1 + ClaimExpenseRate ) * PureEndowment_X(age, interestrate, term) ) / ( 1- InitialExpenseRate )
  return(SinglePremium)}

  #Level Premium for Pure Endowment for (x)
  PureEndowment_X_LP = function(age,interestrate,term,InitialExpenseRate,PremiumExpenseRate,ClaimExpenseRate)
  {LevelPremium = (PureEndowment_X(age, interestrate, term) * (1+ClaimExpenseRate))/((Tempannuity(age,interestrate,term-1)+1)-InitialExpenseRate- PremiumExpenseRate * ((Tempannuity(age,interestrate,term-1)+1)-1))
  return(LevelPremium)}
  
#Endowment Assurance for (x)
EndowmentAssurance_X = function(age, interestrate, term){
  discountfactor = ((1/(1+interestrate)) ^ term) * (Lxs[age-16+1+term] / Lxs[age-16+1])
  ea_X = Termassurance_X(age, interestrate, term) + discountfactor
  return(ea_X)
}

  #Single Premium for Endowment Assurance for (x)
  EndowmentAssurance_X_P = function(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)
  {SinglePremium = ((1+ClaimExpenseRate) * EndowmentAssurance_X(age,interestrate,term))/(1-InitialExpenseRate)
  return (SinglePremium)}
  
  #Level Premium for Endowment Assurance for (x)
  EndowmentAssurance_X_LP = function(age,interestrate,term,InitialExpenseRate,PremiumExpenseRate,ClaimExpenseRate)
  {LevelPremium = (EndowmentAssurance_X(age,interestrate,term) * (1+ClaimExpenseRate))/((Tempannuity(age,interestrate,term-1)+1)-InitialExpenseRate - PremiumExpenseRate * ((Tempannuity(age,interestrate,term-1)+1)-1))
  return (LevelPremium)}
  
#Endowment Assurance for (x) immediately
EndowmentAssurance_X_im = function(age,interestrate,term)
{discountfactor = ((1/(1 + interestrate))^term) * (Lxs[(age-16)+1+term]/Lxs[(age-16)+1])
ea_im = ((1+interestrate)^0.5) * Termassurance_X(age,interestrate,term) + discountfactor
return (ea_im)}

  #Single Premium for Endowment Assurance for (x) immediately
  EndowmentAssurance_X_im_P = function(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)
  {SinglePremium = ((1+ClaimExpenseRate) * EndowmentAssurance_X_im(age, interestrate, term))/ (1-InitialExpenseRate)
  return(SinglePremium)}
  
  #Level Premium for Endowment Assurance for (x) immediately
  EndowmentAssurance_X_im_LP = function(age,interestrate,term,InitialExpenseRate,PremiumExpenserate,ClaimExpenseRate)
  {LevelPremium = (EndowmentAssurance_X_im(age, interestrate, term) * (1+ClaimExpenseRate)) /((Tempannuity(age,interestrate,term-1)+1)-InitialExpenseRate - PremiumExpenserate * ((Tempannuity(age,interestrate,term-1)+1)-1))
  return (LevelPremium)}

#Wholelifeannuity for (y)
  Wholelifeannuity_Y = function(age,interestrate)
  {discountrate = 1/(1 + interestrate)
  survivalprobabilities = (Lxs[-(1:(age-20 + 1))]/Lxs[age-20 + 1])
  alldiscountrates = discountrate^(1:length(survivalprobabilities))
  Output = (sum(c((alldiscountrates * survivalprobabilities))))
  return(Output)}
  
#Tempannuity for (y)
  Tempannuity_Y = function(age,interestrate,term)
  {discountfactor = ((1/(1 + interestrate))^term) * (Lxs[(age-20)+1+term]/Lxs[(age-20)+1]) 
  tay = Wholelifeannuity_Y(age,interestrate) - discountfactor * Wholelifeannuity_Y(age+term,interestrate)
  return(tay)}
  
#Whole Life Assurance for (y)
Wholelifeassurance_Y = function(age, interestrate) {
  Wholelifeannuity_arrears_Y = function(age, interestrate){
    discountrate = 1 / (1+interestrate)
    survivalprobabilities = Lxs[-(1:(age-20+1))] / Lxs[age-20+1]
    alldiscountrate = discountrate ^ (1:length(survivalprobabilities))
    wlaa = sum(c(alldiscountrate * survivalprobabilities))
    return(wlaa)
  }
  d = interestrate / (1 + interestrate)
  WLA = 1 - d*(1 + Wholelifeannuity_arrears_Y(age,interestrate))
  return(WLA)
}

#Term Assurance for (y)
Termassurance_Y = function(age, interestrate, term){
  discountfactor = ((1/(1+interestrate)) ^ term) * (Lxs[age-20+1+term] / Lxs[age-20+1])
  ta_Y = Wholelifeassurance_Y(age, interestrate) - discountfactor * Wholelifeassurance_Y(age+term, interestrate)
  return(ta_Y)
}

  #Single Premium for Term Assurance for (y)
  Termassurance_Y_P = function(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)
  {SinglePremium = ((1+ClaimExpenseRate) * Termassurance_Y(age,interestrate,term))/(1-InitialExpenseRate)
  return(SinglePremium)}

  #Level Premium for Term Assurance for (y)
  Termassurance_Y_LP = function(age,interestrate,term,InitialExpenseRate,PremiumExpenserate,ClaimExpenseRate)
  {LevelPremium = (Termassurance_Y(age,interestrate,term) * (1+ClaimExpenseRate))/((Tempannuity_Y(age,interestrate,term-1)+1)-InitialExpenseRate- PremiumExpenserate * ((Tempannuity_Y(age,interestrate,term-1)+1)-1))
  return (LevelPremium)}

#Term Assurance for (y) immediately
Termassurance_Y_im = function(age,interestrate,term)
{ta_im = ( 1 + interestrate )^0.5 * Termassurance_Y(age,interestrate,term)
return (ta_im)}

  #Single premium for Term Assurance for (y) immediately
  Termassurance_Y_im_P = function(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)
  {SinglePremium = (1 + interestrate)^0.5 * Termassurance_Y_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)
  return(SinglePremium)}  

  #Level premium for Term Assurance for (y) immediately
  Termassurance_Y_im_LP =function(age,interestrate,term,InitialExpenseRate,PremiumExpenseRate,ClaimExpenseRate)
  { LevelPremium = (1 + interestrate)^0.5 * Termassurance_Y_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenseRate,ClaimExpenseRate)
  return(LevelPremium)}

#Pure Endowment for (y)
PureEndowment_Y = function(age, interestrate,term) {
  discountfactor = ((1/(1+interestrate)) ^ term) * (Lxs[age-20+1+term] / Lxs[age-20+1])
  return(discountfactor)
}

  #Single Premium for Pure Endowment for (y) 
  PureEndowment_Y_P = function(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)
  {SinglePremium = ( ( 1 + ClaimExpenseRate ) * PureEndowment_Y(age, interestrate, term) ) / ( 1- InitialExpenseRate )
  return(SinglePremium)}

  #Level Premium for Pure Endowment for (y)
  PureEndowment_Y_LP = function(age,interestrate,term,InitialExpenseRate,PremiumExpenseRate,ClaimExpenseRate)
  {LevelPremium = (PureEndowment_Y(age, interestrate, term) * (1+ClaimExpenseRate))/((Tempannuity_Y(age,interestrate,term-1)+1)-InitialExpenseRate- PremiumExpenseRate * ((Tempannuity_Y(age,interestrate,term-1)+1)-1))
  return(LevelPremium)}

#Endowment Assurance for (y)
EndowmentAssurance_Y = function(age, interestrate, term){
  discountfactor = ((1/(1+interestrate)) ^ term) * (Lxs[age-20+1+term] / Lxs[age-20+1])
  ea_Y = Termassurance_Y(age, interestrate, term) + discountfactor
  return(ea_Y)
}

  #Single Premium for Endowment Assurance for (y)
  EndowmentAssurance_Y_P = function(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)
  {SinglePremium = ((1+ClaimExpenseRate) * EndowmentAssurance_Y(age,interestrate,term))/(1-InitialExpenseRate)
  return (SinglePremium)}

  #Level Premium for Endowment Assurance for (y)
  EndowmentAssurance_Y_LP = function(age,interestrate,term,InitialExpenseRate,PremiumExpenseRate,ClaimExpenseRate)
  {LevelPremium = (EndowmentAssurance_Y(age,interestrate,term) * (1+ClaimExpenseRate))/((Tempannuity_Y(age,interestrate,term-1)+1)-InitialExpenseRate - PremiumExpenseRate * ((Tempannuity_Y(age,interestrate,term-1)+1)-1))
  return (LevelPremium)}

#Endowment Assurance for (y) immediately
EndowmentAssurance_Y_im = function(age,interestrate,term)
{discountfactor = ((1/(1 + interestrate))^term) * (Lxs[(age-20)+1+term]/Lxs[(age-20)+1])
ea_im = ((1+interestrate)^0.5) * Termassurance_Y(age,interestrate,term) + discountfactor
return (ea_im)}

  #Single Premium for Endowment Assurance for (x) immediately
  EndowmentAssurance_Y_im_P = function(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)
  {SinglePremium = ((1+ClaimExpenseRate) * EndowmentAssurance_Y_im(age, interestrate, term))/ (1-InitialExpenseRate)
  return(SinglePremium)}

  #Level Premium for Endowment Assurance for (x) immediately
  EndowmentAssurance_Y_im_LP = function(age,interestrate,term,InitialExpenseRate,PremiumExpenserate,ClaimExpenseRate)
  {LevelPremium = (EndowmentAssurance_Y_im(age, interestrate, term) * (1+ClaimExpenseRate)) /((Tempannuity_Y(age,interestrate,term-1)+1)-InitialExpenseRate - PremiumExpenserate * ((Tempannuity_Y(age,interestrate,term-1)+1)-1))
  return (LevelPremium)}

#Joint Whole Life Annuity
J_Wholelifeannuity = function(ageX, ageY, interestrate) {
  discountrate = 1 / (1+interestrate)
  survivalprobabilities_X = Lxs[-(1:(ageX-16+1))] / Lxs[ageX-16+1]
  survivalprobabilities_Y = Lxs[-(1:(ageY-20+1))] / Lxs[ageY-20+1]
  alldiscountrates = discountrate ^ (1:length(survivalprobabilities_X))
  j_wla = sum(c(alldiscountrates * survivalprobabilities_X * survivalprobabilities_Y))
  return(j_wla)
}

#Joint Temporary Annuity 
J_Tempannuity = function(ageX, ageY, interestrate, term) {
  discountfactor = ((1/(1 + interestrate))^term) * (Lxs[(ageX-16)+1+term]/Lxs[(ageX-16)+1]) * (Lxs[ageY-20+1+term] / Lxs[ageY-20+1])
  ta = J_Wholelifeannuity(ageX, ageY, interestrate) - discountfactor * J_Wholelifeannuity(ageX+term, ageY + term, interestrate)
  return(ta)
}
#Joint Temporary Annuity continuously
J_Tempannuity_cont = function(ageX, ageY, interestrate, term) {
  discountfactor = ((1/(1 + interestrate))^term) * (Lxs[(ageX-16)+1+term]/Lxs[(ageX-16)+1]) * (Lxs[ageY-20+1+term] / Lxs[ageY-20+1])
  tac = (1 + J_Tempannuity(ageX, ageY, interestrate, term)) - 0.5 * (1 - discountfactor)
  return(tac)
}

#Temporary Annuity for (y) continuously
Tempannuity_y_cont = function(age, interestrate, term) {
  discountfactor = ((1/(1 + interestrate))^term) * (Lxs[age-20+1+term] / Lxs[age-20+1])
  tac = (1 + Tempannuity_Y(age, interestrate, term)) - 0.5 * (1 - discountfactor)
  return(tac)
}

#Featured Product
Featured_Products = function(ageX, ageY, interestrate, term) {
  fp = Tempannuity_y_cont(ageY, interestrate, term) - J_Tempannuity_cont(ageX, ageY, interestrate, term)
  return(fp)
}

  #Single Premium for featured product
  Featured_Products_cont_P = function(ageX,ageY,interestrate,term,InitialExpenseRate,ClaimExpenseRate)
  {SinglePremium = ((1+ClaimExpenseRate) * Featured_Products(ageX,ageY,interestrate,term))/(1-InitialExpenseRate)
  return(SinglePremium)}

############################################## shiny app ##############################################

ui = fluidPage(theme = shinytheme("united"),   ### ui
               navbarPage(
                 "Insurance Premium Valuation Tool",
                 tabPanel( "Single life",
                           sidebarPanel( 
                             tags$h3("Policyholder"),
                             radioButtons("LifeS",label = "",choices = list("Life X","Life Y")), # Life
                             sliderInput(inputId = "AgeS", label =  "Age", value = 30, min=20,max=90), #Age
                             sliderInput(inputId = "TermS", label =  "Term", value = 20, min=0,max=100), # Term
                             numericInput("IRS",label = "Interest Rate (in %)", value = 5, min=0,max=15), # IR
                             numericInput("SA",label = "Sum Assured", value = 10000, min=0,max=10000000), #S
                             selectInput(inputId = "InsuranceS",                                   #Insurance
                                         label = "Insurance type",
                                         list("Insurance" = c("Term Assurance",
                                                              "Pure endowment", 
                                                              "Endowment Assurance"))),
                             radioButtons("BenefitPaymentS", label = "Insurance Benefit payment", choices = list("End of year of death","Immediately"), selected = "End of year of death"),
                             radioButtons("PremiumPaymentS", label = "Premium payment", choices = list("Single","Level"), selected = "Single")
                           ),
                           mainPanel(h3("Premium"),
                                     verbatimTextOutput("PS"),
                                     plotOutput("plotS")
                           ),
                           sidebarPanel( 
                             tags$h3("Expense"),
                             numericInput("IES",label = "Initial Expense Rate (in %)", value = 0, min=0,max=15),
                             numericInput("PES",label = "Premimum Expense Rate (in %) (for level premium only)", value = 0, min=0,max=15),
                             numericInput("CES",label = "Claim Expense Rate (in %)", value = 0, min=0,max=15)
                           )
                           
                 ),
                 tabPanel("Featured Product - Caring for (y) after death",
                          sidebarPanel( tags$h3("Policyholders"),
                                        sliderInput(inputId = "AgeXJ", label =  "Age of Policyholder X", value = 30, min=20,max=90),
                                        sliderInput(inputId = "AgeYJ", label =  "Age of Policyholder Y", value = 25, min=20,max=90),
                                        sliderInput(inputId = "TermJ", label =  "Term", value = 20, min=0,max=100),
                                        numericInput("IRJ",label = "Interest Rate (in %)", value = 5, min=0,max=15),
                                        numericInput("BJ",label = "Benefit", value = 10000, min=0,max=10000000)
                                                    ),
                          mainPanel(h3("Premium"),
                                    verbatimTextOutput("PJ"),
                                    plotOutput("plotJ")
                          ),
                          sidebarPanel( 
                            tags$h3("Expense"),
                            numericInput("IEJ",label = "Initial Expense Rate (in %)", value = 0, min=0,max=15),
                            numericInput("CEJ",label = "Claim Expense Rate (in %)", value = 0, min=0,max=15)
                          ))
                 
               ))

server = function(input,output){   ### server
  
  ######################################################## SINGLE LIFE PREMIUM ##################################################
  
  output$PS = renderText({
    age = input$AgeS
    interestrate = input$IRS/100
    InitialExpenseRate = input$IES/100
    PremiumExpenseRate = input$PES/100
    ClaimExpenseRate = input$CES/100
    term = input$TermS               
    ####### Term Assurance premium X
    if((input$LifeS == "Life X")&(input$InsuranceS == "Term Assurance")&(input$BenefitPaymentS =="End of year of death")&(input$PremiumPaymentS=="Single")){ SEPVP = Termassurance_X_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)}
    if((input$LifeS == "Life X")&(input$InsuranceS == "Term Assurance")&(input$BenefitPaymentS =="Immediately ")&(input$PremiumPaymentS=="Single")) {SEPVP = Termassurance_X_im_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)}
    if((input$LifeS == "Life X")&(input$InsuranceS == "Term Assurance")&(input$BenefitPaymentS =="End of year of death")&(input$PremiumPaymentS=="Level")){ SEPVP = Termassurance_X_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenseRate,ClaimExpenseRate)}
    if((input$LifeS == "Life X")&(input$InsuranceS == "Term Assurance")&(input$BenefitPaymentS =="Immediately")&(input$PremiumPaymentS=="Level")) { SEPVP = Termassurance_X_im_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenseRate,ClaimExpenseRate)}
    ####### Term Assurance premium Y
    if((input$LifeS == "Life Y")&(input$InsuranceS == "Term Assurance")&(input$BenefitPaymentS =="End of year of death")&(input$PremiumPaymentS=="Single")){ SEPVP = Termassurance_Y_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)}
    if((input$LifeS == "Life Y")&(input$InsuranceS == "Term Assurance")&(input$BenefitPaymentS =="Immediately")&(input$PremiumPaymentS=="Single")) { SEPVP = Termassurance_Y_im_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)}
    if((input$LifeS == "Life Y")&(input$InsuranceS == "Term Assurance")&(input$BenefitPaymentS =="End of year of death")&(input$PremiumPaymentS=="Level")){ SEPVP = Termassurance_Y_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenseRate,ClaimExpenseRate)}
    if((input$LifeS == "Life Y")&(input$InsuranceS == "Term Assurance")&(input$BenefitPaymentS =="Immediately")&(input$PremiumPaymentS=="Level")) { SEPVP = Termassurance_Y_im_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenseRate,ClaimExpenseRate)}
    ####### Pure endowment premium X&Y
    if((input$LifeS == "Life X")&(input$InsuranceS == "Pure endowment")&(input$PremiumPaymentS=="Single")){ SEPVP = PureEndowment_X_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)}
    if((input$LifeS == "Life Y")&(input$InsuranceS == "Pure endowment")&(input$PremiumPaymentS=="Single")) { SEPVP = PureEndowment_Y_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)}
    if((input$LifeS == "Life X")&(input$InsuranceS == "Pure endowment")&(input$PremiumPaymentS=="Level")){ SEPVP = PureEndowment_X_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenseRate,ClaimExpenseRate)}
    if((input$LifeS == "Life Y")&(input$InsuranceS == "Pure endowment")&(input$PremiumPaymentS=="Level")) { SEPVP = PureEndowment_Y_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenseRate,ClaimExpenseRate)}
    ####### Endowment Assurance premium X
    if((input$LifeS == "Life X")&(input$InsuranceS == "Endowment Assurance")&(input$BenefitPaymentS =="End of year of death")&(input$PremiumPaymentS=="Single")){ SEPVP = EndowmentAssurance_X_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)}
    if((input$LifeS == "Life X")&(input$InsuranceS == "Endowment Assurance")&(input$BenefitPaymentS =="Immediately")&(input$PremiumPaymentS=="Single")) { SEPVP = EndowmentAssurance_X_im_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)}
    if((input$LifeS == "Life X")&(input$InsuranceS == "Endowment Assurance")&(input$BenefitPaymentS =="End of year of death")&(input$PremiumPaymentS=="Level")){ SEPVP = EndowmentAssurance_X_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenseRate,ClaimExpenseRate)}
    if((input$LifeS == "Life X")&(input$InsuranceS == "Endowment Assurance")&(input$BenefitPaymentS =="Immediately")&(input$PremiumPaymentS=="Level")) { SEPVP = EndowmentAssurance_X_im_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenseRate,ClaimExpenseRate)}
    ####### Endowment Assurance premium Y
    if((input$LifeS == "Life Y")&(input$InsuranceS == "Endowment Assurance")&(input$BenefitPaymentS =="End of year of death")&(input$PremiumPaymentS=="Single")){ SEPVP = EndowmentAssurance_Y_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)}
    if((input$LifeS == "Life Y")&(input$InsuranceS == "Endowment Assurance")&(input$BenefitPaymentS =="Immediately ")&(input$PremiumPaymentS=="Single")) { SEPVP = EndowmentAssurance_Y_im_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)}
    if((input$LifeS == "Life Y")&(input$InsuranceS == "Endowment Assurance")&(input$BenefitPaymentS =="End of year of death")&(input$PremiumPaymentS=="Level")){ SEPVP = EndowmentAssurance_Y_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenseRate,ClaimExpenseRate)}
    if((input$LifeS == "Life Y")&(input$InsuranceS == "Endowment Assurance")&(input$BenefitPaymentS =="Immediately)")&(input$PremiumPaymentS=="Level")) { SEPVP = EndowmentAssurance_Y_im_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenseRate,ClaimExpenseRate)}
   
    PremiumS = input$SA * SEPVP 
    print(PremiumS)
  })
  
  
  ######################################################## SINGLE LIFE PREMIUM PLOT##########################################
  output$plotS = renderPlot({
    age = input$AgeS
    interestrate = input$IRS/100
    InitialExpenseRate = input$IES/100
    PremiumExpenserate = input$PES/100
    ClaimExpenseRate = input$CES/100
    term = input$TermS           
    
    interestvec = c(0:15)

  
    ####### Term Assurance premium X
    ##
    if((input$LifeS == "Life X")&(input$InsuranceS == "Term Assurance")&(input$BenefitPaymentS =="End of year of death")&(input$PremiumPaymentS=="Single"))
      {premiumvec <- numeric(length = 16)
      for (i in 1:16){
        premiumvec[i] = Termassurance_X_P(age, interestvec[i]/100, term,InitialExpenseRate,ClaimExpenseRate)
      }
      SEPVP = Termassurance_X_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)}
    ##
    if((input$LifeS == "Life X")&(input$InsuranceS == "Term Assurance")&(input$BenefitPaymentS =="Immediately ")&(input$PremiumPaymentS=="Single")) 
      {premiumvec <- numeric(length = 16)
      for (i in 1:16){
        premiumvec[i] = Termassurance_X_im_P(age, interestvec[i]/100, term,InitialExpenseRate,ClaimExpenseRate)
      }
      SEPVP = Termassurance_X_im_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)}
    ##
    if((input$LifeS == "Life X")&(input$InsuranceS == "Term Assurance")&(input$BenefitPaymentS =="End of year of death")&(input$PremiumPaymentS=="Level"))
      {premiumvec <- numeric(length = 16)
      for (i in 1:16){
        premiumvec[i] = Termassurance_X_LP(age, interestvec[i]/100, term,InitialExpenseRate,PremiumExpenserate, ClaimExpenseRate)
      }
       SEPVP = Termassurance_X_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenserate,ClaimExpenseRate)}
    ##
    if((input$LifeS == "Life X")&(input$InsuranceS == "Term Assurance")&(input$BenefitPaymentS =="Immediately")&(input$PremiumPaymentS=="Level")) 
      {premiumvec <- numeric(length = 16)
      for (i in 1:16){
        premiumvec[i] = Termassurance_X_im_LP(age, interestvec[i]/100, term,InitialExpenseRate, PremiumExpenserate, ClaimExpenseRate)
      }
       SEPVP = Termassurance_X_im_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenserate,ClaimExpenseRate)}
    
    ####### Term Assurance premium Y
    ##
    if((input$LifeS == "Life Y")&(input$InsuranceS == "Term Assurance")&(input$BenefitPaymentS =="End of year of death")&(input$PremiumPaymentS=="Single"))
     { premiumvec <- numeric(length = 16)
     for (i in 1:16){
       premiumvec[i] = Termassurance_Y_P(age, interestvec[i]/100, term,InitialExpenseRate,ClaimExpenseRate)
     }
      SEPVP = Termassurance_Y_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)}
    ##
    if((input$LifeS == "Life Y")&(input$InsuranceS == "Term Assurance")&(input$BenefitPaymentS =="Immediately")&(input$PremiumPaymentS=="Single")) 
      { premiumvec <- numeric(length = 16)
      for (i in 1:16){
        premiumvec[i] = Termassurance_Y_im_P(age, interestvec[i]/100, term,InitialExpenseRate,ClaimExpenseRate)
      }
       SEPVP = Termassurance_Y_im_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)}
    ##
    if((input$LifeS == "Life Y")&(input$InsuranceS == "Term Assurance")&(input$BenefitPaymentS =="End of year of death")&(input$PremiumPaymentS=="Level"))
      { premiumvec <- numeric(length = 16)
      for (i in 1:16){
        premiumvec[i] = Termassurance_Y_LP(age, interestvec[i]/100, term,InitialExpenseRate,PremiumExpenserate, ClaimExpenseRate)
      }
      SEPVP = Termassurance_Y_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenserate,ClaimExpenseRate)}
    ##
    if((input$LifeS == "Life Y")&(input$InsuranceS == "Term Assurance")&(input$BenefitPaymentS =="Immediately")&(input$PremiumPaymentS=="Level")) 
     { premiumvec <- numeric(length = 16)
     for (i in 1:16){
       premiumvec[i] = Termassurance_Y_im_LP(age, interestvec[i]/100, term,InitialExpenseRate,PremiumExpenserate, ClaimExpenseRate)
     }
      SEPVP = Termassurance_Y_im_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenserate,ClaimExpenseRate)}
    
    ####### Pure endowment premium X&Y
    ##
    if((input$LifeS == "Life X")&(input$InsuranceS == "Pure endowment")&(input$PremiumPaymentS=="Single"))
     { premiumvec <- numeric(length = 16)
     for (i in 1:16){
       premiumvec[i] = PureEndowment_X_P(age, interestvec[i]/100, term,InitialExpenseRate,ClaimExpenseRate)
     }
      SEPVP = PureEndowment_X_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)}
    ##
    if((input$LifeS == "Life Y")&(input$InsuranceS == "Pure endowment")&(input$PremiumPaymentS=="Single"))
      { premiumvec <- numeric(length = 16)
      for (i in 1:16){
        premiumvec[i] = PureEndowment_Y_P(age, interestvec[i]/100, term,InitialExpenseRate,ClaimExpenseRate)
      }
      SEPVP = PureEndowment_Y_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)}
    ##
    if((input$LifeS == "Life X")&(input$InsuranceS == "Pure endowment")&(input$PremiumPaymentS=="Level"))
    { premiumvec <- numeric(length = 16)
    for (i in 1:16){
      premiumvec[i] = PureEndowment_X_LP(age, interestvec[i]/100, term,InitialExpenseRate,PremiumExpenserate, ClaimExpenseRate)
    }
      SEPVP = PureEndowment_X_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenserate,ClaimExpenseRate)}
    ##
    if((input$LifeS == "Life Y")&(input$InsuranceS == "Pure endowment")&(input$PremiumPaymentS=="Level")) 
      { premiumvec <- numeric(length = 16)
      for (i in 1:16){
        premiumvec[i] = PureEndowment_Y_LP(age, interestvec[i]/100, term,InitialExpenseRate,PremiumExpenserate,ClaimExpenseRate)
      }
      SEPVP = PureEndowment_Y_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenserate,ClaimExpenseRate)}
    
    ####### Endowment Assurance premium X
    ##
    if((input$LifeS == "Life X")&(input$InsuranceS == "Endowment Assurance")&(input$BenefitPaymentS =="End of year of death")&(input$PremiumPaymentS=="Single"))
    { premiumvec <- numeric(length = 16)
    for (i in 1:16){
      premiumvec[i] = EndowmentAssurance_X_P(age, interestvec[i]/100, term,InitialExpenseRate,ClaimExpenseRate)
    }
      SEPVP = EndowmentAssurance_X_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)}
    ##
    if((input$LifeS == "Life X")&(input$InsuranceS == "Endowment Assurance")&(input$BenefitPaymentS =="Immediately")&(input$PremiumPaymentS=="Single")) 
      {premiumvec <- numeric(length = 16)
      for (i in 1:16){
        premiumvec[i] = EndowmentAssurance_X_im_P(age, interestvec[i]/100, term,InitialExpenseRate,ClaimExpenseRate)
      }
      SEPVP = EndowmentAssurance_X_im_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)}
    ##
    if((input$LifeS == "Life X")&(input$InsuranceS == "Endowment Assurance")&(input$BenefitPaymentS =="End of year of death")&(input$PremiumPaymentS=="Level"))
      { premiumvec <- numeric(length = 16)
      for (i in 1:16){
        premiumvec[i] = EndowmentAssurance_X_LP(age, interestvec[i]/100, term,InitialExpenseRate,PremiumExpenserate,ClaimExpenseRate)
      }
      SEPVP = EndowmentAssurance_X_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenserate,ClaimExpenseRate)}
    ##
    if((input$LifeS == "Life X")&(input$InsuranceS == "Endowment Assurance")&(input$BenefitPaymentS =="Immediately")&(input$PremiumPaymentS=="Level")) 
     { premiumvec <- numeric(length = 16)
     for (i in 1:16){
       premiumvec[i] = EndowmentAssurance_X_im_LP(age, interestvec[i]/100, term,InitialExpenseRate,PremiumExpenserate,ClaimExpenseRate)
     }
      SEPVP = EndowmentAssurance_X_im_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenserate,ClaimExpenseRate)}
    
    ####### Endowment Assurance premium Y
    ##
    if((input$LifeS == "Life Y")&(input$InsuranceS == "Endowment Assurance")&(input$BenefitPaymentS =="End of year of death")&(input$PremiumPaymentS=="Single"))
      { premiumvec <- numeric(length = 16)
      for (i in 1:16){
        premiumvec[i] = EndowmentAssurance_Y_P(age, interestvec[i]/100, term,InitialExpenseRate,ClaimExpenseRate)
      }
      SEPVP = EndowmentAssurance_Y_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)}
    ##
    if((input$LifeS == "Life Y")&(input$InsuranceS == "Endowment Assurance")&(input$BenefitPaymentS =="Immediately ")&(input$PremiumPaymentS=="Single")) 
     { premiumvec <- numeric(length = 16)
     for (i in 1:16){
       premiumvec[i] = EndowmentAssurance_Y_im_P(age, interestvec[i]/100, term,InitialExpenseRate,ClaimExpenseRate)
     }
      SEPVP = EndowmentAssurance_Y_im_P(age,interestrate,term,InitialExpenseRate,ClaimExpenseRate)}
    ##
    if((input$LifeS == "Life Y")&(input$InsuranceS == "Endowment Assurance")&(input$BenefitPaymentS =="End of year of death")&(input$PremiumPaymentS=="Level"))
      { premiumvec <- numeric(length = 16)
      for (i in 1:16){
        premiumvec[i] = EndowmentAssurance_Y_LP(age, interestvec[i]/100, term,InitialExpenseRate,PremiumExpenserate,ClaimExpenseRate)
      }
      SEPVP = EndowmentAssurance_Y_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenserate,ClaimExpenseRate)}
    ##
    if((input$LifeS == "Life Y")&(input$InsuranceS == "Endowment Assurance")&(input$BenefitPaymentS =="Immediately)")&(input$PremiumPaymentS=="Level")) 
     { premiumvec <- numeric(length = 16)
     for (i in 1:16){
       premiumvec[i] = EndowmentAssurance_Y_im_LP(age, interestvec[i]/100, term,InitialExpenseRate,PremiumExpenserate,ClaimExpenseRate)
     }
      SEPVP = EndowmentAssurance_Y_im_LP(age,interestrate,term,InitialExpenseRate,PremiumExpenserate,ClaimExpenseRate)}
    
    PremiumS = input$SA * SEPVP 
    
    premiumvecSA = premiumvec * input$SA
    
    
    plot(y=premiumvecSA,x=interestvec,ylab="Premium($)",xlab="Interest Rate in %",type = "l",main= "Premium")
    abline(h=PremiumS, col="blue")
    
  })
  ######################################################## JOINT LIFE PREMIUM ##################################################
  
  output$PJ = renderText({
    ageX = input$AgeXJ
    ageY = input$AgeYJ
    interestrate = input$IRJ/100
    InitialExpenseRate = input$IEJ/100
    ClaimExpenseRate = input$CEJ/100
    term = input$TermJ  
    
    
    ####### Featured Product
    JEPVP = Featured_Products_cont_P(ageX, ageY, interestrate, term, InitialExpenseRate, ClaimExpenseRate)    
    PremiumJ = input$BJ * JEPVP 
    print(PremiumJ)
  })
  
  ######################################################## JOINT LIFE PREMIUM PLOT ##################################################
  output$plotJ = renderPlot({
    ageX = input$AgeXJ
    ageY = input$AgeYJ
    interestrate = input$IRJ/100
    InitialExpenseRate = input$IEJ/100
    ClaimExpenseRate = input$CEJ/100
    term = input$TermJ  
    
    interestvecc = c(0:15)
    
    
    ####### Featured Product
    premiumvecJ <- numeric(length = 16)
    for (i in 1:16){
      premiumvecJ[i] = Featured_Products_cont_P(ageX, ageY, interestvecc[i]/100, term, InitialExpenseRate, ClaimExpenseRate)
    }
    JEPVP = Featured_Products_cont_P(ageX, ageY, interestrate, term, InitialExpenseRate, ClaimExpenseRate)
    
    PremiumJ =  JEPVP * input$BJ
    
    premiumvecJBJ = premiumvecJ * input$BJ
    
    
    plot(y=premiumvecJBJ,x=interestvecc,ylab="Premium($)",xlab="Interest Rate in %",type = "l",main= "Premium")
    abline(h=PremiumJ, col="blue")
  })

}

shinyApp(ui = ui, server =server)
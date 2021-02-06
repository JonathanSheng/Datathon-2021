library(shiny)
library(tidyverse)
library(lubridate)
library(cluster)

agency_summary <- read.csv("agency_summary.csv")
agency_summary <- agency_summary[2:23]

agencies <- c("Department of Health", "Fire & Emergency Medical Services",
              "Department of  Youth Rehabilitation Services", "District Department of Transportation",
              "Office of the State Superintendent of Education", "Office of the Attorney General",
              "Department of Motor Vehicles", "Office of the Secretary", "Department of Property Management",
              "DC Public Library", "District Department of the Environment", "Department of Human Services",
              "Department of Consumer & Regulatory Affairs", "Department of Mental Health",
              "Office of Contracting & Procurement", "Department of Housing & Community Development",
              "Department of Corrections", "Department of Public Works", "Office of the Tenant Advocate",
              "Office of Police Complaints", "Department on Disability Services",
              "Office of the Chief Technology Officer", "Department of Education",
              "Department of Parks & Recreation", "Office of Community Affairs",
              "Office of the Chief Financial Officer", "Office of Partnership & Grant Services",
              "Office of the Mayor", "DC Taxicab Commission", "Commission on Arts & Humanities",
              "City Administrator/Deputy Mayor", "Department of Employment Services",
              "Metropolitan Police Department", "Office of Cable TV", "Office of Finance & Resource Management",
              "Criminal Justice Coordinating Council", "Office of Unified Communications",
              "DC Department of Human Resources", "Board of Real Property Assessment & Appeals",
              "Office of Public Educational Facilities Modernization",
              "Medical Liability Captive Insurance Agency", "Board of Elections & Ethics",
              "Commission of Judicial Disabilities & Tenure", "Office of Latino Affairs", "Serve DC",
              "Alcoholic Beverage Regulation Administration", "Office of Municipal Planning",
              "Office of Employee Appeals", "Contract Appeals Board", "Homeland Security/Emergency Management",
              "Disability Compensation Fund", "Office of Disability Rights",
              "Department of Insurance, Securities & Banking", "Office of Human Rights",
              "Office of the Chief Medical Examiner", "Office on Aging", "Office of Victim Services",
              "Office of Zoning", "Department of Health Care Finance", "Office of Administrative Hearings",
              "DC Office of Risk Management", "Child & Family Services Administration",
              "Office of the Inspector General", "DC National Guard", "Deputy Mayor for Economic Development",
              "DC Sentencing & Criminal Code Review Committee",
              "Department of Small & Local Business Development", "Office of Motion Pictures & Television",
              "Office on Asian/Pacific Affairs", "Advisory Neighborhood Commission",
              "Office of Veterans Affairs", "DC Lottery & Charitable Games Control Board",
              "Judicial Nomination Commission", "Office of Justice Grants Administration",
              "University of the District of Columbia", "Public Employee Relations Board",
              "District of Columbia Public Schools", "Public Service Commission", "NFP Hospital Corporation",
              "DEPUTY MAYOR FOR PUBLIC SAFETY & JUSTICE", "DC Council",
              "DEPUTY MAYOR FOR HEALTH & HUMAN SERVICES", "OFFICE OF PEOPLE'S COUNSEL",
              "DC BD OF ETHICS AND GOVT ACCOUNTABILITY", "OFFICE OF CAMPAIGN FINANCE",
              "Unknown New Agency", "DEPARTMENT OF FORENSICS SCIENCES", "DC Health Benefit Exchange",
              "DC STATE BOARD OF EDUCATION")


df <- data.frame(agencies, agency_summary$cluster)

names(df) <- c('agencies', 'cluster')

ui <- fluidPage(
  selectInput(inputId = 'agency',
              label = "Select Agency",
              choices = df$agencies),
  textOutput("intro"),
  textOutput("number1"),
  textOutput("number2"),
  textOutput("number3"),
  textOutput("number4"),
  textOutput("number5"),
  textOutput("newline"),
  textOutput("cat_intro"),
  textOutput("cat1"),
  textOutput("cat2"),
  textOutput("cat3"),
  textOutput("newline2"),
  textOutput("similar"),
  textOutput("agency1"),
  textOutput("agency2"),
  textOutput("agency3"),
  textOutput("agency4"),
  textOutput("agency5"),
  textOutput("agency6"),
  textOutput("agency7"),
  textOutput("agency8"),
  textOutput("agency9"),
  textOutput("agency10"),
  textOutput("newline3"),
  textOutput("rec1")
)



server <- function(input, output){
  output$intro <- renderText({
    paste0("For The ", input$agency, " the top 5 vendors are: ")
  })
  output$number1 <- renderText({
    ven1 <- agency_summary[which(agency_summary$agency == input$agency), 14]
    paste0("1) ", ven1)
  })
  output$number2 <- renderText({
    ven2 <- agency_summary[which(agency_summary$agency == input$agency), 15]
    paste0("2) ", ven2)
  })
  output$number3 <- renderText({
    ven3 <- agency_summary[which(agency_summary$agency == input$agency), 16]
    paste0("3) ", ven3)
  })
  output$number4 <- renderText({
    ven4 <- agency_summary[which(agency_summary$agency == input$agency), 17]
    paste0("4) ", ven4)
  })
  output$number5 <- renderText({
    ven5 <- agency_summary[which(agency_summary$agency == input$agency), 18]
    paste0("5) ", ven5)
  })
  output$newline <- renderText({
    paste0("- - - - - - - - -")
  })
  output$cat_intro <- renderText({
    paste0("For The ", input$agency, " the top 3 purchase categories are: ")
  })
  output$cat1 <- renderText({
    paste0("1) ", agency_summary[which(agency_summary$agency == input$agency), 19])
  })
  output$cat2 <- renderText({
    paste0("2) ", agency_summary[which(agency_summary$agency == input$agency), 20])
  })
  output$cat3 <- renderText({
    paste0("3) ", agency_summary[which(agency_summary$agency == input$agency), 21])
  })
  output$newline2 <- renderText({
    paste0("- - - - - - - - -")
  })
  output$similar <- renderText({
    paste0("Similar agencies are:")
  })
  output$agency1 <- renderText({
    paste0("1) ", filter(df, cluster == df[which(df$agencies == input$agency), 2])[!(filter(df, cluster == df[which(df$agencies == input$agency), 2])$agencies == input$agency), 1][1])
  })
  output$agency1 <- renderText({
    paste0("1) ", filter(df, cluster == df[which(df$agencies == input$agency), 2])[!(filter(df, cluster == df[which(df$agencies == input$agency), 2])$agencies == input$agency), 1][2])
  })
  output$agency2 <- renderText({
    paste0("2) ", filter(df, cluster == df[which(df$agencies == input$agency), 2])[!(filter(df, cluster == df[which(df$agencies == input$agency), 2])$agencies == input$agency), 1][3])
  })
  output$agency3 <- renderText({
    paste0("3) ", filter(df, cluster == df[which(df$agencies == input$agency), 2])[!(filter(df, cluster == df[which(df$agencies == input$agency), 2])$agencies == input$agency), 1][4])
  })
  output$agency4 <- renderText({
    paste0("4) ", filter(df, cluster == df[which(df$agencies == input$agency), 2])[!(filter(df, cluster == df[which(df$agencies == input$agency), 2])$agencies == input$agency), 1][5])
  })
  output$agency5 <- renderText({
    paste0("5) ", filter(df, cluster == df[which(df$agencies == input$agency), 2])[!(filter(df, cluster == df[which(df$agencies == input$agency), 2])$agencies == input$agency), 1][6])
  })
  output$agency6 <- renderText({
    paste0("6) ", filter(df, cluster == df[which(df$agencies == input$agency), 2])[!(filter(df, cluster == df[which(df$agencies == input$agency), 2])$agencies == input$agency), 1][7])
  })
  output$agency7 <- renderText({
    paste0("7) ", filter(df, cluster == df[which(df$agencies == input$agency), 2])[!(filter(df, cluster == df[which(df$agencies == input$agency), 2])$agencies == input$agency), 1][8])
  })
  output$agency8 <- renderText({
    paste0("8) ", filter(df, cluster == df[which(df$agencies == input$agency), 2])[!(filter(df, cluster == df[which(df$agencies == input$agency), 2])$agencies == input$agency), 1][9])
  })
  output$agency9 <- renderText({
    paste0("9) ", filter(df, cluster == df[which(df$agencies == input$agency), 2])[!(filter(df, cluster == df[which(df$agencies == input$agency), 2])$agencies == input$agency), 1][10])
  })
  output$agency10 <- renderText({
    paste0("10) ", filter(df, cluster == df[which(df$agencies == input$agency), 2])[!(filter(df, cluster == df[which(df$agencies == input$agency), 2])$agencies == input$agency), 1][1])
  })
  output$newline3 <- renderText({
      paste0("- - - - - - - - -")
  })
  output$rec1 <- renderText({
    x <- c(filter(agency_summary, cluster == df[which(df$agencies == input$agency), 2])$vendor1, filter(agency_summary, cluster == df[which(df$agencies == input$agency), 2])$vendor2, filter(agency_summary, cluster == df[which(df$agencies == input$agency), 2])$vendor3, filter(agency_summary, cluster == df[which(df$agencies == input$agency), 2])$vendor4, filter(agency_summary, cluster == df[which(df$agencies == input$agency), 2])$vendor5)
    xdf <- data.frame(x)
    xdf <- count(xdf, x)
    sorted <- sort(xdf$n, decreasing = T)
    val1 <- which(xdf$n == unique(sorted)[1])
    val2 <- which(xdf$n == unique(sorted)[2])
    val3 <- which(xdf$n == unique(sorted)[3])
    val4 <- which(xdf$n == unique(sorted)[4])
    val5 <- which(xdf$n == unique(sorted)[5])
    val6 <- which(xdf$n == unique(sorted)[6])
    val7 <- which(xdf$n == unique(sorted)[7])
    val8 <- which(xdf$n == unique(sorted)[8])
    val_vec <- c(val1, val2, val3, val4, val5, val6, val7, val8)
    paste0("Three recommended vendors are: 1) ", xdf[val_vec[1], 1], ", 2) ",xdf[val_vec[2], 1], ", 3) ",xdf[val_vec[3], 1])
  })
  
  

}

shinyApp(ui = ui, server = server)

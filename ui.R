library(shiny)

pageWithSidebar(
  headerPanel("Prediction for NCLEX"),
  sidebarPanel(
    selectInput("gender", "Gender", c("M", "F")),
    numericInput("hesiScore", "HESI Score","", min=0, max=1200, value = "0"),
    numericInput("cumGpa", "Cum GPA", "", min=0, max=4, value= "0"),
    numericInput("highSchoolGpa", "High School GPA", "", value= "0"),
    #numericInput("satMath", "SAT Score (Math)", "", min=200, max=1600, value="200"),
    #numericInput("satVerbal", "SAT Score (Verbal)", "", min=200, max=800, value="200"),
    numericInput("totalSatScore", "Total SAT Score", "", min=400, max=1600, value="400"),
    numericInput("accuplacerAlgebra", "Accuplacer Score (Algebra)", "", min=20, max=120, value="20"),
    numericInput("accuplacerCalculus", "Accuplacer Score (Calculus)", "", min=20, max=120, value="20"),
    actionButton("goButton", "Predict")
  ),
  mainPanel(
    textOutput("validate_hesiScore"),
    textOutput("validate_cumGpa"),
    #textOutput("validate_satMath"),
    #textOutput("validate_satVerbal"),
    textOutput("validate_totalSatScore"),
    textOutput("validate_accuplacerAlgebra"),
    textOutput("validate_accuplacerCalculus"),
    titlePanel("Prediction Results"),
    verbatimTextOutput("nText")
  )
)

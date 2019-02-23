library(shiny)

flag_hesiScore <- 0

function(input, output) {
  
  validate_hesiScore <- eventReactive(input$hesiScore,{
    if (input$hesiScore < 0 | input$hesiScore > 1200){
      "HESI Score: Please enter value within 0 and 1200"
    }
  })
  validate_cumGpa <- eventReactive(input$cumGpa,{
    if(input$cumGpa < 0 | input$cumGpa > 4){
      "Cum GPA: Please enter value within 0 and 4"
    }
  })
  #validate_satMath <- eventReactive(input$satMath,{
  #  if(input$satMath < 200 | input$satMath > 800){
  #    "SAT Score (Math): Please enter value within 200 and 800"
  #  }
  #})
  #validate_satVerbal <- eventReactive(input$satVerbal,{
  #  if(input$satVerbal < 200 | input$satVerbal > 800){
  #    "SAT Score (Verbal): Please enter value within 200 and 800"
  #  }
  #})
  validate_totalSatScore <- eventReactive(input$totalSatScore,{
    if(input$totalSatScore < 400 | input$totalSatScore > 1600){
      "Total SAT Score: Please enter value within 400 and 1600"
    }
  })
  validate_accuplacerAlgebra <- eventReactive(input$accuplacerAlgebra,{
    if(input$accuplacerAlgebra < 20 | input$accuplacerAlgebra > 120){
      "Accuplacer Algebra: Please enter value within 20 and 120"
    }
  })
  validate_accuplacerCalculus <- eventReactive(input$accuplacerCalculus,{
    if(input$accuplacerCalculus < 20 | input$accuplacerCalculus > 120){
      "Accuplacer Calculus: Please enter value within 20 and 120"
    }
  })
  
  
  output$validate_hesiScore <- renderText({
    validate_hesiScore()
  })
  output$validate_cumGpa <- renderText({
    validate_cumGpa()
  })
  #output$validate_satMath <- renderText({
  #  validate_satMath()
  #})
  #output$validate_satVerbal <- renderText({
  #  validate_satVerbal()
  #})
  output$validate_totalSatScore <- renderText({
    validate_totalSatScore()
  })
  output$validate_accuplacerAlgebra <- renderText({
    validate_accuplacerAlgebra()
  })
  output$validate_accuplacerCalculus <- renderText({
    validate_accuplacerCalculus()
  })
  
  ntext <- eventReactive(input$goButton, {
    validate(
      need(input$hesiScore != "", "Please enter the numeric value for HESI Score"),
      need(input$cumGpa != "", "Please enter the numeric value for GPA"),
      need(input$highSchoolGpa != "", "Please enter the numeric value for High School GPA"),
      #need(input$satMath != "", "Please enter the numeric value for the SAT Math Score"),
      #need(input$satVerbal != "", "Please enter the numeric value for the SAT Verbal Score"),
      need(input$totalSatScore != "", "Please enter the numeric value for the Total SAT Score"),
      need(input$accuplacerAlgebra != "", "Please enter the numeric value for the Accuplacer Algebra"),
      need(input$accuplacerCalculus != "", "Please enter the numeric value for the Accuplacer Calculus")
    )
    
    #new.df <- data.frame(Gender=input$gender, HESI.Score=input$hesiScore, Cum.GPA=input$cumGpa,
    #                     High.School.GPA=input$highSchoolGpa, SAT.Math=input$satMath,
    #                     SAT.Verbal=input$satVerbal, Total.SAT=input$totalSatScore,
    #                     Accuplacer.Algebra=input$accuplacerAlgebra,
    #                     Accuplacer.Calculus=input$accuplacerCalculus)
    
    new.df <- data.frame(Gender=input$gender, HESI.Score=input$hesiScore, Cum.GPA=input$cumGpa,
                         High.School.GPA=input$highSchoolGpa, Total.SAT=input$totalSatScore,
                         Accuplacer.Algebra=input$accuplacerAlgebra, 
                         Accuplacer.Calculus=input$accuplacerCalculus)
    
    data <- read.csv(file="SAT Score - NCLEX Review with Admissions and Demographic data.csv", header = T)
    
    # Removing unncessory attributes
    data$X <- NULL
    data$Student.ID <- NULL
    data$FirstName <- NULL
    data$LastName <- NULL
    data$Birthdate <- NULL
    data$HESI_ID <- NULL
    data$HESI.Exam <- NULL
    data$Date <- NULL
    data$X.1 <- NULL 
    data$X.2<-NULL 
    data$X.3<-NULL 
    data$X.4<-NULL 
    data$X.5<-NULL 
    data$X.6<-NULL 
    data$X.7<-NULL
    data$Admitted.Major <- NULL
    
    #model <- glm(NCLEX.Outcome ~ HESI.Score + Gender + Cum.GPA + High.School.GPA + SAT.Math +
    #                SAT.Verbal + Total.SAT + Accuplacer.Algebra + Accuplacer.Calculus,
    #                data, family = binomial)
    
    if (new.df$Accuplacer.Algebra <= 20 & new.df$Accuplacer.Calculus){
      model <- glm(NCLEX.Outcome ~ HESI.Score + Gender + Cum.GPA + High.School.GPA +
                     Total.SAT, data, family = binomial)
    }
    else if (new.df$Accuplacer.Algebra <= 20){
      model <- glm(NCLEX.Outcome ~ HESI.Score + Gender + Cum.GPA + High.School.GPA +
                     Total.SAT + Accuplacer.Calculus, data, family = binomial)
    }
    else if (new.df$Accuplacer.Calculus <= 20){
      model <- glm(NCLEX.Outcome ~ HESI.Score + Gender + Cum.GPA + High.School.GPA +
                     Total.SAT + Accuplacer.Algebra, data, family = binomial)
    }
    else{
    model <- glm(NCLEX.Outcome ~ HESI.Score + Gender + Cum.GPA + High.School.GPA +
                   Total.SAT + Accuplacer.Algebra + Accuplacer.Calculus,
                 data, family = binomial)
    }
    
    result <- predict(model, new.df, type="response")
    
    #if(result > 0.5){
    #  prediction <- paste("Chances to Pass in NCLEX Exam", result, sep = " : ")
    #} 
    #else {
    #  prediction <- paste("Chances to Fail in NCLEX Exam", result, sep = " : ")
    #}
    
    if(result < 0.1){
      prediction <- paste("Percentage to Pass in the NCLEX Exam:", 0,"%")
    }
    else{
      prediction <- paste("Percentage to Pass in the NCLEX Exam:", result * 100,"%")
    }
    
    prediction
    
  })
  
  
  
  output$nText <- renderText({
    ntext()
  })
  
  #nText$hesiScoreOutput <- eventReactive(result)
  
}
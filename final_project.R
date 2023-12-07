library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)

source("file1.R")

df <- read.csv("finalPDF.csv")


ui <- fluidPage(
  titlePanel("Info 201 Final Project: Ivy Money"),
  
  p("Created by Randolph Jenkins", "Vridhi Manchanda", "Yaqi Wang"),
  
  tags$style(HTML(".navbar { background-color: #000000; }")),
  
  navbarPage(
    title = "Ivy Money",
    tabPanel("Intro", fluidPage(
      
      titlePanel("Our degree, worth it?"),
      
      br(),
      
      p("At a fall Finance Club meeting, students shared their recruitment experiences.
        Mason's revelation of landing a job at Bain from UW surprised everyone. Given the
        top consulting firms' Ivy League focus and their selective hiring (acceptance rates
        < 1%), we questioned our degree's value. Rather than UW vs. Non-UW, we narrowed our
        focus to Ivy League vs. non-Ivy League post-graduation salary. This analysis may interest
        some groups becuase it shows if going to ivy schools more likely leads to higher salary.
        We used data that indicate the income situation, school type, and many aspects of 269 colleges; with 
        these data, we hope to demonsrate the relationship between ivy schools and salary."),
      
      br(),
      
      img(src = "https://esimoney.com/wp-content/uploads/2017/07/Executive-holding-out-pile-of-cash.jpg",
          width = 500, height = 300, style = "display: block; margin: 0 auto;")
      
    )),
    
    
    tabPanel("Start Salary", fluidPage(
      
      titlePanel("Starting Salary Post-graduation"),
      
      br(),
      
      p("This section uses scatter plot to show the general starting salary trend of
         graduated students from different types of school"),
      
      br(),
      
      sidebarPanel(
        selectInput(inputId = "college_type", label = "Choose College Type", df$School.Type),
        width = 3,
        textOutput("summary_text")
      ),
      
      mainPanel(
        plotlyOutput(outputId = "scatterPlot"),
        
        br(),
        
        p("Through the plot, we can see the overall trend of Ivy schools' students do have
        higher starting median salary than most of schools, but the engineering type schools also show even 
        higher starting median salary than ivy schools")
      )
      
    )),
    
    
    tabPanel("Mid Salary", fluidPage(
                                     
      titlePanel("Mid Median Salary by College Type"),
      
      sidebarLayout(
        
        mainPanel(
          plotlyOutput(outputId = "violinPlot"),
          plotlyOutput(outputId = "scatterPlot2"),
          br(),
          p("Through the plot, we can tell students graduated from Ivy schools show 
            advantages in Mid-career median salary, and students graduated from 
            state colleges show disadvantage compared to others. Ivy school shows 
            highest mean and the violin graph of it shows more people in ivy schools tend to 
            earn higher salary in the violin graph of ivy-type school")
        ),
        
       
        
        sidebarPanel(
          selectInput(inputId = "college_each_type", label = "Choose A College Type
                      to see the trend", choices = df$School.Type),
          width = 3,
        )

        
        
      )
    )),
    
    
    tabPanel("Salary Growth", fluidPage(
      titlePanel("Difference in Salary throughout career"),
      p("This section discusses the difference between starting median 
        salary and mid-career median salary for different types of school; 
        therefore, we can see the salary growth for different types of universities."),
      
      mainPanel(
        plotlyOutput("barChart"),
        plotlyOutput("barChart2")
      ),
      
      sidebarPanel(
        p("In the Difference in Salary throughout career bar chart, we can see 
          Ivy school do have higher difference between starting median salary and 
          mid-career median salary, with almost 60,000 growth.Engineering schools
          comes after, and State school is the last"),
        
        selectInput(inputId = "college_each_type2", label = "Choose A College Type
                      to see the growth of all colleges", choices = df$School.Type),
        width = 3,
      )
    )),
    tabPanel("Summary&About", fluidPage(
      titlePanel("Conclusion"),
      img(src = "https://keystoneacademic-res.cloudinary.com/image/upload/element/17/179136_photo-1623631484725-fef26b75b402.jpg",
          width = 800, height = 480, style = "display: block; margin: 0 auto;"),
      br(),
      p("After comparing the salary at different stages of careers for different types of colleges,
        we do find out that students graduated from Ivy-schools tend to have higher salary and salary growth,
        and State schools comes the last. This 
        situation could due the resources Ivy-schools can provide to students, and people may be interested in 
        situation due to the inequitable resource distriction on students across the country, which may lead to more radical 
        difference between classes. We need more data to strengthen the major takeways we have concluded, and further 
        studies are needed"),
      br(),
      br(),
      h1("Created by Randolph Jenkins", "Vridhi Manchanda", "Yaqi Wang")
      
    ))
  )
  

  
)


server <- function(input, output) {
  # Your server logic goes here
  
  
  output$summary_text <- renderText({
    
    req(input$college_type)
    
      paste("Summary for", input$college_type, "colleges:",
            "Highest Starting Median Salary:", max_startMedianSalary_certain_type(df, input$college_type),
            "Lowest Starting Median Salary:", min_startMedianSalary_certain_type(df, input$college_type))
    
  
  })
  
  output$scatterPlot <-  renderPlotly({
    ggplot(df, aes(x = reorder(School.Type, Starting.Median.Salary.x), y = Starting.Median.Salary.x, color = School.Type)) +
      geom_point() +
      labs(title = "Starting Median Salary According to School types", x = "School Types", y = "Starting Median Slary") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  

  output$violinPlot <- renderPlotly({
     
      ggplot(df, aes(x = School.Type, y = Mid.Career.Median.Salary.x, color = School.Type)) +
      geom_violin() +
      labs(title = "Mid-career median Salary for each college type",
           y = "Mid-career median salary", x = "School.Type") +
      scale_color_discrete(name = "School.Type")
    
    
  })
  
  output$scatterPlot2 <- renderPlotly({
    
    selectedData <- filter(df, df$School.Type == input$college_each_type)
    
    meanValue <- mean(selectedData$Mid.Career.Median.Salary.x)
    
   ggplot(selectedData, aes(y = Mid.Career.Median.Salary.x, x = School.Name, color = School.Name)) +
      geom_point() +
     labs(title = paste("Mid-career median Salary for", input$college_each_type, "college type"),
           y = "Mid-Career-Salary",
           x = "") +
     theme(axis.text.x = element_blank())+ 
     geom_hline(yintercept = meanValue, linetype = "dashed", color = "red")
  })
  
  output$barChart <- renderPlotly({

    ggplot(summarize_df, aes(x = School.Type, y = mean.Start.Mid.Salary.Difference, fill = School.Type)) +
      geom_bar(stat = "identity") +
      labs(title = "Difference between starting-career median salary
                    and mid-career median salary for each type of colleges",
           x = "School.Type",
           y = "mean.Start.Mid.Salary.Difference")
  })
  
  output$barChart2 <- renderPlotly({
    
    selectedData <- filter(df, df$School.Type == input$college_each_type2)
    
    p <- ggplot(selectedData, aes(x = School.Name, y = Start.Mid.Salary.Diff, fill = School.Type)) +
      geom_bar(stat = "identity") +
      labs(title = "Difference between starting-career median salary
                    and mid-career median salary for all colleges in a certain type",
           x = "",
           y = "mean.Start.Mid.Salary.Difference")
  })
  
  
  
}

shinyApp(ui, server)
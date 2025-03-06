# Analytical Hierarchy Process for Multi Criterion Decision Making
# Travis Zalesky
# 3/4/25
#
# This is a Shiny web application. You can run the application by clicking the 'Run App' 
# button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Begin App
library(shiny)
library(shinyWidgets)
library(shinyjs)

myjs <- "
$(document).on('change', '.dynamicSI input', function(){
  Shiny.setInputValue('lastSelectId', this.id, {priority: 'event'});
});
"

ui <- fluidPage(
  lang = 'en',
  theme = bslib::bs_theme(bootswatch = 'sandstone'),  # base styling theme
  useShinyjs(),  # Enable js functionality
  # Extend Shinyjs
  # Reset page function
  extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
  # Page head
  tags$head(
    tags$script(HTML(myjs)),
    # Link to css
    tags$link(
      rel = 'stylesheet', 
      type = 'text/css', 
      href = 'styles.css'  # implied www/ filepath
    )  # end tags$link (css)
  ),  # end tags$head (page head)

  # Application title
  titlePanel(windowTitle = "AHP for MCDM",
    column(
      12,  # column width
      div(class = 'center-title', h1("Analytical Hierarchy Process")),
      div(class = 'center-title', h2("for Multi Criterion Decision Making"))
    )  # end column
  ),  # end titlePanel

  tabsetPanel(
      type = 'tabs',
      id = 'tabs',
      tabPanel(title = 'Home',
        # Header
        div(class = 'center-text',
          h3('Component Variables')
        ),
        div(
          h6('Add the constituent factors of your decision here.')
        ), 

        # Initial text input (minimum 2 variables)
        textInput(inputId = "var1", 
          label = 'Variable 1', 
          value = "", 
          width = NULL, 
          placeholder = 'e.g. Distance'
        ),
        textInput(inputId = "var2", 
          label = 'Variable 2', 
          value = "", 
          width = NULL, 
          placeholder = 'e.g. Time'
        ),

        div(
          id = 'vars_ui'
        ),

        # Action button to generate more rows
        actionButton(inputId = 'addRow',
          label = 'Add Variable',
          class = "btn-primary"
        ),
        # Action Button to reset
        actionButton(inputId = 'reset',
          label = 'Reset',
          class = "btn-danger"
        ),
        # Action Button to submit vars for pwc
        actionButton(inputId = 'submitVars',
          label = 'Submit',
          class = 'btn-success'
        ),

        # User messages
        uiOutput(outputId = 'warning2')
      ),  # end tabPanel (Home)
      
    )  # end tabs

)  # end ui

server <- function(input, output, session) {
  # Reactive Values
  # Variable Count
  varCount <- reactiveVal(2)  # default minimum 2 variables
  # vars for naming sliders
  var1 <- reactiveVal()
  var2 <- reactiveVal()
  # sliderCount <- reactiveVal(1)

  # Add Variable Function
  addVar <- function(count) {
    placeholderText = c('e.g. Distance', 'e.g. Time', 'e.g. Scenery')
    textInput(inputId = paste0("var", varCount()), 
          label = paste0('Variable ', varCount()),
          value = "", 
          width = NULL, 
          placeholder = placeholderText[varCount()]
        )
  }

  clearWarnings <- function(num) {
    output[[paste0('warning', num)]] <- renderUI({return(NULL)})
  }

  # Generate sliders function
  generateSliders <- function(vars) {
    for(i in c(1:length(vars[-1]))) {
      var1 = vars[i]
      # print(var1())
      for(j in c(i:length(vars[-1]))) {
        var2 = vars[j+1]
        # print(var2())
        # print(paste(var1(), var2(), sep = '_'))
        
        insertUI(selector = '#sliders', 
          ui = div(class = 'label-left',
            fluidRow(
              sliderInput(inputId =  paste(var1, var2, sep = '_'),
                label = var1,  # Slider label (left label)
                min = -9,
                max = 9,
                value = 0,
                step = 1,
              ),  # end slider 
              # Right label
              div(class = 'label-right', var2)
            )  # end fluidRow
          )  # end column
        )
      }
      
    }  
  }

  # react to changes in dynamically generated selectInput's
  observeEvent(input$lastSelectId, {

    cat("lastSelectId:", input$lastSelectId, "\n")
    cat("Selection:", input[[input$lastSelectId]], "\n\n")

  })  


  # # Generate Saaty's matrix statistics
  # generateResults <- function(vars) {
  #   # Results Table
  #   # Scale table size to match length of variables list (square)
  #   resultsTable <- data.frame(matrix(ncol = length(vars), nrow = 0))
  #   colnames(resultsTable) <- vars
  #   # Results will be filled in below

  #   # Fill in initial table data from default values
  #   rowIndex <- 1
  #   tableData <- c()
  #   sliderCount(1)
  #   # Iterate through vars list twice (nested)
  #   for(i in c(1:length(vars))) {
  #     var1(vars[i])
  #     for(j in c(1:length(vars)))  {
  #       var2(vars[j])
        
  #       print(paste(var1(), var2(), sep = '_'))

  #       if (var1() == var2()) {
  #         # if var1 = var2, then data = 1 (i.e. table diagonal)
  #         dataPoint <- "1"
  #       } else {
  #         # if var1 != var2, then check sliders for value
          
  #         print(paste0("slide", sliderCount()))
  #         dataPoint <- input[[paste0("slide", sliderCount())]]
  #         sliderCount(sliderCount() + 1)
  #       }
  #       # print(paste(var1(), var2(), sep = '_'))
  #       print(dataPoint)
  #     }  # end lapply (j)
      
  #   }  # end laaply (i)
    
  #     # # Insert table Data into resultsTable
  #     # rowEnd <- rowIndex * length(vars)
  #     # rowStart <- rowEnd - length(vars) + 1
  #     # rowData <- tableData[rowStart:rowEnd]
  #     # rowDf <- as.data.frame.list(rowData)
  #     # row.names(rowDf) <- var1
  #     # names(rowDf) <- vars
  #     # # Append data to resultsTable
  #     # resultsTable <- rbind(resultsTable, rowDf)
      
  #   #   rowIndex <- rowIndex + 1  # increment index
  #   # }  # end for var 1
  #   # # return(resultsTable)
  # }  # end generateResults

  # Render sliders tab function
  renderTabs <- function(vars) {
    insertTab(inputId = 'tabs',
      tabPanel('Sliders',
        # Header
        div(class = 'center-text',
          h3('Variable Pair Wise Comparison')
        ),
        div(
          h6('Adjust the relative importance of each variable pair.')
        ),
        # Sidebar
        sidebarLayout(
          sidebarPanel(id = 'Sidebar',
            div(class = 'center-text',
              h2('Survey Explanation')
            ),
            hr(),  # line break
            div(class = 'center-text',
              strong("Saaty's Scale of Absolute Numbers")),
              img(src='SaatyScale.png', width = '100%'),
              em('Image from Aloui et al. (2024) Fig. A1, and may be subject to copyright. Please do not redistribute!'),
              hr(),  # line break
              HTML({
                "Using Saaty's Scale as a reference, adjust the sliders for each variable pair-wise comparison to your preferred value, in the direction of the most important variable.
                <br><br>
                Special Values:<br>
                &ensp;<b>1 = -1 = Variables of equal importance.</b><br>
                &ensp;<b>0 = No opinion/NA</b>
                <br><br>
                When finished, view and export your results in the next tab."
              }),
          ),  # end sidebarPanel

          mainPanel(# Header section (above tab set panel)
            # Buttons to control sidebar behavior
            actionButton("showSidebar", "<<<"),
            div(class = 'center-text',
              h2('Relative Importance'),
                    
              div(class = 'table',
                div(class = 'table-row',
                  div(class = 'text-left', HTML('Variable A<br>more important')),
                  div(class = 'text-right', HTML('Variable B<br>more important')),
                ),
              ),
            ),
            div(class = 'arrow', '\U21A4|\U21A6'),

            # Dynamic UI created on the fly by server
            tags$div(id = 'sliders', class = 'dynamicSI'),
            
            generateSliders(vars),
            # generateResults(vars),

            # div(class = 'center-text',
            #   h2('Statistics'),
            #   htmlOutput('floodResultsTable'),
            #   htmlOutput('floodStatisticsText'),
            # ),
            # fluidRow(
            #   column(10,
            #     htmlOutput('floodUserMessage')
            #   )
            # )
          )

        )

      )
    )
  }

  # Add Variable Button
  observeEvent(input$addRow, {
    clearWarnings(2)  # Clear warning 2, if any
    if(varCount() == 7) {
      insertUI(selector = '#vars_ui', 
        ui = uiOutput(outputId = 'warning1')
      )
      output$warning1 <- renderUI({
        div(id = 'message1',
          'Caution: More than 7 variables is not recommended due to human limits to process information. Consider breaking your analysis into chunks.',
          class = 'warning-text'
        )
      })
    }
    varCount(varCount() + 1)
    insertUI(selector = '#vars_ui', ui = addVar(varCount()))
  })

  # Reset Button
  observeEvent(input$reset, {
    disable('reset')
    js$refresh_page()
  })  

  # Submit Vars Button
  observeEvent(input$submitVars, {
    clearWarnings(2)

    vars = c()  # a list for vars
    for(i in c(1:varCount())) {
      # Get var from dynamic input
      var <- input[[paste0('var', i)]]
      # If var is valid text
      if(!is.null(var) & var != '') {
        # Add var to list of vars
        vars = c(vars, var)
      }
    }
    
    if(length(vars) < 2) {
      output$warning2 <- renderUI({
        div(id = 'message2', 
          'Decision matrix must have at least 2 variables!',
          class = 'error-text'
        )
      })
    } else {
      renderTabs(vars)
      updateTabsetPanel(session, "tabs", 'Sliders')
      disable('submitVars')
      disable('addRow')
    }
  })

  # JS for sidebar behavior
  observeEvent(input$showSidebar, {
    shinyjs::toggle(id = "Sidebar")

    js_maintab <- paste0('$("div[role=',"'main'",']")')
    
    runjs(paste0('
          width_percent = parseFloat(',js_maintab,'.css("width")) / parseFloat(',js_maintab,'.parent().css("width"));
          if (width_percent == 1){
            ',js_maintab,'.css("width","");
          } else {
            ',js_maintab,'.css("width","100%");
          }
          '))
    
    # Update button text
    if (input$showSidebar %% 2 != 0) {
      updateActionButton(session, "showSidebar", '>>>')
    } else {
      updateActionButton(session, "showSidebar", '<<<')
    }
  })
  
}  # end server

# deploy app
shinyApp(ui = ui, server = server)

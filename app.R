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
library(dplyr)
library(tibble)

ui <- fluidPage(
  lang = 'en',
  theme = bslib::bs_theme(bootswatch = 'sandstone'),  # base styling theme
  useShinyjs(),  # Enable js functionality
  # Extend Shinyjs
  # Reset page function
  extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
  # Page head
  tags$head(
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
      column(width = 4,  # column width
        # Header
        div(class = 'center-text',
          h3('Component Variables'),
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
      )  # end tabPanel (Home)
    ),

    tabPanel(title = "Sliders",
      # Tab Header
      div(class = 'center-text',
        h3('Variable Pair Wise Comparison'),
        h6('Adjust the relative importance of each variable pair.')
      ),
      
      # Sidebar layout
      sidebarLayout(
        sidebarPanel(id = 'Sidebar',
          # Sidebar Header
          div(class = 'center-text',
            h2('Survey Explanation')
          ),
          hr(),  # line break
          # Sidebar content
          div(class = 'center-text',
            # Saaty's absolute numbers figure, with title and caption
            strong("Saaty's Scale of Absolute Numbers")
          ),
          img(src='SaatyScale2.png', width = '100%'),
          # User instructions (HTML)
          HTML({
            "Using Saaty's Scale as a reference, adjust the sliders for each variable pair-wise comparison to your preferred value, in the direction of the most important variable.
            <br><br>
            Special Values:<br>
            &ensp;<b>1 = -1 = Variables of equal importance.</b><br>
            &ensp;<b>0 = No opinion/NA</b>
            <br><br>
            When finished, view and export your results."
          }),
        ),  # end sidebarPanel

        # Main panel
        mainPanel(
          column(width = 6,
            # Main Panel Header
            div(class = 'center-text',
              h2('Relative Importance'),
              # Stylized subheading
              div(class = 'subhead-sliders',
                div(class = 'text-left', HTML('Variable A<br>more important')),
                div(class = 'arrow', '\U21A4|\U21A6'),
                div(class = 'text-right', HTML('Variable B<br>more important')),  
              ),
              hr(),  # line break
              # Divs for dynamically rendered UI
              tags$div(id = 'sliders', class = 'dynamicSI'),
             htmlOutput('userMessage'),
            ),
            tags$div(id = 'resultsDiv', 
              class = 'center-text',
              h4('Results'),  # Table Header
              tableOutput('resultsTable')
            ),
            div(id = 'resultsButtons', class = 'center-text',
              actionButton('toggleEigen', 'Show Eigenvectors', class = 'btn-info'),
              downloadButton('download', 'Download Results', class = 'btn-success')
            )
          )
        )
      )
    )  
  )  # end tabs
)  # end ui

server <- function(input, output, session) {
  # Reactive Values
  # Variable Count
  varCount <- reactiveVal(2)  # default minimum 2 variables
  # vars for naming sliders
  var1 <- reactiveVal()
  var2 <- reactiveVal()
  sliders <- reactiveVal(list())
  # reactive value of selected slider
  val <- reactiveVal()
  # Saaty's table (df)
  saatysTable <- data.frame()
  # Results table (df)
  resultsTable <- data.frame()
  # Toggle eigenvectors
  eigenToggle <- reactiveVal(0)  # 0 = off, 1 = on

  # Functions ------------------------------
  # Add Variable Function 
  addVar <- function(count) {
    # Add a user-input variable to the list of component variables.
    placeholderText = c('e.g. Distance', 'e.g. Time', 'e.g. Scenery')
    textInput(inputId = paste0("var", count), 
          label = paste0('Variable ', count),
          value = "", 
          width = NULL, 
          placeholder = placeholderText[count]
        )
  }

  # Clear Warnings Function
  clearWarnings <- function(num) {
    # Clear user warnings on event.
    output[[paste0('warning', num)]] <- renderUI({return(NULL)})
  }

  # Calculate column totals (Saaty's matrix)
  calcTotals <- function(table, numeric = F) {
    # Get vars from table
    vars <- names(table)
    # Reset table totals (if applicable)
    if('Total' %in% row.names(table)) {
      table <- table[!(row.names(table) %in% c('Total')), ]
    }
    totals <- c()
    for (var in vars) {
      # Evaluate text as numeric fraction
      vals = sapply(table[[var]], function(x) eval(parse(text = as.character(x))))
      total = sum(vals, na.rm = T)
      totals = c(totals, total)
    }
    # Determine if values should be returned as numeric or as character
    if (numeric == F) {
      totals <- as.data.frame.list(as.character(round(totals, 2)))
    } else {
      totals <- as.data.frame.list(round(totals, 2))
    }

    # Update names of totals df
    names(totals) <- vars
    row.names(totals) <- 'Total'
    # bind totals to bottom of table
    table <-rbind(table, totals)

    return(table)
  }

  # Calculate eigenvectors
  calcEigen <- function(table) {
    # Get list of vars from table
    vars = names(table)
    # Save column totals
    totals <- sapply(table['Total', ], function(x) eval(parse(text = as.character(x))))
    # Drop totals from table
    eigenTable <- table[!(row.names(table) %in% c('Total')), ]

    for (name in names(totals)) {
      total = as.numeric(totals[name])  # get named total value
      # Parse table column as numeric
      vect <- sapply(eigenTable[ , name], function(x) eval(parse(text = as.character(x))))
      eigenvect <- vect / total  # calculate eigenvector
      eigenTable[ , name] <- eigenvect  # update eigenTable
    }

    # Calculate weights as row wise means of eigen table
    weights <- rowMeans(eigenTable, na.rm = T)
    totalWeights <- sum(weights)
    resultsTable <<- data.frame(
      Variable = c(vars, 'Total'),
      Weight = c(weights, totalWeights)
    )

    # Calculate Consistency Index
    ciTable <- as.data.frame(list(
      Totals = totals,
      Eigen = rowSums(eigenTable, na.rm = T)/length(totals),
      Eigen_Val = (totals * (rowSums(eigenTable, na.rm = T)/length(totals)))
    ))
    principalEigen <- sum(ciTable$Eigen_Val, na.rm = T)
    
    # Calculate normalized eigenvector totals
    eigenTable <- calcTotals(eigenTable, numeric = T)
    # eigenvectors should sum to 1
    # Add eigenvector values (means) to eigenTable 
    eigenTable$Eigen_Value <- rowSums(eigenTable, na.rm = T)/length(totals)

    # Saaty's Random Index DF
    saatyRi <- as.data.frame(list(
      Order = c(2:30),
      RI = c(
        0, 0.52, 0.89, 1.12, 1.26, 1.36, 1.41, 1.46, 1.49,  # 2-10
        1.52, 1.54, 1.56, 1.58, 1.59, 1.5943, 1.6064, 1.6133, 1.6207, 1.6292,  # 11-20
        1.6358, 1.6403, 1.6462, 1.6497, 1.6556, 1.6587, 1.6631, 1.667, 1.6693, 1.6724  #21-30
      )
    ))

    # Get RI from df based on number of input vars
    # If input vars > 30, use max ri (limit ~1.7)
    if(length(totals) > 30) {
      ri <- max(saatyRi$RI)
    } else {
      # Get ri from appropriate position in df
      ri <- saatyRi[saatyRi$Order == length(totals), ]$RI
    }
    
    # Calculate consistency ratio
    ci = (principalEigen - length(totals))/(length(totals) - 1)
    cr = ci / ri
    
    # Use CR to determine appropriate user message
    if (length(sliders()) == 1) {
      statisticsText <- '<p></p>'
    } else if (principalEigen < length(totals)) {
      statisticsText <- '<p style="color:red">The principal eigen value of your data table is less than the number of inputs. This is commonly the result of setting values to 0. Consider updating any variables set to 0 to improve internal consistency.</p>'
    } else if (cr >= 0.1) {
      statisticsText <- '<p style="color:red">Your consistency ratio is unusually high. This indicates that some of your variable comparisons may not be internally consistent. Consider adjusting the relative importance of one or more variables.</p>'
    } else {
      statisticsText <- paste('<p style="color:green">Your variable values appear to be internally consistent! \U1F389</p>')
    }

    output$userMessage <- renderText(statisticsText)
    printTable(resultsTable)

  }

  # Generate Sliders Function
  generateSliders <- function(vars) {
    # Generate x number of pwc sliders based on y user-generated inputs
    # x = (y * (y-1))/2
    #   where:
    #     x = number of sliders
    #     y = number of user-generated inputs
    #     and variable order does not matter
    for(i in c(1:length(vars[-1]))) {
      var1 = vars[i]
      
      for(j in c(i:length(vars[-1]))) {
        var2 = vars[j+1]
        
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
        # Add list of slider ids to reactive values
        sliders(c(sliders(), paste(var1, var2, sep = '_')))
      }
    }  
  }

  # Genearte Listeners
  generateListeners <- reactive({
    lapply(sliders(), function (s) {
      observeEvent(input[[s]], {
        val(input[[s]])
        updateSaatys(saatysTable, s, val())
      })
    }) 
  })

  # Generate (initial) Saaty's matrix statistics
  generateSaatys <- function(vars) {
    # Results Table
    # Scale table size to match length of variables list (square)
    statisticsTable <- data.frame(matrix(ncol = length(vars), nrow = 0))
    colnames(statisticsTable) <- vars
    # Results will be filled in below

    # Fill in initial table data from default values
    rowIndex <- 1
    tableData <- c()
    # Iterate through varAbbr list twice (nested)
    for (var1 in vars) {
      for (var2 in vars) {
        # Get initial table value for variable pairs
        if (var1 == var2) {
          # if var1 = var2, then data = 1 (i.e. table diagonal)
          dataPoint <- "1"
        } else {
          # All slider values initially set to 0
          dataPoint <- '0'
        }
        tableData <- c(tableData, dataPoint)  # append tableData list
      }

      # Insert table Data into statisticsTable
      # Calculate start and end values for data matrix 'row' within list of data
      rowEnd <- rowIndex * length(vars)
      rowStart <- rowEnd - length(vars) + 1
      # Slice list to get subset data
      rowData <- tableData[rowStart:rowEnd]
      # Convert to 1-row df for rbind()
      rowDf <- as.data.frame.list(rowData)
      # Set df names
      row.names(rowDf) <- var1
      names(rowDf) <- vars
      # Bind row df to results df using row bind
      statisticsTable <- rbind(statisticsTable, rowDf)

      rowIndex <- rowIndex + 1  # increment index
    }

    # Calculate column totals for table
    statisticsTable <- calcTotals(statisticsTable)
    
    return(statisticsTable)
    
  }  # end generateSaatys

  # Beautify table for rendered output
  printTable <- function(table) {
    if (eigenToggle() == 0) {
      # Eigenvectors off
      # Abbreviate vars
      abbr <- unlist(lapply(table$Variable, function(var) {
        if(nchar(var) > 9) {
          var = paste0(substr(var, 1, 9), '.')
        } 
        return(var)
      }))
      table$Variable <- abbr  # Insert abbreviated vars

      output$resultsTable <- renderTable(table, rownames = F)
    } else {
      # Abbreviate vars (shorter)
      vars <- names(saatysTable)
      abbr <- unlist(lapply(vars, function(var) {
        if(nchar(var) > 4) {
          var = paste0(substr(var, 1, 4), '.')
        } 
        return(var)
      }))
      # Get row names as column for saatysTable
      saatysTable <- saatysTable%>%
        rownames_to_column('Variable')
      
      # Join tables
      table <- saatysTable %>%
        left_join(table)%>%
        column_to_rownames('Variable')

      # Use abbreviated column and row names
      names(table) <- c(abbr, 'Weight')
      row.names(table) <-c(abbr, 'Total')
      
      output$resultsTable <- renderTable(table, rownames = T)
    }
  }

  # Update Saaty's matrix
  updateSaatys <- function(table, id, value) {
    # Get vars from last selected id
    vars <- id
    var1 <- strsplit(vars, split = '_')[[1]][1]  # Row name
    var2 <- strsplit(vars, split = '_')[[1]][2]  # Col name
    # Get column and row index matching vars
    i1 <- which(names(table) == var1)
    i2 <- which(row.names(table) == var2)
    # Get inverse of value
    inverse <- paste0('1/', abs(value))

    # Determine sign of value
    if(value == 0) {
      # Update table values with 0s
      table[i2, i1] <- 0
      table[i1, i2] <- 0
    } else if(abs(value) == 1) {
      # Update table values with 1s
      table[i2, i1] <- 1
      table[i1, i2] <- 1
    } else if(value > 1) {
      # Update table values at specified index
      table[i2, i1] <- value
      table[i1, i2] <- inverse
    } else {
      # Reverse the order of column and row indices
      # Update table values at specified index
      table[i2, i1] <- inverse
      table[i1, i2] <- abs(value)  # Positive value
    }

    saatysTable <<- calcTotals(table)

    printTable(saatysTable)
    calcEigen(saatysTable)
  }

  # Observers/Listeners
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
      # Calculate initial Saaty's table
      saatysTable <<- generateSaatys(vars)
      # renderTabs(vars, saatysTable)
      # Generate and insert sliders into ui
      generateSliders(vars)
      printTable(saatysTable)
      calcEigen(saatysTable)
      updateTabsetPanel(session, "tabs", 'Sliders')
      disable('submitVars')
      disable('addRow')
      generateListeners()
    }
  })

  # Show eigenvectors in results table
  observeEvent(input$toggleEigen, {
    # Update toggle value
    if(eigenToggle() == 0) {
      eigenToggle(1)
    } else {
      eigenToggle(0)
    }

    # Print results table
    printTable(resultsTable)
  })

  # Download Data
  output$download <- downloadHandler(
      filename = function() {
        paste0('AHP-data-', Sys.Date(), '.csv', sep='')
      },
      content = function(file) {
        for (name in names(saatysTable)) {
          # Parse table column as numeric
          vect <- sapply(saatysTable[ , name], function(x) eval(parse(text = as.character(x))))
          saatysTable[ , name] <- vect  # update eigenTable
        }
        # Bundle data
        # Get row names as column for saatysTable
        saatysTable <- saatysTable%>%
          rownames_to_column('Variable')
        
        # Join tables
        data <- saatysTable %>%
          left_join(resultsTable)%>%
          column_to_rownames('Variable')

        write.csv(data, file, row.names = T)
      }
    )
  
  # Initial state
  # Sliders tab user message
  output$userMessage <- renderText(
    '<p style="color:red">Please add a minimum of 2 component variables to the HOME tab. When you are ready click SUBMIT.</p>'
  )
  
}  # end server

# deploy app
shinyApp(ui = ui, server = server)

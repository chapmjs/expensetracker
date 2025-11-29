# Expense Tracker Shiny Application
# Version 1.0

# Load required libraries
library(shiny)
library(shinydashboard)
library(DBI)
library(RMySQL)
library(DT)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(tidyr)

# Database configuration
# Note: In production, store these in environment variables or config file
#source("config.R")

# Function to get database connection
get_db_connection <- function() {
  tryCatch({
    dbConnect(
      RMySQL::MySQL(),
      host = db_config$host,
      dbname = db_config$dbname,
      user = db_config$user,
      password = db_config$password,
      port = db_config$port
    )
  }, error = function(e) {
    stop(paste("Database connection failed:", e$message))
  })
}

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Family Expense Tracker"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Add Expense", tabName = "add_expense", icon = icon("plus-circle")),
      menuItem("View Expenses", tabName = "view_expenses", icon = icon("table")),
      menuItem("Reports", tabName = "reports", icon = icon("chart-bar")),
      menuItem("Settings", tabName = "settings", icon = icon("cog"))
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 5px;
        }
        .btn-primary {
          background-color: #3c8dbc;
          border-color: #367fa9;
        }
      "))
    ),

    tabItems(
      # Add Expense Tab
      tabItem(
        tabName = "add_expense",
        fluidRow(
          box(
            title = "Add New Expense",
            status = "primary",
            solidHeader = TRUE,
            width = 12,

            fluidRow(
              column(4,
                dateInput("expense_date", "Date:",
                         value = Sys.Date(),
                         max = Sys.Date())
              ),
              column(4,
                textInput("vendor", "Vendor:",
                         placeholder = "Where was the purchase made?")
              ),
              column(4,
                numericInput("amount", "Amount ($):",
                            value = 0,
                            min = 0,
                            step = 0.01)
              )
            ),

            fluidRow(
              column(4,
                selectInput("category", "Category:",
                           choices = NULL)  # Will be populated from database
              ),
              column(4,
                selectInput("buyer", "Buyer:",
                           choices = NULL,  # Will be populated
                           multiple = FALSE)
              ),
              column(4,
                textInput("new_buyer", "Or Add New Buyer:",
                         placeholder = "Enter name if not in list")
              )
            ),

            fluidRow(
              column(12,
                textAreaInput("note", "Notes:",
                            placeholder = "Optional: Add any additional information",
                            rows = 3,
                            width = "100%")
              )
            ),

            fluidRow(
              column(12,
                actionButton("add_expense_btn", "Add Expense",
                           class = "btn-primary btn-lg"),
                actionButton("clear_form_btn", "Clear Form",
                           class = "btn-default btn-lg")
              )
            )
          )
        ),

        fluidRow(
          box(
            title = "Recent Expenses",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("recent_expenses")
          )
        )
      ),

      # View Expenses Tab
      tabItem(
        tabName = "view_expenses",
        fluidRow(
          box(
            title = "Filter Options",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,

            fluidRow(
              column(3,
                dateRangeInput("date_range", "Date Range:",
                             start = floor_date(Sys.Date(), "month"),
                             end = Sys.Date())
              ),
              column(3,
                selectInput("filter_category", "Category:",
                           choices = c("All" = "", "Loading..."),
                           multiple = TRUE)
              ),
              column(3,
                selectInput("filter_buyer", "Buyer:",
                           choices = c("All" = "", "Loading..."),
                           multiple = TRUE)
              ),
              column(3,
                br(),
                actionButton("apply_filters", "Apply Filters",
                           class = "btn-primary")
              )
            )
          )
        ),

        fluidRow(
          box(
            title = "All Expenses",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("all_expenses")
          )
        )
      ),

      # Reports Tab
      tabItem(
        tabName = "reports",
        fluidRow(
          box(
            title = "Report Options",
            status = "primary",
            solidHeader = TRUE,
            width = 12,

            fluidRow(
              column(4,
                selectInput("report_type", "Report Type:",
                           choices = c("Monthly Summary" = "monthly",
                                     "Weekly Summary" = "weekly",
                                     "Category Breakdown" = "category",
                                     "Buyer Analysis" = "buyer"))
              ),
              column(4,
                dateRangeInput("report_date_range", "Date Range:",
                             start = floor_date(Sys.Date() - months(3), "month"),
                             end = Sys.Date())
              ),
              column(4,
                br(),
                actionButton("generate_report", "Generate Report",
                           class = "btn-primary")
              )
            )
          )
        ),

        fluidRow(
          box(
            title = "Summary Statistics",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            uiOutput("summary_stats")
          )
        ),

        fluidRow(
          box(
            title = "Report Visualization",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("report_plot", height = "400px")
          )
        ),

        fluidRow(
          box(
            title = "Detailed Report",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("report_table")
          )
        )
      ),

      # Settings Tab
      tabItem(
        tabName = "settings",
        fluidRow(
          box(
            title = "Manage Categories",
            status = "warning",
            solidHeader = TRUE,
            width = 6,

            textInput("new_category", "Add New Category:"),
            textInput("category_description", "Description:"),
            actionButton("add_category_btn", "Add Category",
                       class = "btn-warning"),
            hr(),
            DT::dataTableOutput("categories_table")
          ),

          box(
            title = "Manage Buyers",
            status = "warning",
            solidHeader = TRUE,
            width = 6,

            textInput("new_buyer_setting", "Add New Buyer:"),
            textInput("buyer_email", "Email (optional):"),
            actionButton("add_buyer_btn", "Add Buyer",
                       class = "btn-warning"),
            hr(),
            DT::dataTableOutput("buyers_table")
          )
        ),

        fluidRow(
          box(
            title = "Database Status",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("db_status")
          )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {

  # Reactive values
  values <- reactiveValues(
    categories = NULL,
    buyers = NULL,
    expenses = NULL,
    db_connected = FALSE
  )

  # Initialize database connection and load data
  observe({
    tryCatch({
      con <- get_db_connection()

      # Load categories
      categories_query <- "SELECT category_name FROM categories ORDER BY category_name"
      values$categories <- dbGetQuery(con, categories_query)$category_name

      # Load buyers
      buyers_query <- "SELECT DISTINCT buyer_name FROM buyers ORDER BY buyer_name"
      buyers_result <- dbGetQuery(con, buyers_query)
      if (nrow(buyers_result) > 0) {
        values$buyers <- buyers_result$buyer_name
      } else {
        values$buyers <- c("Add a buyer in Settings")
      }

      values$db_connected = TRUE

      dbDisconnect(con)

      # Update UI elements
      updateSelectInput(session, "category",
                       choices = values$categories)
      updateSelectInput(session, "buyer",
                       choices = values$buyers)
      updateSelectInput(session, "filter_category",
                       choices = c("All" = "", values$categories))
      updateSelectInput(session, "filter_buyer",
                       choices = c("All" = "", values$buyers))

    }, error = function(e) {
      showNotification(paste("Database connection error:", e$message),
                      type = "error",
                      duration = 10)
      values$db_connected = FALSE
    })
  })

  # Add Expense
  observeEvent(input$add_expense_btn, {
    req(input$expense_date, input$vendor, input$amount)

    # Determine buyer
    buyer_to_use <- ifelse(nchar(input$new_buyer) > 0,
                           input$new_buyer,
                           input$buyer)

    if (is.null(buyer_to_use) || buyer_to_use == "" || buyer_to_use == "Add a buyer in Settings") {
      showNotification("Please select or enter a buyer name",
                      type = "error")
      return()
    }

    tryCatch({
      con <- get_db_connection()

      # Add new buyer if needed
      if (nchar(input$new_buyer) > 0) {
        buyer_check <- dbGetQuery(con,
          sprintf("SELECT COUNT(*) as count FROM buyers WHERE buyer_name = '%s'",
                  dbEscapeStrings(con, input$new_buyer)))

        if (buyer_check$count == 0) {
          dbExecute(con,
            sprintf("INSERT INTO buyers (buyer_name) VALUES ('%s')",
                    dbEscapeStrings(con, input$new_buyer)))
        }
      }

      # Insert expense
      insert_query <- sprintf(
        "INSERT INTO expenses (expense_date, vendor, amount, category, buyer, note)
         VALUES ('%s', '%s', %f, '%s', '%s', '%s')",
        format(input$expense_date, "%Y-%m-%d"),
        dbEscapeStrings(con, input$vendor),
        input$amount,
        dbEscapeStrings(con, input$category),
        dbEscapeStrings(con, buyer_to_use),
        dbEscapeStrings(con, input$note)
      )

      dbExecute(con, insert_query)
      dbDisconnect(con)

      showNotification("Expense added successfully!",
                      type = "success")

      # Clear form
      updateTextInput(session, "vendor", value = "")
      updateNumericInput(session, "amount", value = 0)
      updateTextInput(session, "new_buyer", value = "")
      updateTextAreaInput(session, "note", value = "")

      # Refresh buyers list if new buyer was added
      if (nchar(input$new_buyer) > 0) {
        con <- get_db_connection()
        buyers_result <- dbGetQuery(con,
          "SELECT DISTINCT buyer_name FROM buyers ORDER BY buyer_name")
        values$buyers <- buyers_result$buyer_name
        dbDisconnect(con)

        updateSelectInput(session, "buyer", choices = values$buyers,
                         selected = buyer_to_use)
        updateSelectInput(session, "filter_buyer",
                         choices = c("All" = "", values$buyers))
      }

    }, error = function(e) {
      showNotification(paste("Error adding expense:", e$message),
                      type = "error")
    })
  })

  # Clear Form
  observeEvent(input$clear_form_btn, {
    updateDateInput(session, "expense_date", value = Sys.Date())
    updateTextInput(session, "vendor", value = "")
    updateNumericInput(session, "amount", value = 0)
    updateTextInput(session, "new_buyer", value = "")
    updateTextAreaInput(session, "note", value = "")
  })

  # Recent Expenses Table
  output$recent_expenses <- DT::renderDataTable({
    invalidateLater(5000)  # Refresh every 5 seconds

    tryCatch({
      con <- get_db_connection()
      recent_query <- "
        SELECT
          expense_date as Date,
          vendor as Vendor,
          CONCAT('$', FORMAT(amount, 2)) as Amount,
          category as Category,
          buyer as Buyer,
          note as Note
        FROM expenses
        ORDER BY created_at DESC
        LIMIT 10
      "
      recent_data <- dbGetQuery(con, recent_query)
      dbDisconnect(con)

      DT::datatable(recent_data,
                   options = list(
                     pageLength = 5,
                     lengthChange = FALSE,
                     searching = FALSE
                   ))
    }, error = function(e) {
      DT::datatable(data.frame(Message = "Unable to load recent expenses"))
    })
  })

  # All Expenses Table
  output$all_expenses <- DT::renderDataTable({
    input$apply_filters  # Trigger on filter apply

    tryCatch({
      con <- get_db_connection()

      # Build query with filters
      where_clauses <- c()

      if (!is.null(input$date_range)) {
        where_clauses <- c(where_clauses,
          sprintf("expense_date BETWEEN '%s' AND '%s'",
                  format(input$date_range[1], "%Y-%m-%d"),
                  format(input$date_range[2], "%Y-%m-%d")))
      }

      if (!is.null(input$filter_category) && length(input$filter_category) > 0 && input$filter_category[1] != "") {
        categories_list <- paste0("'", input$filter_category, "'", collapse = ", ")
        where_clauses <- c(where_clauses,
          sprintf("category IN (%s)", categories_list))
      }

      if (!is.null(input$filter_buyer) && length(input$filter_buyer) > 0 && input$filter_buyer[1] != "") {
        buyers_list <- paste0("'", input$filter_buyer, "'", collapse = ", ")
        where_clauses <- c(where_clauses,
          sprintf("buyer IN (%s)", buyers_list))
      }

      where_clause <- ifelse(length(where_clauses) > 0,
                            paste("WHERE", paste(where_clauses, collapse = " AND ")),
                            "")

      query <- sprintf("
        SELECT
          id as ID,
          expense_date as Date,
          vendor as Vendor,
          CONCAT('$', FORMAT(amount, 2)) as Amount,
          category as Category,
          buyer as Buyer,
          note as Note,
          created_at as 'Added On'
        FROM expenses
        %s
        ORDER BY expense_date DESC, created_at DESC
      ", where_clause)

      data <- dbGetQuery(con, query)
      dbDisconnect(con)

      DT::datatable(data,
                   options = list(
                     pageLength = 25,
                     scrollX = TRUE,
                     order = list(list(1, 'desc'))
                   ),
                   extensions = 'Buttons',
                   filter = 'top')
    }, error = function(e) {
      DT::datatable(data.frame(Message = paste("Error loading expenses:", e$message)))
    })
  })

  # Generate Report
  observeEvent(input$generate_report, {
    req(input$report_type, input$report_date_range)

    tryCatch({
      con <- get_db_connection()

      start_date <- format(input$report_date_range[1], "%Y-%m-%d")
      end_date <- format(input$report_date_range[2], "%Y-%m-%d")

      # Generate appropriate report based on selection
      if (input$report_type == "monthly") {
        query <- sprintf("
          SELECT
            YEAR(expense_date) as Year,
            MONTHNAME(expense_date) as Month,
            category as Category,
            SUM(amount) as Total,
            COUNT(*) as Transactions
          FROM expenses
          WHERE expense_date BETWEEN '%s' AND '%s'
          GROUP BY YEAR(expense_date), MONTH(expense_date), category
          ORDER BY YEAR(expense_date) DESC, MONTH(expense_date) DESC, SUM(amount) DESC
        ", start_date, end_date)

      } else if (input$report_type == "weekly") {
        query <- sprintf("
          SELECT
            DATE_SUB(expense_date, INTERVAL WEEKDAY(expense_date) DAY) as 'Week Starting',
            category as Category,
            SUM(amount) as Total,
            COUNT(*) as Transactions
          FROM expenses
          WHERE expense_date BETWEEN '%s' AND '%s'
          GROUP BY YEARWEEK(expense_date), category
          ORDER BY YEARWEEK(expense_date) DESC, SUM(amount) DESC
        ", start_date, end_date)

      } else if (input$report_type == "category") {
        query <- sprintf("
          SELECT
            category as Category,
            SUM(amount) as 'Total Spent',
            COUNT(*) as 'Number of Transactions',
            ROUND(AVG(amount), 2) as 'Average Transaction',
            MIN(amount) as 'Smallest',
            MAX(amount) as 'Largest'
          FROM expenses
          WHERE expense_date BETWEEN '%s' AND '%s'
          GROUP BY category
          ORDER BY SUM(amount) DESC
        ", start_date, end_date)

      } else if (input$report_type == "buyer") {
        query <- sprintf("
          SELECT
            buyer as Buyer,
            SUM(amount) as 'Total Spent',
            COUNT(*) as 'Number of Transactions',
            ROUND(AVG(amount), 2) as 'Average Transaction',
            GROUP_CONCAT(DISTINCT category) as 'Categories'
          FROM expenses
          WHERE expense_date BETWEEN '%s' AND '%s'
          GROUP BY buyer
          ORDER BY SUM(amount) DESC
        ", start_date, end_date)
      }

      report_data <- dbGetQuery(con, query)

      # Get summary statistics
      summary_query <- sprintf("
        SELECT
          COUNT(*) as total_transactions,
          SUM(amount) as total_spent,
          AVG(amount) as avg_transaction,
          MIN(amount) as min_amount,
          MAX(amount) as max_amount
        FROM expenses
        WHERE expense_date BETWEEN '%s' AND '%s'
      ", start_date, end_date)

      summary_data <- dbGetQuery(con, summary_query)

      dbDisconnect(con)

      # Update summary statistics
      output$summary_stats <- renderUI({
        fluidRow(
          column(2,
            valueBox(
              value = paste0("$", format(round(summary_data$total_spent, 2),
                                        big.mark = ",")),
              subtitle = "Total Spent",
              color = "green",
              width = 12
            )
          ),
          column(2,
            valueBox(
              value = summary_data$total_transactions,
              subtitle = "Transactions",
              color = "blue",
              width = 12
            )
          ),
          column(2,
            valueBox(
              value = paste0("$", round(summary_data$avg_transaction, 2)),
              subtitle = "Avg Transaction",
              color = "yellow",
              width = 12
            )
          ),
          column(2,
            valueBox(
              value = paste0("$", round(summary_data$min_amount, 2)),
              subtitle = "Smallest",
              color = "aqua",
              width = 12
            )
          ),
          column(2,
            valueBox(
              value = paste0("$", round(summary_data$max_amount, 2)),
              subtitle = "Largest",
              color = "red",
              width = 12
            )
          ),
          column(2,
            valueBox(
              value = as.integer(difftime(input$report_date_range[2],
                                         input$report_date_range[1],
                                         units = "days")),
              subtitle = "Days in Period",
              color = "purple",
              width = 12
            )
          )
        )
      })

      # Create visualization
      output$report_plot <- renderPlotly({
        if (nrow(report_data) > 0) {
          if (input$report_type == "monthly") {
            # Create month order for proper sorting
            report_data$MonthNum <- match(report_data$Month, month.name)
            report_data <- report_data %>%
              arrange(Year, MonthNum)

            p <- plot_ly(report_data,
                        x = ~paste(Month, Year),
                        y = ~Total,
                        color = ~Category,
                        type = 'bar') %>%
              layout(title = "Monthly Expenses by Category",
                    xaxis = list(title = "Month"),
                    yaxis = list(title = "Amount ($)"),
                    barmode = 'stack')

          } else if (input$report_type == "weekly") {
            p <- plot_ly(report_data,
                        x = ~`Week Starting`,
                        y = ~Total,
                        color = ~Category,
                        type = 'bar') %>%
              layout(title = "Weekly Expenses by Category",
                    xaxis = list(title = "Week Starting"),
                    yaxis = list(title = "Amount ($)"),
                    barmode = 'stack')

          } else if (input$report_type == "category") {
            p <- plot_ly(report_data,
                        labels = ~Category,
                        values = ~`Total Spent`,
                        type = 'pie',
                        textinfo = 'label+percent',
                        hovertemplate = '%{label}<br>$%{value:,.2f}<br>%{percent}<extra></extra>') %>%
              layout(title = "Expense Distribution by Category")

          } else if (input$report_type == "buyer") {
            p <- plot_ly(report_data,
                        x = ~Buyer,
                        y = ~`Total Spent`,
                        type = 'bar',
                        marker = list(color = 'rgba(50, 171, 96, 0.7)')) %>%
              layout(title = "Total Spending by Buyer",
                    xaxis = list(title = "Buyer"),
                    yaxis = list(title = "Amount ($)"))
          }

          p
        } else {
          plotly_empty() %>%
            layout(title = "No data available for selected period")
        }
      })

      # Update report table
      output$report_table <- DT::renderDataTable({
        DT::datatable(report_data,
                     options = list(
                       pageLength = 15,
                       scrollX = TRUE
                     ),
                     extensions = 'Buttons') %>%
          formatCurrency(columns = which(names(report_data) %in%
                                        c("Total", "Total Spent", "Average Transaction",
                                          "Smallest", "Largest")),
                        currency = "$")
      })

    }, error = function(e) {
      showNotification(paste("Error generating report:", e$message),
                      type = "error")
    })
  })

  # Add Category
  observeEvent(input$add_category_btn, {
    req(input$new_category)

    tryCatch({
      con <- get_db_connection()

      insert_query <- sprintf(
        "INSERT INTO categories (category_name, description) VALUES ('%s', '%s')
         ON DUPLICATE KEY UPDATE description = VALUES(description)",
        dbEscapeStrings(con, input$new_category),
        dbEscapeStrings(con, input$category_description)
      )

      dbExecute(con, insert_query)

      # Refresh categories
      categories_query <- "SELECT category_name FROM categories ORDER BY category_name"
      values$categories <- dbGetQuery(con, categories_query)$category_name

      dbDisconnect(con)

      updateSelectInput(session, "category", choices = values$categories)
      updateSelectInput(session, "filter_category",
                       choices = c("All" = "", values$categories))
      updateTextInput(session, "new_category", value = "")
      updateTextInput(session, "category_description", value = "")

      showNotification("Category added successfully!",
                      type = "success")

    }, error = function(e) {
      showNotification(paste("Error adding category:", e$message),
                      type = "error")
    })
  })

  # Add Buyer
  observeEvent(input$add_buyer_btn, {
    req(input$new_buyer_setting)

    tryCatch({
      con <- get_db_connection()

      insert_query <- sprintf(
        "INSERT INTO buyers (buyer_name, email) VALUES ('%s', %s)
         ON DUPLICATE KEY UPDATE email = VALUES(email)",
        dbEscapeStrings(con, input$new_buyer_setting),
        ifelse(nchar(input$buyer_email) > 0,
               sprintf("'%s'", dbEscapeStrings(con, input$buyer_email)),
               "NULL")
      )

      dbExecute(con, insert_query)

      # Refresh buyers
      buyers_query <- "SELECT DISTINCT buyer_name FROM buyers ORDER BY buyer_name"
      values$buyers <- dbGetQuery(con, buyers_query)$buyer_name

      dbDisconnect(con)

      updateSelectInput(session, "buyer", choices = values$buyers)
      updateSelectInput(session, "filter_buyer",
                       choices = c("All" = "", values$buyers))
      updateTextInput(session, "new_buyer_setting", value = "")
      updateTextInput(session, "buyer_email", value = "")

      showNotification("Buyer added successfully!",
                      type = "success")

    }, error = function(e) {
      showNotification(paste("Error adding buyer:", e$message),
                      type = "error")
    })
  })

  # Categories Table
  output$categories_table <- DT::renderDataTable({
    invalidateLater(10000)  # Refresh every 10 seconds

    tryCatch({
      con <- get_db_connection()
      categories_data <- dbGetQuery(con,
        "SELECT category_name as Category, description as Description
         FROM categories ORDER BY category_name")
      dbDisconnect(con)

      DT::datatable(categories_data,
                   options = list(
                     pageLength = 5,
                     lengthChange = FALSE
                   ))
    }, error = function(e) {
      DT::datatable(data.frame(Message = "Unable to load categories"))
    })
  })

  # Buyers Table
  output$buyers_table <- DT::renderDataTable({
    invalidateLater(10000)  # Refresh every 10 seconds

    tryCatch({
      con <- get_db_connection()
      buyers_data <- dbGetQuery(con,
        "SELECT buyer_name as Buyer, email as Email
         FROM buyers ORDER BY buyer_name")
      dbDisconnect(con)

      DT::datatable(buyers_data,
                   options = list(
                     pageLength = 5,
                     lengthChange = FALSE
                   ))
    }, error = function(e) {
      DT::datatable(data.frame(Message = "Unable to load buyers"))
    })
  })

  # Database Status
  output$db_status <- renderPrint({
    if (values$db_connected) {
      tryCatch({
        con <- get_db_connection()

        # Get some statistics
        stats <- dbGetQuery(con, "
          SELECT
            (SELECT COUNT(*) FROM expenses) as total_expenses,
            (SELECT COUNT(*) FROM categories) as total_categories,
            (SELECT COUNT(*) FROM buyers) as total_buyers,
            (SELECT MAX(created_at) FROM expenses) as last_expense
        ")

        dbDisconnect(con)

        cat("Database Status: CONNECTED\n")
        cat("=====================================\n")
        cat("Host:", db_config$host, "\n")
        cat("Database:", db_config$dbname, "\n")
        cat("=====================================\n")
        cat("Total Expenses:", stats$total_expenses, "\n")
        cat("Total Categories:", stats$total_categories, "\n")
        cat("Total Buyers:", stats$total_buyers, "\n")
        cat("Last Expense Added:", as.character(stats$last_expense), "\n")

      }, error = function(e) {
        cat("Database Status: ERROR\n")
        cat("Error:", e$message, "\n")
      })
    } else {
      cat("Database Status: NOT CONNECTED\n")
      cat("Please check your database configuration\n")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

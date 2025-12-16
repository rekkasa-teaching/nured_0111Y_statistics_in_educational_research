library(shiny)
library(bslib)
library(plotly)

# UI
ui <- page_sidebar(
  title = "Υπολογισμός Εκατοστημορίων Κατανομών",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C3E50",
    base_font = font_google("Roboto")
  ),
  
  sidebar = sidebar(
    width = 350,
    
    # Distribution selector
    radioButtons(
      "distribution",
      "Επιλέξτε Κατανομή:",
      choices = c("Τυπική Κανονική (Z)" = "normal",
                  "Student's t" = "t"),
      selected = "normal"
    ),
    
    hr(),
    
    # Numeric input for percentile
    numericInput(
      "percentile",
      "Εκατοστημόριο (%):",
      value = 95,
      min = 0.01,
      max = 99.99,
      step = 0.01
    ),
    
    # Conditional input: degrees of freedom for t-distribution
    conditionalPanel(
      condition = "input.distribution == 't'",
      hr(),
      numericInput(
        "df",
        "Βαθμοί Ελευθερίας:",
        value = 10,
        min = 1,
        max = 1000,
        step = 1
      )
    ),
    
    hr(),
    
    actionButton(
      "calculate",
      "Υπολογισμός",
      class = "btn-primary btn-lg w-100",
      icon = icon("calculator")
    )
  ),
  
  # Main panel
  card(
    card_header(
      class = "bg-primary text-white",
      "Αποτελέσματα"
    ),
    card_body(
      uiOutput("result_box"),
      hr(),
      plotlyOutput("dist_plot", height = "450px")
    )
  ),
  
  # Information card
  card(
    card_header("Πληροφορίες"),
    card_body(
      tags$p(
        tags$strong("Τυπική Κανονική Κατανομή (Z):"),
        "Η τυπική κανονική κατανομή έχει μέση τιμή 0 και τυπική απόκλιση 1."
      ),
      tags$p(
        tags$strong("Κατανομή Student's t:"),
        "Η κατανομή t είναι παρόμοια με την κανονική κατανομή αλλά έχει πιο βαριές ουρές. 
        Καθώς οι βαθμοί ελευθερίας αυξάνονται, προσεγγίζει την τυπική κανονική κατανομή."
      ),
      tags$p(
        tags$em("Το εκατοστημόριο αναπαριστά την τιμή κάτω από την οποία βρίσκεται ένα δεδομένο ποσοστό των παρατηρήσεων.")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Calculate percentile value
  result <- eventReactive(input$calculate, {
    p <- input$percentile / 100
    
    if (input$distribution == "normal") {
      value <- qnorm(p)
      dist_name <- "Τυπική Κανονική"
      param_text <- ""
    } else {
      df <- input$df
      value <- qt(p, df = df)
      dist_name <- "Student's t"
      param_text <- paste0(" (β.ε. = ", df, ")")
    }
    
    list(
      value = value,
      percentile = input$percentile,
      distribution = dist_name,
      param_text = param_text,
      p = p,
      df = if(input$distribution == "t") input$df else NULL
    )
  })
  
  # Display result
  output$result_box <- renderUI({
    req(result())
    r <- result()
    
    value_box(
      title = paste0(r$percentile, "ο Εκατοστημόριο"),
      value = tags$h2(
        style = "color: #2C3E50; font-weight: bold;",
        sprintf("%.4f", r$value)
      ),
      showcase = icon("chart-line"),
      theme = "primary",
      p(
        paste0("Κατανομή: ", r$distribution, r$param_text)
      ),
      p(
        tags$em(
          paste0("P(X ≤ ", sprintf("%.4f", r$value), ") = ", r$percentile, "%")
        )
      )
    )
  })
  
  # Plot distribution
  output$dist_plot <- renderPlotly({
    req(result())
    r <- result()
    
    # Generate x values - always use -4 to 4 range
    x <- seq(-4, 4, length.out = 1000)
    
    # Calculate y values based on distribution
    if (r$distribution == "Τυπική Κανονική") {
      y <- dnorm(x)
    } else {
      y <- dt(x, df = r$df)
    }
    
    # Determine shading: always shade the smaller tail
    if (r$value < 0) {
      # Shade the left tail (below the quantile)
      fill_idx <- which(x <= r$value)
      fill_x <- c(x[fill_idx], r$value, min(x))
      fill_y <- c(y[fill_idx], 0, 0)
      shade_pct <- r$percentile
      label_x <- min(x) + (r$value - min(x)) / 2
      shade_text <- sprintf("%.2f%%<br>(κάτω από %.3f)", shade_pct, r$value)
      # Position line text to the left
      text_x <- r$value - 0.2
    } else {
      # Shade the right tail (above the quantile)
      fill_idx <- which(x >= r$value)
      fill_x <- c(r$value, x[fill_idx], max(x))
      fill_y <- c(0, y[fill_idx], 0)
      shade_pct <- 100 - r$percentile
      label_x <- r$value + (max(x) - r$value) / 2
      shade_text <- sprintf("%.2f%%<br>(πάνω από %.3f)", shade_pct, r$value)
      # Position line text to the right
      text_x <- r$value + 0.2
    }
    
    # Create plotly figure
    fig <- plot_ly()
    
    # Add shaded area
    fig <- fig %>%
      add_trace(
        x = fill_x,
        y = fill_y,
        type = "scatter",
        mode = "lines",
        fill = "tozeroy",
        fillcolor = "rgba(93, 173, 226, 0.7)",
        line = list(width = 0),
        showlegend = FALSE,
        hoverinfo = "skip"
      )
    
    # Add main distribution curve
    fig <- fig %>%
      add_trace(
        x = x,
        y = y,
        type = "scatter",
        mode = "lines",
        line = list(color = "#2C3E50", width = 3),
        name = "Πυκνότητα",
        hovertemplate = "Τιμή: %{x:.3f}<br>Πυκνότητα: %{y:.4f}<extra></extra>"
      )
    
    # Add vertical line at critical value
    fig <- fig %>%
      add_trace(
        x = c(r$value, r$value),
        y = c(0, max(y)),
        type = "scatter",
        mode = "lines",
        line = list(color = "#e74c3c", width = 3, dash = "dash"),
        showlegend = FALSE,
        hoverinfo = "skip"
      )
    
    # Add annotation for critical value
    fig <- fig %>%
      add_annotations(
        x = text_x,
        y = max(y) * 0.95,
        text = sprintf("<b>%.3f</b>", r$value),
        showarrow = FALSE,
        font = list(size = 22, color = "#e74c3c", family = "Arial"),
        xanchor = "center",
        yanchor = "top"
      )
    
    # Add annotation for shaded percentage
    fig <- fig %>%
      add_annotations(
        x = label_x,
        y = max(y) * 0.5,
        text = shade_text,
        showarrow = FALSE,
        font = list(size = 20, color = "#FFFFFF", family = "Arial"),
        xanchor = "center",
        yanchor = "middle"
      )
    
    # Layout configuration
    fig <- fig %>%
      layout(
        title = list(
          text = paste0("<b>Κατανομή ", r$distribution, r$param_text, "</b><br>",
                       "<sup>Εκατοστημόριο: ", r$percentile, "%</sup>"),
          font = list(size = 20, family = "Arial")
        ),
        xaxis = list(
          title = list(text = "<b>Τιμή</b>", font = list(size = 16, family = "Arial")),
          tickfont = list(size = 14),
          gridcolor = "#e0e0e0",
          zeroline = TRUE,
          range = c(-4, 4)
        ),
        yaxis = list(
          title = list(text = "<b>Πυκνότητα</b>", font = list(size = 16, family = "Arial")),
          tickfont = list(size = 14),
          gridcolor = "#e0e0e0",
          zeroline = TRUE
        ),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        hovermode = "closest",
        font = list(family = "Arial", size = 14)
      ) %>%
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("pan2d", "lasso2d", "select2d", "autoScale2d")
      )
    
    fig
  })
}

# Run app
shinyApp(ui = ui, server = server)

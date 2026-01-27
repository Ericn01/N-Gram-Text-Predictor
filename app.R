# ============================================================================
# N-Gram Word Prediction Application
# ============================================================================

library(shiny)
library(data.table)
library(stringr)
library(fst)
library(bslib)
library(bsicons)

# ============================================================================
# LOAD PRE-TRAINED MODELS
# ============================================================================

MODEL_DIR <- "model_data"

load_models <- function() {
  cat("Loading models...\n")
  mods <- list(
    "5" = read_fst(file.path(MODEL_DIR, "ngram_5.fst"), as.data.table = TRUE),
    "4" = read_fst(file.path(MODEL_DIR, "ngram_4.fst"), as.data.table = TRUE),
    "3" = read_fst(file.path(MODEL_DIR, "ngram_3.fst"), as.data.table = TRUE),
    "2" = read_fst(file.path(MODEL_DIR, "ngram_2.fst"), as.data.table = TRUE),
    "1" = read_fst(file.path(MODEL_DIR, "ngram_1.fst"), as.data.table = TRUE)
  )

  for (i in 2:5) {
    setkey(mods[[as.character(i)]], prefix)
  }
  
  return(mods)
}

models <- load_models()

# ============================================================================
# PREDICTION FUNCTIONS
# ============================================================================

predict_word_modular <- function(input_text, models, n_results = 3) {
  # 1. Clean and tokenize input
  clean_text <- tolower(input_text) %>% 
    str_replace_all("[^a-z' ]", "") %>% 
    str_trim() %>% 
    str_split("\\s+") %>% 
    unlist()
  
  # Handle empty input
  if (length(clean_text) == 0 || input_text == "") {
    return(list(
      res = head(models[["1"]], n_results), 
      method = "Top Unigrams"
    ))
  }
  
  # 2. Iterative Backoff (Starts at 5-gram)
  # i=5 looks for 4 previous words, i=2 looks for 1 previous word
  for (i in 5:2) {
    if (length(clean_text) >= (i - 1)) {
      current_prefix <- paste(tail(clean_text, i - 1), collapse = " ")
      
      # Binary search lookup 
      matches <- models[[as.character(i)]][prefix == current_prefix]
      
      if (nrow(matches) > 0) {
        return(list(
          res = head(matches[order(-prob)], n_results), 
          method = paste0(i, "-gram Model")
        ))
      }
    }
  }
  
  # 3. Final Fallback to Unigram
  # We use the most frequent words in the English language
  return(list(
    res = head(models[["1"]], n_results), 
    method = "Unigram Fallback"
  ))
}

# ============================================================================
# UI DEFINITION
# ============================================================================

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  tags$head(
    tags$style(HTML("
      .container-fluid {
        display: flex;
        align-items: center;
        justify-content: center;
        min-height: 100vh;
        padding: 20px;
      }
      body { 
        background-color: #f0f7f4; 
        margin: 0;
      }
      .main-card { 
        max-width: 600px; 
        width: 95%; 
        padding: 40px; 
        background: #ffffff; 
        border-radius: 30px; 
        box-shadow: 0 20px 40px rgba(120, 150, 140, 0.15);
        border: 1px solid #e0eee8;
      }
      .prediction-btn {
        background: #f8fffb;
        border: 2px solid #d1eadd;
        color: #5a7d6c;
        padding: 8px 18px;
        border-radius: 12px;
        font-weight: 600;
        margin: 4px;
        transition: all 0.2s;
      }
      .prediction-btn:hover {
        background: #78c2ad;
        color: white;
        transform: translateY(-2px);
      }
      .stats-container {
        margin-top: 20px;
        padding-top: 20px;
        border-top: 1px solid #eee;
      }
      .bar-wrapper {
        margin-bottom: 10px;
        text-align: left;
      }
      .bar-label {
        font-size: 0.85rem;
        color: #4a6358;
        display: flex;
        justify-content: space-between;
        margin-bottom: 3px;
      }
      .bar-bg {
        background: #e9f2ee;
        height: 8px;
        border-radius: 4px;
        overflow: hidden;
      }
      .bar-fill {
        background: #78c2ad;
        height: 100%;
        transition: width 0.6s ease-out;
      }
      
      .toggle-link {
        color: #a3b8ae;
        text-decoration: none;
        font-size: 0.9rem;
        cursor: pointer;
        display: inline-flex;
        align-items: center;
        gap: 5px;
        margin-top: 15px;
      }
      
      .toggle-link:hover { color: #78c2ad; }
      textarea { border-radius: 15px !important; resize: none; }
      
      .shiny-output-error-validation {
        color: #a3b8ae;
        font-style: italic;
        margin: 20px 0;
      }
    "))
  ),
  
  div(class = "main-card",
      div(style = "text-align: center; margin-bottom: 25px;",
          h2("NextWord AI", style = "font-weight: 800; color: #4a6358;"),
          p("Intelligent N-Gram Suggestions", style = "color: #8da99c;")
      ),
      
      textAreaInput("user_input", label = NULL, 
                    placeholder = "Start typing something...", 
                    rows = 3, width = "100%"),
      
      div(style = "text-align: center; margin-top: 20px;",
          uiOutput("prediction_buttons")
      ),
      
      div(style = "text-align: center;",
          uiOutput("stats_toggle_ui"),
          conditionalPanel(
            condition = "output.show_stats_condition == true",
            div(class = "stats-container",
                uiOutput("confidence_bars"),
                div(style = "margin-top: 15px;", uiOutput("method_info"))
            )
          )
      )
  )
)
# ============================================================================
# SERVER LOGIC
# ============================================================================
server <- function(input, output, session) {
  
  show_stats <- reactiveVal(FALSE)
  observeEvent(input$toggle_stats, { show_stats(!show_stats()) })
  
  prediction_data <- reactive({
    req(input$user_input)
    
    # 1. Clean Input
    clean_text <- tolower(input$user_input) %>% 
      str_replace_all("[^a-z' ]", "") %>% 
      str_trim() %>% 
      str_split("\\s+") %>% 
      unlist()
    
    if(length(clean_text) == 0 || (length(clean_text) == 1 && clean_text[1] == "")) return(NULL)
    
    result <- NULL
    method_used <- ""
    
    # 2. Back-off Logic (Tiers 5 down to 2)
    for (i in 5:2) {
      if (length(clean_text) >= (i - 1)) {
        current_prefix <- paste(tail(clean_text, i - 1), collapse = " ")
        matches <- models[[as.character(i)]][prefix == current_prefix]
        
        if (!is.null(matches) && nrow(matches) > 0) {
          # Sort and take top 5
          result <- head(matches[order(-prob)], 5)
          method_used <- paste0("Context: ", i, "-gram model")
          break
        }
      }
    }
    
    # 3. Final Fallback to Unigrams (Tier 1)
    if (is.null(result)) {
      # Use the unigram model, which has columns [phrase, freq]
      # We convert it to match the schema of the higher models [prediction, prob]
      unigrams <- models[["1"]]
      
      # Standardize column names and calculate a mock probability for the chart
      result <- head(unigrams[order(-freq)], 5) %>%
        as.data.table() %>%
        setnames(old = "phrase", new = "prediction", skip_absent = TRUE)
      
      # Calculate probability if it doesn't exist
      if(!"prob" %in% names(result)) {
        total_freq <- sum(models[["1"]]$freq)
        result[, prob := freq / total_freq]
      }
      
      method_used <- "Context: General vocabulary (No prefix match)"
    }
    
    # Final check to ensure we have data
    if(is.null(result) || nrow(result) == 0) return(NULL)
    
    list(res = result, method = method_used)
  })
  
  # Validation for conditional panel visibility
  output$show_stats_condition <- reactive({
    !is.null(prediction_data()) && show_stats()
  })
  outputOptions(output, "show_stats_condition", suspendWhenHidden = FALSE)
  
  output$prediction_buttons <- renderUI({
    # Defensive programming: If input is gibberish and unigrams fail
    validate(need(prediction_data(), "No suggestions available for this input."))
    
    data <- prediction_data()
    tagList(
      lapply(1:nrow(data$res), function(i) {
        word <- data$res$prediction[i]
        actionButton(
          inputId = paste0("btn_", i), 
          label = word,
          class = "prediction-btn",
          onclick = sprintf("Shiny.setInputValue('insert_word', '%s', {priority: 'event'});", word)
        )
      })
    )
  })
  
  output$confidence_bars <- renderUI({
    data <- prediction_data()
    if (is.null(data$res)) return(NULL)
    
    df <- data$res
    max_p <- max(df$prob, na.rm = TRUE)
    
    tagList(
      lapply(1:nrow(df), function(i) {
        pct <- round(df$prob[i] * 100, 1)
        # Normalize visually so the best match is always a decent length
        vis_width <- if(max_p > 0) (df$prob[i] / max_p) * 100 else 0
        
        div(class = "bar-wrapper",
            div(class = "bar-label", 
                span(strong(df$prediction[i])), 
                span(paste0(pct, "%"))
            ),
            div(class = "bar-bg", 
                div(class = "bar-fill", style = sprintf("width: %f%%;", vis_width))
            )
        )
      })
    )
  })
  
  output$stats_toggle_ui <- renderUI({
    req(input$user_input)
    icon_name <- if (show_stats()) "chevron-up" else "chevron-down"
    actionLink("toggle_stats", 
               label = span(bs_icon(icon_name), if(show_stats()) "Hide Stats" else "Show Prediction Confidence"), 
               class = "toggle-link")
  })
  
  output$method_info <- renderUI({
    req(prediction_data())
    div(style = "font-size: 0.75rem; color: #a3b8ae; font-style: italic;", 
        prediction_data()$method)
  })
  
  observeEvent(input$insert_word, {
    new_text <- paste0(str_trim(input$user_input), " ", input$insert_word, " ")
    updateTextAreaInput(session, "user_input", value = new_text)
  })
}

shinyApp(ui, server)
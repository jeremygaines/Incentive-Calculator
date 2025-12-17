server <- function(input, output, session) {
  # Storage for saved archetypes
  saved_archetypes <- reactiveValues(
    data = list(),
    # Will store list of archetype results
    push_funding_visible = FALSE  # Track visibility of push funding section
  )
  
  output$revenue_start_stage_selector <- renderUI({
    df <- DF()
    choices <- setNames(1:nrow(df),
                        paste("After Stage", 1:nrow(df), "-", df$`Stage Name`))
    selectInput(
      "revenue_start_stage",
      "Revenue begins after stage:",
      choices = choices,
      selected = nrow(df)
    )
  })
  
  # Real-time milestone share warning
  output$milestone_share_warning <- renderUI({
    # Only check when milestones+AMC is selected
    if (is.null(input$mechanisms) ||
        input$mechanisms != "milestones_amc") {
      return(NULL)
    }
    
    df <- hot_to_r(input$stages_table)
    if (is.null(df) || !("Milestone?" %in% names(df))) {
      return(NULL)
    }
    
    milestone_stages <- df %>% filter(`Milestone?` == TRUE)
    
    if (nrow(milestone_stages) == 0) {
      return(NULL)
    }
    
    warnings <- list()
    errors <- list()
    
    # Check for milestones with zero stage share
    zero_share_stages <- milestone_stages %>%
      filter(is.na(`Stage Share (%)`) | `Stage Share (%)` == 0)
    
    if (nrow(zero_share_stages) > 0) {
      stage_names <- paste(
        sprintf(
          "Stage %d (%s)",
          zero_share_stages$Stage,
          zero_share_stages$`Stage Name`
        ),
        collapse = ", "
      )
      errors <- c(errors, list(
        sprintf(
          "Milestone selected but Stage Share is 0%% for: %s. Please assign a percentage or uncheck the milestone.",
          stage_names
        )
      ))
    }
    
    # Check if total exceeds 100%
    total_pct <- sum(milestone_stages$`Stage Share (%)`, na.rm = TRUE)
    
    if (total_pct > 100) {
      errors <- c(errors, list(
        sprintf(
          "Milestone percentages sum to %.1f%%, which exceeds 100%%. Please reduce the Stage Share values.",
          total_pct
        )
      ))
    }
    
    # Info message when exactly 100% (no AMC component)
    if (total_pct == 100 && length(errors) == 0) {
      warnings <- c(
        warnings,
        list(
          "Milestone percentages sum to exactly 100%. The entire incentive will be paid as milestones (no AMC component)."
        )
      )
    }
    
    # Render errors (red)
    error_ui <- NULL
    if (length(errors) > 0) {
      error_ui <- div(
        style = "margin-top: 10px; margin-bottom: 10px; padding: 12px 16px;
               background: linear-gradient(135deg, rgba(239, 68, 68, 0.12), rgba(220, 38, 38, 0.08));
               border-left: 4px solid #ef4444;
               border-radius: 8px;",
        if (length(errors) == 1) {
          p(style = "margin: 0; color: #991b1b; font-size: 13px; font-weight: 600; line-height: 1.5;", HTML(paste0("⚠️ ", errors[[1]])))
        } else {
          tagList(
            p(style = "margin: 0 0 8px 0; color: #991b1b; font-size: 13px; font-weight: 600;", "⚠️ Milestone configuration issues:"),
            tags$ul(style = "margin: 0; padding-left: 20px; color: #7f1d1d; font-size: 12px;", lapply(errors, function(e)
              tags$li(e)))
          )
        }
      )
    }
    
    # Render warnings (yellow)
    warning_ui <- NULL
    if (length(warnings) > 0 && length(errors) == 0) {
      warning_ui <- div(
        style = "margin-top: 10px; margin-bottom: 10px; padding: 12px 16px;
               background: linear-gradient(135deg, rgba(251, 191, 36, 0.12), rgba(245, 158, 11, 0.08));
               border-left: 4px solid #f59e0b;
               border-radius: 8px;",
        p(style = "margin: 0; color: #92400e; font-size: 13px; font-weight: 500; line-height: 1.5;", HTML(paste0(
          "ℹ️ ", warnings[[1]]
        )))
      )
    }
    
    tagList(error_ui, warning_ui)
  })
  # Real-time stage data validation warning
  output$stage_data_warning <- renderUI({
    df <- hot_to_r(input$stages_table)
    
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    # Define required columns and their display names
    required_cols <- c(
      "Duration" = "Duration",
      "Cost" = "Cost",
      "Probability (%)" = "Probability (%)",
      "Share of costs covered by other sources (%)" = "Share of costs covered"
    )
    
    # Check for missing values in each required column
    missing_info <- list()
    
    for (col_name in names(required_cols)) {
      if (col_name %in% names(df)) {
        missing_rows <- which(is.na(df[[col_name]]) | df[[col_name]] == "")
        if (length(missing_rows) > 0) {
          missing_info[[required_cols[col_name]]] <- missing_rows
        }
      }
    }
    
    # Check Stage Name separately (empty string is also invalid)
    if ("Stage Name" %in% names(df)) {
      missing_names <- which(is.na(df$`Stage Name`) |
                               trimws(df$`Stage Name`) == "")
      if (length(missing_names) > 0) {
        missing_info[["Stage Name"]] <- missing_names
      }
    }
    
    # Check for negative values in numeric columns
    
    negative_info <- list()
    numeric_cols <- c("Duration", "Cost", "Probability (%)")
    
    for (col_name in numeric_cols) {
      if (col_name %in% names(df)) {
        negative_rows <- which(!is.na(df[[col_name]]) & df[[col_name]] < 0)
        if (length(negative_rows) > 0) {
          negative_info[[col_name]] <- negative_rows
        }
      }
    }
    
    # Check for probability > 100
    
    prob_over_100 <- NULL
    if ("Probability (%)" %in% names(df)) {
      over_rows <- which(!is.na(df$`Probability (%)`) &
                           df$`Probability (%)` > 100)
      if (length(over_rows) > 0) {
        prob_over_100 <- over_rows
      }
    }
    
    # Build warning messages
    warnings <- list()
    
    if (length(missing_info) > 0) {
      for (col in names(missing_info)) {
        rows <- missing_info[[col]]
        warnings <- c(warnings, list(
          sprintf(
            "Missing %s in stage%s %s",
            col,
            if (length(rows) > 1)
              "s"
            else
              "",
            paste(rows, collapse = ", ")
          )
        ))
      }
    }
    
    if (length(negative_info) > 0) {
      for (col in names(negative_info)) {
        rows <- negative_info[[col]]
        warnings <- c(warnings, list(
          sprintf(
            "Negative %s in stage%s %s (must be ≥ 0)",
            col,
            if (length(rows) > 1)
              "s"
            else
              "",
            paste(rows, collapse = ", ")
          )
        ))
      }
    }
    
    if (!is.null(prob_over_100)) {
      warnings <- c(warnings, list(
        sprintf(
          "Probability exceeds 100%% in stage%s %s",
          if (length(prob_over_100) > 1)
            "s"
          else
            "",
          paste(prob_over_100, collapse = ", ")
        )
      ))
    }
    
    # Return warning UI if there are issues
    if (length(warnings) > 0) {
      div(
        style = "margin-top: 10px; margin-bottom: 10px; padding: 12px 16px;
               background: linear-gradient(135deg, rgba(239, 68, 68, 0.12), rgba(220, 38, 38, 0.08));
               border-left: 4px solid #ef4444;
               border-radius: 8px;",
        p(style = "margin: 0 0 8px 0; color: #991b1b; font-size: 13px; font-weight: 600;", "⚠️ Stage data issues:"),
        tags$ul(style = "margin: 0; padding-left: 20px; color: #7f1d1d; font-size: 12px;", lapply(warnings, function(w)
          tags$li(w)))
      )
    }
  })
  
  # WARNINGS AND ERRORS --------------------------------------------------------
  # Update the existing validation_status reactive
  validation_status <- reactive({
    errors <- list()
    warnings <- list()
    
    
    df <- hot_to_r(input$stages_table)
    if (!is.null(df) && nrow(df) > 0) {
      required_cols <- c(
        "Duration",
        "Cost",
        "Probability (%)",
        "Share of costs covered by other sources (%)"
      )
      
      for (col_name in required_cols) {
        if (col_name %in% names(df)) {
          if (any(is.na(df[[col_name]]))) {
            errors <- c(errors, list(
              sprintf(
                "Missing values in '%s' column. Please fill in all required fields.",
                col_name
              )
            ))
            break  # One error message is enough
          }
        }
      }
      
      # Check for negative values
      if (any(!is.na(df$Duration) & df$Duration < 0) ||
          any(!is.na(df$Cost) & df$Cost < 0) ||
          any(!is.na(df$`Probability (%)`) &
              df$`Probability (%)` < 0)) {
        errors <- c(
          errors,
          list(
            "Stage data contains negative values. Duration, Cost, and Probability must be ≥ 0."
          )
        )
      }
      
      # Check probability bounds
      if (any(!is.na(df$`Probability (%)`) &
              df$`Probability (%)` > 100)) {
        errors <- c(errors, list("Probability cannot exceed 100%."))
      }
    }
    
    # Check if milestones are configured properly when using Milestones+AMC
    if (!is.null(input$mechanisms) &&
        input$mechanisms == "milestones_amc") {
      df <- hot_to_r(input$stages_table)
      if (!is.null(df) && "Milestone?" %in% names(df)) {
        milestone_stages <- df %>% filter(`Milestone?` == TRUE)
        
        # ERROR: Milestone percentages exceed 100%
        if (nrow(milestone_stages) > 0) {
          total_pct <- sum(milestone_stages$`Stage Share (%)`, na.rm = TRUE)
          if (total_pct > 100) {
            errors <- c(errors, list(
              sprintf(
                "Milestone percentages sum to %.1f%%, which exceeds 100%%. Please adjust.",
                total_pct
              )
            ))
          }
          
          # ERROR: Milestone with zero stage share
          zero_share <- milestone_stages %>%
            filter(is.na(`Stage Share (%)`) |
                     `Stage Share (%)` == 0)
          
          if (nrow(zero_share) > 0) {
            errors <- c(
              errors,
              list(
                "One or more milestones have 0% Stage Share. Please assign a percentage or uncheck the milestone."
              )
            )
          }
        }
      }
    }
    
    # Check if target probability exceeds eta when feasibility is enabled
    if (!is.null(input$use_feasibility) && input$use_feasibility) {
      if (!is.null(input$target_probability) && !is.null(input$eta)) {
        if (input$target_probability >= input$eta) {
          errors <- c(errors, list(
            sprintf(
              "Target probability (%.0f%%) cannot exceed or equal global possibility parameter (%.0f%%). Please adjust your parameters.",
              input$target_probability ,
              input$eta
            )
          ))
        }
      }
    }
    
    # Check mechanism selection
    if (is.null(input$mechanisms) ||
        length(input$mechanisms) == 0) {
      warnings <- c(warnings,
                    list("Please select an incentive mechanism before calculating."))
    }
    
    # Check if milestones are configured properly when using Milestones+AMC
    if (!is.null(input$mechanisms) &&
        input$mechanisms == "milestones_amc") {
      df <- hot_to_r(input$stages_table)
      if (!is.null(df) && "Milestone?" %in% names(df)) {
        milestone_stages <- df %>% filter(`Milestone?` == TRUE)
        
        # ERROR: No milestones selected
        if (nrow(milestone_stages) == 0) {
          errors <- c(
            errors,
            list(
              "Milestones + AMC requires at least one stage marked as a milestone. Please check the 'Milestone?' box for at least one stage."
            )
          )
        }
        
        # ERROR: Milestone percentages exceed 100%
        if (nrow(milestone_stages) > 0) {
          total_pct <- sum(milestone_stages$`Stage Share (%)`, na.rm = TRUE)
          if (total_pct > 100) {
            errors <- c(errors, list(
              sprintf(
                "Milestone percentages sum to %.1f%%, which exceeds 100%%. Please adjust.",
                total_pct
              )
            ))
          }
        }
      }
    }
    
    # Validate all numeric inputs are actually numbers
    
    if (!is.null(input$amc_calculation_mode) &&
        input$amc_calculation_mode == "topup_to_units" &&
        (input$mechanisms == "amc" ||
         input$mechanisms == "milestones_amc")) {
      feasibility <- amc_feasibility_check()
      if (!is.null(feasibility) && !feasibility$feasible) {
        errors <- c(errors, list(
          sprintf(
            "AMC top-up insufficient. Minimum needed: $%s/unit. Please increase the top-up amount, lifecycle duration, or monthly units.",
            format(
              round(feasibility$min_topup, 2),
              big.mark = ",",
              scientific = FALSE
            )
          )
        ))
      }
    }
    
    numeric_inputs <- list(
      list(val = input$target_probability, name = "Target probability"),
      list(val = input$discount_rate, name = "Discount rate"),
      list(val = input$profit_per_unit, name = "Profit per unit"),
      list(val = input$revenue_lifecycle, name = "Revenue lifecycle"),
      list(val = input$amc_topup, name = "AMC top-up"),
      list(val = input$amc_lifecycle, name = "AMC lifecycle")
    )
    
    for (inp in numeric_inputs) {
      if (!is.null(inp$val) && (is.na(inp$val) || !is.numeric(inp$val))) {
        errors <- c(errors, list(sprintf(
          "%s must be a valid number", inp$name
        )))
      }
    }
    
    # Check units validation
    units_check <- amc_units_validation()
    if (!is.null(units_check) && !units_check$valid) {
      errors <- c(errors, list(units_check$error))
    }
    
    
    list(
      errors = errors,
      warnings = warnings,
      has_errors = length(errors) > 0
    )
  })
  
  # Update the validation warnings output
  output$validation_warnings <- renderUI({
    val <- validation_status()
    
    # Disable/enable calculate button based on errors
    if (val$has_errors) {
      shinyjs::disable("calculate")
    } else {
      shinyjs::enable("calculate")
    }
    
    if (length(val$errors) == 0 && length(val$warnings) == 0) {
      return(NULL)
    }
    
    tagList(
      # Errors (blocking)
      if (length(val$errors) > 0) {
        lapply(val$errors, function(err) {
          div(
            class = "warning-box",
            style = "background: linear-gradient(135deg, rgba(239, 68, 68, 0.15), rgba(220, 38, 38, 0.1)); border-left: 4px solid #ef4444;",
            p(
              class = "warning-text",
              style = "color: #991b1b; margin: 0;",
              tags$strong("⚠️ Error: "),
              err
            )
          )
        })
      },
      
      # Warnings (non-blocking)
      if (length(val$warnings) > 0) {
        lapply(val$warnings, function(warn) {
          div(
            class = "warning-box",
            style = "background: linear-gradient(135deg, rgba(251, 191, 36, 0.15), rgba(245, 158, 11, 0.1)); border-left: 4px solid #f59e0b;",
            p(
              class = "warning-text",
              style = "color: #92400e; margin: 0;",
              tags$strong("⚠️ Warning: "),
              warn
            )
          )
        })
      }
    )
  })
  
  # Toggle push funding visibility
  observeEvent(input$toggle_push_funding, {
    saved_archetypes$push_funding_visible <- !saved_archetypes$push_funding_visible
    
    # Update button icon
    if (saved_archetypes$push_funding_visible) {
      updateActionButton(session, "toggle_push_funding", icon = icon("chevron-up"))
    } else {
      updateActionButton(session, "toggle_push_funding", icon = icon("chevron-down"))
    }
  })
  
  # Render push funding content
  output$push_funding_content <- renderUI({
    if (saved_archetypes$push_funding_visible) {
      results <- calculated_results()
      unit_label <- get_unit_label()
      
      div(
        style = "margin-top: 15px; padding: 20px 24px;
             background: linear-gradient(135deg, rgba(102, 34, 96, 0.05), rgba(102, 34, 96, 0.05));
             border-radius: 12px; border-left: 4px solid #662260;
             font-family: 'Proxima Nova', sans-serif; line-height: 1.6;
             animation: slideDown 0.3s ease-out;",
        
        p(
          style = "margin: 0; font-size: 15px; color: #2d3748; font-weight: 500;",
          sprintf(
            "If grants or other push funding were provided to each firm at every stage, the total cost would be $%s%s. There is a %.1f%% chance that none of the %d firms would succeed, meaning all payments would be wasted.",
            format(round(results$total_milestone_cost, 1), big.mark = ","),
            substr(unit_label, 2, 2),
            (100 - results$global_pos * 100),
            results$n_firms
          )
        )
      )
    }
  })
  
  # Helper function to get unit label
  get_unit_label <- reactive({
    switch(
      input$cost_unit,
      "M" = "$M",
      "K" = "$K",
      "B" = "$B",
      "1" = "$"
    )
  })
  
  # Helper function to get unit name
  get_unit_name <- reactive({
    switch(
      input$cost_unit,
      "M" = "millions of dollars",
      "K" = "thousands of dollars",
      "B" = "billions of dollars",
      "1" = "dollars"
    )
  })
  
  # Reactive adoption curve generator - use revenue params if model_revenue is enabled, otherwise AMC params
  adoption_curve <- reactive({
    # Use revenue parameters if enabled, otherwise AMC parameters
    if (!is.null(input$model_revenue) && input$model_revenue) {
      T <- ifelse(is.null(input$revenue_lifecycle),
                  10,
                  input$revenue_lifecycle)
      uptake_model <- ifelse(is.null(input$revenue_uptake_model),
                             "linear",
                             input$revenue_uptake_model)
    } else {
      T <- ifelse(is.null(input$amc_lifecycle), 10, input$amc_lifecycle)
      uptake_model <- ifelse(is.null(input$amc_uptake_model),
                             "linear",
                             input$amc_uptake_model)
    }
    
    total_months <- ceiling(T * 12)
    months <- seq(1, total_months)
    
    if (uptake_model == "linear") {
      # Use monthly units directly (no division by 12)
      if (!is.null(input$model_revenue) && input$model_revenue) {
        units <- rep(ifelse(
          is.null(input$revenue_monthly_units),
          0,
          input$revenue_monthly_units
        ),
        total_months)
      } else {
        units <- rep(ifelse(
          is.null(input$amc_monthly_units),
          0,
          input$amc_monthly_units
        ),
        total_months)
      }
    } else {
      # logistic S-shaped
      if (!is.null(input$model_revenue) && input$model_revenue) {
        K <- ifelse(is.null(input$revenue_max_units),
                    0,
                    input$revenue_max_units)
        r <- ifelse(is.null(input$revenue_growth),
                    1,
                    input$revenue_growth)
        t0 <- ifelse(
          is.null(input$revenue_peak_time),
          total_months / 2,
          input$revenue_peak_time * 12
        )
      } else {
        K <- ifelse(is.null(input$amc_max_units), 0, input$amc_max_units)
        r <- ifelse(is.null(input$amc_growth), 1, input$amc_growth)
        t0 <- ifelse(is.null(input$amc_peak_time),
                     total_months / 2,
                     input$amc_peak_time * 12)
      }
      units <- K / (1 + exp(-r * (months - t0)))
    }
    tibble(month = months, units = units)
  })
  
  output$amc_uptake_plot <- renderPlotly({
    # Only show plot for S-shaped model
    req(input$amc_uptake_model == "s_shaped")
    req(!input$model_revenue)  # Only show if not using revenue model
    
    df <- adoption_curve()
    p <- plot_ly(
      df,
      x = ~ month,
      y = ~ units,
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#662260', width = 2)
    ) %>%
      layout(
        xaxis = list(title = 'Month', gridcolor = '#D3D4D9'),
        yaxis = list(title = 'Units (monthly)', gridcolor = '#D3D4D9'),
        plot_bgcolor = '#ffffff',
        paper_bgcolor = '#ffffff',
        font = list(family = "Proxima Nova, sans-serif", size = 11),
        margin = list(
          t = 10,
          r = 10,
          b = 40,
          l = 50
        )
      ) %>%
      config(displayModeBar = FALSE)
    p
  })
  
  output$revenue_uptake_plot <- renderPlotly({
    # Only show plot for S-shaped model
    req(input$revenue_uptake_model == "s_shaped")
    req(input$model_revenue)
    
    df <- adoption_curve()
    p <- plot_ly(
      df,
      x = ~ month,
      y = ~ units,
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#8DB790', width = 2)
    ) %>%
      layout(
        xaxis = list(title = 'Month', gridcolor = '#D3D4D9'),
        yaxis = list(title = 'Units (monthly)', gridcolor = '#D3D4D9'),
        plot_bgcolor = '#ffffff',
        paper_bgcolor = '#ffffff',
        font = list(family = "Proxima Nova, sans-serif", size = 11),
        margin = list(
          t = 10,
          r = 10,
          b = 40,
          l = 50
        )
      ) %>%
      config(displayModeBar = FALSE)
    p
  })
  
  # ============ MILESTONE HELPER FUNCTIONS ============
  
  # Helper: Validate milestone percentages sum to 100%
  validate_milestone_percentages <- function(df) {
    milestone_stages <- df %>% filter(`Milestone?` == TRUE)
    
    if (nrow(milestone_stages) == 0) {
      return(list(
        valid = FALSE,
        total = 0,
        error = "No milestone stages selected"
      ))
    }
    
    total_pct <- sum(milestone_stages$`Stage Share (%)`, na.rm = TRUE)
    
    # Stage Share (%) is 0-100, allow ≤100% (remainder goes to AMC)
    list(
      valid = total_pct > 0 && total_pct <= 101,
      total = total_pct,
      n_milestones = nrow(milestone_stages),
      error = if (total_pct <= 0) {
        "Milestone percentages must be greater than 0%"
      } else if (total_pct > 101) {
        sprintf("Milestone percentages sum to %.1f%%, cannot exceed 100%%",
                total_pct)
      } else
        NULL
    )
  }
  
  # Function to calculate metrics up to a specific stage
  calculate_to_stage <- function(df, stage_num, discount_rate) {
    df_to_stage <- df %>% filter(Stage <= stage_num)
    
    calc_df <- df_to_stage %>%
      rename(push_funding = `Share of costs covered by other sources (%)`) %>%
      mutate(push_funding = push_funding / 100) %>%
      clean_names() %>%
      rename(prob = probability_percent) %>%
      mutate(prob = prob / 100) %>%  # Convert percentage to decimal
      mutate(
        cost_after_push = cost * (1 - push_funding),
        cumsum_duration = cumsum(duration)
      ) %>%
      mutate(disc_cost = pmap_dbl(list(
        cost_after_push, duration, cumsum_duration
      ), \(cost_after_push, dur, cum_dur) {
        dur_months <- round(dur * 12)
        start_month <- round((cum_dur - dur) * 12) + 1
        months <- seq(start_month, start_month + dur_months - 1)
        monthly_rate <- (1 + discount_rate / 100)^(1 / 12)
        sum((cost_after_push / dur_months) * (monthly_rate^months))
      })) %>%
      mutate(
        previous_prob = lag(prob),
        previous_prob = replace_na(previous_prob, 1),
        cumprod_prob = cumprod(previous_prob),
        expected_stage_cost = cumprod_prob * disc_cost
      )
    
    list(
      cost_to_stage = sum(calc_df$expected_stage_cost),
      prob_to_stage = prod(df_to_stage$`Probability (%)` / 100),
      duration_to_stage = sum(df_to_stage$Duration)
    )
  }
  
  # Main milestone + AMC calculation function
  calculate_milestones <- function(df,
                                   discount_rate,
                                   target_probability,
                                   use_feasibility = FALSE,
                                   eta = 1,
                                   topup = NULL,
                                   ac_template = NULL,
                                   unit_factor = NULL,
                                   revenue_pv = 0,
                                   global_pos = NULL,
                                   calculation_mode = "topup_to_units",
                                   target_units = NULL,
                                   revenue_start_month = NULL) {
    # Validate calculation_mode
    if (is.null(calculation_mode) || is.na(calculation_mode)) {
      calculation_mode <- "topup_to_units"
    }
    
    # Validate milestones
    validation <- validate_milestone_percentages(df)
    if (!validation$valid) {
      return(
        list(
          valid = FALSE,
          error = validation$error,
          n_milestones = 0,
          milestones = list(),
          amc_component = NULL
        )
      )
    }
    
    # Get milestone stages
    milestone_stages <- df %>% filter(`Milestone?` == TRUE) %>% arrange(Stage)
    
    # Check if any milestones are selected
    if (nrow(milestone_stages) == 0) {
      return(
        list(
          valid = FALSE,
          error = "Please select at least one stage as a milestone by checking the 'Milestone?' box.",
          n_milestones = 0,
          milestones = list(),
          amc_component = NULL
        )
      )
    }
    
    # Calculate full project metrics to get total incentive needed (everything at PV/t0)
    full_metrics <- calculate_project_metrics(df,
                                              discount_rate,
                                              target_probability,
                                              use_feasibility,
                                              eta)
    
    gross_incentive <- full_metrics$shared_prize  # This is GROSS prize at PV (t0) BEFORE revenue
    n_firms <- full_metrics$n_firms
    global_pos <- full_metrics$global_pos
    total_duration <- full_metrics$tot_duration
    
    # STEP 1 & 2: Subtract revenue impact from gross prize to get net incentive (this is x)
    net_incentive <- max(0, gross_incentive - revenue_pv)
    
    # STEP 3: Calculate each milestone payment as % of NET incentive (after revenue offset)
    milestone_list <- list()
    total_milestone_pct <- 0
    
    for (i in 1:nrow(milestone_stages)) {
      stage_num <- milestone_stages$Stage[i]
      stage_name <- milestone_stages$`Stage Name`[i]
      stage_pct <- milestone_stages$`Stage Share (%)`[i]  # This is 0-100
      
      # Get stage timing for display
      stage_info <- df %>% filter(Stage == stage_num)
      duration_to_stage <- sum(df$Duration[df$Stage <= stage_num])
      prob_to_stage <- prod(df$`Probability (%)` [df$Stage <= stage_num] / 100)
      
      # Calculate milestone payment (percentage of NET incentive, after revenue offset)
      milestone_payment <- net_incentive * (stage_pct / 100)
      total_milestone_pct <- total_milestone_pct + stage_pct
      
      milestone_list[[i]] <- list(
        stage = stage_num,
        stage_name = stage_name,
        stage_pct = stage_pct,
        milestone_payment = milestone_payment,
        prob_to_stage = prob_to_stage,
        duration_to_stage = duration_to_stage
      )
    }
    
    # Calculate AMC component percentage
    amc_pct <- 100 - total_milestone_pct
    
    # Total milestone payments (this is y)
    total_milestone_payments <- sum(sapply(milestone_list, function(m)
      m$milestone_payment))
    
    # STEP 4: Calculate AMC target as x - y (net incentive minus milestones)
    amc_target_amount <- net_incentive - total_milestone_payments
    
    # Get final stage name for AMC display
    final_stage_num <- max(df$Stage)
    final_stage_name <- df$`Stage Name`[df$Stage == final_stage_num]
    
    # Initialize AMC component
    amc_component <- list(
      amc_pct = amc_pct,
      amc_payment = amc_target_amount,
      final_stage = final_stage_num,
      final_stage_name = final_stage_name,
      expected_amc_cost = amc_target_amount * global_pos * n_firms
    )
    
    # Calculate actual AMC using the helper function
    if (!is.null(ac_template) && nrow(ac_template) > 0) {
      # Handle both calculation modes
      if (calculation_mode == "units_to_topup" &&
          !is.null(target_units)) {
        # Back-calculate the top-up needed to cover target_units
        topup_result <- calculate_topup_from_units(
          target_units = target_units,
          ac_template = ac_template,
          discount_rate = discount_rate,
          unit_factor = unit_factor,
          target_amount = amc_target_amount,
          # This is the AMC portion (x - y)
          revenue_start_month = revenue_start_month
        )
        
        if (topup_result$feasible) {
          amc_component$topup <- topup_result$topup
          amc_component$units_to_cover_total <- topup_result$units_covered
          amc_component$amc_size_undiscounted <- (topup_result$topup * topup_result$units_covered) / unit_factor
          
          
          
          amc_component$expected_amc <- amc_component$amc_size_undiscounted * global_pos
          amc_component$amc_covers_target <- TRUE
          amc_component$amc_target_amount <- amc_target_amount
          amc_component$optimization_mode <- "units_to_topup"
        } else {
          amc_component$amc_covers_target <- FALSE
          amc_component$optimization_mode <- "units_to_topup"
          amc_component$error <- topup_result$error
        }
        
      } else if (calculation_mode == "topup_to_units" &&
                 !is.null(topup) && topup > 0) {
        # Use the provided topup and calculate coverage
        amc_calc <- calculate_amc_for_target(
          target_amount = amc_target_amount,
          topup = topup,
          ac_template = ac_template,
          discount_rate = discount_rate / 100,
          unit_factor = unit_factor,
          global_pos = global_pos,
          calculation_mode = calculation_mode,
          target_units = NULL,
          revenue_start_month = if (!is.null(revenue_start_month))
            revenue_start_month
          else
            total_duration * 12
        )
        
        # Add results to amc_component
        amc_component$topup <- amc_calc$topup
        amc_component$units_to_cover_total <- amc_calc$units_to_cover_total
        amc_component$amc_size_undiscounted <- amc_calc$amc_size_undiscounted
        
        amc_component$expected_amc <- amc_calc$expected_amc
        amc_component$amc_covers_target <- amc_calc$covers_prize
        amc_component$amc_target_amount <- amc_target_amount
        amc_component$optimization_mode <- amc_calc$optimization_mode
        
      } else {
        # No valid parameters
        amc_component$amc_covers_target <- NA
      }
    } else {
      # No AMC parameters provided or topup is 0
      amc_component$amc_covers_target <- NA
    }
    # Calculate totals
    expected_milestone_cost <- total_milestone_payments * global_pos * n_firms
    
    # Total expected cost
    if (!is.null(amc_component$amc_covers_target) &&
        !is.na(amc_component$amc_covers_target)) {
      if (amc_component$amc_covers_target) {
        # Use the actual expected AMC when it covers
        total_expected_cost <- expected_milestone_cost + amc_component$expected_amc
      } else {
        # Fallback when AMC doesn't cover
        total_expected_cost <- expected_milestone_cost + (amc_component$amc_payment * global_pos * n_firms)
      }
    } else {
      total_expected_cost <- expected_milestone_cost + (amc_component$amc_payment * global_pos * n_firms)
    }
    
    return(
      list(
        valid = TRUE,
        n_milestones = nrow(milestone_stages),
        milestones = milestone_list,
        amc_component = amc_component,
        gross_incentive = gross_incentive,
        # Added: track gross for display
        net_incentive = net_incentive,
        # Added: track net for display
        n_firms = n_firms,
        global_pos = global_pos,
        total_milestone_payments = total_milestone_payments,
        expected_milestone_cost = expected_milestone_cost,
        total_expected_cost = total_expected_cost,
        revenue_pv = revenue_pv
      )
    )
  }
  
  
  # Reactive value that only updates when Calculate button is clicked
  calculated_results <- eventReactive(input$calculate, {
    # Validation now handled by validation_status reactive
    mech <- input$mechanisms
    if (is.null(mech) || length(mech) == 0) {
      showNotification("Please select at least one incentive mechanism before calculating.",
                       type = "error")
      df_prev <- DF()
      return(
        calculate_project_metrics(
          df_prev,
          input$discount_rate,
          input$target_probability,
          input$use_feasibility,
          if (input$use_feasibility)
            input$eta / 100
          else
            1
        )
      )
    }
    
    # Get the data and calculate base metrics
    df <- hot_to_r(input$stages_table)
    use_feasibility <- input$use_feasibility
    eta <- if (use_feasibility)
      input$eta
    else
      1
    eta_prop <- eta / 100
    
    results <- calculate_project_metrics(df,
                                         input$discount_rate,
                                         input$target_probability,
                                         use_feasibility,
                                         eta_prop)
    
    # Extract base values
    prob_of_success <- results$prob_of_success
    cost_to_attempt <- results$cost_to_attempt  # Expected development costs
    tot_duration <- results$tot_duration
    unit_factor <- switch(
      isolate(input$cost_unit),
      "M" = 1e6,
      "K" = 1e3,
      "B" = 1e9,
      "1" = 1
    )
    
    # Store mechanism
    results$mechanisms <- mech
    
    # =====================================================================
    # STEP 1: Calculate Expected Revenue (if enabled)
    # =====================================================================
    revenue_pv <- 0
    revenue_pv_raw <- 0
    prob_reaching_revenue <- 0
    
    if (!is.null(input$model_revenue) && input$model_revenue) {
      # Determine when revenue starts
      revenue_start_stage <- as.numeric(input$revenue_start_stage)
      
      if (revenue_start_stage == 0) {
        # Default: revenue starts after all stages
        revenue_start_month <- tot_duration
        prob_reaching_revenue <- prob_of_success  # All stages must succeed
        
        # Get adoption curve
        ac <- adoption_curve()
        profit_per_unit <- input$profit_per_unit
        
        # Calculate PV (all at same probability)
        ac_pv <- ac %>%
          mutate(
            profit = units * profit_per_unit,
            time_from_present = revenue_start_month + month,
            discount_factor = 1 / ((1 + input$discount_rate / 100)^(time_from_present /
                                                                      12)),
            pv_profit = profit * discount_factor
          )
        
        revenue_pv_raw <- sum(ac_pv$pv_profit) / unit_factor
        revenue_pv <- revenue_pv_raw * prob_reaching_revenue
        
      } else {
        # Revenue starts after completing the selected stage
        revenue_start_month <- sum(df$Duration[1:revenue_start_stage])
        
        # Probability through selected stage
        prob_to_revenue_start <- prod(df$`Success Probability`[1:revenue_start_stage])
        
        # Get the remaining stages after revenue starts
        if (revenue_start_stage < nrow(df)) {
          remaining_stages <- df[(revenue_start_stage + 1):nrow(df), ]
        } else {
          remaining_stages <- data.frame()  # No remaining stages
        }
        
        # Get adoption curve
        ac <- adoption_curve()
        profit_per_unit <- input$profit_per_unit
        
        # Add basic columns
        ac <- ac %>%
          mutate(
            profit = units * profit_per_unit,
            time_from_present = revenue_start_month + month,
            discount_factor = 1 / ((1 + input$discount_rate / 100)^(time_from_present /
                                                                      12))
          )
        
        # Initialize phase probability
        ac$phase_prob <- 0
        
        if (nrow(remaining_stages) > 0) {
          cumulative_months <- 0
          
          for (i in 1:nrow(remaining_stages)) {
            stage_duration_months <- remaining_stages$Duration[i] * 12
            
            # Months in this phase
            phase_start <- cumulative_months
            phase_end <- cumulative_months + stage_duration_months
            
            # Probability through previous stages only (not including current stage)
            prob_to_this_stage <- if (i == 1) {
              prob_to_revenue_start
            } else {
              prob_to_revenue_start * prod(remaining_stages$`Success Probability`[1:(i -
                                                                                       1)])
            }
            
            # Mark months in this stage
            ac$phase_prob[ac$month > phase_start &
                            ac$month <= phase_end] <- prob_to_this_stage
            
            cumulative_months <- phase_end
          }
          
          # After all stages complete
          ac$phase_prob[ac$month > cumulative_months] <- prob_of_success
        } else {
          # No remaining stages - all revenue is after completion
          ac$phase_prob <- prob_to_revenue_start
        }
        
        # Calculate PVs
        revenue_pv_raw <- sum(ac$profit * ac$discount_factor) / unit_factor  # If successful (no probability adjustment)
        revenue_pv <- sum(ac$profit * ac$phase_prob * ac$discount_factor) / unit_factor  # Probability-adjusted
        
        # For display: weighted average probability
        total_revenue <- sum(ac$profit * ac$discount_factor)
        if (total_revenue > 0) {
          prob_reaching_revenue <- sum(ac$profit * ac$phase_prob * ac$discount_factor) / total_revenue
        } else {
          prob_reaching_revenue <- 0
        }
      }
      
      # Store revenue details
      results$revenue_pv_raw <- revenue_pv_raw
      results$revenue_pv <- revenue_pv
      results$prob_reaching_revenue <- prob_reaching_revenue
      results$revenue_start_month <- revenue_start_month
      results$revenue_start_stage <- revenue_start_stage
      
    } else {
      results$revenue_pv_raw <- 0
      results$revenue_pv <- 0
      results$prob_reaching_revenue <- 0
      results$commercially_viable <- FALSE
    }
    
    # =====================================================================
    # STEP 2: Calculate Net Cost to Attempt (Costs - Expected Revenue)
    # =====================================================================
    net_cost_to_attempt <- max(0, cost_to_attempt - revenue_pv)
    results$net_cost_to_attempt <- net_cost_to_attempt
    
    # Check if commercially viable (revenue covers all costs)
    if (!is.null(input$model_revenue) &&
        input$model_revenue && revenue_pv > 0) {
      results$commercially_viable <- (revenue_pv >= cost_to_attempt)
    } else {
      results$commercially_viable <- FALSE
    }
    
    # =====================================================================
    # STEP 3: Calculate Incentive Based on Mechanism (using NET cost)
    # =====================================================================
    
    if (mech == "shared") {
      # ===== SHARED PRIZE MECHANISM =====
      
      # Calculate number of firms needed
      if (prob_of_success < 1) {
        if (use_feasibility) {
          n_firms_temp <- ceiling(log(1 - (
            input$target_probability / 100 / eta_prop
          )) / log(1 - prob_of_success))
        } else {
          n_firms_temp <- ceiling(log(1 - input$target_probability / 100) / log(1 - prob_of_success))
        }
      } else {
        n_firms_temp <- 1
      }
      
      results$n_firms <- n_firms_temp
      
      # Calculate shared prize using NET cost to attempt
      shared_prize <- expected_shared_prize(n_firms_temp, prob_of_success, net_cost_to_attempt)
      results$shared_prize <- shared_prize
      results$gross_prize <- shared_prize
      results$net_prize <- shared_prize
      
    } else if (mech == "amc") {
      # ===== AMC MECHANISM =====
      
      # Determine when AMC payments start
      if (!is.null(input$model_revenue) && input$model_revenue) {
        # Use revenue timing
        amc_start_month <- results$revenue_start_month * 12
      } else {
        # Use AMC payout start stage
        amc_start_stage <- as.numeric(input$amc_payout_start_stage)
        if (amc_start_stage == 0) {
          # After all stages
          amc_start_month <- tot_duration * 12
        } else {
          # After specified stage
          amc_start_month <- sum(df$Duration[1:amc_start_stage]) * 12
        }
      }
      
      # Determine calculation mode and parameters
      calc_mode <- input$amc_calculation_mode
      topup_param <- if (calc_mode == "topup_to_units")
        input$amc_topup
      else
        NULL
      units_param <- if (calc_mode == "units_to_topup")
        input$amc_target_units
      else
        NULL
      
      # Calculate number of firms needed
      if (prob_of_success < 1) {
        if (use_feasibility) {
          n_firms_temp <- ceiling(log(1 - (
            input$target_probability / 100 / eta_prop
          )) / log(1 - prob_of_success))
        } else {
          n_firms_temp <- ceiling(log(1 - input$target_probability / 100) / log(1 - prob_of_success))
        }
      } else {
        n_firms_temp <- 1
      }
      
      results$n_firms <- n_firms_temp
      
      # Calculate target prize amount using NET cost
      target_prize <- expected_shared_prize(n_firms_temp, prob_of_success, net_cost_to_attempt)
      
      # Get AMC adoption curve
      ac_template <- adoption_curve()
      # REMOVE THIS LINE: topup <- ifelse(is.null(input$amc_topup), 0, input$amc_topup)
      
      # Calculate global probability
      base_global_pos <- 1 - (1 - prob_of_success)^n_firms_temp
      global_pos <- if (use_feasibility)
        pmin(base_global_pos * eta_prop, eta_prop)
      else
        base_global_pos
      
      # Calculate AMC needed to cover target
      amc_results <- calculate_amc_for_target(
        target_amount = target_prize,
        topup = topup_param,
        # This is correct - uses the conditional value
        ac_template = ac_template,
        discount_rate = input$discount_rate / 100,
        unit_factor = unit_factor,
        global_pos = global_pos,
        calculation_mode = calc_mode,
        target_units = units_param,
        revenue_start_month =  amc_start_month
      )
      
      results$amc <- amc_results
      results$shared_prize <- target_prize
      results$gross_prize <- target_prize
      results$net_prize <- target_prize
      
    } else if (mech == "milestones_amc") {
      # ===== MILESTONES + AMC MECHANISM =====
      
      # Determine when AMC payments start
      if (!is.null(input$model_revenue) && input$model_revenue) {
        # Use revenue timing
        amc_start_month <- results$revenue_start_month * 12
      } else {
        # Use AMC payout start stage
        amc_start_stage <- as.numeric(input$amc_payout_start_stage)
        if (amc_start_stage == 0) {
          amc_start_month <- tot_duration * 12
        } else {
          amc_start_month <- sum(df$Duration[1:amc_start_stage]) * 12
        }
      }
      
      
      # Determine calculation mode and parameters (same as standalone AMC)
      calc_mode <- input$amc_calculation_mode
      topup_param <- if (calc_mode == "topup_to_units")
        input$amc_topup
      else
        NULL
      units_param <- if (calc_mode == "units_to_topup")
        input$amc_target_units
      else
        NULL
      
      # Calculate number of firms needed
      if (prob_of_success < 1) {
        if (use_feasibility) {
          n_firms_temp <- ceiling(log(1 - (
            input$target_probability / 100 / eta_prop
          )) / log(1 - prob_of_success))
        } else {
          n_firms_temp <- ceiling(log(1 - input$target_probability / 100) / log(1 - prob_of_success))
        }
      } else {
        n_firms_temp <- 1
      }
      
      results$n_firms <- n_firms_temp
      
      # Calculate milestones allocation - CORRECTED ARGUMENTS
      milestone_results <- calculate_milestones(
        df = df,
        discount_rate = input$discount_rate,
        target_probability = input$target_probability,
        use_feasibility = use_feasibility,
        eta = eta_prop,
        topup = topup_param,
        ac_template = adoption_curve(),
        unit_factor = unit_factor,
        revenue_pv = revenue_pv,
        # Use the revenue_pv calculated earlier in this function
        global_pos = NULL,
        # Let calculate_milestones compute it internally
        calculation_mode = calc_mode,
        target_units = units_param,
        revenue_start_month = amc_start_month
      )
      
      results$milestones <- milestone_results
      
      # Store prizes based on net cost
      target_prize <- expected_shared_prize(n_firms_temp, prob_of_success, net_cost_to_attempt)
      results$shared_prize <- target_prize
      results$gross_prize <- target_prize
      results$net_prize <- target_prize
    }
    
    # Store the table data that was used for this calculation
    results$table_data <- df
    
    # Store the cost unit
    results$cost_unit <- isolate(input$cost_unit)
    
    return(results)
  }, ignoreNULL = FALSE)
  
  
  # Real-time AMC feasibility check (add after calculated_results reactive, around line 600)
  amc_feasibility_check <- reactive({
    # Only check in topup_to_units mode
    if (is.null(input$amc_calculation_mode) ||
        input$amc_calculation_mode != "topup_to_units") {
      return(NULL)
    }
    
    if (is.null(input$mechanisms) ||
        !(input$mechanisms %in% c("amc", "milestones_amc"))) {
      return(NULL)
    }
    
    # Need valid inputs
    if (is.null(input$amc_topup) || input$amc_topup <= 0) {
      return(NULL)
    }
    
    # Need stages table
    df <- hot_to_r(input$stages_table)
    if (is.null(df))
      return(NULL)
    
    # Get basic parameters
    prob_of_success <- prod(df$`Probability (%)` / 100)
    discount_rate <- input$discount_rate / 100
    tot_duration <- sum(df$Duration)
    
    # Get cost to attempt (quick calculation)
    cost_to_attempt <- calculate_project_metrics(
      df,
      input$discount_rate,
      input$target_probability,
      input$use_feasibility,
      if (input$use_feasibility)
        input$eta / 100
      else
        1
    )$cost_to_attempt
    
    # Calculate revenue if enabled AND determine revenue_start_month
    revenue_pv <- 0
    revenue_start_month <- 0
    
    if (!is.null(input$model_revenue) && input$model_revenue) {
      # Simple revenue calculation for the check
      ac <- adoption_curve()
      profit_per_unit <- input$profit_per_unit
      revenue_start_stage <- as.numeric(input$revenue_start_stage)
      
      if (revenue_start_stage == 0) {
        revenue_start_month <- tot_duration * 12  # CALCULATE IT
        prob_reaching_revenue <- prob_of_success
      } else {
        revenue_start_month <- sum(df$Duration[1:revenue_start_stage]) * 12  # CALCULATE IT
        prob_reaching_revenue <- prod(df$`Probability (%)`[1:revenue_start_stage] / 100)
      }
      
      ac_pv <- ac %>%
        mutate(
          profit = units * profit_per_unit,
          time_from_present = revenue_start_month + month,
          discount_factor = 1 / ((1 + discount_rate)^(time_from_present /
                                                        12)),
          pv_profit = profit * discount_factor * prob_reaching_revenue
        )
      
      unit_factor <- switch(
        input$cost_unit,
        "M" = 1e6,
        "K" = 1e3,
        "B" = 1e9,
        "1" = 1
      )
      revenue_pv <- sum(ac_pv$pv_profit) / unit_factor
    } else {
      # If NOT modeling revenue, use AMC payout start stage
      amc_start_stage <- as.numeric(input$amc_payout_start_stage)
      if (amc_start_stage == 0) {
        revenue_start_month <- tot_duration * 12
      } else {
        revenue_start_month <- sum(df$Duration[1:amc_start_stage]) * 12
      }
    }
    
    net_cost <- max(0, cost_to_attempt - revenue_pv)
    
    # Calculate what we need for the mechanism
    if (input$mechanisms == "amc") {
      target_amount <- net_cost
    } else if (input$mechanisms == "milestones_amc") {
      if (!("Milestone?" %in% names(df))) {
        return(NULL)
      }
      # Get milestone percentages
      milestone_stages <- df %>% filter(`Milestone?` == TRUE)
      if (nrow(milestone_stages) == 0)
        return(NULL)
      
      total_milestone_pct <- sum(milestone_stages$`Stage Share (%)`, na.rm = TRUE)
      amc_pct <- 100 - total_milestone_pct
      target_amount <- net_cost * (amc_pct / 100)
    } else {
      return(NULL)
    }
    
    # Calculate what the current topup can actually cover
    ac_template <- adoption_curve()
    unit_factor <- switch(
      input$cost_unit,
      "M" = 1e6,
      "K" = 1e3,
      "B" = 1e9,
      "1" = 1
    )
    topup <- input$amc_topup
    target_amount_val <- target_amount * unit_factor
    
    # Check for invalid values before proceeding
    if (is.null(topup) || is.na(topup) || is.infinite(topup) ||
        is.null(target_amount_val) ||
        is.na(target_amount_val) ||
        is.infinite(target_amount_val) ||
        is.null(revenue_start_month) ||
        is.na(revenue_start_month) ||
        is.infinite(revenue_start_month)) {
      return(NULL)
    }
    
    
    # NOW revenue_start_month is defined
    ac <- ac_template %>%
      mutate(
        month_adjusted = month + ceiling(revenue_start_month),
        discount_factor = 1 / ((1 + discount_rate)^(month_adjusted / 12)),
        paid_amount = topup * units,
        paid_amount_pv = paid_amount * discount_factor,
        cumulative_payment_pv = cumsum(paid_amount_pv)
      )
    
    # Add safety check for NAs in the result
    if (any(is.na(ac$cumulative_payment_pv)) ||
        any(is.infinite(ac$cumulative_payment_pv))) {
      return(
        list(
          feasible = FALSE,
          min_topup = NA,
          current_topup = topup,
          error = "Invalid calculation result - values too large or invalid"
        )
      )
    }
    
    max_coverage_pv <- max(ac$cumulative_payment_pv)
    
    # Safety check before comparison
    if (is.na(max_coverage_pv) || is.infinite(max_coverage_pv) ||
        is.na(target_amount_val) ||
        is.infinite(target_amount_val)) {
      return(
        list(
          feasible = FALSE,
          min_topup = NA,
          current_topup = topup,
          error = "Cannot calculate feasibility - invalid values"
        )
      )
    }
    
    feasible <- max_coverage_pv >= target_amount_val
    
    if (!feasible) {
      # Calculate minimum topup needed
      # Sum of discounted units
      sum_discounted_units <- sum(ac$units * ac$discount_factor, na.rm = TRUE)
      
      if (is.na(sum_discounted_units) ||
          sum_discounted_units <= 0) {
        min_topup <- NA
      } else {
        min_topup <- target_amount_val / sum_discounted_units
      }
      
      return(
        list(
          feasible = FALSE,
          min_topup = min_topup,
          current_topup = topup,
          shortage = target_amount_val - max_coverage_pv
        )
      )
    }
    
    return(list(feasible = TRUE))
  })
  
  # Render the warning UI
  output$amc_feasibility_warning <- renderUI({
    check <- amc_feasibility_check()
    if (is.null(check))
      return(NULL)
    
    if (!check$feasible) {
      div(
        style = "margin-top: 10px; margin-bottom: 10px; padding: 12px 16px;
               background: linear-gradient(135deg, rgba(239, 68, 68, 0.12), rgba(220, 38, 38, 0.08));
               border-left: 4px solid #ef4444;
               border-radius: 8px;
               animation: slideIn 0.3s ease-out;",
        p(
          style = "margin: 0; color: #991b1b; font-size: 13px; font-weight: 600; line-height: 1.5;",
          sprintf(
            "⚠️ Current top-up insufficient. Minimum needed: $%s/unit",
            format(
              round(check$min_topup, 2),
              big.mark = ",",
              scientific = FALSE
            )
          )
        ),
        p(
          style = "margin: 8px 0 0 0; color: #7f1d1d; font-size: 12px; font-style: italic",
          "Increase the top-up amount, lifecycle duration, or monthly units to make this AMC feasible."
        )
      )
    }
  })
  
  
  # Validation check for units exceeding lifecycle capacity
  amc_units_validation <- reactive({
    # Only check in units_to_topup mode
    if (is.null(input$amc_calculation_mode) ||
        input$amc_calculation_mode != "units_to_topup") {
      return(NULL)
    }
    
    if (is.null(input$mechanisms) ||
        !(input$mechanisms %in% c("amc", "milestones_amc"))) {
      return(NULL)
    }
    
    # Need valid units input
    if (is.null(input$amc_target_units) ||
        is.na(input$amc_target_units) ||
        input$amc_target_units <= 0) {
      return(NULL)
    }
    
    # Get adoption curve
    ac <- adoption_curve()
    if (is.null(ac) || nrow(ac) == 0) {
      return(NULL)
    }
    
    # Calculate total lifecycle capacity
    total_units_available <- sum(ac$units, na.rm = TRUE)
    
    if (is.na(total_units_available) ||
        total_units_available <= 0) {
      return(NULL)
    }
    
    # Check if user's target exceeds capacity
    if (input$amc_target_units > total_units_available) {
      return(
        list(
          valid = FALSE,
          target_units = input$amc_target_units,
          total_available = total_units_available,
          error = sprintf(
            "Target units (%s) exceed lifecycle capacity (%s units)",
            format(
              round(input$amc_target_units, 0),
              big.mark = ",",
              scientific = FALSE
            ),
            format(
              round(total_units_available, 0),
              big.mark = ",",
              scientific = FALSE
            )
          )
        )
      )
    }
    
    return(list(valid = TRUE))
  })
  
  # Render the units validation warning UI
  output$amc_units_validation_warning <- renderUI({
    check <- amc_units_validation()
    if (is.null(check))
      return(NULL)
    
    if (!check$valid) {
      div(
        style = "margin-top: 10px; margin-bottom: 10px; padding: 12px 16px;
             background: linear-gradient(135deg, rgba(239, 68, 68, 0.12), rgba(220, 38, 38, 0.08));
             border-left: 4px solid #ef4444;
             border-radius: 8px;
             animation: slideIn 0.3s ease-out;",
        p(style = "margin: 0; color: #991b1b; font-size: 13px; font-weight: 600; line-height: 1.5;", sprintf("⚠️ %s", check$error)),
        p(
          style = "margin: 8px 0 0 0; color: #7f1d1d; font-size: 12px; font-style: italic",
          "Please reduce the target units or increase the lifecycle duration/uptake rate."
        )
      )
    }
  })
  
  # Dynamic instruction text for stage data
  output$stage_instructions <- renderUI({
    unit_name <- get_unit_name()
    
    base_instructions <- paste0(
      "<strong>Stage Name:</strong> Name or label for each stage (e.g., 'Discovery', 'Phase 1 Trial')<br>",
      "<strong>Duration:</strong> Length of the stage in years<br>",
      "<strong>Cost:</strong> Total cost for this stage in ",
      unit_name,
      "<br>",
      "<strong>Probability:</strong> Probability of success for this stage (0 to 100)<br>",
      "<strong>Share of costs covered by other sources:</strong> Percentage of costs funded by sources other than the prize (0 to 100)<br>"
    )
    
    # Only show milestone instructions when milestones+AMC are selected
    milestone_instructions <- ""
    if (!is.null(input$mechanisms) &&
        'milestones_amc' == input$mechanisms) {
      milestone_instructions <- paste0(
        "<strong>Milestone?:</strong> Check this box if a milestone payment should be made after successful completion of this stage<br>",
        "<strong>Stage Share (%):</strong> What percentage of the total incentive should be paid at this milestone (0 to 100, must sum to ≤100 across all checked stages). Remainder goes to AMC.<br>"
      )
    }
    
    div(class = "instruction-text", HTML(paste0(
      base_instructions, milestone_instructions
    )))
  })
  
  DF <- reactive({
    # Get current data - either from edited table or default
    if (!is.null(input$stages_table)) {
      current_data <- hot_to_r(input$stages_table)
      
      # If milestone columns are missing (because they were hidden), add them back
      if (!("Milestone?" %in% colnames(current_data))) {
        current_data$`Milestone?` <- TRUE
      }
      if (!("Stage Share (%)" %in% colnames(current_data))) {
        current_data$`Stage Share (%)` <- 10
      }
      
      # Handle column name changes (old "Probability" to new "Probability (%)")
      if ("Probability" %in% colnames(current_data) &&
          !("Probability (%)" %in% colnames(current_data))) {
        current_data$`Probability (%)` <- current_data$Probability * 100
        current_data$Probability <- NULL
      }
      if ("Stage Share" %in% colnames(current_data) &&
          !("Stage Share (%)" %in% colnames(current_data))) {
        current_data$`Stage Share (%)` <- current_data$`Stage Share` * 100
        current_data$`Stage Share` <- NULL
      }
      
      # Ensure columns are in the right order
      col_order <- c(
        "Stage",
        "Stage Name",
        "Duration",
        "Cost",
        "Probability (%)",
        "Share of costs covered by other sources (%)",
        "Milestone?",
        "Stage Share (%)"
      )
      current_data <- current_data[, col_order]
      
    } else {
      current_data <- data.frame(
        Stage = 1:4,
        `Stage Name` = c(
          "Prototyping",
          "R&D",
          "Clinical trials",
          "Regulatory approval"
        ),
        Duration = c(2, 1.5, 1.4, 0.8),
        Cost = c(6.08, 3.72, 3.64, 0.61),
        `Probability (%)` = c(40, 80, 60, 90),
        `Share of costs covered by other sources (%)` = c(80, 80, 80, 50),
        `Milestone?` = c(FALSE, FALSE, FALSE, TRUE),
        `Stage Share (%)` = c(0, 0, 0, 100),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
    
    # Adjust for number of stages
    n <- input$num_stages
    current_rows <- nrow(current_data)
    
    if (n > current_rows) {
      # Add new rows
      extra <- n - current_rows
      new_rows <- data.frame(
        Stage = (current_rows + 1):n,
        `Stage Name` = paste("Stage", (current_rows + 1):n),
        Duration = rep(1, extra),
        Cost = rep(10, extra),
        `Probability (%)` = rep(80, extra),
        `Share of costs covered by other sources (%)` = rep(0.0, extra),
        `Milestone?` = rep(FALSE, extra),
        `Stage Share (%)` = rep(10, extra),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      current_data <- rbind(current_data, new_rows)
    } else if (n < current_rows) {
      # Remove rows
      current_data <- current_data[1:n, ]
    }
    
    # Ensure Stage column is sequential
    current_data$Stage <- 1:nrow(current_data)
    current_data
  })
  
  output$stages_table <- renderRHandsontable({
    df <- DF()
    
    # Show milestone columns when milestones_amc is selected
    show_milestones <- !is.null(input$mechanisms) &&
      'milestones_amc' %in% input$mechanisms
    
    if (show_milestones) {
      rht <- rhandsontable(
        df,
        rowHeaders = NULL,
        stretchH = "all",
        width = "100%",
        height = "auto"
      ) %>%
        hot_col(1, readOnly = TRUE, width = 50) %>%  # Stage column
        hot_col(2, width = 120) %>%  # Stage Name column
        hot_col(3, width = 80, format = "0.0") %>%  # Duration column
        hot_col(4, width = 70, format = "0.00") %>%  # Cost column
        hot_col(5, width = 90, format = "0.0") %>%  # Probability (%) column - no decimals for %
        hot_col(6, width = 120, format = "0.0") %>%  # Share of costs column
        hot_col(7, width = 80, type = "checkbox") %>%  # Milestone? column
        hot_col(8, width = 70, format = "0.0") %>%  # Stage Share (%) column - no decimals for %
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_table(
          highlightCol = TRUE,
          highlightRow = TRUE,
          contextMenu = FALSE,
          stretchH = "all"
        )
    } else {
      # Hide last two columns when milestones not selected
      df_subset <- df[, 1:6]  # Only show first 6 columns
      rht <- rhandsontable(
        df_subset,
        rowHeaders = NULL,
        stretchH = "all",
        width = "100%",
        height = "auto"
      ) %>%
        hot_col(1, readOnly = TRUE, width = 50) %>%  # Stage column
        hot_col(2, width = 120) %>%  # Stage Name column
        hot_col(3, width = 80, format = "0.0") %>%  # Duration column
        hot_col(4, width = 70, format = "0.0") %>%  # Cost column
        hot_col(5, width = 90, format = "0.0") %>%  # Probability (%) column - no decimals for %
        hot_col(6, width = 120, format = "0.0") %>%  # Share of costs column
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_table(
          highlightCol = TRUE,
          highlightRow = TRUE,
          contextMenu = FALSE,
          stretchH = "all"
        )
    }
    
    rht
  })
  
  # Update revenue start stage dropdown when stages change
  observe({
    df <- hot_to_r(input$stages_table)
    
    # Add null checks and validation
    if (is.null(df) ||
        nrow(df) == 0 || !("Stage Name" %in% names(df))) {
      return()
    }
    
    # Make sure Stage Name column has no NAs
    if (any(is.na(df$`Stage Name`))) {
      return()
    }
    
    # Create choices for ALL stages
    n_stages <- nrow(df)
    stage_names <- df$`Stage Name`
    
    # Build choices
    stage_choices <- list()
    
    for (i in 1:(n_stages - 1)) {
      # Stages 1 through n-1
      stage_choices[[paste0("After completing ", stage_names[i])]] <- as.character(i)
    }
    
    # Last stage is the default (equivalent to "after all stages")
    stage_choices[[paste0("After completing ", stage_names[n_stages], " (default)")]] <- "0"
    
    updateSelectInput(session,
                      "revenue_start_stage",
                      choices = stage_choices,
                      selected = "0")
  }) %>% bindEvent(input$stages_table)
  
  # Update AMC payout start stage dropdown when stages change OR revenue settings change
  observe({
    df <- hot_to_r(input$stages_table)
    
    # Add null checks and validation
    if (is.null(df) ||
        nrow(df) == 0 || !("Stage Name" %in% names(df))) {
      return()
    }
    
    # Make sure Stage Name column has no NAs
    if (any(is.na(df$`Stage Name`))) {
      return()
    }
    
    # Create choices for ALL stages
    n_stages <- nrow(df)
    stage_names <- df$`Stage Name`
    
    # Build choices
    stage_choices <- list()
    
    for (i in 1:(n_stages - 1)) {
      # Stages 1 through n-1
      stage_choices[[paste0("After completing ", stage_names[i])]] <- as.character(i)
    }
    
    # Last stage is the default (equivalent to "after all stages")
    stage_choices[[paste0("After completing ", stage_names[n_stages], " (default)")]] <- "0"
    
    # Determine default selection
    default_selection <- "0"  # Start with standard default
    
    # If revenue modeling is enabled, sync to revenue start stage
    if (!is.null(input$model_revenue) &&
        input$model_revenue &&
        !is.null(input$revenue_start_stage)) {
      default_selection <- input$revenue_start_stage
    }
    
    updateSelectInput(session,
                      "amc_payout_start_stage",
                      choices = stage_choices,
                      selected = default_selection)
  }) %>% bindEvent(input$stages_table,
                   input$model_revenue,
                   input$revenue_start_stage)
  # Save archetype button
  observeEvent(input$save_archetype, {
    req(input$archetype_name)
    
    archetype_name <- trimws(input$archetype_name)
    
    if (archetype_name == "") {
      showNotification("Please enter a name for the archetype", type = "warning")
      return()
    }
    
    # Get current results from the reactive
    results <- calculated_results()
    
    # Store the archetype with all necessary data
    saved_archetypes$data[[archetype_name]] <- list(name = archetype_name,
                                                    results = results,
                                                    timestamp = Sys.time())
    
    # Update the selectInput choices
    updateSelectInput(
      session,
      "selected_archetypes",
      choices = names(saved_archetypes$data),
      selected = names(saved_archetypes$data)
    )
    
    # Clear the name input
    updateTextInput(session, "archetype_name", value = "")
    
    showNotification(paste("Saved:", archetype_name), type = "message")
  })
  
  # Clear all archetypes
  observeEvent(input$clear_archetypes, {
    saved_archetypes$data <- list()
    updateSelectInput(session,
                      "selected_archetypes",
                      choices = NULL,
                      selected = NULL)
    showNotification("All saved archetypes cleared", type = "message")
  })
  
  # Notify if more than 4 archetypes selected
  observe({
    if (!is.null(input$selected_archetypes) &&
        length(input$selected_archetypes) > 4) {
      showNotification(
        "Only the first 4 archetypes will be displayed",
        type = "warning",
        duration = 3
      )
    }
  })
  
  # Show expanded plot in modal
  observeEvent(input$expand_plot, {
    showModal(modalDialog(
      title = div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        span("Probability vs Prize Cost", style = "font-size: 1.5rem; font-weight: 700; color: #662260;"),
        actionButton(
          "close_modal",
          "✕",
          style = "background: none; border: none; font-size: 24px;
                             color: #58595B; cursor: pointer; padding: 0;",
          onclick = "Shiny.setInputValue('close_expanded_plot', Math.random())"
        )
      ),
      plotlyOutput("prize_plot_expanded", height = "calc(90vh - 100px)"),
      size = "xl",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Close modal handler
  observeEvent(input$close_expanded_plot, {
    removeModal()
  })
  # Helper to calculate AMC size for a given n
  
  # Helper to calculate AMC size for a given n - uses same code path as purple cards
  calculate_amc_for_n_v2 <- function(n,
                                     prob_of_success,
                                     net_cost_to_attempt,
                                     discount_rate,
                                     topup,
                                     ac_template,
                                     unit_factor,
                                     revenue_start_month,
                                     calc_mode = "topup_to_units",
                                     target_units = NULL) {
    # Calculate the prize amount for this n (at t=0 PV)
    target_amount <- expected_shared_prize(n, prob_of_success, net_cost_to_attempt)
    
    # Calculate global PoS for this n
    base_global_pos <- 1 - (1 - prob_of_success)^n
    
    # Use the EXACT same function as purple card
    if (calc_mode == "topup_to_units" &&
        !is.null(topup) && topup > 0) {
      # Call the same function the purple card uses
      amc_result <- calculate_amc_for_target(
        target_amount = target_amount,
        topup = topup,
        ac_template = ac_template,
        discount_rate = discount_rate,
        unit_factor = unit_factor,
        optimization_mode = "topup_to_units",
        revenue_start_month = revenue_start_month
      )
      
      return(
        list(
          amc_size = amc_result$amc_size_undiscounted,
          global_pos = base_global_pos,
          covers = amc_result$covers_prize
        )
      )
      
    } else if (calc_mode == "units_to_topup" &&
               !is.null(target_units) && target_units > 0) {
      # Call the same function the purple card uses
      topup_result <- calculate_topup_from_units(
        target_units = target_units,
        ac_template = ac_template,
        discount_rate = discount_rate,
        unit_factor = unit_factor,
        target_amount = target_amount,
        revenue_start_month = revenue_start_month
      )
      
      if (topup_result$feasible) {
        return(list(
          amc_size = (topup_result$topup * topup_result$units_covered) / unit_factor,
          global_pos = base_global_pos,
          covers = TRUE
        ))
      } else {
        return(list(
          amc_size = 0,
          global_pos = base_global_pos,
          covers = FALSE
        ))
      }
      
    } else {
      return(list(
        amc_size = 0,
        global_pos = base_global_pos,
        covers = FALSE
      ))
    }
  }
  
  # Helper function to generate the plot (reusable for both regular and expanded views)
  generate_prize_plot <- function() {
    results    <- calculated_results()
    
    # Handle NULL results (validation error)
    if (is.null(results)) {
      return(
        plot_ly() %>% layout(
          title = "Please fix validation errors and click Calculate",
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
      )
    }
    
    unit_label <- get_unit_label()
    mech      <- results$mechanisms
    
    if (is.null(mech)) {
      plot_mech <- "shared"
    }
    
    prob_of_success <- results$prob_of_success
    cost_to_attempt <- results$cost_to_attempt
    use_feasibility <- results$use_feasibility
    eta             <- results$eta
    
    eta_prop = results$eta
    
    max_achievable_prob <- if (use_feasibility)
      eta_prop
    else
      1
    
    # Helper: n range for current scenario
    target_prob_for_calc <- max_achievable_prob * 0.99
    if (prob_of_success < 1) {
      n_max <- ceiling(log(1 - target_prob_for_calc) / log(1 - prob_of_success))
    } else {
      n_max <- 1
    }
    n_max <- max(n_max, results$n_firms + 5)
    n_max <- min(n_max, 100)
    n_values <- 1:n_max
    
    p <- plot_ly()
    y_max <- max_achievable_prob
    
    # Check if current curve matches any saved archetype
    show_current_curve <- TRUE
    if (!is.null(input$selected_archetypes) &&
        length(input$selected_archetypes) > 0) {
      for (name in input$selected_archetypes) {
        arch <- saved_archetypes$data[[name]]
        if (!is.null(arch)) {
          arch_res <- arch$results
          if (!is.null(arch_res$prob_of_success) &&
              !is.null(arch_res$cost_to_attempt) &&
              !is.null(arch_res$mechanisms) &&
              abs(arch_res$prob_of_success - prob_of_success) < 0.0001 &&
              abs(arch_res$cost_to_attempt - cost_to_attempt) < 0.0001 &&
              arch_res$use_feasibility == use_feasibility &&
              arch_res$mechanisms == mech &&
              # Arch_res$eta is from 0 to 100, and eta is from 0 to 100
              (!use_feasibility ||
               abs(arch_res$eta - eta) < 0.0001)) {
            show_current_curve <- FALSE
            break
          }
        }
      }
    }
    
    # Determine what to plot based on mechanism
    plot_data <- NULL
    
    if (mech == "shared") {
      # Plot prize curve - use NET cost (after revenue adjustment)
      plot_data <- data.frame(n = n_values) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          base_global_pos = 1 - (1 - prob_of_success)^n,
          global_pos = if (use_feasibility)
            pmin(base_global_pos * eta_prop, eta_prop)
          else
            base_global_pos,
          x_value = expected_shared_prize(n, prob_of_success, results$net_cost_to_attempt)  # ← USE NET COST
        ) %>%
        dplyr::ungroup()
      
      
    } else if (mech == "amc") {
      if (is.null(results$amc) || !results$amc$covers_prize) {
        # Create an elegant empty state instead of broken plot
        return(
          plot_ly() %>%
            layout(
              xaxis = list(visible = FALSE, range = c(0, 1)),
              yaxis = list(visible = FALSE, range = c(0, 1)),
              annotations = list(
                list(
                  x = 0.5,
                  y = 0.5,
                  text = paste0(
                    "<b>AMC Parameters Needed</b><br>",
                    "<span style='font-size: 14px; color: #666;'>",
                    if (!is.null(results$amc$error)) {
                      results$amc$error
                    } else {
                      paste0(
                        "The AMC parameters do not generate enough<br>",
                        "cumulative payments to cover the target amount.<br><br>",
                        "Try: Increasing top-up amount, lifecycle duration,<br>",
                        "or uptake rate"
                      )
                    },
                    "</span>"
                  ),
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE,
                  font = list(
                    size = 16,
                    color = "#662260",
                    family = "Proxima Nova"
                  )
                ),
                list(
                  x = 0.5,
                  y = 0.15,
                  text = "⚙️",
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE,
                  font = list(size = 48)
                )
              ),
              plot_bgcolor = "rgba(242, 215, 238, 0.1)",
              paper_bgcolor = "#ffffff",
              margin = list(
                t = 40,
                r = 40,
                b = 40,
                l = 40
              )
            ) %>%
            config(displayModeBar = FALSE)
        )
      }
      
      ac_template <- adoption_curve()
      unit_factor <- switch(
        results$cost_unit,
        "M" = 1e6,
        "K" = 1e3,
        "B" = 1e9,
        "1" = 1
      )
      
      # Get the calculation mode and parameters
      calc_mode <- if (!is.null(input$amc_calculation_mode))
        input$amc_calculation_mode
      else
        "topup_to_units"
      
      # For plotting, we need to handle both modes
      if (calc_mode == "units_to_topup") {
        # In units mode, we can't easily plot a curve since topup varies with n
        
        return(
          plot_ly() %>%
            layout(
              xaxis = list(visible = FALSE, range = c(0, 1)),
              yaxis = list(visible = FALSE, range = c(0, 1)),
              annotations = list(
                list(
                  x = 0.5,
                  y = 0.6,
                  text = "<b>Probability vs Cost Curve</b>",
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE,
                  font = list(
                    size = 20,
                    color = "#662260",
                    family = "Proxima Nova",
                    weight = "bold"
                  )
                ),
                list(
                  x = 0.5,
                  y = 0.3,
                  text = paste0(
                    "<span style='font-size: 14px; color: #666;'>",
                    "To see the probability vs cost curve:<br><br>",
                    "1. Switch calculation mode to 'Specify top-up → Calculate units'<br>",
                    "2. Enter the calculated top-up from above<br>",
                    "3. Click Calculate to generate the curve",
                    "</span>"
                  ),
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE,
                  font = list(
                    size = 14,
                    color = "#666",
                    family = "Proxima Nova"
                  )
                ),
                list(
                  x = 0.5,
                  y = 0.15,
                  text = "📊",
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE,
                  font = list(size = 48)
                )
              ),
              plot_bgcolor = "rgba(242, 215, 238, 0.1)",
              paper_bgcolor = "#ffffff",
              margin = list(
                t = 40,
                r = 40,
                b = 40,
                l = 40
              )
            ) %>%
            config(displayModeBar = FALSE)
        )
        
      } else if (calc_mode == "topup_to_units" &&
                 !is.null(results$amc$topup) &&
                 results$amc$topup > 0) {
        topup <- results$amc$topup
        
        # Get revenue start month (EXACT same as purple card uses - no conversion)
        revenue_start_month <- if (!is.null(results$revenue_start_month)) {
          results$revenue_start_month  # Keep as-is, same as purple card
        } else {
          results$tot_duration
        }
        
        plot_data <- data.frame(n = n_values) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(amc_calc = list(
            calculate_amc_for_n_v2(
              n,
              prob_of_success,
              results$net_cost_to_attempt,
              input$discount_rate / 100,
              topup,
              ac_template,
              unit_factor,
              revenue_start_month,
              calc_mode = "topup_to_units"
            )
          )) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            x_value = purrr::map_dbl(amc_calc, ~ .x$amc_size),
            global_pos = purrr::map_dbl(amc_calc, ~ .x$global_pos),
            covers = purrr::map_lgl(amc_calc, ~ .x$covers)
          ) %>%
          dplyr::filter(covers) %>%
          dplyr::select(-amc_calc)
        
        if (nrow(plot_data) == 0) {
          return(
            plot_ly() %>%
              layout(
                xaxis = list(visible = FALSE, range = c(0, 1)),
                yaxis = list(visible = FALSE, range = c(0, 1)),
                annotations = list(
                  list(
                    x = 0.5,
                    y = 0.5,
                    text = paste0(
                      "<b>Cannot Display Coverage Scenarios</b><br>",
                      "<span style='font-size: 14px; color: #666;'>",
                      "The AMC parameters do not cover any probability levels.<br><br>",
                      "Adjust: Top-up amount, lifecycle duration, or target units",
                      "</span>"
                    ),
                    xref = "paper",
                    yref = "paper",
                    showarrow = FALSE,
                    font = list(
                      size = 16,
                      color = "#662260",
                      family = "Proxima Nova"
                    )
                  ),
                  list(
                    x = 0.5,
                    y = 0.35,
                    text = "📊",
                    xref = "paper",
                    yref = "paper",
                    showarrow = FALSE,
                    font = list(size = 48)
                  )
                ),
                plot_bgcolor = "rgba(242, 215, 238, 0.1)",
                paper_bgcolor = "#ffffff",
                margin = list(
                  t = 40,
                  r = 40,
                  b = 40,
                  l = 40
                )
              ) %>%
              config(displayModeBar = FALSE)
          )
        }
        
        if (use_feasibility) {
          plot_data <- plot_data %>% dplyr::mutate(global_pos = pmin(global_pos * eta_prop, eta_prop))
        }
        
      } else {
        # In topup mode, plot the full curve
        topup <- results$amc$topup
        
        plot_data <- data.frame(n = n_values) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(amc_calc = list(
            calculate_amc_for_n_v2(
              n,
              prob_of_success,
              results$net_cost_to_attempt,
              input$discount_rate / 100,
              topup,
              ac_template,
              unit_factor,
              revenue_start_month,
              calc_mode = "topup_to_units"
            )
          )) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            x_value = purrr::map_dbl(amc_calc, ~ .x$amc_size),
            global_pos = purrr::map_dbl(amc_calc, ~ .x$global_pos),
            covers = purrr::map_lgl(amc_calc, ~ .x$covers)
          ) %>%
          dplyr::filter(covers) %>%
          dplyr::select(-amc_calc)
        
        if (nrow(plot_data) == 0) {
          return(
            plot_ly() %>%
              layout(
                xaxis = list(visible = FALSE, range = c(0, 1)),
                yaxis = list(visible = FALSE, range = c(0, 1)),
                annotations = list(
                  list(
                    x = 0.5,
                    y = 0.5,
                    text = paste0(
                      "<b>Cannot Display Coverage Scenarios</b><br>",
                      "<span style='font-size: 14px; color: #666;'>",
                      "The AMC parameters do not cover any probability levels.<br><br>",
                      "Adjust: Top-up amount, lifecycle duration, or target units",
                      "</span>"
                    ),
                    xref = "paper",
                    yref = "paper",
                    showarrow = FALSE,
                    font = list(
                      size = 16,
                      color = "#662260",
                      family = "Proxima Nova"
                    )
                  ),
                  list(
                    x = 0.5,
                    y = 0.35,
                    text = "📊",
                    xref = "paper",
                    yref = "paper",
                    showarrow = FALSE,
                    font = list(size = 48)
                  )
                ),
                plot_bgcolor = "rgba(242, 215, 238, 0.1)",
                paper_bgcolor = "#ffffff",
                margin = list(
                  t = 40,
                  r = 40,
                  b = 40,
                  l = 40
                )
              ) %>%
              config(displayModeBar = FALSE)
          )
        }
        
        if (use_feasibility) {
          plot_data <- plot_data %>% dplyr::mutate(global_pos = pmin(global_pos * eta_prop, eta_prop))
        }
      }
      
      
    } else if (mech == "milestones_amc") {
      # For plotting, we need to handle both modes
      calc_mode <- if (!is.null(input$amc_calculation_mode))
        input$amc_calculation_mode
      else
        "topup_to_units"
      if (calc_mode == "units_to_topup") {
        # In units mode, we can't easily plot a curve since topup varies with n
        
        return(
          plot_ly() %>%
            layout(
              xaxis = list(visible = FALSE, range = c(0, 1)),
              yaxis = list(visible = FALSE, range = c(0, 1)),
              annotations = list(
                list(
                  x = 0.5,
                  y = 0.6,
                  text = "<b>Probability vs Cost Curve</b>",
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE,
                  font = list(
                    size = 20,
                    color = "#662260",
                    family = "Proxima Nova",
                    weight = "bold"
                  )
                ),
                list(
                  x = 0.5,
                  y = 0.3,
                  text = paste0(
                    "<span style='font-size: 14px; color: #666;'>",
                    "To see the probability vs cost curve:<br><br>",
                    "1. Switch calculation mode to 'Specify top-up → Calculate units'<br>",
                    "2. Enter the calculated top-up from above<br>",
                    "3. Click Calculate to generate the curve",
                    "</span>"
                  ),
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE,
                  font = list(
                    size = 14,
                    color = "#666",
                    family = "Proxima Nova"
                  )
                ),
                list(
                  x = 0.5,
                  y = 0.15,
                  text = "📊",
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE,
                  font = list(size = 48)
                )
              ),
              plot_bgcolor = "rgba(242, 215, 238, 0.1)",
              paper_bgcolor = "#ffffff",
              margin = list(
                t = 40,
                r = 40,
                b = 40,
                l = 40
              )
            ) %>%
            config(displayModeBar = FALSE)
        )
      }
      
      # For milestones+AMC, we need to calculate the actual total cost for each n
      if (is.null(results$milestones) ||
          !results$milestones$valid) {
        return(
          plot_ly() %>%
            layout(
              xaxis = list(visible = FALSE, range = c(0, 1)),
              yaxis = list(visible = FALSE, range = c(0, 1)),
              annotations = list(
                list(
                  x = 0.5,
                  y = 0.5,
                  text = "<b>Milestones Configuration Invalid</b><br><span style='font-size: 14px; color: #666;'>Please configure milestones properly</span>",
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE,
                  font = list(
                    size = 16,
                    color = "#662260",
                    family = "Proxima Nova"
                  )
                ),
                list(
                  x = 0.5,
                  y = 0.35,
                  text = "⚙️",
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE,
                  font = list(size = 48)
                )
              ),
              plot_bgcolor = "rgba(242, 215, 238, 0.1)",
              paper_bgcolor = "#ffffff",
              margin = list(
                t = 40,
                r = 40,
                b = 40,
                l = 40
              )
            ) %>%
            config(displayModeBar = FALSE)
        )
      }
      
      # Get milestone percentages
      df <- results$table_data
      milestone_stages <- df %>% filter(`Milestone?` == TRUE)
      total_milestone_pct <- sum(milestone_stages$`Stage Share (%)`, na.rm = TRUE)
      amc_pct <- 100 - total_milestone_pct
      
      calc_mode <- if (!is.null(input$amc_calculation_mode))
        input$amc_calculation_mode
      else
        "topup_to_units"
      
      # Get topup from results (which has the calculated value in units_to_topup mode)
      if (!is.null(results$milestones$amc_component) &&
          !is.null(results$milestones$amc_component$topup) &&
          results$milestones$amc_component$topup > 0) {
        topup <- results$milestones$amc_component$topup
      } else {
        topup <- 0
      }
      
      ac_template <- adoption_curve()
      unit_factor <- switch(
        results$cost_unit,
        "M" = 1e6,
        "K" = 1e3,
        "B" = 1e9,
        "1" = 1
      )
      revenue_pv <- results$revenue_pv
      
      # Calculate for each n - use NET cost
      plot_data <- data.frame(n = n_values) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          base_global_pos = 1 - (1 - prob_of_success)^n,
          global_pos = if (use_feasibility)
            pmin(base_global_pos * eta, eta)
          else
            base_global_pos,
          # CHANGED THIS
          gross_prize = expected_shared_prize(n, prob_of_success, cost_to_attempt),
          net_prize = max(0, gross_prize - revenue_pv),
          # Subtract revenue here
          milestone_payment = net_prize * (total_milestone_pct / 100),
          amc_target = net_prize * (amc_pct / 100)
        ) %>%
        dplyr::ungroup()
      
      # Get revenue start month for AMC calculations
      revenue_start_month <- if (!is.null(results$revenue_start_month)) {
        results$revenue_start_month * 12
      } else {
        results$tot_duration * 12
      }
      
      # Calculate actual AMC for each row
      if (topup > 0 &&
          !is.null(ac_template) && nrow(ac_template) > 0) {
        plot_data <- plot_data %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            amc_calc = list(
              calculate_amc_for_target(
                target_amount = amc_target,
                topup = topup,
                ac_template = ac_template,
                discount_rate = input$discount_rate / 100,
                unit_factor = unit_factor,
                global_pos = global_pos,
                optimization_mode = "topup_to_units",
                revenue_start_month = revenue_start_month
              )
            ),
            amc_covers = amc_calc$covers_prize,
            amc_amount = ifelse(
              amc_calc$covers_prize,
              amc_calc$amc_size_undiscounted,
              NA
            ),
            x_value = ifelse(
              amc_calc$covers_prize,
              milestone_payment + amc_calc$amc_size_undiscounted,
              NA
            )
          ) %>%
          dplyr::ungroup() %>%
          dplyr::select(-amc_calc) %>%
          dplyr::filter(amc_covers)
      } else {
        plot_data <- plot_data %>%
          dplyr::mutate(x_value = NA, amc_covers = FALSE) %>%
          dplyr::filter(amc_covers)
      }
      
      # Check if we have any valid data
      if (nrow(plot_data) == 0) {
        return(
          plot_ly() %>%
            layout(
              xaxis = list(visible = FALSE, range = c(0, 1)),
              yaxis = list(visible = FALSE, range = c(0, 1)),
              annotations = list(
                list(
                  x = 0.5,
                  y = 0.5,
                  text = "<b>AMC Component Does Not Cover Target</b><br><span style='font-size: 14px; color: #666;'>The AMC parameters do not cover any probability levels.<br><br>Adjust: Top-up amount, lifecycle duration, or target units</span>",
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE,
                  font = list(
                    size = 16,
                    color = "#662260",
                    family = "Proxima Nova"
                  )
                ),
                list(
                  x = 0.5,
                  y = 0.35,
                  text = "⚙️",
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE,
                  font = list(size = 48)
                )
              ),
              plot_bgcolor = "rgba(242, 215, 238, 0.1)",
              paper_bgcolor = "#ffffff",
              margin = list(
                t = 40,
                r = 40,
                b = 40,
                l = 40
              )
            ) %>%
            config(displayModeBar = FALSE)
        )
      }
      
    }
    
    # Add current curve
    if (show_current_curve &&
        !is.null(plot_data) && nrow(plot_data) > 0) {
      p <- p %>% add_trace(
        data = plot_data,
        x = ~ x_value,
        y = ~ global_pos,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#662260", width = 3),
        marker = list(color = "#662260", size = 7),
        hoverinfo = "text",
        text = ~ paste0(
          "Current<br>Attempts: ",
          n,
          "<br>Global PoS: ",
          sprintf("%.1f%%", global_pos * 100),
          "<br>Cost: $",
          format(round(x_value, 1), big.mark = ","),
          substr(unit_label, 2, 2)
        ),
        name = "Current"
      )
      
      # Current selection point
      current_row <- plot_data[plot_data$n == results$n_firms, , drop = FALSE]
      if (nrow(current_row) == 1) {
        p <- p %>% add_markers(
          data = current_row,
          x = ~ x_value,
          y = ~ global_pos,
          marker = list(
            color = "#FFFFFF",
            size = 14,
            line = list(color = "#662260", width = 3)
          ),
          name = "Current selection",
          hoverinfo = "text",
          text = paste0(
            "Current selection<br>Firms: ",
            results$n_firms,
            "<br>Global PoS: ",
            sprintf("%.1f%%", current_row$global_pos * 100),
            "<br>Cost: $",
            format(round(current_row$x_value, 1), big.mark = ","),
            substr(unit_label, 2, 2)
          )
        )
        
      }
    }
    
    # Add saved archetypes - each plotted in its own mechanism's units
    if (!is.null(input$selected_archetypes) &&
        length(input$selected_archetypes) > 0) {
      colors <- c("#662260",
                  "#58595B",
                  "#FFB52C",
                  "#DE7C5A",
                  "#8DB790",
                  "#F2D7EE",
                  "#D3D4D9")
      selected_archetypes <- input$selected_archetypes[1:min(7, length(input$selected_archetypes))]
      
      for (i in seq_along(selected_archetypes)) {
        name <- selected_archetypes[i]
        arch <- saved_archetypes$data[[name]]
        if (is.null(arch))
          next
        
        arch_res <- arch$results
        if (is.null(arch_res$prob_of_success) ||
            is.null(arch_res$cost_to_attempt))
          next
        
        arch_prob <- arch_res$prob_of_success
        arch_cost <- arch_res$cost_to_attempt
        arch_use_feas <- arch_res$use_feasibility
        # The below is from 0 to 100
        arch_eta <- arch_res$eta
        arch_mech <- arch_res$mechanisms
        
        # Convert to proportion
        arch_eta_prop = arch_eta 
        
        arch_max_achievable <- if (arch_use_feas)
          arch_eta_prop
        else
          1
        y_max <- max(y_max, arch_max_achievable)
        
        arch_target_prob <- arch_max_achievable * 0.99
        if (arch_prob < 1) {
          arch_n_max <- ceiling(log(1 - arch_target_prob) / log(1 - arch_prob))
        } else {
          arch_n_max <- 1
        }
        arch_n_max <- max(arch_n_max, arch_res$n_firms + 5)
        arch_n_max <- min(arch_n_max, 100)
        arch_n_values <- 1:arch_n_max
        
        # Plot archetype in shared prize equivalent (for cross-mechanism comparison)
        arch_data <- data.frame(n = arch_n_values) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            base_global_pos = 1 - (1 - arch_prob)^n,
            global_pos = if (arch_use_feas)
              pmin(base_global_pos * arch_eta_prop, arch_eta_prop)
            else
              base_global_pos,
            x_value = expected_shared_prize(n, arch_prob, arch_cost)
          ) %>%
          dplyr::ungroup()
        
        col_idx <- ((i - 1) %% length(colors)) + 1
        arch_color <- colors[col_idx]
        
        is_current <- !show_current_curve &&
          abs(arch_prob - prob_of_success) < 0.0001 &&
          abs(arch_cost - cost_to_attempt) < 0.0001 &&
          arch_use_feas == use_feasibility &&
          arch_mech == mech &&
          # Both of the etas below are from 0 to 100
          (!use_feasibility || abs(arch_eta - eta) < 0.0001)
        
        display_name <- name
        is_current_display <- is_current
        
        line_style <- if (is_current_display) {
          list(color = arch_color, width = 3)
        } else {
          list(color = arch_color,
               width = 2,
               dash = "dot")
        }
        
        mode_style <- if (is_current_display)
          "lines+markers"
        else
          "lines"
        marker_style <- if (is_current_display)
          list(color = arch_color, size = 7)
        else
          list()
        
        arch_data <- arch_data %>%
          dplyr::mutate(
            hover_text = paste0(
              display_name,
              if (is_current_display)
                " (current)"
              else
                "",
              "<br>Attempts: ",
              n,
              "<br>Global PoS: ",
              sprintf("%.1f%%", global_pos * 100),
              "<br>Cost (prize equiv.): $",
              format(round(x_value, 1), big.mark = ","),
              substr(unit_label, 2, 2)
            )
          )
        
        p <- p %>% add_trace(
          data = arch_data,
          x = ~ x_value,
          y = ~ global_pos,
          type = "scatter",
          mode = mode_style,
          line = line_style,
          marker = marker_style,
          hoverinfo = "text",
          text = ~ hover_text,
          name = if (is_current_display)
            paste(display_name, "(current)")
          else
            display_name,
          showlegend = TRUE
        )
        
        arch_point <- arch_data[arch_data$n == arch_res$n_firms, , drop = FALSE]
        if (nrow(arch_point) == 1) {
          p <- p %>% add_markers(
            data = arch_point,
            x = ~ x_value,
            y = ~ global_pos,
            marker = list(
              color = if (is_current_display)
                "#FFFFFF"
              else
                arch_color,
              size = if (is_current_display)
                14
              else
                10,
              symbol = if (is_current_display)
                "circle"
              else
                "diamond",
              line = list(
                color = if (is_current_display)
                  arch_color
                else
                  arch_color,
                width = if (is_current_display)
                  3
                else
                  0
              )
            ),
            name = paste(display_name, "selection"),
            hoverinfo = "text",
            text = paste0(
              display_name,
              if (is_current_display)
                " (current)"
              else
                "",
              " – selection<br>",
              "Firms: ",
              arch_res$n_firms,
              "<br>Global PoS: ",
              sprintf("%.1f%%", arch_point$global_pos * 100),
              "<br>Cost (prize equiv.): $",
              format(round(arch_point$x_value[1], 1), big.mark = ","),
              substr(unit_label, 2, 2)
            ),
            showlegend = FALSE
          )
        }
      }
    }
    
    y_max <- y_max * 1.02
    
    p %>%
      layout(
        xaxis = list(
          title = list(
            text = paste0("Size of pull mechanism, ", unit_label),
            standoff = 20,
            font = list(
              family = "Proxima Nova",
              size = 14,
              color = "#58595B"
            )
          ),
          gridcolor = "#D3D4D9",
          tickformat = ",.0f"
        ),
        yaxis = list(
          title = list(
            text = "Probability at least one firm succeeds",
            standoff = 15,
            font = list(
              family = "Proxima Nova",
              size = 14,
              color = "#58595B"
            )
          ),
          gridcolor = "#D3D4D9",
          tickformat = ".0%",
          range = c(0, y_max)
        ),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        font = list(
          family = "Proxima Nova, sans-serif",
          size = 12,
          color = "#58595B"
        ),
        hovermode = "closest",
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.18,
          yanchor = "top",
          bgcolor = "rgba(255,255,255,0.9)",
          borderwidth = 0,
          font = list(family = "Proxima Nova", size = 14)
        ),
        margin = list(
          t = 30,
          r = 40,
          b = 130,
          l = 60
        ),
        autosize = TRUE
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  }
  
  output$results <- renderUI({
    results <- calculated_results()
    unit_label <- get_unit_label()
    
    # ==== Get data for calculations - USE STORED TABLE DATA ====
    df <- results$table_data  # Changed from: hot_to_r(input$stages_table)
    
    # Only calculate if df exists (for first load)
    if (is.null(df)) {
      df <- DF()  # Use default data on first load
    }
    
    summary_duration <- sum(df$Duration)
    summary_cost <- sum(df$Cost)
    summary_prob <- results$prob_of_success
    summary_push <- results$push_funding
    
    # ==== INCENTIVE MECHANISMS SUMMARY (Purple Cards) ====
    
    incentive_display <- div(
      id = "incentive_summary",
      # Section Header
      div(
        h4(
          "Incentive Mechanisms Summary",
          style = "background: #662260;
            background-clip: text;
            -webkit-background-clip: text;
            -webkit-text-fill-color: transparent;
            margin-top: 0;
            margin-bottom: 15px;
            font-size: 1.7rem; font-weight: 800;"
        ),
        tags$div(style = "width: 80px; height: 3px; background: #662260; border-radius: 2px; margin-bottom: 25px;")
      ),
      
      # Advanced settings indicator if enabled
      if (results$use_feasibility) {
        div(
          style = "background: rgba(242, 215, 238, 0.3);
              padding: 12px 16px; border-radius: 10px; margin-bottom: 20px;
              border-left: 3px solid #F2D7EE;",
          p(style = "margin: 0; font-size: 13px; color: #58595B; font-weight: 500;", HTML(
            sprintf(
              "⚙️ <strong>Advanced settings enabled:</strong> Global possibility (η = %.0f%%)",
              results$eta
            )
          ))
        )
      },
      
      # Purple gradient container for the cards
      div(
        style = "background: #F2D7EE;
             padding: 30px;
             border-radius: 16px;
             box-shadow: 0 10px 40px white;
             margin-bottom: 30px;",
        
        # Container for the cards (shows all selected mechanisms)
        div(
          style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(220px, 1fr)); gap: 20px;",
          
          # CARD: NUMBER OF FIRMS (always shown)
          div(
            style = "padding: 24px;
         border-left: 4px solid #662260;
         background-color: rgba(255,255,255,0.5);
         backdrop-filter: blur(10px);
         border-radius: 12px;
         display: flex; flex-direction: column;
         transition: all 0.3s ease;
         cursor: default;",
            onmouseover = "this.style.backgroundColor='rgba(255,255,255,0.15)'; this.style.transform='translateY(-5px)';",
            onmouseout = "this.style.backgroundColor='rgba(255,255,255,0.5)'; this.style.transform='translateY(0)';",
            
            div(
              "Number of Firms Incentivized",
              style = "font-size: 11px; color: #662260;
                 text-transform: uppercase; letter-spacing: 1px;
                 margin-bottom: 12px; font-weight: 600;"
            ),
            
            div(results$n_firms, style = "font-size: 32px; font-weight: 800; color: #662260;")
          ),
          
          # CARD: GLOBAL PROBABILITY (always shown)
          div(
            style = "padding: 24px;
         border-left: 4px solid #662260;
         background-color: rgba(255,255,255,0.5);
         backdrop-filter: blur(10px);
         border-radius: 12px;
         display: flex; flex-direction: column;
         transition: all 0.3s ease;
         cursor: default;",
            onmouseover = "this.style.backgroundColor='rgba(255,255,255,0.15)'; this.style.transform='translateY(-5px)';",
            onmouseout = "this.style.backgroundColor='rgba(255,255,255,0.5)'; this.style.transform='translateY(0)';",
            
            div(
              "Probability At Least One Firm Succeeds",
              style = "font-size: 11px; color: #662260;
                 text-transform: uppercase; letter-spacing: 1px;
                 margin-bottom: 12px; font-weight: 600;"
            ),
            
            div(
              div(sprintf('%.1f%%', results$global_pos * 100), style = "font-size: 32px; font-weight: 800; color: #662260;"),
              
              # Show target if different from actual
              if (abs(results$global_pos * 100 - results$target_probability) > 0.001) {
                div(
                  sprintf('(Target: %.1f%%)', results$target_probability),
                  style = "font-size: 12px; font-weight: 500; color: #662260;
                     margin-top: 6px;"
                )
              }
            )
          ),
          
          # CARD: SHARED PRIZE (only if "shared" is selected)
          if (!is.null(results$mechanisms) &&
              'shared' %in% results$mechanisms) {
            div(
              style = "padding: 24px;
                 border-left: 4px solid #662260;
                 background-color: rgba(255,255,255,0.5);
                 backdrop-filter: blur(10px);
                 border-radius: 12px;
                 display: flex; flex-direction: column;
                 transition: all 0.3s ease;
                 cursor: default;",
              onmouseover = "this.style.backgroundColor='rgba(255,255,255,0.15)'; this.style.transform='translateY(-5px)';",
              onmouseout = "this.style.backgroundColor='rgba(255,255,255,0.5)'; this.style.transform='translateY(0)';",
              
              div("Prize", style = "font-size: 11px; color: #662260; text-transform: uppercase; letter-spacing: 1px;
                    margin-bottom: 12px; font-weight: 600;"),
              
              # Show post-revenue amount (net) as main number if modeling revenue
              div(if (!is.null(results$model_revenue) &&
                      results$model_revenue &&
                      results$revenue_pv > 0) {
                sprintf("$%s%s",
                        format(round(
                          max(0, results$shared_prize - results$revenue_pv), 1
                        ), big.mark = ","),
                        substr(unit_label, 2, 2))
              } else {
                sprintf("$%s%s",
                        format(round(results$shared_prize, 1), big.mark = ","),
                        substr(unit_label, 2, 2))
              }, style = "font-size: 32px; font-weight: 800; color: #662260;"),
              
              # Expected shared prize (smaller text below)
              div(if (!is.null(results$model_revenue) &&
                      results$model_revenue &&
                      results$revenue_pv > 0) {
                # Calculate expected from NET prize (after revenue), not gross
                net_prize <- max(0, results$shared_prize - results$revenue_pv)
                expected_net_prize <- net_prize * results$global_pos
                #sprintf("Expected: $%s%s", format(round(expected_net_prize, 1), big.mark=","), substr(unit_label, 2, 2))
              } else {
                #sprintf("Expected: $%s%s", format(round(results$expected_shared_prize, 1), big.mark=","), substr(unit_label, 2, 2))
              }, style = "font-size: 14px; font-weight: 500; color: #662260; margin-top: 8px;")
            )
          },
          
          # CARD: AMC (only if "amc" is selected)
          if (!is.null(results$mechanisms) &&
              'amc' %in% results$mechanisms) {
            amc <- results$amc
            has_error <- !is.null(amc) &&
              (!is.na(amc$covers_prize) && !amc$covers_prize)
            
            # FOR UNITS_TO_TOPUP MODE: Calculate the exact value shown in scenarios table
            displayed_amc_value <- NULL
            displayed_topup <- NULL
            if (!is.null(input$amc_calculation_mode) &&
                input$amc_calculation_mode == "units_to_topup" &&
                !is.null(input$amc_target_units) &&
                !is.na(input$amc_target_units)) {
              # Use the EXACT same calculation as scenarios table
              ac_template <- adoption_curve()
              unit_factor <- switch(
                results$cost_unit,
                "M" = 1e6,
                "K" = 1e3,
                "B" = 1e9,
                "1" = 1
              )
              #target_amount <- expected_shared_prize(results$n_firms, results$prob_of_success, results$net_cost_to_attempt)
              
              target_amount <- expected_shared_prize(
                results$n_firms,
                results$prob_of_success,
                results$net_cost_to_attempt  # This already has revenue subtracted
              )
              
              # Calculate for user's exact units (same as scenarios table)
              topup_result <- calculate_topup_from_units(
                target_units = input$amc_target_units,
                ac_template = ac_template,
                discount_rate = input$discount_rate,
                # NOT divided by 100
                unit_factor = unit_factor,
                target_amount = target_amount,
                # I changed the 12 out of the below once....
                revenue_start_month = if (!is.null(results$revenue_start_month))
                  results$revenue_start_month
                else
                  0  # Convert years to months
              )
              
              if (topup_result$feasible) {
                displayed_amc_value <- (topup_result$topup * topup_result$units_covered) / unit_factor
                displayed_topup <- topup_result$topup
              }
            }
            
            div(
              style = paste0(
                "padding: 24px;
               border-left: 4px solid ",
                if (has_error)
                  "rgba(239, 68, 68, 0.8)"
                else
                  "#662260",
                ";
               background-color: ",
                if (has_error)
                  "rgba(239, 68, 68, 0.15)"
                else
                  "rgba(255,255,255,0.5)",
                ";
               backdrop-filter: blur(10px);
               border-radius: 12px;
               display: flex; flex-direction: column;
               transition: all 0.3s ease;
               cursor: default;"
              ),
              onmouseover = if (has_error) {
                "this.style.backgroundColor='rgba(239, 68, 68, 0.15)'; this.style.transform='translateY(-5px)';"
              } else {
                "this.style.backgroundColor='rgba(255,255,255,0.15)'; this.style.transform='translateY(-5px)';"
              },
              onmouseout = if (has_error) {
                "this.style.backgroundColor='rgba(239, 68, 68, 0.5)'; this.style.transform='translateY(0)';"
              } else {
                "this.style.backgroundColor='rgba(255,255,255,0.5)'; this.style.transform='translateY(0)';"
              },
              
              div(
                "AMC Amount",
                style = "font-size: 11px; color: #662260; text-transform: uppercase;
                  letter-spacing: 1px;margin-bottom: 12px; font-weight: 600;"
              ),
              
              if (!is.null(amc)) {
                div(
                  div(if (has_error) {
                    "—"
                  }  else if (!is.null(displayed_amc_value)) {
                    # USE THE SCENARIOS TABLE VALUE
                    sprintf(
                      "$%s%s",
                      format(
                        round(displayed_amc_value, 1),
                        big.mark = ",",
                        scientific = FALSE
                      ),
                      substr(unit_label, 2, 2)
                    )
                  } else {
                    # This should match the table: topup * units_covered in the right units
                    sprintf(
                      "$%s%s",
                      if (is.infinite(amc$amc_size_undiscounted))
                        "∞"
                      else
                        format(
                          round(amc$amc_size_undiscounted, 1),
                          big.mark = ",",
                          scientific = FALSE
                        ),
                      substr(unit_label, 2, 2)
                    )
                  }, style = "font-size: 32px; font-weight: 800; color: #662260;"),
                  
                  # Show PV (present value at product launch) for reference
                  if (!has_error &&
                      !is.null(amc$amc_size_pv) &&
                      !is.na(amc$amc_size_pv) &&
                      !is.infinite(amc$amc_size_pv)) {
                    div(sprintf(
                      "(PV at launch: $%s%s)",
                      format(round(amc$amc_size_pv, 1), big.mark = ","),
                      substr(unit_label, 2, 2)
                    ),
                    style = "font-size: 11px; font-weight: 500; color: #662260; margin-top: 4px; font-style: italic;")
                  },
                  
                  # Covers units (smaller text below)
                  div(if (has_error) {
                    "Covers — units"
                  } else {
                    sprintf(
                      "Covers %s units",
                      if (is.infinite(amc$units_to_cover_total))
                        "∞"
                      else
                        format(
                          round(amc$units_to_cover_total, 0),
                          big.mark = ",",
                          scientific = FALSE
                        )
                    )
                  }, style = "font-size: 13px; font-weight: 500; color: #662260;margin-top: 8px;"),
                  
                  # Top-up info (if available)
                  if (!is.null(amc$topup) && amc$topup > 0) {
                    is_calculated <- !is.null(amc$optimization_mode) &&
                      amc$optimization_mode == "units_to_topup"
                    topup_to_show <- if (!is.null(displayed_topup))
                      displayed_topup
                    else
                      amc$topup  # ← USE DISPLAYED VALUE
                    div(sprintf(
                      "Top-up: $%s/unit%s",
                      format(round(topup_to_show, 1), big.mark = ","),
                      if (is_calculated)
                        " (calculated)"
                      else
                        ""
                    ),
                    style = "font-size: 12px; font-weight: 500; color: #662260;margin-top: 4px; font-style: italic;")
                  },
                  
                  # Error indicator
                  if (has_error) {
                    div(
                      "⚠️ Does not cover costs",
                      style = "font-size: 11px; font-weight: 600; color: #662260;
                       margin-top: 8px; text-transform: uppercase; letter-spacing: 0.5px;"
                    )
                  }
                )
              } else {
                div("AMC parameters required", style = "font-size: 14px; color: black;")
              }
            )
          },
          
          # CARD: MILESTONES + AMC (only if "milestones_amc" is selected)
          if (!is.null(results$mechanisms) &&
              'milestones_amc' %in% results$mechanisms) {
            ms_data <- results$milestones
            has_error <- is.null(ms_data) || !ms_data$valid
            
            # Check if AMC has an error
            has_amc_error <- !has_error &&
              !is.null(ms_data$amc_component) &&
              !is.na(ms_data$amc_component$amc_covers_target) &&
              !ms_data$amc_component$amc_covers_target
            
            #FOR UNITS_TO_TOPUP MODE: Calculate the exact value shown in scenarios table
            displayed_milestone_amc_value <- NULL
            displayed_milestone_topup <- NULL
            if (!is.null(input$amc_calculation_mode) &&
                input$amc_calculation_mode == "units_to_topup" &&
                !is.null(input$amc_target_units) &&
                !is.na(input$amc_target_units) &&
                !has_error) {
              # Use the EXACT same calculation as scenarios table
              ac_template <- adoption_curve()
              unit_factor <- switch(
                results$cost_unit,
                "M" = 1e6,
                "K" = 1e3,
                "B" = 1e9,
                "1" = 1
              )
              
              # For milestones+AMC, the target is the AMC component only
              target_amount <- ms_data$amc_component$amc_target_amount
              
              # Calculate for user's exact units (same as scenarios table)
              topup_result <- calculate_topup_from_units(
                target_units = input$amc_target_units,
                ac_template = ac_template,
                discount_rate = input$discount_rate,
                # NOT divided by 100
                unit_factor = unit_factor,
                target_amount = target_amount,
                revenue_start_month = if (!is.null(results$revenue_start_month))
                  results$revenue_start_month
                else
                  0
              )
              
              if (topup_result$feasible) {
                displayed_milestone_amc_value <- (topup_result$topup * topup_result$units_covered) / unit_factor
                displayed_milestone_topup <- topup_result$topup
              }
            }
            
            div(
              style = paste0(
                "padding: 24px;
                 border-left: 4px solid ",
                if (has_error ||
                    has_amc_error)
                  "rgba(239, 68, 68, 0.8)"
                else
                  "#662260",
                ";
                 background-color: ",
                if (has_error ||
                    has_amc_error)
                  "rgba(239, 68, 68, 0.5)"
                else
                  "rgba(255,255,255,0.5)",
                ";
                 backdrop-filter: blur(10px);
                 border-radius: 12px;
                 display: flex; flex-direction: column;
                 transition: all 0.3s ease;
                 cursor: default;"
              ),
              onmouseover = if (has_error || has_amc_error) {
                "this.style.backgroundColor='rgba(239, 68, 68, 0.15)'; this.style.transform='translateY(-5px)';"
              } else {
                "this.style.backgroundColor='rgba(255,255,255,0.15)'; this.style.transform='translateY(-5px)';"
              },
              onmouseout = if (has_error || has_amc_error) {
                "this.style.backgroundColor='rgba(239, 68, 68, 0.5)'; this.style.transform='translateY(0)';"
              } else {
                "this.style.backgroundColor='rgba(255,255,255,0.5)'; this.style.transform='translateY(0)';"
              },
              
              div(
                "Milestones + AMC",
                style = "font-size: 11px; color: #662260;
                   text-transform: uppercase; letter-spacing: 1px;
                   margin-bottom: 12px; font-weight: 600;"
              ),
              
              if (!is.null(ms_data) && !has_error) {
                div(
                  # Total amount - show hyphen if AMC error
                  div(if (has_amc_error) {
                    "—"
                  } else {
                    # Calculate actual total: milestones + actual AMC
                    amc_amount <- if (!is.null(displayed_milestone_amc_value)) {
                      displayed_milestone_amc_value
                    } else if (!is.null(ms_data$amc_component$amc_covers_target) &&
                               !is.na(ms_data$amc_component$amc_covers_target) &&
                               ms_data$amc_component$amc_covers_target) {
                      ms_data$amc_component$amc_size_undiscounted
                    } else {
                      ms_data$amc_component$amc_payment  # Fallback to theoretical
                    }
                    
                    actual_total <- ms_data$total_milestone_payments + amc_amount
                    
                    sprintf("$%s%s",
                            format(round(actual_total, 1), big.mark = ","),
                            substr(unit_label, 2, 2))
                  }, style = "font-size: 32px; font-weight: 800; color: #662260;"),
                  
                  # Breakdown - show hyphen for AMC if error
                  if (!has_amc_error) {
                    amc_amount <- if (!is.null(displayed_milestone_amc_value)) {
                      displayed_milestone_amc_value
                    } else if (!is.null(ms_data$amc_component$amc_covers_target) &&
                               !is.na(ms_data$amc_component$amc_covers_target) &&
                               ms_data$amc_component$amc_covers_target) {
                      ms_data$amc_component$amc_size_undiscounted
                    } else {
                      ms_data$amc_component$amc_payment
                    }
                    div(
                      sprintf(
                        "Milestones: $%s%s | AMC: $%s%s",
                        format(
                          round(ms_data$total_milestone_payments, 1),
                          big.mark = ","
                        ),
                        substr(unit_label, 2, 2),
                        format(round(amc_amount, 1), big.mark = ","),
                        substr(unit_label, 2, 2)
                      ),
                      style = "font-size: 12px; font-weight: 500; color: #662260; margin-top: 8px;"
                    )
                  } else {
                    div(sprintf(
                      "Milestones: $%s%s | AMC: —",
                      format(
                        round(ms_data$total_milestone_payments, 1),
                        big.mark = ","
                      ),
                      substr(unit_label, 2, 2)
                    ),
                    style = "font-size: 12px; font-weight: 500; color: #662260; margin-top: 8px;")
                  },
                  # Milestone info - only show if no error
                  if (!has_amc_error) {
                    # Use the SAME amc_amount calculation as the main display above
                    amc_amount <- if (!is.null(displayed_milestone_amc_value)) {
                      displayed_milestone_amc_value  # Use the recalculated value
                    } else if (!is.null(ms_data$amc_component$amc_covers_target) &&
                               !is.na(ms_data$amc_component$amc_covers_target) &&
                               ms_data$amc_component$amc_covers_target) {
                      ms_data$amc_component$amc_size_undiscounted
                    } else {
                      ms_data$amc_component$amc_payment
                    }
                    actual_total <- ms_data$total_milestone_payments + amc_amount
                    
                    div(
                      sprintf(
                        "%d milestone%s (%.0f%% of total)",
                        ms_data$n_milestones,
                        if (ms_data$n_milestones != 1)
                          "s"
                        else
                          "",
                        (ms_data$total_milestone_payments / actual_total) * 100
                      ),
                      style = "font-size: 12px; font-weight: 500; color: #662260;margin-top: 4px; font-style: italic;"
                    )
                  },
                  
                  # Warning if AMC doesn't cover
                  if (has_amc_error) {
                    div(
                      "⚠️ AMC does not cover target",
                      style = "font-size: 11px; font-weight: 600; color: #662260;
                     margin-top: 8px; text-transform: uppercase; letter-spacing: 0.5px;"
                    )
                  }
                )
              } else {
                div(
                  div(
                    if (has_error)
                      "Invalid configuration"
                    else
                      "Milestones required",
                    style = "font-size: 14px; color: rgba(255,255,255,0.8);"
                  ),
                  if (has_error && !is.null(ms_data$error)) {
                    div(ms_data$error, style = "font-size: 11px; font-weight: 600; color: #662260;
                     margin-top: 8px;")
                  }
                )
              }
            )
          }
          
          
        )
        
      )
    )
    
    
    # ==== COMMERCIAL VIABILITY WARNING (if revenue eliminates need for incentive) ====
    commercial_viability_warning <- NULL
    if (!is.null(results$commercially_viable) &&
        results$commercially_viable) {
      commercial_viability_warning <- div(
        style = "background: linear-gradient(135deg, rgba(141, 183, 144, 0.25), rgba(141, 183, 144, 0.15));
         border: 3px solid #8DB790;
         padding: 30px 40px;
         border-radius: 16px;
         margin-top: 20px;
         margin-bottom: 30px;
         box-shadow: 0 8px 30px rgba(141, 183, 144, 0.3);",
        
        div(
          style = "display: flex; align-items: center; gap: 20px; margin-bottom: 20px;",
          div(style = "font-size: 64px; line-height: 1;", "✓"),
          div(
            div(style = "font-size: 32px; font-weight: 800; color: #065f46; line-height: 1.2; margin-bottom: 8px;", "Commercially Viable!"),
            div(style = "font-size: 18px; font-weight: 600; color: #047857;", "Expected revenue covers all development costs")
          )
        ),
        
        div(
          style = "background: rgba(255, 255, 255, 0.7); padding: 20px 24px; border-radius: 12px; border-left: 4px solid #059669;",
          p(style = "color: #065f46; font-size: 15px; line-height: 1.7; margin: 0; font-weight: 500;", HTML(
            sprintf(
              "The expected private revenue (PV: <strong>$%s%s</strong>) is sufficient to cover all development costs (<strong>$%s%s</strong>). This is a commercially profitable enterprise that <strong>does not require public financial support or incentive payments</strong>. The innovation can be pursued through standard private investment and market mechanisms.",
              format(round(results$revenue_pv, 1), big.mark = ","),
              substr(get_unit_label(), 2, 2),
              format(round(results$cost_to_attempt, 1), big.mark = ","),
              substr(get_unit_label(), 2, 2)
            )
          ))
        )
      )
    }
    # ==== AMC WARNING (if applicable) ====
    amc_warning <- NULL
    if (!is.null(results$mechanisms) &&
        results$mechanisms == 'amc') {
      if (!is.null(results$amc) && !results$amc$covers_prize) {
        amc_warning <- div(class = "warning-box",
                           style = "margin-top: 20px; margin-bottom: 20px;",
                           p(
                             class = "warning-text",
                             HTML(
                               "⚠️ <strong>AMC Warning:</strong> The AMC will never cover the prize cost with the current parameters. The cumulative subsidy payments over the product lifecycle do not reach the prize amount. Consider increasing the top-up amount, lifecycle duration, or uptake rate."
                             )
                           ))
      }
    }
    
    # ==== AMC SCENARIOS TABLE (if units_to_topup mode) ====
    amc_scenarios_display <- NULL
    
    if (!is.null(results$mechanisms) &&
        (results$mechanisms == 'amc' ||
         results$mechanisms == 'milestones_amc') &&
        !is.null(input$amc_calculation_mode) &&
        input$amc_calculation_mode == "units_to_topup") {
      # Calculate scenarios
      ac_template <- adoption_curve()
      unit_factor <- switch(
        results$cost_unit,
        "M" = 1e6,
        "K" = 1e3,
        "B" = 1e9,
        "1" = 1
      )
      
      # Get target amount based on mechanism
      if (results$mechanisms == "amc") {
        # Use net cost to attempt (already adjusted for revenue)
        target_amount <- expected_shared_prize(results$n_firms,
                                               results$prob_of_success,
                                               results$net_cost_to_attempt)
      } else if (results$mechanisms == "milestones_amc") {
        # For milestones+AMC, use the AMC component target (net incentive minus milestones)
        if (!is.null(results$milestones) &&
            results$milestones$valid) {
          target_amount <- results$milestones$amc_component$amc_target_amount  # THIS IS THE KEY LINE
          print(paste0("Target amount", target_amount))
          print(results$milestones)
        } else {
          target_amount <- 0
        }
      }
      total_units_available <- sum(ac_template$units)
      
      # Generate default scenarios at different percentages
      percentages <- seq(0.2, 1.0, length.out = 5)
      
      # Always add user's selection if it exists
      user_units <- input$amc_target_units
      ac_units_available <- sum(ac_template$units)  # Use actual curve units
      
      if (!is.null(user_units) &&
          !is.na(user_units) && user_units > 0) {
        user_pct <- user_units / ac_units_available  # Use ac_units, not total_units_available
        
        if (user_pct > 0 && user_pct <= 1) {
          is_duplicate <- any(abs(percentages - user_pct) < 0.01)
          if (!is_duplicate) {
            percentages <- sort(c(percentages, user_pct))
          }
        }
      }
      
      scenarios <- lapply(percentages, function(pct) {
        target_units <- round(total_units_available * pct, 0)
        
        result <- calculate_topup_from_units(
          target_units = target_units,
          ac_template = ac_template,
          discount_rate = input$discount_rate,
          unit_factor = unit_factor,
          target_amount = target_amount,
          revenue_start_month = if (!is.null(results$revenue_start_month))
            results$revenue_start_month
          else
            0
        )
        
        if (result$feasible) {
          list(
            coverage_pct = pct * 100,
            target_units = target_units,
            units_covered = result$units_covered,
            topup = result$topup,
            total_amc = result$topup * result$units_covered,
            feasible = TRUE,
            is_custom = !is.null(user_units) &&
              !is.na(user_units) &&
              abs(target_units - user_units) < (total_units_available * 0.01)
          )
        } else {
          list(
            coverage_pct = pct * 100,
            target_units = target_units,
            feasible = FALSE,
            error = result$error
          )
        }
      })
      
      # Filter to only feasible scenarios
      scenarios <- Filter(function(s)
        s$feasible, scenarios)
      
      if (!is.null(scenarios) && length(scenarios) > 0) {
        amc_scenarios_display <- div(
          style = "margin-top: 25px; margin-bottom: 25px;",
          
          # Section Header
          div(
            h4(
              "AMC Coverage Scenarios",
              style = "background: #662260;
                    background-clip: text;
                    -webkit-background-clip: text;
                    -webkit-text-fill-color: transparent;
                    margin-top: 0;
                    margin-bottom: 15px;
                    font-size: 1.5rem; font-weight: 800;"
            ),
            tags$div(style = "width: 80px; height: 3px; background: #662260; border-radius: 2px; margin-bottom: 15px;"),
            p(
              style = "color: #58595B; font-size: 13px; margin-bottom: 20px; font-style: italic;",
              sprintf(
                "Different coverage levels and their required subsidy amounts (total lifecycle capacity: %s units):",
                format(
                  round(total_units_available, 0),
                  big.mark = ",",
                  scientific = FALSE
                )
              )
            )
          ),
          
          # Scenarios table
          div(
            style = "background: white; border-radius: 12px; overflow: hidden;
                 box-shadow: 0 4px 20px rgba(0,0,0,0.08);",
            
            # Header row
            div(
              style = "display: grid; grid-template-columns: 1fr 1fr 1fr 1fr;
                   background: #662260; color: white; padding: 16px 20px;
                   font-weight: 700; font-size: 13px; text-transform: uppercase;
                   letter-spacing: 0.5px;",
              div("Coverage", style = "text-align: left;"),
              div("Units Covered", style = "text-align: right;"),
              div("Top-up per Unit", style = "text-align: right;"),
              div("Total AMC", style = "text-align: right;")
            ),
            
            # Data rows
            lapply(seq_along(scenarios), function(i) {
              s <- scenarios[[i]]
              
              # Check if this is the current selection
              is_current <- FALSE
              is_custom <- FALSE
              
              if (!is.null(user_units) &&
                  !is.na(user_units) &&
                  !is.null(s$units_covered) &&
                  !is.na(s$units_covered)) {
                is_current <- abs(s$units_covered - user_units) < max(1, total_units_available * 0.01)
              }
              
              # Check if this is a custom (non-standard) percentage
              standard_pcts <- seq(20, 100, length.out = 5)
              is_custom <- !any(abs(standard_pcts - s$coverage_pct) < 1)
              
              # Calculate actual AMC size (should match what's in the purple card)
              # This is the undiscounted total AMC commitment
              amc_total_displayed <- s$topup * s$units_covered / unit_factor
              
              div(
                style = paste0(
                  "display: grid; grid-template-columns: 1fr 1fr 1fr 1fr; ",
                  "padding: 16px 20px; border-bottom: 1px solid #e2e8f0; ",
                  "transition: background 0.2s ease; ",
                  if (is_current)
                    "background: rgba(102, 34, 96, 0.08); border-left: 4px solid #662260; font-weight: 600;"
                  else if (is_custom)
                    "background: rgba(251, 191, 36, 0.08); border-left: 4px solid #f59e0b;"
                  else if (i %% 2 == 0)
                    "background: rgba(0,0,0,0.02);"
                  else
                    ""
                ),
                onmouseover = if (!is_current)
                  "this.style.background='rgba(102, 34, 96, 0.05)';"
                else
                  "",
                onmouseout = if (!is_current) {
                  if (is_custom)
                    "this.style.background='rgba(251, 191, 36, 0.08)';"
                  else if (i %% 2 == 0)
                    "this.style.background='rgba(0,0,0,0.02)';"
                  else
                    "this.style.background='white';"
                } else
                  "",
                
                div(
                  style = "text-align: left; font-weight: 600; color: #662260;",
                  sprintf("%.1f%%", s$coverage_pct),
                  if (is_custom) {
                    span(" (custom)", style = "color: #92400e; font-size: 11px; font-weight: 500; margin-left: 5px;")
                  },
                  if (is_current) {
                    span(" ←", style = "color: #662260; font-weight: 800; margin-left: 8px; font-size: 16px;")
                  }
                ),
                div(
                  style = "text-align: right; color: #2d3748; font-weight: 500;",
                  format(
                    round(s$units_covered, 0),
                    big.mark = ",",
                    scientific = FALSE
                  )
                ),
                div(style = "text-align: right; color: #2d3748; font-weight: 600;", sprintf(
                  "$%s",
                  format(
                    round(s$topup, 1),
                    big.mark = ",",
                    scientific = FALSE
                  )
                )),
                div(style = "text-align: right; color: #2d3748; font-weight: 700;", sprintf(
                  "$%s%s",
                  format(
                    round(amc_total_displayed, 1),
                    big.mark = ",",
                    scientific = FALSE
                  ),
                  substr(unit_label, 2, 2)
                ))
              )
            })
          ),
          
          # Footnote
          div(
            style = "margin-top: 12px; padding: 12px 16px;
                 background: rgba(102, 34, 96, 0.05); border-radius: 8px;
                 border-left: 3px solid #662260;",
            p(
              style = "margin: 0; font-size: 12px; color: #58595B; font-style: italic;",
              "Coverage percentages represent different proportions of the total product lifecycle capacity. ",
              if (!is.null(user_units) && !is.na(user_units)) {
                "Your custom selection is highlighted with a yellow background and marked as (custom). "
              },
              "The arrow (←) indicates your current selection."
            )
          )
        )
      } else {
        # Show error if no scenarios could be calculated
        if (!is.null(results$amc) && !is.null(results$amc$error)) {
          amc_scenarios_display <- div(
            style = "margin-top: 25px; padding: 20px; background: rgba(239, 68, 68, 0.1);
                 border-left: 4px solid #ef4444; border-radius: 8px;",
            p(
              style = "color: #991b1b; margin: 0; font-weight: 600;",
              "⚠️ Cannot calculate scenarios: ",
              results$amc$error
            )
          )
        }
      }
    }
    
    
    # ==== Milestones + AMC WARNING (if applicable) ====
    milestones_warning <- NULL
    if (!is.null(results$mechanisms) &&
        results$mechanisms == 'milestones_amc') {
      if (!is.null(results$milestones)) {
        if (!results$milestones$valid) {
          milestones_warning <- div(class = "warning-box",
                                    style = "margin-top: 20px; margin-bottom: 20px;",
                                    p(class = "warning-text", HTML(
                                      sprintf(
                                        "⚠️ <strong>Milestones + AMC Warning:</strong> %s",
                                        results$milestones$error
                                      )
                                    )))
        } else if (!is.null(results$milestones$amc_component) &&
                   !is.na(results$milestones$amc_component$amc_covers_target) &&
                   !results$milestones$amc_component$amc_covers_target) {
          milestones_warning <- div(class = "warning-box",
                                    style = "margin-top: 20px; margin-bottom: 20px;",
                                    p(
                                      class = "warning-text",
                                      HTML(
                                        "⚠️ <strong>Milestones + AMC Warning:</strong> The AMC component will never cover its target amount with the current parameters. The cumulative subsidy payments over the product lifecycle do not reach the required AMC portion. Consider increasing the top-up amount, lifecycle duration, or uptake rate."
                                      )
                                    ))
        }
      }
    }
    
    # ==== MILESTONE SUMMARY (if enabled) ====
    milestone_display <- NULL
    
    
    # CALCULATE DISPLAYED VALUES FOR MILESTONES+AMC (for both milestone panel and purple card)
    displayed_milestone_amc_value <- NULL
    displayed_milestone_topup <- NULL
    
    if (!is.null(results$mechanisms) &&
        'milestones_amc' %in% results$mechanisms &&
        !is.null(results$milestones) && results$milestones$valid &&
        !is.null(input$amc_calculation_mode) &&
        input$amc_calculation_mode == "units_to_topup" &&
        !is.null(input$amc_target_units) &&
        !is.na(input$amc_target_units)) {
      ms_data <- results$milestones
      
      # Use the EXACT same calculation as scenarios table
      ac_template <- adoption_curve()
      unit_factor <- switch(
        results$cost_unit,
        "M" = 1e6,
        "K" = 1e3,
        "B" = 1e9,
        "1" = 1
      )
      
      # For milestones+AMC, the target is the AMC component only
      target_amount <- ms_data$amc_component$amc_target_amount
      
      # Calculate for user's exact units (same as scenarios table)
      topup_result <- calculate_topup_from_units(
        target_units = input$amc_target_units,
        ac_template = ac_template,
        discount_rate = input$discount_rate,
        # NOT divided by 100
        unit_factor = unit_factor,
        target_amount = target_amount,
        revenue_start_month = if (!is.null(results$revenue_start_month))
          results$revenue_start_month
        else
          0
      )
      
      if (topup_result$feasible) {
        displayed_milestone_amc_value <- (topup_result$topup * topup_result$units_covered) / unit_factor
        displayed_milestone_topup <- topup_result$topup
      }
    }
    
    
    if (!is.null(results$milestones) && results$milestones$valid) {
      ms <- results$milestones
      
      milestone_display <- div(
        style = "margin-top: 25px; margin-bottom: 25px;",
        
        # Section Header
        div(
          h4(
            "Milestone Payment Structure",
            style = "background: #662260;
            background-clip: text;
            -webkit-background-clip: text;
            -webkit-text-fill-color: transparent;
            margin-top: 0;
            margin-bottom: 15px;
            font-size: 1.7rem; font-weight: 800;"
          ),
          tags$div(style = "width: 80px; height: 3px; background: #662260; border-radius: 2px; margin-bottom: 20px;")
        ),
        
        # Pink gradient container
        div(
          style = "background: #662260;
             padding: 25px 30px;
             border-radius: 16px;
             box-shadow: 0 10px 40px white;
             margin-bottom: 20px;",
          
          # Each milestone + AMC
          div(
            style = "display: flex; flex-direction: column; gap: 15px;",
            
            # Milestones
            lapply(ms$milestones, function(m) {
              div(
                style = "background-color: rgba(255,255,255,0.15);
       padding: 15px 20px;
       border-radius: 10px;
       border-left: 4px solid white;
       transition: all 0.3s ease;
       cursor: pointer;",
                onmouseover = "this.style.transform='translateX(8px)'; this.style.boxShadow='0 8px 30px rgba(0,0,0,0.3)'; this.style.backgroundColor='rgba(255,255,255,0.25)';",
                onmouseout = "this.style.transform='translateX(0)'; this.style.boxShadow='none'; this.style.backgroundColor='rgba(255,255,255,0.15)';",
                
                div(
                  style = "display: flex; justify-content: space-between; align-items: center; color: white",
                  
                  # Left: Stage info
                  div(
                    div(sprintf(
                      "Stage %d: %s", m$stage, m$stage_name
                    ), style = "font-size: 16px; font-weight: 700; margin-bottom: 5px; color: white"),
                    div(
                      sprintf(
                        "%.1f%% of total prize | Prob to stage: %.1f%%",
                        m$stage_pct,
                        m$prob_to_stage * 100
                      ),
                      style = "font-size: 12px; color: white"
                    )
                  ),
                  
                  # Right: Amount
                  div(sprintf(
                    "$%s%s",
                    format(round(m$milestone_payment, 1), big.mark =
                             ","),
                    substr(unit_label, 2, 2)
                  ), style = "font-size: 24px; font-weight: 800; color: white")
                )
              )
            }),
            
            # AMC Component (appears after all milestones)
            if (!is.null(ms$amc_component) &&
                ms$amc_component$amc_pct > 0) {
              amc_comp <- ms$amc_component
              has_amc_error <- !is.na(amc_comp$amc_covers_target) &&
                !amc_comp$amc_covers_target
              
              div(
                style = paste0(
                  "background-color: ",
                  if (has_amc_error)
                    "rgba(239, 68, 68, 0.2)"
                  else
                    "rgba(255,255,255,0.15)",
                        "; padding: 15px 20px;
                         border-radius: 10px;
                         border-left: 4px solid white;
                   transition: all 0.3s ease;
       cursor: pointer;",
                  if (has_amc_error)
                    "rgba(239, 68, 68, 0.8)"
                  else
                    "white",
                  ";"
                ), onmouseover = "this.style.transform='translateX(8px)'; this.style.boxShadow='0 8px 30px rgba(0,0,0,0.3)'; this.style.backgroundColor='rgba(255,255,255,0.25)';",
                onmouseout = "this.style.transform='translateX(0)'; this.style.boxShadow='none'; this.style.backgroundColor='rgba(255,255,255,0.15)';",
                
                div(
                  style = "display: flex; justify-content: space-between; align-items: center; color: white",
                  
                  # Left: AMC info
                  div(
                    div(
                      sprintf(
                        "After Stage %d: %s - AMC",
                        amc_comp$final_stage,
                        amc_comp$final_stage_name
                      ),
                      style = "font-size: 16px; font-weight: 700; margin-bottom: 5px; color: white"
                    ),
                    div(if (!is.na(amc_comp$amc_covers_target) &&
                            !has_amc_error) {
                      # Use displayed values if available
                      units_to_show <- if (!is.null(displayed_milestone_amc_value)) {
                        input$amc_target_units  # Show the user's target units
                      } else {
                        amc_comp$units_to_cover_total
                      }
                      
                      topup_to_show <- if (!is.null(displayed_milestone_topup)) {
                        displayed_milestone_topup
                      } else {
                        amc_comp$topup
                      }
                      
                      sprintf(
                        "%.1f%% of total prize | Covers %s units | $%s/unit top-up",
                        amc_comp$amc_pct,
                        format(
                          round(units_to_show, 0),
                          big.mark = ",",
                          scientific = FALSE
                        ),
                        format(
                          round(topup_to_show, 2),
                          big.mark = ",",
                          scientific = FALSE
                        )
                      )
                    } else if (has_amc_error) {
                      sprintf("%.1f%% of total prize | ⚠️ AMC does not cover target amount",
                              amc_comp$amc_pct)
                    } else {
                      sprintf("%.1f%% of total prize (AMC parameters needed)",
                              amc_comp$amc_pct)
                    }, style = "font-size: 12px; color: white")
                  ),
                  
                  # Right: Amount - use displayed values if available
                  div(if (has_amc_error) {
                    "—"
                  } else {
                    amc_amount <- if (!is.null(displayed_milestone_amc_value)) {
                      displayed_milestone_amc_value
                    } else if (!is.na(amc_comp$amc_covers_target) &&
                               amc_comp$amc_covers_target) {
                      amc_comp$amc_size_undiscounted
                    } else {
                      amc_comp$amc_payment
                    }
                    
                    sprintf("$%s%s",
                            format(round(amc_amount, 1), big.mark =
                                     ","),
                            substr(unit_label, 2, 2))
                  }, style = "font-size: 24px; font-weight: 800; color: white")
                )
              )
            }
          ),
          
          # Summary
          div(
            style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid rgba(255,255,255,0.3);
     display: flex; justify-content: space-between; align-items: center;",
            
            div(
              style = "color: white; font-size: 14px; font-weight: 500;",
              sprintf(
                "%d firms incentivized | %.1f%% global probability of success",
                ms$n_firms,
                ms$global_pos * 100
              )
            ),
            
            div(
              div("Total Incentive", style = "font-size: 12px; text-align: right; color: white"),
              div({
                # Calculate actual total: milestones + actual AMC amount
                has_amc_error <- !is.null(ms$amc_component) &&
                  !is.na(ms$amc_component$amc_covers_target) &&
                  !ms$amc_component$amc_covers_target
                
                if (has_amc_error) {
                  "—"
                } else {
                  # Use displayed value if available, otherwise stored value
                  amc_amount <- if (!is.null(displayed_milestone_amc_value)) {
                    displayed_milestone_amc_value
                  } else if (!is.null(ms$amc_component$amc_covers_target) &&
                             !is.na(ms$amc_component$amc_covers_target) &&
                             ms$amc_component$amc_covers_target) {
                    ms$amc_component$amc_size_undiscounted
                  } else {
                    ms$amc_component$amc_payment  # Fallback to theoretical
                  }
                  
                  actual_total <- ms$total_milestone_payments + amc_amount
                  
                  sprintf("$%s%s",
                          format(round(actual_total, 1), big.mark = ","),
                          substr(unit_label, 2, 2))
                }
              }, style = "font-size: 20px; font-weight: 800; color: white")
            )
          )
        )
      )
    }
    
    # ==== REVENUE MODELING INFO (if enabled) ====
    revenue_info_display <- NULL
    if (!is.null(input$model_revenue) &&
        input$model_revenue &&
        !is.null(results$revenue_pv) && results$revenue_pv > 0) {
      revenue_info_display <- div(
        style = "margin-top: 25px; margin-bottom: 25px;",
        
        # Section Header
        div(
          h4(
            "Revenue Impact",
            style = "background: #065f46;
        background-clip: text;
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        margin-top: 0;
        margin-bottom: 10px;
        font-size: 1.7rem; font-weight: 800;"
          ),
          tags$div(style = "width: 80px; height: 3px; background: linear-gradient(90deg, #10b981, #059669); border-radius: 2px; margin-bottom: 10px;"),
          p(style = "color: #065f46; font-size: 13px; margin-bottom: 15px; font-weight: 600; font-style: italic;", if (!is.null(results$commercially_viable) &&
                                                                                                                       results$commercially_viable) {
            "✓ Project is commercially viable! Expected revenue covers all development costs."
          } else {
            revenue_start_msg <- if (!is.null(results$revenue_start_stage) &&
                                     results$revenue_start_stage > 0) {
              df <- results$table_data
              stage_name <- df$Name[results$revenue_start_stage]
              sprintf(
                "Revenue begins after completing %s (at month %.0f). Expected revenue (probability-adjusted) reduces the net cost.",
                stage_name,
                results$revenue_start_month
              )
            } else {
              "Expected revenue (probability-adjusted) reduces the net cost that needs to be incentivized."
            }
            revenue_start_msg
          })
        ),
        
        # Green gradient container
        div(
          style = "background: #8DB790;
         padding: 25px 30px;
         border-radius: 16px;
         box-shadow: 0 10px 40px rgba(141, 183, 144, 0.3);
         margin-bottom: 20px;",
          
          div(style = "color: white; font-size: 15px; font-weight: 600; margin-bottom: 15px;", "How Revenue Reduces Required Incentive"),
          
          div(
            style = "display: grid; grid-template-columns: 1fr auto 1fr auto 1fr; gap: 15px; align-items: center; justify-content: center;",
            
            # Expected Development Costs
            div(
              style = "text-align: center; padding: 15px;
     background-color: rgba(255,255,255,0.15);
     border-radius: 10px;
     transition: all 0.3s ease;
     cursor: pointer;",
              onmouseover = "this.style.transform='scale(1.05)'; this.style.boxShadow='0 8px 30px rgba(0,0,0,0.3)'; this.style.backgroundColor='rgba(255,255,255,0.25)';",
              onmouseout = "this.style.transform='scale(1)'; this.style.boxShadow='none'; this.style.backgroundColor='rgba(255,255,255,0.15)';",
              div(
                "Expected Development Costs",
                style = "font-size: 11px; color: rgba(255,255,255,0.9);
                     text-transform: uppercase; letter-spacing: 1px; margin-bottom: 8px;"
              ),
              div(sprintf(
                "$%s%s",
                format(round(results$cost_to_attempt, 1), big.mark =
                         ","),  substr(unit_label, 2, 2)
              ), style = "font-size: 24px; font-weight: 800; color: white;")
            ),
            
            # Minus sign
            div("−", style = "font-size: 32px; font-weight: 300; color: white;"),
            
            # Expected Revenue
            div(
              style = "text-align: center; padding: 15px;
     background-color: rgba(255,255,255,0.15);
     border-radius: 10px;
     transition: all 0.3s ease;
     cursor: pointer;",
              onmouseover = "this.style.transform='scale(1.05)'; this.style.boxShadow='0 8px 30px rgba(0,0,0,0.3)'; this.style.backgroundColor='rgba(255,255,255,0.25)';",
              onmouseout = "this.style.transform='scale(1)'; this.style.boxShadow='none'; this.style.backgroundColor='rgba(255,255,255,0.15)';",
              div(
                "Expected Revenue",
                style = "font-size: 11px; color: rgba(255,255,255,0.9);
                     text-transform: uppercase; letter-spacing: 1px; margin-bottom: 4px;"
              ),
              div(
                style = "font-size: 10px; color: rgba(255,255,255,0.8); margin-bottom: 6px; font-style: italic;",
                sprintf(
                  "(%.0f%% prob × $%s%s PV)",
                  results$prob_reaching_revenue * 100,
                  format(round(results$revenue_pv_raw, 1), big.mark =
                           ","),
                  substr(unit_label, 2, 2)
                )
              ),
              div(sprintf(
                "$%s%s",
                format(round(results$revenue_pv, 1), big.mark =
                         ","),
                substr(unit_label, 2, 2)
              ), style = "font-size: 24px; font-weight: 800; color: white;")
            ),
            
            # Equals sign
            div("=", style = "font-size: 32px; font-weight: 300; color: white;"),
            
            # Net Cost
            
            div(
              style = "text-align: center; padding: 15px;
     background-color: rgba(255,255,255,0.15);
     border-radius: 10px;
      border: 2px solid white;
     transition: all 0.3s ease;
     cursor: pointer;",
              onmouseover = "this.style.transform='scale(1.05)'; this.style.boxShadow='0 8px 30px rgba(0,0,0,0.3)'; this.style.backgroundColor='rgba(255,255,255,0.25)';",
              onmouseout = "this.style.transform='scale(1)'; this.style.boxShadow='none'; this.style.backgroundColor='rgba(255,255,255,0.15)';",
              div(
                "Net Cost to Attempt",
                style = "font-size: 11px; color: white;
                     text-transform: uppercase; letter-spacing: 1px; margin-bottom: 8px; font-weight: 700;"
              ),
              div(sprintf(
                "$%s%s",
                format(round(results$net_cost_to_attempt, 1), big.mark =
                         ","),
                substr(unit_label, 2, 2)
              ), style = "font-size: 24px; font-weight: 800; color: white; text-shadow: 0 2px 8px rgba(0,0,0,0.2);")
            )
          ),
          
          # Explanation
          div(
            style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid rgba(255,255,255,0.3);
               color: rgba(255,255,255,0.95); font-size: 13px; text-align: center; font-weight: 500;",
            if (!is.null(results$commercially_viable) &&
                results$commercially_viable) {
              sprintf(
                "Expected revenue ($%s%s) fully covers expected development costs ($%s%s). No public incentive is needed.",
                format(round(results$revenue_pv, 1), big.mark =
                         ","),
                substr(unit_label, 2, 2),
                format(round(results$cost_to_attempt, 1), big.mark =
                         ","),
                substr(unit_label, 2, 2)
              )
            } else {
              sprintf(
                "Expected revenue of $%s%s (probability-adjusted) reduces net cost to $%s%s. The incentive mechanism is sized to cover this net cost.",
                format(round(results$revenue_pv, 1), big.mark =
                         ","),
                substr(unit_label, 2, 2),
                format(round(results$net_cost_to_attempt, 1), big.mark =
                         ","),
                substr(unit_label, 2, 2)
              )
            }
          )
        )
      )
    }
    
    
    # ==== INDIVIDUAL FIRM SUMMARY (Horizontal Strip Style) ====
    
    individual_firm_display <- div(
      # Section Header
      div(
        h4(
          "Individual Firm Summary",
          style = "background: #662260;
                    background-clip: text;
                    -webkit-background-clip: text;
                    -webkit-text-fill-color: transparent;
                    margin-top: 30px; margin-bottom: 15px;
                    font-size: 1.7rem; font-weight: 800;"
        ),
        tags$div(style = "width: 80px; height: 3px; background: #662260; border-radius: 2px; margin-bottom: 25px;")
      ),
      
      # Strip 1: Total Duration
      div(
        class = "firm-strip",
        onmouseover = "this.style.transform='translateX(5px)'; this.style.boxShadow='0 6px 25px rgba(102, 34, 96, 0.25)';",
        onmouseout = "this.style.transform='translateX(0)'; this.style.boxShadow='0 2px 10px rgba(0,0,0,0.05)';",
        style = "display: flex; justify-content: space-between; align-items: center;
                 background: rgba(102, 34, 96, 0.08);
                 border-radius: 12px; padding: 20px 24px;
                 font-family: 'Proxima Nova', sans-serif;
                 margin-bottom: 16px; border-left: 4px solid #662260;
                 transition: all 0.3s ease;
                 box-shadow: 0 2px 10px rgba(0,0,0,0.05);",
        
        div(style = "font-size: 17px; color: #2d3748; font-weight: 600;", "Total duration:"),
        div(
          style = "font-size: 24px; font-weight: 800;
                   background: #662260;
                   background-clip: text;
                   -webkit-background-clip: text;
                   -webkit-text-fill-color: transparent;",
          sprintf('%.1f years', summary_duration)
        )
      ),
      
      # Strip 2: Probability of Success
      div(
        class = "firm-strip",
        onmouseover = "this.style.transform='translateX(5px)'; this.style.boxShadow='0 6px 25px rgba(102, 34, 96, 0.25)';",
        onmouseout = "this.style.transform='translateX(0)'; this.style.boxShadow='0 2px 10px rgba(0,0,0,0.05)';",
        style = "display: flex; justify-content: space-between; align-items: center;
                 background: rgba(102, 34, 96, 0.08);
                 border-radius: 12px; padding: 20px 24px;
                 font-family: 'Proxima Nova', sans-serif;
                 margin-bottom: 16px; border-left: 4px solid #662260;
                 transition: all 0.3s ease;
                 box-shadow: 0 2px 10px rgba(0,0,0,0.05);",
        
        div(style = "font-size: 17px; color: #2d3748; font-weight: 600;", "Probability of success:"),
        div(
          style = "font-size: 24px; font-weight: 800;
                   background: #662260;
                   background-clip: text;
                   -webkit-background-clip: text;
                   -webkit-text-fill-color: transparent;",
          sprintf('%.1f%%', summary_prob * 100)
        )
      ),
      
      # Strip 3: Expected Cost
      div(
        class = "firm-strip",
        onmouseover = "this.style.transform='translateX(5px)'; this.style.boxShadow='0 6px 25px rgba(102, 34, 96, 0.25)';",
        onmouseout = "this.style.transform='translateX(0)'; this.style.boxShadow='0 2px 10px rgba(0,0,0,0.05)';",
        style = "display: flex; justify-content: space-between; align-items: center;
                 background: rgba(102, 34, 96, 0.08);
                 border-radius: 12px; padding: 20px 24px;
                 font-family: 'Proxima Nova', sans-serif;
                 margin-bottom: 16px; border-left: 4px solid #662260;
                 transition: all 0.3s ease;
                 box-shadow: 0 2px 10px rgba(0,0,0,0.05);",
        
        div(style = "font-size: 17px; color: #2d3748; font-weight: 600;", "Expected development costs:"),
        div(
          style = "font-size: 24px; font-weight: 800;
                   background: #662260;
                   background-clip: text;
                   -webkit-background-clip: text;
                   -webkit-text-fill-color: transparent;",
          sprintf("$%s%s", format(
            round(results$cost_to_attempt, 1), big.mark = ","
          ),  substr(unit_label, 2, 2))
        )
      )
    )
    
    # ==== PUSH FUNDING SECTION (Collapsible) ====
    
    push_funding_section <- div(
      style = "margin-top: 30px; margin-bottom: 30px;",
      
      # Button to toggle the content
      actionButton(
        "toggle_push_funding",
        "What about push funding?",
        icon = icon("chevron-down"),
        style = "background: #F2D7EE;
                               color: black;
                               border: none;
                               font-weight: 600;
                               padding: 15px 25px;
                               border-radius: 10px;
                               font-family: 'Proxima Nova', sans-serif;
                               font-size: 15px;
                               box-shadow: 0 4px 15px #F2D7EE;
                               transition: all 0.3s ease;
                               width: 100%;
                               text-align: left;"
      ),
      
      # Collapsible content
      uiOutput("push_funding_content")
    )
    
    # Save Results section
    save_section <- div(
      hr(style = "border: none; height: 2px; background: #662260; margin: 30px 0;"),
      div(
        h3("Save Results", style = "font-size: 1.6rem; color: #662260; font-weight: 700; margin-bottom: 15px;"),
        tags$div(style = "width: 100px; height: 3px; background: #662260; border-radius: 2px; margin-bottom: 20px;")
      ),
      div(
        style = "background: rgba(102, 34, 96, 0.05);
               padding: 24px; border-radius: 16px; border: 2px solid rgba(102, 34, 96, 0.1);",
        p(
          style = "color: #58595B; font-size: 13px; margin-bottom: 15px; line-height: 1.5;",
          strong("Save scenarios to compare:"),
          " Give your current calculation a name and save it. You can then modify parameters and save additional scenarios. All saved scenarios will appear below and on the chart for easy comparison."
        ),
        textInput("archetype_name", "Archetype Name:", placeholder = "e.g., Baseline Scenario"),
        actionButton(
          "save_archetype",
          "Save Current Results",
          class = "btn-block orange-button",
          style = "background: #FFB52C; margin-top: 15px; color: black;  box-shadow: 0 4px 20px #FFB52C;"
        )
      )
    )
    
    if (!is.null(results$commercially_viable) &&
        results$commercially_viable) {
      # Show ONLY the commercial viability banner, skip incentive_display
      return(
        div(
          class = "results-container",
          commercial_viability_warning,
          revenue_info_display,
          individual_firm_display
        )
      )
    }
    
    # If archetypes are selected, show comparison
    if (!is.null(input$selected_archetypes) &&
        length(input$selected_archetypes) > 0) {
      colors <- c("#662260",
                  "#58595B",
                  "#FFB52C",
                  "#DE7C5A",
                  "#8DB790",
                  "#F2D7EE",
                  "#D3D4D9")
      
      # Limit to first 4 archetypes
      selected_archetypes <- input$selected_archetypes[1:min(7, length(input$selected_archetypes))]
      
      comparison_items <- lapply(seq_along(selected_archetypes), function(i) {
        name <- selected_archetypes[i]
        arch <- saved_archetypes$data[[name]]
        if (!is.null(arch)) {
          # Get the unit for this archetype
          arch_unit <- arch$results$cost_unit
          arch_unit_label <- switch(
            arch_unit,
            "M" = "$M",
            "K" = "$K",
            "B" = "$B",
            "1" = "$"
          )
          
          color_idx <- ((i - 1) %% length(colors)) + 1
          div(
            style = paste0(
              "flex: 1; min-width: 240px; margin: 10px; padding: 24px;
                     background: white;
                     border-radius: 16px; border-left: 4px solid ",
              colors[color_idx],
              ";
                     box-shadow: 0 4px 20px rgba(0,0,0,0.08);
                     transition: all 0.3s ease;"
            ),
            h5(
              name,
              style = paste0(
                "color: ",
                colors[color_idx],
                "; margin-bottom: 16px; margin-top: 0; font-size: 1.2rem; font-weight: 800;"
              )
            ),
            tags$table(
              style = "width: 100%; font-size: 14px; font-family: 'Proxima Nova', sans-serif;",
              tags$tr(
                tags$td("Cost to attempt:", style = "padding: 8px 4px; color: #58595B; font-weight: 500;"),
                tags$td(paste0(
                  "$", format(round(
                    arch$results$cost_to_attempt, 1
                  ), big.mark = ",")
                ), style = "text-align: right; padding: 8px 4px; font-weight: 700; color: #2d3748;")
              ),
              tags$tr(
                tags$td("Individual firm PoS:", style = "padding: 8px 4px; color: #58595B; font-weight: 500;"),
                tags$td(
                  sprintf("%.1f%%", arch$results$prob_of_success * 100),
                  style = "text-align: right; padding: 8px 4px; font-weight: 700; color: #2d3748;"
                )
              ),
              tags$tr(
                tags$td("Firms:", style = "padding: 8px 4px; color: #58595B; font-weight: 500;"),
                tags$td(arch$results$n_firms, style = "text-align: right; padding: 8px 4px; font-weight: 700; color: #2d3748;")
              ),
              tags$tr(
                tags$td("Global PoS:", style = "padding: 8px 4px; color: #58595B; font-weight: 500;"),
                tags$td(
                  sprintf("%.1f%%", arch$results$global_pos * 100),
                  style = "text-align: right; padding: 8px 4px; font-weight: 700; color: #2d3748;"
                )
              ),
              tags$tr(
                tags$td("Prize:", style = "padding: 8px 4px; color: #58595B; font-weight: 500;"),
                tags$td(paste0(
                  "$", format(round(arch$results$shared_prize, 1), big.mark = ",")
                ), style = "text-align: right; padding: 8px 4px; font-weight: 700; color: #2d3748;")
              )
            )
          )
        }
      })
      
      comparison_display <- div(
        hr(style = "border: none; height: 2px; background: #662260; margin: 30px 0;"),
        div(
          h4(
            "Saved Archetypes",
            style = "background: #662260;
                   background-clip: text;
                   -webkit-background-clip: text;
                   -webkit-text-fill-color: transparent;
                   font-weight: 800; font-size: 1.3rem;"
          ),
          tags$div(style = "width: 60px; height: 3px; background: #662260; border-radius: 2px; margin-bottom: 20px;")
        ),
        div(style = "display: flex; flex-wrap: wrap; justify-content: flex-start; gap: 10px;", comparison_items)
      )
      
      div(
        class = "results-container",
        incentive_display,
        amc_warning,
        amc_scenarios_display,
        milestones_warning,
        milestone_display,
        revenue_info_display,
        individual_firm_display,
        push_funding_section,
        save_section,
        comparison_display
        
      )
    } else {
      div(
        class = "results-container",
        incentive_display,
        amc_warning,
        amc_scenarios_display,
        milestones_warning,
        milestone_display,
        revenue_info_display,
        individual_firm_display,
        push_funding_section,
        save_section
      )
    }
  })
  
  
  output$prize_plot <- renderPlotly({
    generate_prize_plot()
  })
  
  output$prize_plot_expanded <- renderPlotly({
    generate_prize_plot()
  })
  
  output$plot_section <- renderUI({
    results <- calculated_results()
    
    # Hide plot if commercially viable
    if (!is.null(results$commercially_viable) &&
        results$commercially_viable) {
      return(NULL)
    }
    
    # Show normal plot section
    tagList(
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
        h4("Probability vs Prize Cost", style = "font-size: 1.3rem; margin-bottom: 0; color: #662260; font-weight: 600;"),
        actionButton(
          "expand_plot",
          "",
          icon = icon("expand"),
          style = "background: #662260;
                      color: white;
                      border: none;
                      padding: 10px 15px;
                      border-radius: 8px;
                      box-shadow: 0 4px 10px rgba(102, 34, 96, 0.3);
                      transition: all 0.3s ease;
                      cursor: pointer;",
          title = "Expand to full screen"
        )
      ),
      plotlyOutput("prize_plot", height = "500px"),
      br(),
      hr(style = "border: none; height: 2px; background: #662260; margin: 30px 0;"),
      div(
        h3("Compare Scenarios", style = "font-size: 1.6rem; color: #662260; font-weight: 700; margin-bottom: 15px;"),
        tags$div(style = "width: 100px; height: 3px; background: #662260; border-radius: 2px; margin-bottom: 20px;")
      ),
      div(
        p(
          style = "color: #58595B; font-size: 13px; margin-bottom: 12px; font-style: italic;",
          "Select saved scenarios below to compare them on the chart:"
        ),
        selectInput(
          "selected_archetypes",
          "Select Scenarios to Display:",
          choices = NULL,
          multiple = TRUE
        ),
        actionButton("clear_archetypes", "Clear All Saved", class = "btn-sm btn-warning")
      )
    )
  })
}

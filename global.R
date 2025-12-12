library(shiny)
library(rhandsontable)
library(tidyverse)
library(janitor)
library(plotly)
library(shinyjs)

# Functions --------------------------------------------------------------------

expected_shared_prize <- function(n, p, cost) {
  k_vals <- 1:n
  probs <- choose(n - 1, k_vals - 1) * p^k_vals * (1 - p)^(n - k_vals)
  denom <- sum(probs / k_vals)
  ev <- cost / denom
  return(ev)
  
}

# Calculate expected costs
calculate_project_metrics <- function(data,
                                      discount_rate,
                                      target_probability,
                                      use_feasibility = FALSE,
                                      eta = 1) {
  print(eta)
  df = as_tibble(data)
  total_stages = max(data$Stage)
  
  calc_df = df %>%
    rename(push_funding = `Share of costs covered by other sources (%)`) %>%
    mutate(push_funding = push_funding / 100) %>%
    clean_names() %>%
    rename(prob = probability_percent) %>%
    mutate(prob = prob / 100) %>%
    mutate(
      cumsum_prob = cumprod(prob),
      cost_after_push = cost * (1 - push_funding),
      cumsum_duration = cumsum(duration)
    ) %>%
    mutate(
      # Convert costs into discounted amounts ....
      disc_cost = pmap_dbl(list(
        cost_after_push, duration, cumsum_duration
      ), \(cost_after_push, dur, cum_dur) {
        dur_months <- round(dur * 12)
        start_month <- round((cum_dur - dur) * 12) + 1
        months <- seq(start_month, start_month + dur_months - 1)
        monthly_rate <- (1 + discount_rate / 100)^(1 / 12)
        sum((cost_after_push / dur_months) * (monthly_rate^months))
      })
    ) %>%
    # Calculate the exp. cost per stage ...
    mutate(
      previous_prob = lag(prob),
      previous_prob = replace_na(previous_prob, 1),
      cumprod_prob = cumprod(previous_prob),
      eta_adjustment = if (use_feasibility && nrow(df) > 1) {
        ifelse(stage == 1, 1, eta)
      } else {
        1
      },
      expected_stage_cost = cumprod_prob * disc_cost * eta_adjustment
    )
  
  # Calculate individual firm PoS and costs ...
  prob_of_success = prod(calc_df$prob)
  cost_to_attempt = sum(calc_df$expected_stage_cost)
  tot_duration = sum(calc_df$duration)
  
  # target_probability is in percent (0–100) from the UI
  target_prop <- target_probability / 100  # now in [0,1]
  # Adjust for feasibility if enabled
  target_base <- if (use_feasibility &&
                     eta > 0)
    target_prop / eta
  else
    target_prop
  
  # Clamp between 0 and something just under 1
  target_base <- pmin(pmax(target_base, 0), 0.99999)
  
  # Make sure target is valid and handle edge cases
  if (target_base >= 0.99999) {
    target_base <- 0.99999  # Cap to avoid log(0)
  }
  
  # Calculate number of firms needed
  if (prob_of_success < 1 && target_base > 0 && target_base < 1) {
    n_firms_needed = log(1 - target_base) / log(1 - prob_of_success)
    n_firms = ceiling(n_firms_needed)
    
    # Ensure at least 1 firm
    if (n_firms < 1 || is.infinite(n_firms) || is.nan(n_firms)) {
      n_firms = 1
    }
  } else {
    n_firms = 1
  }
  
  # Recalculate the actual prob achieved with this integer number of firms
  base_global_pos <- 1 - (1 - prob_of_success)^n_firms
  
  # Apply feasibility adjustment if enabled
  if (use_feasibility) {
    global_pos <- base_global_pos * eta
  } else {
    global_pos <- base_global_pos
  }
  
  # Calculate return values .....
  guaranteed_prize = (cost_to_attempt / prob_of_success) * n_firms
  shared_prize = expected_shared_prize(n_firms, prob_of_success, cost_to_attempt)
  exp_shared_prize = shared_prize * global_pos
  total_milestone_cost = n_firms * cost_to_attempt
  wasted_milestone_cost = (1 - global_pos) *  total_milestone_cost
  push_funding = sum(calc_df$cost * calc_df$push_funding)
  
  return(
    list(
      total_milestone_cost = total_milestone_cost,
      wasted_milestone_cost = wasted_milestone_cost,
      prob_of_success = prob_of_success,
      cost_to_attempt = cost_to_attempt,
      shared_prize = shared_prize,
      expected_shared_prize = exp_shared_prize,
      n_firms = n_firms,
      global_pos = global_pos,
      target_probability = target_probability,
      push_funding = push_funding,
      cost_unit = data$cost_unit,
      use_feasibility = use_feasibility,
      eta = eta,
      tot_duration = tot_duration
    )
  )
}

# Helper function to calculate AMC for a given target amount
calculate_amc_for_target <- function(target_amount,
                                     ac_template,
                                     discount_rate,
                                     revenue_start_month = NULL,
                                     unit_factor = 1e6,
                                     optimization_mode = "topup_to_units",
                                     # Legacy parameters for backward compatibility - to make global updates on
                                     topup = NULL,
                                     global_pos = NULL,
                                     calculation_mode = NULL,
                                     target_units = NULL) {
  # Handle legacy parameter names
  if (!is.null(calculation_mode)) {
    optimization_mode <- calculation_mode
  }
  
  # If revenue_start_month is NULL, default to 0
  if (is.null(revenue_start_month)) {
    revenue_start_month <- 0
  }
  
  # Check if ac_template is valid
  if (is.null(ac_template) || !is.data.frame(ac_template) ||
      nrow(ac_template) == 0 || any(is.na(ac_template))) {
    return(
      list(
        topup = if (optimization_mode == "topup_to_units")
          target_amount
        else
          0,
        units_to_cover_total = 0,
        amc_size_undiscounted = 0,
        amc_size_pv = 0,
        expected_amc = 0,
        covers_prize = FALSE,
        optimization_mode = optimization_mode
      )
    )
  }
  
  if (optimization_mode == "topup_to_units") {
    # Mode 1: Given $/unit (topup), calculate how many units are covered
    # target_amount is the prize pv at prize payout time
    # topup is the $/unit payment
    
    if (!is.null(topup)) {
      topup_val <- topup  # Use the actual topup parameter
    } else {
      topup_val <- target_amount  # Fallback if topup not provided
    }
    target_pv <- target_amount  # The prize pv to match (in millions)
    
    # Sequential month-by-month calculation tracking pv and nominal
    remaining_prize_pv <- target_pv * unit_factor  # Convert to actual dollars
    
    covered_units <- 0
    total_pv_payments <- 0
    total_nominal_payments <- 0
    
    for (i in 1:nrow(ac_template)) {
      if (remaining_prize_pv <= 1e-6)
        break
      
      # ac_template$month[i] is months after prize payout
      # Discount relative to prize payout time, not t=0
      months_after_prize <- ac_template$month[i]
      discount_factor <- 1 / ((1 + discount_rate)^(months_after_prize /
                                                     12))
      pv_per_unit <- as.numeric(topup_val) * discount_factor
      
      units_needed <- remaining_prize_pv / pv_per_unit
      units_available <- ac_template$units[i]
      
      
      if (units_needed <= units_available) {
        # Partial month
        covered_units <- covered_units + units_needed
        pv_this_month <- units_needed * pv_per_unit
        nominal_this_month <- units_needed * as.numeric(topup_val)
        
        total_pv_payments <- total_pv_payments + pv_this_month
        total_nominal_payments <- total_nominal_payments + nominal_this_month
        remaining_prize_pv <- 0
        break
      } else {
        # Full month
        covered_units <- covered_units + units_available
        pv_this_month <- as.numeric(units_available) * pv_per_unit
        nominal_this_month <- as.numeric(units_available) * as.numeric(topup_val)
        
        total_pv_payments <- total_pv_payments + pv_this_month
        total_nominal_payments <- total_nominal_payments + nominal_this_month
        remaining_prize_pv <- remaining_prize_pv - pv_this_month
      }
    }
    
    units_to_cover_total <- covered_units
    amc_size_undiscounted <- total_nominal_payments / unit_factor  # Convert back to millions
    amc_size_pv <- total_pv_payments / unit_factor  # Convert back to millions
    topup_result <- topup_val
    
    
  } else {
    # Check if we have target_units specified
    if (!is.null(target_units) &&
        !is.na(target_units) && target_units > 0) {
      # Use the helper function to back-calculate topup for specific units
      topup_calc <- calculate_topup_from_units(
        target_units = target_units,
        ac_template = ac_template,
        discount_rate = discount_rate,
        unit_factor = unit_factor,
        target_amount = target_amount,
        revenue_start_month = revenue_start_month
      )
      
      if (!topup_calc$feasible) {
        # Return error state
        return(
          list(
            topup = topup_calc$topup,
            # May be NA
            units_to_cover_total = 0,
            amc_size_undiscounted = 0,
            amc_size_pv = 0,
            expected_amc = 0,
            covers_prize = FALSE,
            optimization_mode = optimization_mode,
            error = topup_calc$error
          )
        )
      }
      
      # if succesful, Use the exact same calculation as the scenarios table
      topup_result <- topup_calc$topup
      units_to_cover_total <- topup_calc$units_covered
      
      # Calculate total nominal AMC (undiscounted)
      amc_size_undiscounted <- (topup_result * units_to_cover_total) / unit_factor
      
      # Calculate PV - the PV should equal target_amount since that's what we solved for
      amc_size_pv <- target_amount  # This is the target prize PV we're trying to match
      
    } else {
      # Fallback: No target_units provided - this shouldn't happen in units_to_topup mode
      return(
        list(
          topup = NA,
          units_to_cover_total = 0,
          amc_size_undiscounted = 0,
          amc_size_pv = 0,
          expected_amc = 0,
          covers_prize = FALSE,
          optimization_mode = optimization_mode,
          error = "No target units specified for units_to_topup mode"
        )
      )
    }
  }
  
  
  return(
    list(
      topup = topup_result,
      units_to_cover_total = units_to_cover_total,
      amc_size_undiscounted = amc_size_undiscounted,
      amc_size_pv = amc_size_pv,
      expected_amc = amc_size_pv,
      covers_prize = TRUE,
      optimization_mode = optimization_mode
    )
  )
}

# Function to back-calculate top-up from target units
calculate_topup_from_units <- function(target_units,
                                       ac_template,
                                       discount_rate,
                                       unit_factor,
                                       target_amount,
                                       revenue_start_month = 0) {
  # Validate inputs
  if (is.null(target_units) ||
      is.na(target_units) ||
      is.infinite(target_units) || target_units <= 0) {
    return(list(
      topup = NA,
      feasible = FALSE,
      error = "Invalid target units"
    ))
  }
  
  if (is.null(ac_template) || nrow(ac_template) == 0) {
    return(list(
      topup = NA,
      feasible = FALSE,
      error = "Adoption curve is empty"
    ))
  }
  
  if (is.null(target_amount) ||
      is.na(target_amount) || is.infinite(target_amount)) {
    return(list(
      topup = NA,
      feasible = FALSE,
      error = "Invalid target amount"
    ))
  }
  
  # Calculate total available units over lifecycle
  total_units_available <- sum(ac_template$units)
  
  if (target_units > total_units_available) {
    return(list(
      topup = NA,
      feasible = FALSE,
      error = sprintf(
        "Target units (%s) exceed lifecycle capacity (%s units)",
        format(round(target_units, 0), big.mark = ","),
        format(round(total_units_available, 0), big.mark = ",")
      )
    ))
  }
  
  # If target is less than one month's worth, handle specially
  if (nrow(ac_template) > 0) {
    min_monthly_units <- min(ac_template$units)
    
    if (is.na(min_monthly_units) ||
        is.infinite(min_monthly_units)) {
      return(
        list(
          topup = NA,
          feasible = FALSE,
          error = "Adoption curve contains invalid unit values"
        )
      )
    }
    
    if (!is.na(target_units) &&
        !is.na(min_monthly_units) && target_units < min_monthly_units) {
      # For very small targets, use fractional month approach
      fraction_of_month <- target_units / min_monthly_units
      
      # Take just the first month and scale it
      ac <- ac_template %>%
        slice(1) %>%
        mutate(
          month_from_now = month + ceiling(revenue_start_month),
          monthly_rate = 1 / (((
            1 + discount_rate / 100
          )^(1 / 12))^month_from_now),
          # Scale units and discounting by fraction
          units = units * fraction_of_month,
          discounted_units = units * monthly_rate
        )
      
      sum_discounted_units <- sum(ac$discounted_units)
      
      if (sum_discounted_units > 0 &&
          !is.na(sum_discounted_units)) {
        target_amount_dollars <- target_amount * unit_factor
        required_topup <- target_amount_dollars / sum_discounted_units
        
        if (!is.na(required_topup) &&
            !is.infinite(required_topup)) {
          return(
            list(
              topup = required_topup,
              units_covered = target_units,
              feasible = TRUE,
              error = NULL,
              sum_discounted_units = sum_discounted_units
            )
          )
        }
      }
      
      return(list(
        topup = NA,
        feasible = FALSE,
        error = sprintf(
          "Target units (%s) is less than one month of production (%s units). Consider increasing target.",
          format(
            round(target_units, 0),
            big.mark = ",",
            scientific = FALSE
          ),
          format(
            round(min_monthly_units, 0),
            big.mark = ",",
            scientific = FALSE
          )
        )
      ))
    }
  }
  
  
  # Calculate discounted units up to target
  ac <- ac_template %>%
    mutate(
      month_from_now = month + ceiling(revenue_start_month),
      monthly_rate = 1 / (((
        1 + discount_rate / 100
      )^(1 / 12))^month_from_now),
      discounted_units = units * monthly_rate,
      cumsum_units = cumsum(units)
    ) %>%
    filter(cumsum_units <= target_units)
  
  # Check if we got any rows
  if (nrow(ac) == 0) {
    return(list(
      topup = NA,
      feasible = FALSE,
      error = "No units available within target"
    ))
  }
  
  # Sum of discounted units
  sum_discounted_units <- sum(ac$discounted_units)
  
  if (sum_discounted_units <= 0 || is.na(sum_discounted_units)) {
    return(list(
      topup = NA,
      feasible = FALSE,
      error = "No discounted units available"
    ))
  }
  
  # Back-calculate required top-up
  target_amount_dollars <- target_amount * unit_factor
  required_topup <- target_amount_dollars / sum_discounted_units
  
  # Check if result is valid
  if (is.na(required_topup) || is.infinite(required_topup)) {
    return(list(
      topup = NA,
      feasible = FALSE,
      error = "Could not calculate valid top-up amount"
    ))
  }
  
  # Calculate actual units covered
  units_covered <- sum(ac$units)
  
  return(
    list(
      topup = required_topup,
      units_covered = units_covered,
      feasible = TRUE,
      error = NULL,
      sum_discounted_units = sum_discounted_units
    )
  )
}
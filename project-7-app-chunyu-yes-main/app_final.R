# Multi‑page refactor using bslib::page_navbar + per‑page sidebars
library(shiny)
library(bslib)
library(DT)
library(plotly)
library(ggplot2)
library(shinycssloaders)

# ---- Download studyplanner ----
if (!requireNamespace("studyplanner", quietly = TRUE)) {
  devtools::install_github("chunyu-yes/studyplanner", subdir = "studyplanner")

}
library(studyplanner)

# ---- Helpers ----
fmt_num <- function(x, digits = 4) {
  if (is.null(x) || is.na(x)) return("--")
  formatC(as.numeric(x), format = "f", digits = digits)
}
fmt_pct <- function(x, digits = 1) {
  if (is.null(x) || is.na(x)) return("--")
  paste0(fmt_num(100 * as.numeric(x), digits), "%")
}
fmt_dollar <- function(x) {
  if (is.null(x) || is.na(x)) return("--")
  paste0("$", format(round(as.numeric(x), 0), big.mark = ",", scientific = FALSE))
}

validate_inputs <- function(input) {
  errors <- c()
  if (input$delta <= 0) errors <- c(errors, "Treatment effect (δ) must be > 0.")
  if (input$sigma <= 0) errors <- c(errors, "SD (σ) must be > 0.")
  if (input$rho < 0 || input$rho >= 1) errors <- c(errors, "Correlation (ρ) must be in [0,1).")
  if (input$alpha <= 0 || input$alpha >= 0.5) errors <- c(errors, "Alpha (α) must be in (0,0.5).")
  if (input$target_power <= 0 || input$target_power >= 1) errors <- c(errors, "Target power must be in (0,1).")
  if (input$budget <= 0) errors <- c(errors, "Budget must be > 0.")
  if (input$cost_per_subject <= 0) errors <- c(errors, "Cost per subject must be > 0.")
  errors
}

# ---- Theme ----
custom_theme <- bs_theme(
  version = 5,
  bg = "#fafbfc",
  fg = "#1a202c",
  primary = "#2563eb",
  secondary = "#64748b",
  success = "#059669",
  info = "#0ea5e9",
  warning = "#f59e0b",
  danger = "#ef4444",
  base_font = font_google("Inter"),
  heading_font = font_google("Poppins", wght = c(400, 600))
) |>
  bs_add_rules(
    ".card{box-shadow:0 1px 3px rgba(0,0,0,.12),0 1px 2px rgba(0,0,0,.24);border:none;margin-bottom:1.25rem;transition:box-shadow .3s} .card:hover{box-shadow:0 4px 8px rgba(0,0,0,.15),0 2px 4px rgba(0,0,0,.12)} .card-header{background-color:rgba(37,99,235,.05);border-bottom:1px solid rgba(37,99,235,.1);font-weight:600;color:#1e40af} .value-box{box-shadow:0 2px 4px rgba(0,0,0,.1);border:none;transition:transform .2s,box-shadow .2s} .value-box:hover{transform:translateY(-2px);box-shadow:0 4px 12px rgba(0,0,0,.15)} .btn-primary{background:linear-gradient(135deg,#2563eb 0%,#1d4ed8 100%);border:none;font-weight:500} .btn-primary:hover{background:linear-gradient(135deg,#1d4ed8 0%,#1e40af 100%);transform:translateY(-1px);box-shadow:0 4px 8px rgba(37,99,235,.3)} .accordion-button:not(.collapsed){background-color:rgba(37,99,235,.08);color:#1e40af} .alert-danger{background-color:#fef2f2;border-color:#fecaca;color:#991b1b} .bslib-value-box{min-height:140px} .bslib-value-box .value{font-size:2rem;line-height:1.1;font-weight:700} .vb-sub{opacity:.85;font-size:.9rem;margin-top:.25rem}"
  )

small <- tags$small

# ===================== UI =====================
ui <- page_sidebar(
  theme = custom_theme,
  title = tags$div(
    class = "d-flex align-items-center gap-2",
    icon("brain", class = "text-primary fs-4"),
    span("Alzheimer's Clinical Trial Planner", style = "font-weight:600;font-size:1.1rem;")
  ),
  # ---- GLOBAL SIDEBAR (persists across all tabs) ----
  sidebar = sidebar(
    width = 380,
    uiOutput("validation_errors"),
    conditionalPanel(
      condition = "$('html').hasClass('shiny-busy')",
      div(class = "alert alert-info d-flex align-items-center mb-3",
          icon("spinner"), span("  Calculating study parameters..."))
    ),
    accordion(
      id = "acc", open = c("eff","stat"), multiple = TRUE,
      accordion_panel(
        title = tags$span(icon("chart-line", class = "me-2"), "Effect Size"), value = "eff",
        div(class = "mb-3",
            tags$label("Treatment Effect (δ)", class = "form-label fw-semibold"),
            tooltip(sliderInput("delta", NULL, min = 0, max = 0.2, value = 0.05, step = 0.01, width = "100%"),
                    "Expected mean difference between treatment and control"),
            small("Current: ", textOutput("delta_display", inline = TRUE), class = "text-muted")
        ),
        div(class = "mb-3",
            tags$label("Standard Deviation (σ)", class = "form-label fw-semibold"),
            tooltip(numericInput("sigma", NULL, value = 0.20, min = 0, max = 5, step = 0.01, width = "100%"),
                    "Population SD of the outcome")),
        div(class = "mb-3",
            tags$label("Correlation (ρ)", class = "form-label fw-semibold"),
            tooltip(sliderInput("rho", NULL, min = 0, max = 0.99, value = 0.90, step = 0.01, width = "100%"),
                    "Correlation between baseline and follow‑up"),
            small("Current: ", textOutput("rho_display", inline = TRUE), class = "text-muted"))
      ),
      accordion_panel(
        title = tags$span(icon("calculator", class = "me-2"), "Statistics"), value = "stat",
        div(class = "mb-3",
            tags$label("Significance Level (α)", class = "form-label fw-semibold"),
            tooltip(sliderInput("alpha", NULL, min = 0, max = 0.20, value = 0.05, step = 0.01, width = "100%"),
                    "Two‑sided Type I error"),
            small("Current: ", textOutput("alpha_display", inline = TRUE), class = "text-muted")
        ),
        div(class = "mb-3",
            tags$label("Target Power", class = "form-label fw-semibold"),
            tooltip(sliderInput("target_power", NULL, min = 0, max = 1.00, value = 0.90, step = 0.1, width = "100%"),
                    "Desired probability of detecting the effect"),
            small("Current: ", textOutput("power_display", inline = TRUE), class = "text-muted")
        ),
        div(class = "mb-3",
            tags$label("Sample Size for Instant Calculations", class = "form-label fw-semibold"),
            tooltip(sliderInput("n_power", NULL, min = 0, max = 500, value = 50, step = 1, width = "100%"),
                    "n used for quick power calcs & visualization")
        )
      ),
      accordion_panel(
        title = tags$span(icon("sliders-h", class = "me-2"), "Curve Controls"), value = "curve",
        tooltip(sliderInput("n_max", "Max n for Curves", min = 0, max = 500, value = 200, step = 1),
                "Upper limit of n for power/cost curves"),
        tooltip(sliderInput("delta_max", "Max δ for Curve", min = 0, max = 0.40, value = 0.15, step = 0.005),
                "Upper limit of δ in power‑vs‑effect curve")
      ),
      accordion_panel(
        title = tags$span(icon("dollar-sign", class = "me-2"), "Budget"), value = "cost",
        div(class = "mb-3",
            tags$label("Total Budget", class = "form-label fw-semibold"),
            div(class = "input-group",
                span("$", class = "input-group-text"),
                tooltip(numericInput("budget", NULL, value = 1000000, min = 5000, step = 10000, width = "100%"),
                        "Maximum available funding"))
        ),
        div(class = "mb-3",
            tags$label("Cost per Subject", class = "form-label fw-semibold"),
            div(class = "input-group",
                span("$", class = "input-group-text"),
                tooltip(numericInput("cost_per_subject", NULL, value = 10000, min = 100, step = 100, width = "100%"),
                        "Variable per‑participant costs"))
        ),
        div(class = "mb-3",
            tags$label("Fixed Costs", class = "form-label fw-semibold"),
            div(class = "input-group",
                span("$", class = "input-group-text"),
                tooltip(numericInput("fixed_costs", NULL, value = 5000, min = 0, step = 1000, width = "100%"),
                        "One‑time setup costs"))
        )
      )
    ),
    br(),
    actionButton("calculate", "Calculate", class = "btn btn-primary w-100"),
    br(), br(),
    downloadButton("download_plan", "Download Study Plan (CSV)", class = "btn btn-outline-secondary w-100")
  ),

  # ---- BODY with NAV TABS ----
  bslib::navset_bar(
    nav_panel(
      "Design",
      layout_columns(
        col_widths = c(3,3,3,3),
        value_box(title = "Adjusted Effect Size (d)", value = textOutput("vb_d"), showcase = icon("chart-line"), theme_color = "primary"),
        value_box(title = "Power @ n (instant)", value = textOutput("vb_power"), showcase = icon("bolt"), theme_color = "success", div(class = "vb-sub", textOutput("vb_power_n"))),
        value_box(title = "Required Sample Size", value = textOutput("vb_reqn"), showcase = icon("bullseye"), theme_color = "info"),
        value_box(title = "Optimal n (Budget)", value = textOutput("vb_optn"), showcase = icon("dollar-sign"), theme_color = "warning")
      ),
      layout_columns(
        col_widths = c(6,6),
        card(card_header("Detailed Outputs"), card_body(tags$small(class = "text-muted", "After adjusting the parameters, click Calculate to refresh the results"), hr(), fluidRow(column(6, h6("Power with n"), verbatimTextOutput("power_result")), column(6, h6("Total Cost with n"), verbatimTextOutput("cost_result"))), hr(), h6("Required Sample Size"), verbatimTextOutput("sample_size_result"))),
        card(card_header("Study Plan (quick view)"), card_body(DTOutput("study_plan_dt")))
      )
    ),
    nav_panel(
      "Curves",
      card(card_header("Exploratory Curves"),
           card_body(
             tabsetPanel(
               tabPanel("Power vs Sample Size", withSpinner(plotlyOutput("power_curve", height = 360))),
               tabPanel("Cost vs Sample Size",  withSpinner(plotlyOutput("cost_curve",  height = 360))),
               tabPanel("Power vs Effect Size", withSpinner(plotlyOutput("effect_curve", height = 360)))
             )
           )
      )
    ),
    nav_panel(
      "Study Plan",
      layout_columns(
        col_widths = c(7,5),
        card(card_header("Complete Study Plan (key fields)"), card_body(DTOutput("study_plan_dt_full"))),
        card(card_header("Selected Inputs"), card_body(tableOutput("input_summary")))
      )
    ),
    nav_panel(
      "About",
      card(card_body(
        HTML("<h3>About this app</h3>
<p>This planner uses <b>studyplanner</b> utilities to compute adjusted effect sizes, power, and budget-aware designs for two-arm trials with baseline adjustment. Use the <i>Design</i> page to set assumptions, explore <i>Curves</i> for sensitivity, and export a CSV on the <i>Study Plan</i> page.</p>")
      ))
    ),
    nav_spacer(),
    nav_item(a("GitHub: studyplanner", href = "https://github.com/chunyu-yes/studyplanner", target = "_blank"))
  )
)

# ===================== SERVER =====================
server <- function(input, output, session) {
  # live badges
  output$delta_display <- renderText(fmt_num(input$delta, 3))
  output$rho_display   <- renderText(fmt_num(input$rho, 2))
  output$alpha_display <- renderText(fmt_num(input$alpha, 3))
  output$power_display <- renderText(fmt_pct(input$target_power, 1))

  # validation panel
  output$validation_errors <- renderUI({
    errs <- validate_inputs(input)
    if (length(errs) == 0) return(NULL)
    div(class = "alert alert-danger", tags$ul(lapply(errs, function(e) tags$li(e))))
  })

  # calculations triggered by Calculate
  calculations <- eventReactive(input$calculate, {
    errs <- validate_inputs(input)
    validate(need(length(errs) == 0, "Please fix inputs in the sidebar."))

    d <- effect_size_adj(delta = input$delta, sigma = input$sigma, rho = input$rho)
    power <- power_two_arm(d, n = input$n_power, alpha = input$alpha)
    required_n <- sample_size_two_arm(d, alpha = input$alpha, target_power = input$target_power)

    cost_raw <- study_cost(
      n = input$n_power,
      cost_per_subject = input$cost_per_subject,
      fixed_costs = input$fixed_costs
    )
    # Robustly coerce to a single total cost (handles vector/list returns)
    cost_total <- suppressWarnings({
      if (!is.null(cost_raw[["total"]])) as.numeric(cost_raw[["total"]]) else sum(as.numeric(cost_raw))
    })

    optimization <- optimize_under_budget(
      d, alpha = input$alpha, budget = input$budget, cost_per_subject = input$cost_per_subject, fixed_costs = input$fixed_costs
    )

    plan <- study_planner(
      delta = as.numeric(input$delta), sigma = as.numeric(input$sigma), rho = as.numeric(input$rho),
      alpha = as.numeric(input$alpha), target_power = as.numeric(input$target_power),
      budget = as.numeric(input$budget), cost_per_subject = as.numeric(input$cost_per_subject), fixed_costs = input$fixed_costs
    )

    list(effect_size = d, power = power, required_n = required_n,
         cost_total = cost_total, optimization = optimization, study_plan = plan)
  }, ignoreInit = TRUE)

  # value boxes
  output$vb_d      <- renderText({ req(input$calculate > 0); fmt_num(calculations()$effect_size, 4) })
  output$vb_power  <- renderText({ req(input$calculate > 0); fmt_pct(calculations()$power, 1) })
  output$vb_power_n<- renderText({ req(input$calculate > 0); paste0("n = ", input$n_power) })
  output$vb_reqn   <- renderText({ req(input$calculate > 0); paste0(ceiling(calculations()$required_n)) })
  output$vb_optn   <- renderText({ req(input$calculate > 0); opt <- calculations()$optimization; paste0(opt$n, " (", fmt_pct(opt$power, 1), ")") })

  # detail texts
  output$power_result <- renderText({ req(input$calculate > 0); paste0("Power with n = ", input$n_power, ": ", fmt_num(calculations()$power, 4)) })
  output$sample_size_result <- renderText({ req(input$calculate > 0); paste0("Required sample size for ", fmt_pct(input$target_power, 0), " power: ", ceiling(calculations()$required_n)) })
  output$cost_result <- renderText({ req(input$calculate > 0); paste0("Total cost with n = ", input$n_power, ": ", fmt_dollar(calculations()$cost_total)) })

  # quick study plan table (Design page)
  output$study_plan_dt <- renderDT({
    req(input$calculate > 0)
    plan <- calculations()$study_plan
    df <- data.frame(
      Metric = c("Adjusted Effect Size (d)", "Required Sample Size", "Optimal n (Budget)", "Achievable Power", "Total Cost"),
      Value  = c(fmt_num(plan$d_adj, 4), plan$n_target, plan$budget_opt$n, fmt_pct(plan$budget_opt$power, 1), fmt_dollar(plan$budget_opt$cost)),
      check.names = FALSE
    )
    datatable(df, rownames = FALSE, options = list(dom = "t", paging = FALSE), class = "compact stripe hover")
  })

  # full plan table (Study Plan page)
  output$study_plan_dt_full <- renderDT({
    req(input$calculate > 0)
    plan <- calculations()$study_plan
    out <- data.frame(
      d_adj = plan$d_adj,
      n_required = plan$n_target,
      n_opt = plan$budget_opt$n,
      power_opt = plan$budget_opt$power,
      cost_opt = plan$budget_opt$cost
    )
    datatable(out, rownames = FALSE, options = list(pageLength = 10, autoWidth = TRUE))
  })

  # inputs summary (Study Plan page)
  output$input_summary <- renderTable({
    req(input$calculate > 0)
    data.frame(
      Parameter = c("Treatment Effect (δ)", "SD (σ)", "Correlation (ρ)", "Alpha (α, one-side)", "Target power", "n (power calc)", "Budget", "Cost/subject (MRI)", "Fixed costs (two waves)"),
      Value = c(
        fmt_num(input$delta, 3), fmt_num(input$sigma, 3), fmt_num(input$rho, 2), fmt_num(input$alpha, 3),
        fmt_pct(input$target_power, 1), as.integer(input$n_power), fmt_dollar(input$budget), fmt_dollar(input$cost_per_subject), fmt_dollar(input$fixed_costs)
      ),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "s")

  # download
  output$download_plan <- downloadHandler(
    filename = function() paste0("study_plan_", Sys.Date(), ".csv"),
    content = function(file) {
      plan <- calculations()$study_plan
      out <- data.frame(
        d_adj = plan$d_adj,
        n_required = plan$n_target,
        n_opt = plan$budget_opt$n,
        power_opt = plan$budget_opt$power,
        cost_opt = plan$budget_opt$cost,
        alpha = input$alpha,
        target_power = input$target_power,
        delta = input$delta,
        sigma = input$sigma,
        rho = input$rho,
        budget = input$budget,
        cost_per_subject = input$cost_per_subject,
        fixed_costs = input$fixed_costs
      )
      write.csv(out, file, row.names = FALSE)
    }
  )

  # plots
  output$power_curve <- renderPlotly({
    req(input$calculate > 0)
    d <- calculations()$effect_size
    n_seq <- seq(10, input$n_max, by = max(10, floor(input$n_max/50)))
    powers <- sapply(n_seq, function(n) power_two_arm(d, n, input$alpha))
    req_n <- as.numeric(calculations()$required_n); tgt <- as.numeric(input$target_power)
    plot_ly(x = ~n_seq, y = ~powers, type = "scatter", mode = "lines", name = "Power") |>
      add_markers(x = req_n, y = approx(n_seq, powers, xout = req_n)$y, name = "Required n", hoverinfo = "text", text = paste0("n = ", req_n)) |>
      layout(title = "Power vs Sample Size", xaxis = list(title = "Sample Size (n)"), yaxis = list(title = "Power", range = c(0,1)),
             shapes = list(list(type = "line", xref = "x", x0 = req_n, x1 = req_n, yref = "paper", y0 = 0, y1 = 1, line = list(dash = "dash")),
                           list(type = "line", xref = "paper", x0 = 0, x1 = 1, yref = "y", y0 = tgt, y1 = tgt, line = list(dash = "dash"))))
  })

  output$cost_curve <- renderPlotly({
    req(input$calculate > 0)
    n_seq <- seq(10, input$n_max, by = max(10, floor(input$n_max/50)))
    costs <- sapply(n_seq, function(n) { cr <- study_cost(n, input$cost_per_subject, input$fixed_costs); if (!is.null(cr[["total"]])) as.numeric(cr[["total"]]) else sum(as.numeric(cr)) })
    budg  <- as.numeric(input$budget)
    plot_ly(x = ~n_seq, y = ~costs, type = "scatter", mode = "lines", name = "Cost") |>
      layout(title = "Total Cost vs Sample Size", xaxis = list(title = "Sample Size (n)"), yaxis = list(title = "Total Cost"),
             shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, yref = "y", y0 = budg, y1 = budg, line = list(dash = "dash"))))
  })

  output$effect_curve <- renderPlotly({
    req(input$calculate > 0)
    delta_seq <- seq(0.01, input$delta_max, by = 0.005)
    powers <- sapply(delta_seq, function(delta) {
      d <- effect_size_adj(delta, input$sigma, input$rho)
      power_two_arm(d, input$n_power, input$alpha)
    })
    dline <- as.numeric(input$delta); tgt <- as.numeric(input$target_power)
    plot_ly(x = ~delta_seq, y = ~powers, type = "scatter", mode = "lines", name = "Power vs Effect Size") |>
      add_markers(x = dline, y = approx(delta_seq, powers, xout = dline)$y, name = "Chosen δ", hoverinfo = "text", text = paste0("δ = ", fmt_num(dline, 3))) |>
      layout(title = "Power vs Treatment Effect Size (δ)", xaxis = list(title = "Treatment Effect (δ)"), yaxis = list(title = "Power", range = c(0,1)),
             shapes = list(list(type = "line", xref = "x", x0 = dline, x1 = dline, yref = "paper", y0 = 0, y1 = 1, line = list(dash = "dash")),
                           list(type = "line", xref = "paper", x0 = 0, x1 = 1, yref = "y", y0 = tgt, y1 = tgt, line = list(dash = "dash"))))
  })
}yes

shinyApp(ui, server)


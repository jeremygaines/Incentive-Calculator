ui <- fluidPage(
  useShinyjs(),
  tags$style(HTML(".shiny-busy { cursor: wait !important; }")),
  titlePanel(
    div(style = "color: white; font-weight: 800; font-size: 3.5rem; text-shadow: 0 2px 20px rgba(0,0,0,0.3); letter-spacing: -1px; text-transform: uppercase;", "Pull Sizing Calculator")
  ),
  
  # Introduction section
  div(
    style = "background: rgba(255, 255, 255, 0.95); backdrop-filter: blur(20px); padding: 25px 30px; border-radius: 16px; margin: 0 0 30px 0; box-shadow: 0 10px 40px rgba(0, 0, 0, 0.2);",
    h4("About This Calculator", style = "color: #662260; font-weight: 700; margin-top: 0; margin-bottom: 15px;"),
    p(
      style = "font-size: 15px; line-height: 1.7; color: #2d3748; margin-bottom: 5px;",
      "This calculator helps you determine how large a pull mechanism needs to be to incentivize a given level of innovation. It calculates the needed size of different mechanisms:",
      tags$ol(
        style = "margin-left: 20px; ",
        tags$li(
          tags$strong("Prizes:"),
          " Successful participants split a prize upon completion of all development stages"
        ),
        tags$li(
          tags$strong("Advanced Market Commitments (AMCs):"),
          " Participants receive a subsidy payment (top-up) per unit of the innovation sold over the product lifecycle"
        ),
        tags$li(
          tags$strong("Milestones + AMCs:"),
          " Firms receive lump-sum payments upon completing specific development stages, followed by per-unit subsidies (top-ups) for commercialization"
        )
      ),
      "The calculator allows you to model different scenarios including: multiple development stages with varying costs and success probabilities, different adoption and revenue curves, flexible AMC parameters (top-up amounts or target unit coverage), revenue generation timing (which stage revenue begins), and milestone payment schedules."
    ),
    tags$a(
      href = "https://williamjackarnesen.github.io/msa-amc-sizing/",
      target = "_blank",
      style = "color: #662260; font-weight: 600; text-decoration: none; font-size: 14px;",
      "Read more about the methodology →"
    ),
    p(style = "color: #58595B; font-size: 14px; margin-top: 12px; ", "Masterminds: 💅 Georgia with moral support from 👑 Hassan"),
  ),
  
  tags$head(tags$style(
    HTML(
      "

   @font-face {
    font-family: 'Proxima Nova';
    src: url('proximanova_regular.ttf') format('truetype');
    font-weight: 400;
    font-style: normal;
    font-display: swap;
  }

  /* ========== BODY WITH SUBTLE GRADIENT ========== */
  body {
    font-family: 'Proxima Nova', 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif !important;
    background: linear-gradient(180deg, #662260 0%, #8E2F8B 35%, #C83BC9 70%) !important;
    background-attachment: fixed !important;
    min-height: 100vh;
  }

  .container-fluid {
    padding: 30px;
  }

  h2 {
    font-family: 'Proxima Nova', 'Inter', sans-serif !important;
    font-weight: 700;
    color: #1a1a2e;
    letter-spacing: -0.5px;
  }

  h1 {
    color: white !important;
    font-size: 3rem !important;
    font-weight: 800;
    text-shadow: 0 2px 20px rgba(0,0,0,0.3);
    margin-bottom: 30px;
    letter-spacing: -1px;
    text-transform: uppercase;
  }

  h3 {
    font-size: 2.2rem !important;
    font-weight: 700;
    color: #662260;
    margin-bottom: 20px;
    text-transform: uppercase;
  }

  h4 {
    font-size: 1.8rem !important;
    margin-top: 20px;
    margin-bottom: 12px;
    color: #662260;
    font-weight: 600;
  }

  h5 {
    font-size: 1.6rem !important;
    text-transform: uppercase;
    margin-top: 20px;
    margin-bottom: 12px;
    color: #662260;
    font-weight: 600;
  }

  /* Make Advanced Settings stand out more */
  .advanced-settings-box .checkbox label {
    font-size: 16px !important;
    font-weight: 600 !important;
    color: #1a202c !important;
    line-height: 1.4;
  }

  .checkbox label,
  .radio label {
    white-space: normal !important;
    line-height: 1.4 !important;
    word-wrap: break-word;
    max-width: 100%;
  }

  .advanced-settings-box .checkbox {
    margin-top: 5px;
    margin-bottom: 15px;
  }

  /* Advanced settings header specifically */
  .advanced-settings-header {
    font-size: 1.8rem !important;
    color: #662260 !important;
    font-weight: 700 !important;
    margin-top: 0 !important;
    margin-bottom: 20px !important;
    padding-bottom: 10px;
    border-bottom: 3px solid #662260;
    display: flex;
    align-items: center;
    gap: 10px;
  }

   /* ========== VERTICAL TABS ========== */
  .tabbable {
    display: flex;
    gap: 20px;
    align-items: flex-start;
  }

  .nav-pills {
    flex-direction: column !important;
    width: 70px !important;
    flex-shrink: 0;
    position: sticky !important;
    top: 20px !important;
    align-self: flex-start !important;
    z-index: 100;
    transition: width 0.3s ease !important;
    overflow: visible !important;
  }

  .nav-pills:hover {
    width: 200px !important;
  }

  .nav-pills > li {
    width: 100%;
    margin-bottom: 15px;
  }

  /* ========== TAB BUTTONS (DEFAULT / HOVER / ACTIVE) ========== */

  /* Base pill */
  .nav-pills > li > a {
    background-color: #D3D4D9 !important;
    background-image: none !important;

    color: #58595B !important;
    border-radius: 12px !important;
    border: 2px solid transparent !important;

    padding: 25px 15px !important;
    font-family: 'Proxima Nova', sans-serif !important;
    font-weight: 600 !important;

    font-size: 0 !important;              /* hide text */
    white-space: nowrap !important;

    display: flex !important;
    align-items: center !important;
    justify-content: center !important;
    gap: 10px !important;

    transition: background-color 0.25s ease, transform 0.25s ease, box-shadow 0.25s ease !important;
  }

  /* Icons */
  .nav-pills > li > a > i {
    font-size: 28px !important;
    min-width: 28px !important;
    color: #58595B !important;
  }

  /* Reveal text on sidebar hover */
  .nav-pills:hover > li > a {
    font-size: 20px !important;
    justify-content: flex-start !important;
    padding-left: 20px !important;
    padding-right: 20px !important;
  }

    .nav-pills > li:not(.active) > a:hover {
      background-color: #BDBEC2 !important;
      background-image: none !important;
      transform: translateX(3px) !important;
    }

  /* Non-active dim */
  .nav-pills > li:not(.active) > a {
    opacity: 0.7 !important;
  }

    /* ACTIVE TAB – force gradient using shorthand background */
    .nav-pills > li.active > a,
    .nav-pills > li.active > a:hover,
    .nav-pills > li.active > a:focus {
      background: linear-gradient(135deg, white 0%, white 100%) !important;
      color: #111 !important;
      border: 3px solid #662260 !important;
      outline: 3px solid rgba(102, 34, 96, 0.25) !important;
      outline-offset: 2px !important;
      transform: none !important;
      box-shadow: 0 10px 30px rgba(102, 34, 96, 0.25) !important;
    }

  /* Active icon */
  .nav-pills > li.active > a > i {
    color: #111 !important;
  }

  .tab-content {
    flex: 1;
    background: rgba(255, 255, 255, 0.95);
    backdrop-filter: blur(20px);
    padding: 30px;
    border-radius: 20px;
    box-shadow: none !important;
    border: 1px solid rgba(255, 255, 255, 0.2);
    min-width: 0;
  }

  .input-panel, .results-panel {
    background: transparent;
    backdrop-filter: none;
    padding: 0;
    border-radius: 0;
    box-shadow: none;
    border: none;
    max-height: none;
    overflow: visible;
  }

  /* ========== GOLD BUTTON ========== */
  .gold-button {
    background: #FFB52C;
    color: #1a1a2e;
    border: none;
    font-weight: 700;
    padding: 15px 30px;
    border-radius: 12px;
    font-family: 'Proxima Nova', sans-serif;
    font-size: 16px;
    letter-spacing: 0.3px;
    box-shadow: 0 10px 30px rgba(252, 180, 44, 0.4);
    position: relative;
    overflow: hidden;
    transition: all 0.3s ease;
  }

  .gold-button:hover {
    background: #FFC34D;
    color: #1a1a2e;
    transform: translateY(-3px);
    box-shadow: 0 15px 40px rgba(252, 180, 44, 0.6);
  }

  .gold-button:active {
    transform: translateY(-1px);
  }

  .gold-button:disabled {
    background: #cccccc !important;
    color: #666666 !important;
    cursor: not-allowed !important;
    box-shadow: none !important;
    opacity: 0.6;
  }

  .gold-button:disabled:hover {
    transform: none !important;
    box-shadow: none !important;
  }

  /* ========== ORANGE BUTTON ========== */
  .orange-button {
    background: #DE7C5A;
    color: white;
    border: none;
    font-weight: 600;
    padding: 15px 30px;
    border-radius: 12px;
    font-family: 'Proxima Nova', sans-serif;
    font-size: 16px;
    letter-spacing: 0.3px;
    box-shadow: 0 10px 30px rgba(222, 124, 90, 0.4);
    position: relative;
    overflow: hidden;
    transition: all 0.3s ease;
  }

  .orange-button:hover {
    background: #E88B6A;
    color: white;
    transform: translateY(-3px);
    box-shadow: 0 15px 40px rgba(222, 124, 90, 0.6);
  }

  .orange-button:active {
    transform: translateY(-1px);
  }

  /* ========== PURPLE BUTTON ========== */
  .purple-button {
    background: #662260;
    color: white;
    border: none;
    font-weight: 600;
    padding: 15px 30px;
    border-radius: 12px;
    font-family: 'Proxima Nova', sans-serif;
    font-size: 16px;
    letter-spacing: 0.3px;
    box-shadow: 0 10px 30px rgba(102, 34, 96, 0.4);
    position: relative;
    overflow: hidden;
    transition: all 0.3s ease;
  }

  .purple-button::before {
    content: '';
    position: absolute;
    top: 0;
    left: -100%;
    width: 100%;
    height: 100%;
    background: linear-gradient(90deg, transparent, rgba(255,255,255,0.3), transparent);
    transition: left 0.5s;
  }

  .purple-button:hover::before {
    left: 100%;
  }

  .purple-button:hover {
    color: white;
    transform: translateY(-3px);
    box-shadow: 0 15px 40px rgba(102, 34, 96, 0.6);
  }

  .purple-button:active {
    transform: translateY(-1px);
  }

  /* ========== FORM CONTROLS WITH FOCUS GLOW ========== */
  .form-control, .form-select {
    border-radius: 10px;
    border: 2px solid #e0e7ff;
    padding: 12px 16px;
    font-family: 'Proxima Nova', sans-serif;
    background: white;
    font-size: 15px;
    transition: all 0.3s ease;
  }

  .form-control:focus, .form-select:focus {
    border-color: #F2D7EE !important;
    box-shadow: 0 0 0 4px rgba(255, 181, 44, 0.25), 0 0 20px rgba(255, 181, 44, 0.15) !important;
    outline: none;
  }

  .form-group {
    margin-bottom: 20px;
    min-width: 0;
  }

  /* ========== HANDSONTABLE ========== */
  .handsontable {
    font-size: 13px;
    font-family: 'Proxima Nova', sans-serif;
    border-radius: 12px;
    overflow: hidden;
    box-shadow: 0 4px 15px rgba(0,0,0,0.1);
  }

  .handsontable th {
    background: #662260 !important;
    color: white !important;
    font-weight: 600 !important;
    padding: 6px 4px !important;
    white-space: normal !important;
    line-height: 1.1 !important;
    vertical-align: middle !important;
    word-wrap: break-word !important;
    font-family: 'Proxima Nova', sans-serif !important;
    text-transform: uppercase;
    font-size: 10px;
    letter-spacing: 0.3px;
    height: auto !important;
    max-height: 45px !important;
  }

  .handsontable td {
    background-color: white !important;
    font-family: 'Proxima Nova', sans-serif !important;
    vertical-align: middle !important;
    text-align: center !important;
    padding: 8px 6px !important;
    border-color: #D3D4D9 !important;
    font-size: 14px;
  }

  .handsontable td:nth-child(2) {
    text-align: left !important;
    font-weight: 500;
  }

  .handsontable td.htDimmed {
    background-color: #fafafa !important;
  }

  .handsontable tbody tr:hover td {
    background-color: #f8f9ff !important;
  }

  /* ========== SECTION HEADERS ========== */
  .section-header {
    margin-top: 25px;
    margin-bottom: 12px;
    padding-bottom: 8px;
    background: #662260;
    background-clip: text;
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    font-size: 1.2rem;
    position: relative;
    font-weight: 600;
  }

  .section-header::after {
    content: '';
    position: absolute;
    bottom: 0;
    left: 0;
    width: 50px;
    height: 2px;
    background: #662260;
    border-radius: 2px;
  }

  /* ========== HELP TEXT STYLES ========== */
  .help-text {
    background: rgba(102, 34, 96, 0.1);
    padding: 16px 20px;
    border-radius: 12px;
    border-left: 4px solid #662260;
    margin-bottom: 20px;
    font-family: 'Proxima Nova', sans-serif;
    color: #2d3748;
    font-size: 15px;
    line-height: 1.6;
    font-weight: 500;
  }

  .background-help-text {
    background: rgba(255, 255, 255, 0.85);
    backdrop-filter: blur(10px);
    padding: 12px 18px;
    border-radius: 10px;
    margin-bottom: 15px;
    font-family: 'Proxima Nova', sans-serif;
    color: #2d3748;
    font-size: 14px;
    line-height: 1.6;
    font-weight: 500;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
    border-left: 3px solid rgba(102, 34, 96, 0.6);
  }

  .instruction-text {
    padding: 0px;
    margin-top: 6px;
    margin-bottom: 16px;
    font-size: 13px;
    color: #58595B;
    font-family: 'Proxima Nova', sans-serif;
    line-height: 1.5;
    font-weight: 400;
    font-style: italic;
  }

  .warning-box {
    background: linear-gradient(135deg, rgba(239, 68, 68, 0.1), rgba(220, 38, 38, 0.05));
    border-left: 4px solid #ef4444;
    padding: 16px 20px;
    border-radius: 10px;
    margin-bottom: 20px;
    font-family: 'Proxima Nova', sans-serif;
  }

  .warning-text {
    color: #991b1b;
    font-size: 14px;
    line-height: 1.6;
    font-weight: 500;
    margin: 0;
  }

  /* ========== RESULTS CONTAINER WITH SLIDE-UP ========== */
  .results-container {
    background: white;
    padding: 30px;
    border-radius: 16px;
    box-shadow: 0 4px 20px rgba(0, 0, 0, 0.08);
    font-family: 'Proxima Nova', sans-serif;
    animation: slide-up-fade 0.6s cubic-bezier(0.4, 0, 0.2, 1);
  }

  @keyframes slide-up-fade {
    from {
      opacity: 0;
      transform: translateY(40px);
    }
    to {
      opacity: 1;
      transform: translateY(0);
    }
  }

  .result-item {
    margin-bottom: 20px;
    padding: 20px;
    background: rgba(102, 34, 96, 0.05);
    border-radius: 12px;
    border-left: 4px solid #662260;
    transition: all 0.3s ease;
  }

  .result-item:hover {
    transform: translateX(5px);
    box-shadow: 0 5px 15px rgba(102, 34, 96, 0.2);
  }

  .result-label {
    font-size: 15px;
    color: #58595B;
    margin-bottom: 8px;
    font-family: 'Proxima Nova', sans-serif;
    font-weight: 500;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .result-value {
    font-size: 24px;
    font-weight: 700;
    background: #662260;
    background-clip: text;
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    font-family: 'Proxima Nova', sans-serif;
  }

  #prize_plot {
    background: white;
    padding: 20px;
    border-radius: 16px;
    box-shadow: 0 4px 20px rgba(0, 0, 0, 0.08);
    overflow: hidden;
  }

  label {
    font-weight: 600;
    color: #1a202c;
    margin-bottom: 8px;
    font-size: 15px;
    letter-spacing: 0.3px;
    white-space: normal;
    line-height: 1.3;
    display: block;
    width: 100%;
  }

  .btn-warning {
    background: #DE7C5A;
    border: none;
    color: white;
    font-weight: 600;
    border-radius: 8px;
    padding: 8px 16px;
    transition: all 0.3s ease;
  }

  .btn-warning:hover {
    transform: translateY(-2px);
    box-shadow: 0 8px 20px rgba(245, 87, 108, 0.4);
    color: white;
  }

  #toggle_push_funding:hover {
    transform: translateY(-2px);
    box-shadow: 0 6px 20px rgba(222, 124, 90, 0.5);
  }

  #expand_plot:hover {
    transform: translateY(-2px);
    box-shadow: 0 6px 20px rgba(102, 34, 96, 0.5);
  }

  hr {
    border: none;
    height: 2px;
    background: #662260;
    margin: 30px 0;
  }

  /* ========== CUSTOM SCROLLBAR ========== */
  ::-webkit-scrollbar {
    width: 10px;
  }

  ::-webkit-scrollbar-track {
    background: rgba(0,0,0,0.1);
    border-radius: 10px;
  }

  ::-webkit-scrollbar-thumb {
    background: #662260;
    border-radius: 10px;
  }

  ::-webkit-scrollbar-thumb:hover {
    background: #662260;
  }

  /* ========== MODAL STYLES ========== */
  .modal-dialog.modal-xl {
    max-width: 95vw !important;
    width: 95vw !important;
    margin: 2.5vh auto !important;
  }

  .modal-content {
    height: 95vh !important;
    border-radius: 20px !important;
    border: none !important;
    box-shadow: 0 25px 80px rgba(0, 0, 0, 0.4) ;
  }

  .modal-body {
    padding: 30px !important;
    display: flex !important;
    flex-direction: column !important;
    height: calc(95vh - 60px) !important;
  }

  .modal-header {
    border-bottom: 2px solid #D3D4D9 !important;
    padding: 20px 30px !important;
    background: rgba(102, 34, 96, 0.05) !important;
    border-radius: 20px 20px 0 0 !important;
  }

  /* ========== SLIDER STYLING ========== */
  .irs--shiny .irs-bar {
    background: #662260;
    border: none;
  }

  .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
    background: #662260;
    color: white;
  }

  .irs--shiny .irs-handle {
    background: #662260;
    border: 2px solid white;
    box-shadow: 0 2px 8px rgba(118, 75, 162, 0.4);
  }

  .irs--shiny .irs-handle:hover {
    background: #662260;
  }

  /* ========== ROW SPACING ========== */
  .row {
    margin-left: -5px !important;
    margin-right: -5px !important;
  }

  .row > div[class*='col-'] {
    padding-left: 5px !important;
    padding-right: 5px !important;
  }

  .input-panel .row {
    margin-left: -8px !important;
    margin-right: -8px !important;
  }

  .input-panel .row > div[class*='col-'] {
    padding-left: 8px !important;
    padding-right: 8px !important;
  }

  .input-panel h4,
  .input-panel h5 {
    padding-bottom: 10px;
    border-bottom: 3px solid #662260;
    margin-bottom: 15px !important;
  }

  /* ==========  CARD SHIMMER EFFECT ========== */
  #incentive_summary > div:last-child {
    position: relative !important;
    overflow: hidden !important;
  }

  #incentive_summary > div:last-child::before {
    content: '' !important;
    position: absolute !important;
    top: 0 !important;
    left: -100% !important;
    width: 100% !important;
    height: 100% !important;
    background: linear-gradient(90deg, transparent, rgba(255,255,255,0.2), transparent) !important;
    animation: shimmer 5s infinite !important;
    pointer-events: none !important;
    z-index: 1 !important;
  }

  @keyframes shimmer {
    0% { left: -100%; }
    40%, 100% { left: 100%; }
  }

  /* ========== CARD HOVER EFFECTS ========== */
  .firm-strip {
    transition: transform 0.3s ease, box-shadow 0.3s ease !important;
  }

  .firm-strip:hover {
    transform: translateX(8px) !important;
    box-shadow: 0 8px 30px rgba(102, 34, 96, 0.25) !important;
  }

"
    )
  ), tags$script(
    HTML(
      "
      $(document).on('click', '#calculate', function() {
        setTimeout(function() {
          // Switch to Results tab
          $('a[data-value=\"results_tab\"]').tab('show');

          // Wait for tab content to render, then scroll to results container
          setTimeout(function() {
            var element = document.querySelector('.results-container');
            if (element) {
              var offsetTop = $(element).offset().top - 20;
              $('html, body').animate({
                scrollTop: offsetTop
              }, 300);
            }
          }, 300);
        }, 200);
      });
    "
    )
  )),
  
  # Main tabset with vertical tabs on left
  tabsetPanel(
    id = "main_tabs",
    type = "pills",
    
    # Tab 1: Inputs
    tabPanel(
      "CONFIGURE",
      value = "inputs",
      icon = icon("gear"),
      div(
        class = "input-panel",
        h3("Inputs & Parameters", style = "font-size: 1.8rem; margin-top: 0; color: #662260; font-weight: 700;"),
        
        # Mechanism selection (full width)
        h4("Incentive Mechanisms to Model", style = "font-size: 1.6rem; margin-top: 0px; margin-bottom: 10px; color: #662260; font-weight: 600;"),
        radioButtons(
          "mechanisms",
          "Select Mechanism:",
          choices = c(
            "Prize" = "shared",
            "AMC (Advance Market Commitment)" = "amc",
            "Milestones + AMC" = "milestones_amc"
          ),
          selected = "shared"
        ),
        div(
          class = "instruction-text",
          "Select one mechanism. All mechanisms incentivize completion of all stages."
        ),
        
        # Stage Data section (full width for table)
        h4("Stage Data", style = "font-size: 1.6rem; margin-top: 20px; margin-bottom: 10px; color: #662260; font-weight: 600;"),
        p(
          style = "color: #58595B; font-size: 13px; line-height: 1.5; margin-bottom: 16px; font-style: italic;",
          "Innovation projects typically progress through sequential stages (e.g., research, prototyping, clinical trials)."
        ),
        
        fluidRow(column(
          6,
          numericInput(
            "num_stages",
            "Number of Stages:",
            value = 4,
            min = 1,
            max = 20,
            step = 1
          )
        ), column(
          6,
          selectInput(
            "cost_unit",
            "Cost Unit:",
            choices = c(
              "Millions ($M)" = "M",
              "Thousands ($K)" = "K",
              "Billions ($B)" = "B",
              "Dollars ($)" = "1"
            ),
            selected = "M"
          )
        )),
        
        rHandsontableOutput("stages_table"),
        uiOutput("milestone_share_warning"),
        uiOutput("stage_data_warning"),
        uiOutput("stage_instructions"),
        
        # Core Parameters (two columns)
        h4("Core Parameters", style = "font-size: 1.6rem; margin-top: 20px; margin-bottom: 10px; color: #662260; font-weight: 600;"),
        
        fluidRow(column(
          6,
          numericInput(
            "discount_rate",
            "Discount Rate (%):",
            value = 9,
            min = 0,
            max = 100,
            step = 1
          ),
          div(class = "instruction-text", "Annual discount rate (e.g., 0.09 = 9%)")
        ), column(
          6,
          numericInput(
            "target_probability",
            "Target Probability of Success (%):",
            value = 80,
            min = 0,
            max = 100,
            step = 1
          ),
          div(class = "instruction-text", "Desired probability that at least one firm succeeds")
        )),
        
        # Advanced Settings
        div(
          class = "advanced-settings-box",
          h5(class = "advanced-settings-header", "Advanced Settings"),
          
          fluidRow(
            column(
              6,
              checkboxInput(
                "use_feasibility",
                "Account for fundamental feasibility uncertainty",
                value = FALSE
              ),
              conditionalPanel(
                condition = "input.use_feasibility == true",
                numericInput(
                  "eta",
                  "Global Possibility Parameter (η):",
                  value = 100,
                  min = 0,
                  max = 100,
                  step = 1
                ),
                div(
                  class = "instruction-text",
                  "The probability that the innovation is fundamentally achievable (0 to 1)."
                )
              )
            ),
            column(
              6,
              checkboxInput("model_revenue", "Model private returns & uptake", value = FALSE),
              div(
                class = "instruction-text",
                HTML(
                  "Account for private revenue that firms will earn from successful innovation."
                )
              )
            )
          )
        ),
        
        # Revenue Parameters (conditional, two columns)
        conditionalPanel(
          condition = "input.model_revenue == true",
          h5("Revenue Parameters", style = "margin-top: 10px; font-weight: 700; color: #662260;"),
          
          
          fluidRow(
            column(
              6,
              numericInput(
                "profit_per_unit",
                "Profit per unit (dollars):",
                value = 1,
                min = 0,
                step = 1
              ),
              numericInput(
                "revenue_lifecycle",
                "Product life cycle (years):",
                value = 10,
                min = 1,
                step = 1
              )
            ),
            column(
              6,
              selectInput(
                "revenue_start_stage",
                "Revenue begins after completing:",
                choices = NULL,
                selected = NULL
              ),
              div(
                class = "instruction-text",
                "Revenue generation starts at the beginning of the next stage. Probability adjusts based on stage completion."
              )
            ),
            column(
              6,
              radioButtons(
                "revenue_uptake_model",
                "Uptake model:",
                choices = c("Linear" = "linear", "S-shaped (logistic)" = "s_shaped"),
                selected = "linear",
                inline = TRUE
              ),
              conditionalPanel(
                condition = "input.revenue_uptake_model == 'linear'",
                numericInput(
                  "revenue_monthly_units",
                  "Monthly units sold (constant):",
                  value = 10000,
                  min = 0,
                  step = 1000
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.revenue_uptake_model == 's_shaped'",
            fluidRow(
              column(
                4,
                numericInput(
                  "revenue_max_units",
                  "Max monthly capacity:",
                  value = 10000,
                  min = 0,
                  step = 100
                )
              ),
              column(
                4,
                numericInput(
                  "revenue_growth",
                  "Growth rate (k):",
                  value = 0.5,
                  min = 0.01,
                  step = 0.01
                )
              ),
              column(
                4,
                numericInput(
                  "revenue_peak_time",
                  "Peak time (years):",
                  value = 5,
                  min = 0,
                  step = 0.1
                )
              )
            ),
            plotlyOutput("revenue_uptake_plot", height = "220px")
          )
        ),
        
        # AMC Parameters (conditional, two columns)
        conditionalPanel(
          condition = "(input.mechanisms && input.mechanisms == 'amc') || (input.mechanisms && input.mechanisms == 'milestones_amc')",
          h5("AMC Parameters", style = "margin-top: 10px; font-weight: 700; color: #662260;"),
          
          # Mode selector
          radioButtons(
            "amc_calculation_mode",
            "Calculation mode:",
            choices = c(
              "Specify top-up  ➔  Calculate units covered" = "topup_to_units",
              "Specify units to cover  ➔  Calculate top-up" = "units_to_topup"
            ),
            selected = "topup_to_units"
          ),
          
          div(
            class = "instruction-text",
            style = "margin-bottom: 15px;",
            conditionalPanel(
              condition = "input.amc_calculation_mode == 'topup_to_units'",
              "Enter the subsidy amount per unit. The calculator will determine how many units need to be covered to reach the target AMC amount."
            ),
            conditionalPanel(
              condition = "input.amc_calculation_mode == 'units_to_topup'",
              "Enter the number of units you want the AMC to cover. The calculator will determine the required subsidy per unit."
            )
          ),
          
          fluidRow(
            # Conditional: Show top-up input or units input
            column(
              6,
              conditionalPanel(
                condition = "input.amc_calculation_mode == 'topup_to_units'",
                numericInput(
                  "amc_topup",
                  "Top-up per unit (dollars):",
                  value = 50,
                  min = 0,
                  step = 1
                )
              ),
              uiOutput("amc_feasibility_warning"),
              conditionalPanel(
                condition = "input.amc_calculation_mode == 'units_to_topup'",
                numericInput(
                  "amc_target_units",
                  "Target units to cover:",
                  value = 100000,
                  min = 1,
                  step = 1000
                ),
                uiOutput("amc_units_validation_warning")
              )
            )
          ),
          
          conditionalPanel(
            condition = "!input.model_revenue",
            fluidRow(
              column(
                6,
                numericInput(
                  "amc_lifecycle",
                  "Product life cycle (years):",
                  value = 10,
                  min = 1,
                  step = 1
                ),
                selectInput(
                  "amc_payout_start_stage",
                  "AMC payouts begin after completing:",
                  choices = c("After all stages (default)" = "0"),
                  selected = "0"
                ),
                radioButtons(
                  "amc_uptake_model",
                  "Uptake model:",
                  choices = c("Linear" = "linear", "S-shaped (logistic)" = "s_shaped"),
                  selected = "linear",
                  inline = TRUE
                )
              ),
              column(
                6,
                conditionalPanel(
                  condition = "input.amc_uptake_model == 'linear'",
                  numericInput(
                    "amc_monthly_units",
                    "Monthly units sold (constant):",
                    value = 10000,
                    min = 0,
                    step = 1000
                  )
                )
              )
            ),
            conditionalPanel(
              condition = "input.amc_uptake_model == 's_shaped'",
              fluidRow(
                column(
                  4,
                  numericInput(
                    "amc_max_units",
                    "Max monthly capacity:",
                    value = 10000,
                    min = 0,
                    step = 100
                  )
                ),
                column(
                  4,
                  numericInput(
                    "amc_growth",
                    "Growth rate (k):",
                    value = 0.5,
                    min = 0.01,
                    step = 0.01
                  )
                ),
                column(
                  4,
                  numericInput(
                    "amc_peak_time",
                    "Peak time (years):",
                    value = 5,
                    min = 0,
                    step = 0.1
                  )
                )
              ),
              plotlyOutput("amc_uptake_plot", height = "220px")
            )
          ),
          
          conditionalPanel(
            condition = "input.model_revenue",
            div(
              class = "instruction-text",
              style = "background: rgba(141, 183, 144, 0.15); padding: 12px; border-radius: 8px; border-left: 3px solid #8DB790;",
              HTML(
                "<strong>Note:</strong> Using product lifecycle and uptake model from Revenue Parameters above.  AMC payments begin at the same time as revenue generation begins."
              )
            )
          )
        ),
        
        uiOutput("validation_warnings"),
        br(),
        actionButton(
          "calculate",
          "Calculate Results →",
          class = "btn-lg btn-block gold-button",
          onclick = "setTimeout(function(){$('a[data-value=\"results\"]').tab('show');}, 100);"
        )
      )
    ),
    
    # Tab 2: Results
    tabPanel(
      "RESULTS",
      value = "results_tab",
      icon = icon("chart-bar"),
      div(
        id = "results_panel",
        class = "results-panel",
        uiOutput("results"),
        br(),
        uiOutput("plot_section")
      )
    )
  )
)
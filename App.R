library(shiny)
library(dplyr)
library(ggplot2)

# ── Load norm data ─────────────────────────────────────────────────────────────
norm_data <- read.csv("norm_data.csv", stringsAsFactors = FALSE)

# ── Item definitions ───────────────────────────────────────────────────────────
items_de <- list(
  hg = list(
    subscale_label = "Hilfsbereitschaft (HG)",
    color = "#2196F3",
    items = list(
      hg_1  = "In einem Zwiespalt wende ich mich lieber den Schwachen zu als den Starken.",
      hg_3  = "Ich würde durchaus mein eigenes Wohlergehen gefährden, um hungernden und kranken Menschen zu helfen.",
      hg_7  = "Trotz der Kosten für mich, unterstütze ich auch mir unbekannte Personen.",
      hg_10 = "Ich tue anderen Menschen oft ohne Vorbehalte oder Erwartungen etwas Gutes.",
      hg_13 = "In einer Notsituation würde ich wahrscheinlich spontan mein Leben riskieren, um fremde Menschen zu retten."
    )
  ),
  pp = list(
    subscale_label = "Peer Bestrafung (PP)",
    color = "#E53935",
    items = list(
      pp_1  = "Wenn sich einzelne Personen Sonderrechte herausnehmen, suche ich nach Verbündeten, um diese Person auszubremsen.",
      pp_8  = "Wenn jemand die Gemeinschaft absichtlich ausnützt, revanchiere ich mich diskret auf irgendeine Weise.",
      pp_12 = "Ich beobachte genau, ob sich jemand im Team daneben benimmt.",
      pp_14 = "Wenn sich eine Person auf Kosten meiner Gruppe Vorteile verschafft, arbeite ich im Privaten darauf hin, dass sie damit scheitert.",
      pp_15 = "Wer die geltenden Regeln zu seinen eigenen Gunsten auslegt, wird früher oder später von mir und meinen Freunden dafür zur Rechenschaft gezogen."
    )
  ),
  mc = list(
    subscale_label = "Moralischer Mut (MC)",
    color = "#43A047",
    items = list(
      mc_1  = "Ich hinterfrage offen die Entscheidungen von Autoritäten oder Vorgesetzten.",
      mc_3  = "Es ist schon vorgekommen, dass ich Personen vor den Kopf gestoßen habe aufgrund meiner moralischen Überzeugungen.",
      mc_18 = "Wichtige Veränderungen für alle versuche ich auch gegen den erklärten Widerstand der Allgemeinheit durchzusetzen.",
      mc_20 = "Ich kämpfe gegen unrechte Anweisungen 'von oben', auch wenn es mich am Ende mehr kostet als dass es mir nützt.",
      mc_23 = "Im Konfliktfall konfrontiere ich die Täter."
    )
  )
)

items_en <- list(
  cr = list(
    subscale_label = "Costly Rewarding (CR)",
    color = "#2196F3",
    items = list(
      hg_1  = "When unsure, I rather stand with the weak than the strong.",
      hg_3  = "I would jeopardize my own well-being in order to help the sick and hungry.",
      hg_4  = "I volunteer with humanitarian aid organizations to the extent possible.",
      hg_7  = "In an emergency I would probably spontaneously risk my own life to save complete strangers.",
      hg_10 = "Despite potential costs I might incur, I support people whether or not I know them personally."
    )
  ),
  cp = list(
    subscale_label = "Costly Punishment (CP)",
    color = "#E53935",
    items = list(
      pp_1  = "When someone demands special privileges for themselves, I look for others with whom I can try to stop them.",
      pp_6  = "I join up with others to act against individuals who behave unfairly.",
      pp_11 = "Within my circle of family and friends, I talk to others about people who behave egoistically.",
      pp_15 = "Those who construe the rules to their advantage will sooner or later be held accountable by my friends and me.",
      pp_17 = "I do my best to assure that selfish behavior is sanctioned."
    )
  ),
  cc = list(
    subscale_label = "Costly Countercontrol (CC)",
    color = "#43A047",
    items = list(
      mc_2  = "I would speak my mind even if it puts me in great danger.",
      mc_7  = "I closely examine the instructions of public authorities.",
      mc_15 = "I defend my personal convictions, even if I risk libel and slander.",
      mc_20 = "I fight against unjust instructions from authorities, even if it costs me more than it benefits me.",
      mc_23 = "In a conflict, I confront the attacker."
    )
  )
)

# ── Education labels ──────────────────────────────────────────────────────────
# Coding: 1=no degree, 2=Hauptschule, 3=Mittlere Reife, 4=Vocational,
#         5=Abitur/Fachabitur, 6=University, 7=Doctorate
edu_de <- c(
  "Kein Abschluss"                                                        = "1",
  "Hauptschulabschluss"                                                   = "2",
  "Mittlere Reife (Realschulabschluss)"                                   = "3",
  "Berufsausbildung"                                                      = "4",
  "Abitur / Fachabitur / Fachhochschulreife"                              = "5",
  "Universitätsabschluss (Bachelor, Master, Diplom o.ä.)"               = "6",
  "Promotion"                                                             = "7"
)
edu_en <- c(
  "No degree"                                                             = "1",
  "Lower secondary school certificate (Hauptschulabschluss)"             = "2",
  "Intermediate school certificate (Mittlere Reife)"                     = "3",
  "Vocational training"                                                   = "4",
  "Upper secondary / higher education entrance qualification (Abitur)"   = "5",
  "University degree (Bachelor, Master, Diplom or equivalent)"           = "6",
  "Doctorate"                                                             = "7"
)

gender_de <- c("Weiblich" = "f", "M\u00e4nnlich" = "m", "Nicht-bin\u00e4r" = "nb")
gender_en <- c("Female" = "f", "Male" = "m", "Non-binary" = "nb")

scale_anchors_de <- c(
  "trifft überhaupt\nnicht zu", "trifft\nnicht zu", "trifft eher\nnicht zu",
  "trifft\neher zu", "trifft\nzu", "trifft\nvollkommen zu"
)
scale_anchors_en <- c(
  "strongly\ndisagree", "disagree", "rather\ndisagree",
  "rather\nagree", "agree", "strongly\nagree"
)

# ── Helper: percentile ─────────────────────────────────────────────────────────
compute_percentile <- function(score, reference_scores) {
  ref <- reference_scores[!is.na(reference_scores)]
  if (length(ref) < 10) return(NULL)
  round(mean(ref <= score) * 100)
}

get_reference <- function(lang, gender_val, age_val, edu_val) {
  sub <- norm_data %>% filter(language == lang)
  
  # If no demographics provided, return full language sample
  has_gender <- !is.null(gender_val) && gender_val != ""
  has_age    <- !is.null(age_val)    && !is.na(age_val)
  has_edu    <- !is.null(edu_val)    && edu_val != ""
  
  if (!has_gender && !has_age && !has_edu) {
    return(list(data = sub, level = "full"))
  }
  
  # Try progressively broader filters using only provided demographics
  r1 <- sub
  if (has_gender) r1 <- r1 %>% filter(gender == gender_val)
  if (has_age)    r1 <- r1 %>% filter(abs(age - age_val) <= 10)
  if (has_edu)    r1 <- r1 %>% filter(education == as.numeric(edu_val))
  if (nrow(r1) >= 20) return(list(data = r1, level = "specific"))
  
  r2 <- sub
  if (has_gender) r2 <- r2 %>% filter(gender == gender_val)
  if (has_age)    r2 <- r2 %>% filter(abs(age - age_val) <= 15)
  if (nrow(r2) >= 20) return(list(data = r2, level = "broad"))
  
  return(list(data = sub, level = "full"))
}

# ── Gauge plot ─────────────────────────────────────────────────────────────────
make_gauge <- function(pct, label, color) {
  angle_rad <- (1 - pct / 100) * pi  # pi = left (0%), 0 = right (100%)
  
  # Arc data
  full_arc  <- data.frame(
    x = cos(seq(pi, 0, length.out = 300)),
    y = sin(seq(pi, 0, length.out = 300))
  )
  fill_frac <- pct / 100
  fill_arc  <- data.frame(
    x = cos(seq(pi, pi - fill_frac * pi, length.out = max(2, round(fill_frac * 300)))),
    y = sin(seq(pi, pi - fill_frac * pi, length.out = max(2, round(fill_frac * 300))))
  )
  
  needle_x <- cos(angle_rad) * 0.62
  needle_y <- sin(angle_rad) * 0.62
  
  ggplot() +
    geom_path(data = full_arc, aes(x = x, y = y), color = "#e8e8e8", linewidth = 16, lineend = "round") +
    geom_path(data = fill_arc, aes(x = x, y = y), color = color,    linewidth = 16, lineend = "round") +
    annotate("segment", x = 0, y = 0, xend = needle_x, yend = needle_y,
             color = "#111", linewidth = 2, lineend = "round") +
    annotate("point",   x = 0, y = 0, color = "#111", size = 5) +
    annotate("text", x = 0,     y = -0.28, label = paste0(pct, "%"),
             size = 8, fontface = "bold", color = "#111", family = "sans") +
    annotate("text", x = 0,     y = -0.52, label = label,
             size = 4, color = "#111", fontface = "bold", family = "sans") +
    annotate("text", x = -1.18, y = -0.08, label = "0%",   size = 3.2, color = "#333") +
    annotate("text", x =  1.18, y = -0.08, label = "100%", size = 3.2, color = "#333") +
    coord_fixed(xlim = c(-1.45, 1.45), ylim = c(-0.72, 1.25)) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(4, 4, 4, 4))
}

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
      * { box-sizing: border-box; }
      body {
        font-family: 'Inter', sans-serif;
        background: #f4f6fb;
        color: #1a1a2e;
        margin: 0; padding: 0;
      }
      .app-header {
        background: linear-gradient(135deg, #1a1a2e 0%, #0f3460 100%);
        color: white;
        padding: 22px 40px 18px;
        display: flex; align-items: center; justify-content: space-between; gap: 20px;
      }
      .app-header-text h2 { margin: 0 0 4px; font-size: 1.9rem; font-weight: 700; }
      .app-header-text p  { margin: 0; opacity: 0.72; font-size: 1.05rem; }
      .btn-home {
        background: rgba(255,255,255,0.12); color: white;
        border: 2px solid rgba(255,255,255,0.35);
        border-radius: 10px; padding: 9px 18px;
        font-size: 1rem; font-weight: 600; cursor: pointer;
        white-space: nowrap; transition: all 0.18s;
        font-family: 'Inter', sans-serif;
        flex-shrink: 0;
      }
      .btn-home:hover { background: rgba(255,255,255,0.22); border-color: white; }
      .lang-bar {
        background: #0a2744;
        padding: 8px 40px;
        display: flex; align-items: center; gap: 14px;
      }
      .lang-bar .control-label { color: rgba(255,255,255,0.65); font-size: 0.95rem; margin: 0; white-space: nowrap; }
      .lang-bar .form-control  { max-width: 160px; padding: 5px 10px; border-radius: 8px; font-size: 1rem; }
      .main-wrap { max-width: 820px; margin: 28px auto; padding: 0 18px 40px; }
      .card {
        background: white; border-radius: 14px;
        padding: 28px 30px; margin-bottom: 22px;
        box-shadow: 0 2px 14px rgba(0,0,0,0.06);
      }
      .card-title {
        font-size: 1.2rem; font-weight: 600; color: #0f3460;
        margin: 0 0 18px; padding-bottom: 12px;
        border-bottom: 2px solid #eef2ff;
      }
      .demo-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 18px; }
      @media(max-width:580px){ .demo-grid { grid-template-columns: 1fr; } }
      .scale-banner {
        background: #eef2ff; border-left: 4px solid #0f3460;
        border-radius: 0 8px 8px 0;
        padding: 14px 20px; font-size: 1.15rem; color: #0f3460;
        font-weight: 500; margin-bottom: 22px;
      }
      .sub-header {
        display: inline-block;
        font-size: 1.1rem; font-weight: 600;
        border-radius: 8px; padding: 6px 14px;
        margin-bottom: 18px;
      }
      .item-block { margin-bottom: 26px; padding-bottom: 22px; border-bottom: 1px solid #f2f2f2; }
      .item-block:last-child { border-bottom: none; margin-bottom: 0; }
      .item-text { font-size: 1.18rem; line-height: 1.65; margin-bottom: 14px; color: #2a2a2a; }

      /* Radio as clickable text buttons – no visible circle */
      .radio-wrap .shiny-options-group {
        display: flex !important; flex-wrap: wrap; gap: 5px;
      }
      .radio-wrap .shiny-options-group label {
        display: flex; flex-direction: column; align-items: center;
        background: #f8f9ff; border: 2px solid #e4e9f5;
        border-radius: 10px; padding: 8px 12px; cursor: pointer;
        transition: all 0.15s; min-width: 54px;
        font-size: 0.98rem; color: #555; font-weight: 500;
        line-height: 1.35; text-align: center; user-select: none;
      }
      .radio-wrap .shiny-options-group label:hover { border-color: #0f3460; background: #eef2ff; color: #0f3460; }
      .radio-wrap .shiny-options-group input[type='radio'] { display: none; }
      .radio-wrap .shiny-options-group label:has(input:checked) {
        background: #0f3460 !important; color: white !important;
        border-color: #0f3460 !important; font-weight: 600;
      }
      .btn-primary-fab {
        background: linear-gradient(135deg, #0f3460, #1565c0);
        color: white; border: none; border-radius: 12px;
        padding: 14px 36px; font-size: 1.1rem; font-weight: 600;
        cursor: pointer; width: 100%; margin-top: 8px;
        transition: opacity 0.18s; letter-spacing: 0.2px;
      }
      .btn-primary-fab:hover { opacity: 0.88; }
      .btn-secondary-fab {
        background: white; color: #0f3460;
        border: 2px solid #0f3460; border-radius: 12px;
        padding: 11px 30px; font-size: 1.05rem; font-weight: 600;
        cursor: pointer; transition: all 0.18s; margin-bottom: 30px;
      }
      .btn-secondary-fab:hover { background: #eef2ff; }
      .err-box {
        background: #fff0f0; border: 1px solid #ffcdd2;
        border-radius: 8px; padding: 9px 15px;
        color: #c62828; font-size: 1rem; margin-bottom: 14px;
      }
      .gauge-grid { display: grid; grid-template-columns: repeat(3,1fr); gap: 16px; }
      @media(max-width:600px){ .gauge-grid { grid-template-columns: 1fr; } }
      .gauge-tile {
        background: white; border-radius: 12px;
        padding: 16px 12px 10px; text-align: center;
        box-shadow: 0 2px 10px rgba(0,0,0,0.06);
      }
      .gauge-mean { font-size: 1.1rem; color: #222; margin-top: 8px; font-weight: 500; }
      .pct-box {
        background: #f8f9ff; border-radius: 10px;
        padding: 16px 20px; font-size: 1.1rem; color: #1a1a2e;
        line-height: 1.6; margin-bottom: 16px;
      }
      .badge-comp {
        display: inline-block; background: #deeaf8; color: #0f3460;
        border-radius: 20px; padding: 6px 18px;
        font-size: 1.1rem; font-weight: 600; margin-bottom: 16px;
      }
      .optional-hint {
        font-size: 1.05rem; color: #666; margin-bottom: 18px;
        padding: 10px 14px; background: #f8f9ff;
        border-radius: 8px; border-left: 3px solid #aac4e8;
      }
      .form-control, .selectize-input {
        border-radius: 9px !important; border: 2px solid #e4e9f5 !important;
        font-family: 'Inter', sans-serif !important;
      }
      .btn-print {
        background: white; color: #0f3460;
        border: 2px solid #0f3460; border-radius: 12px;
        padding: 11px 30px; font-size: 1.05rem; font-weight: 600;
        cursor: pointer; transition: all 0.18s;
        font-family: 'Inter', sans-serif;
      }
      .btn-print:hover { background: #eef2ff; }
      .results-buttons { display: flex; gap: 12px; margin-bottom: 30px; flex-wrap: wrap; }
      @media print {
        .app-header, .lang-bar, .results-buttons, .pct-box, .badge-comp { display: none !important; }
        body { background: white !important; }
        .card { box-shadow: none !important; border: 1px solid #ddd; }
        .main-wrap { margin: 0 !important; padding: 0 !important; max-width: 100% !important; }
      }
    "))
  ),
  
  # Header
  div(class = "app-header",
      div(class = "app-header-text",
          tags$h2(textOutput("hdr_title")),
          tags$p(textOutput("hdr_subtitle"))
      ),
      conditionalPanel("output.current_panel != 'demo'",
                       actionButton("btn_home", label = textOutput("lbl_home"), class = "btn-home")
      )
  ),
  
  # Language bar – only visible on first page
  conditionalPanel("output.current_panel == 'demo'",
                   div(class = "lang-bar",
                       tags$label(class = "control-label", textOutput("lbl_lang")),
                       selectInput("sel_lang", label = NULL,
                                   choices = c("Deutsch" = "DE", "English" = "EN"),
                                   selected = "DE", width = "160px")
                   )
  ),
  
  div(class = "main-wrap",
      
      # ── Step 1: Demographics (all optional) ──
      conditionalPanel("output.current_panel == 'demo'",
                       div(class = "card",
                           div(class = "card-title", textOutput("lbl_demo_hdr")),
                           div(class = "optional-hint", textOutput("lbl_optional_hint")),
                           div(class = "demo-grid",
                               numericInput("inp_age",    textOutput("lbl_age"),    value = NA, min = 18, max = 100),
                               selectInput("inp_gender",  textOutput("lbl_gender"), choices = c("–" = ""))
                           ),
                           selectInput("inp_edu", textOutput("lbl_edu"), choices = c("–" = "")),
                           uiOutput("ui_demo_err"),
                           actionButton("btn_next", textOutput("lbl_next"), class = "btn-primary-fab")
                       )
      ),
      
      # ── Step 2: Items ──
      conditionalPanel("output.current_panel == 'items'",
                       div(class = "scale-banner", textOutput("lbl_scale")),
                       uiOutput("ui_items"),
                       uiOutput("ui_item_err"),
                       actionButton("btn_submit", textOutput("lbl_submit"), class = "btn-primary-fab",
                                    style = "margin-top: 14px;")
      ),
      
      # ── Step 3: Results ──
      conditionalPanel("output.current_panel == 'results'",
                       div(class = "card",
                           div(class = "card-title", textOutput("lbl_results_hdr")),
                           uiOutput("ui_comp_badge"),
                           div(class = "pct-box", textOutput("lbl_pct_info")),
                           div(class = "gauge-grid", uiOutput("ui_gauges"))
                       ),
                       div(class = "results-buttons",
                           actionButton("btn_back", textOutput("lbl_back"), class = "btn-secondary-fab")
                       )
      )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  rv_panel      <- reactiveVal("demo")
  rv_scores     <- reactiveVal(NULL)
  rv_ref        <- reactiveVal(NULL)
  rv_item_order <- reactiveVal(NULL)  # per-session random order, stable on back/forward
  
  is_de <- reactive({ input$sel_lang == "DE" })
  
  output$current_panel <- reactive({ rv_panel() })
  outputOptions(output, "current_panel", suspendWhenHidden = FALSE)
  
  # ── Labels ──────────────────────────────────────────────────────────────────
  output$hdr_title    <- renderText({ if(is_de()) "FAB – Fragebogen zu altruistischen Verhaltensweisen" else "FAB – Facets of Altruistic Behaviors Scale" })
  output$hdr_subtitle <- renderText({ "" })
  output$lbl_lang     <- renderText({ if(is_de()) "Sprache" else "Language" })
  output$lbl_demo_hdr <- renderText({ if(is_de()) "Angaben zu Ihrer Person" else "About You" })
  output$lbl_age      <- renderText({ if(is_de()) "Alter" else "Age" })
  output$lbl_gender   <- renderText({ if(is_de()) "Geschlecht" else "Gender" })
  output$lbl_edu      <- renderText({ if(is_de()) "Höchster Bildungsabschluss" else "Highest level of education" })
  output$lbl_next     <- renderText({ if(is_de()) "Weiter \u2192" else "Next \u2192" })
  output$lbl_scale    <- renderText({ if(is_de()) "Bitte geben Sie an, wie sehr die folgenden Aussagen typischerweise auf Sie zutreffen." else "Please state honestly and spontaneously how you would typically behave or act." })
  output$lbl_submit   <- renderText({ if(is_de()) "Auswerten" else "Submit" })
  output$lbl_results_hdr <- renderText({ if(is_de()) "Ihre Ergebnisse" else "Your Results" })
  output$lbl_pct_info <- renderText({
    if(is_de()) "Ein Prozentrang von z.B. 75 bedeutet, dass Sie höhere Werte aufweisen als 75 % der Vergleichspersonen."
    else "A percentile rank of e.g. 75 means you score higher than 75% of the comparison group."
  })
  output$lbl_back          <- renderText({ if(is_de()) "\u2190 Zur\u00fcck" else "\u2190 Back" })
  output$lbl_home          <- renderText({ if(is_de()) "\u2302 Start" else "\u2302 Home" })
  output$lbl_print         <- renderText({ if(is_de()) "Ergebnisse drucken" else "Print results" })
  output$lbl_optional_hint <- renderText({
    if(is_de()) "Optional: Ihre Angaben werden verwendet, um Ihre Ergebnisse mit einer passenden Vergleichsgruppe zu vergleichen. Sie können auch ohne Angaben fortfahren."
    else "Optional: Your details help us compare your results to a matching group. You can also continue without providing them."
  })
  
  # ── Update dropdowns when language changes ───────────────────────────────────
  observeEvent(input$sel_lang, {
    updateSelectInput(session, "inp_gender",
                      choices = c("–" = "", if(is_de()) gender_de else gender_en))
    updateSelectInput(session, "inp_edu",
                      choices = c("–" = "", if(is_de()) edu_de else edu_en))
    rv_item_order(NULL)  # reset so new language gets fresh randomized order
  })
  
  # ── Step 1 → Step 2 ─────────────────────────────────────────────────────────
  output$ui_demo_err <- renderUI(NULL)
  
  observeEvent(input$btn_next, {
    # All fields optional – only validate age format if something was entered
    age_val <- input$inp_age
    age_ok  <- is.na(age_val) || (age_val >= 18 && age_val <= 100)
    if (!age_ok) {
      output$ui_demo_err <- renderUI(div(class = "err-box",
                                         if(is_de()) "Bitte geben Sie ein gültiges Alter ein (18–100) oder lassen Sie das Feld leer."
                                         else "Please enter a valid age (18–100) or leave the field empty."))
    } else {
      output$ui_demo_err <- renderUI(NULL)
      rv_panel("items")
    }
  })
  
  # ── Build item UI (randomized, no subscale headers) ──────────────────────────
  output$ui_items <- renderUI({
    items_def <- if(is_de()) items_de else items_en
    anchors   <- if(is_de()) scale_anchors_de else scale_anchors_en
    
    # Collect all items as a flat named list (item_key -> item_text)
    all_items <- list()
    for (sk in names(items_def)) {
      for (ik in names(items_def[[sk]]$items)) {
        all_items[[ik]] <- items_def[[sk]]$items[[ik]]
      }
    }
    
    # Generate order once per session – stable if user navigates back & forth
    if (is.null(rv_item_order())) {
      rv_item_order(sample(names(all_items)))
    }
    item_order <- rv_item_order()
    
    div(class = "card",
        tagList(lapply(item_order, function(ik) {
          div(class = "item-block",
              div(class = "item-text", all_items[[ik]]),
              div(class = "radio-wrap",
                  radioButtons(paste0("r_", ik), label = NULL,
                               choices = setNames(1:6, anchors),
                               selected = character(0), inline = TRUE)
              )
          )
        }))
    )
  })
  
  # ── Step 2 → Step 3 ─────────────────────────────────────────────────────────
  output$ui_item_err <- renderUI(NULL)
  
  observeEvent(input$btn_submit, {
    items_def <- if(is_de()) items_de else items_en
    sub_means <- list()
    any_missing <- FALSE
    
    for (sk in names(items_def)) {
      vals <- sapply(names(items_def[[sk]]$items), function(ik) {
        v <- input[[paste0("r_", ik)]]
        if (is.null(v) || v == "") NA_real_ else as.numeric(v)
      })
      if (any(is.na(vals))) { any_missing <- TRUE; break }
      sub_means[[sk]] <- mean(vals)
    }
    
    if (any_missing) {
      output$ui_item_err <- renderUI(div(class = "err-box",
                                         if(is_de()) "Bitte beantworten Sie alle Fragen." else "Please answer all questions."))
      return()
    }
    
    output$ui_item_err <- renderUI(NULL)
    
    ref <- get_reference(
      lang       = input$sel_lang,
      gender_val = input$inp_gender,
      age_val    = if(is.na(input$inp_age)) NA else as.numeric(input$inp_age),
      edu_val    = input$inp_edu
    )
    rv_ref(ref)
    
    subscale_cols <- if(is_de()) c(hg="hg", pp="pp", mc="mc") else c(cr="cr", cp="cp", cc="cc")
    
    pcts <- lapply(names(sub_means), function(sk) {
      col <- subscale_cols[[sk]]
      compute_percentile(sub_means[[sk]], ref$data[[col]])
    })
    names(pcts) <- names(sub_means)
    
    rv_scores(list(means = sub_means, percentiles = pcts))
    rv_panel("results")
  })
  
  # ── Results UI ───────────────────────────────────────────────────────────────
  output$ui_comp_badge <- renderUI({
    ref <- rv_ref(); if(is.null(ref)) return(NULL)
    n <- nrow(ref$data)
    lbl_text <- switch(ref$level,
                       "specific" = if(is_de()) paste0("Vergleichsgruppe (passend): n\u00a0=\u00a0", n, " Personen")
                       else        paste0("Matched comparison group: n\u00a0=\u00a0", n, " participants"),
                       "broad"    = if(is_de()) paste0("Vergleichsgruppe (breit): n\u00a0=\u00a0", n, " Personen")
                       else        paste0("Broad comparison group: n\u00a0=\u00a0", n, " participants"),
                       if(is_de()) paste0("Gesamtstichprobe: n\u00a0=\u00a0", n, " Personen")
                       else        paste0("Full sample: n\u00a0=\u00a0", n, " participants")
    )
    div(style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:10px; margin-bottom:16px;",
        div(class = "badge-comp", style = "margin-bottom:0;", lbl_text),
        tags$button(textOutput("lbl_print"), class = "btn-print",
                    onclick = "window.print()")
    )
  })
  
  output$ui_gauges <- renderUI({
    sc <- rv_scores(); if(is.null(sc)) return(NULL)
    items_def <- if(is_de()) items_de else items_en
    lapply(names(sc$means), function(sk) {
      div(class = "gauge-tile",
          plotOutput(paste0("g_", sk), height = "195px"),
          div(class = "gauge-mean", paste0("\u00d8 ", round(sc$means[[sk]], 2), " / 6"))
      )
    })
  })
  
  observe({
    sc <- rv_scores(); if(is.null(sc)) return()
    items_def <- if(is_de()) items_de else items_en
    for (sk in names(sc$means)) {
      local({
        key <- sk
        pct <- sc$percentiles[[key]]
        sub <- items_def[[key]]
        output[[paste0("g_", key)]] <- renderPlot({
          if(is.null(pct))
            ggplot() + annotate("text",x=0,y=0,label="n/a",size=7,color="#aaa") + theme_void()
          else
            make_gauge(pct, sub$subscale_label, sub$color)
        }, bg = "white")
      })
    }
  })
  
  # ── Back ────────────────────────────────────────────────────────────────────
  observeEvent(input$btn_back, {
    rv_panel("demo")
    rv_scores(NULL)
    rv_ref(NULL)
    rv_item_order(NULL)  # reset so a new run gets a fresh order
  })
  
  observeEvent(input$btn_home, {
    rv_panel("demo")
    rv_scores(NULL)
    rv_ref(NULL)
    rv_item_order(NULL)
  })
}

shinyApp(ui, server)
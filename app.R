library(shiny)
library(dplyr)
library(ggplot2)

# ── Load norm data ─────────────────────────────────────────────────────────────
norm_data <- read.csv("norm_data.csv", stringsAsFactors = FALSE)

# ── Item definitions ───────────────────────────────────────────────────────────
items_de <- list(
  hg = list(
    subscale_label = "Altruistisches Verstärken (Help Giving)",
    color = "#004E9E",
    items = list(
      hg_1  = "In einem Zwiespalt wende ich mich lieber den Schwachen zu als den Starken.",
      hg_3  = "Ich würde durchaus mein eigenes Wohlergehen gefährden, um hungernden und kranken Menschen zu helfen.",
      hg_7  = "Trotz der Kosten für mich, unterstütze ich auch mir unbekannte Personen.",
      hg_10 = "Ich tue anderen Menschen oft ohne Vorbehalte oder Erwartungen etwas Gutes.",
      hg_13 = "In einer Notsituation würde ich wahrscheinlich spontan mein Leben riskieren, um fremde Menschen zu retten."
    )
  ),
  pp = list(
    subscale_label = "Altruistisches Bestrafen (Peer Punishment)",
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
    subscale_label = "Altruistischer Widerstand (Moral Courage)",
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
    subscale_label = "Costly Rewarding (Help Giving)",
    color = "#004E9E",
    items = list(
      hg_1  = "When unsure, I rather stand with the weak than the strong.",
      hg_3  = "I would jeopardize my own well-being in order to help the sick and hungry.",
      hg_4  = "I volunteer with humanitarian aid organizations to the extent possible.",
      hg_7  = "In an emergency I would probably spontaneously risk my own life to save complete strangers.",
      hg_10 = "Despite potential costs I might incur, I support people whether or not I know them personally."
    )
  ),
  cp = list(
    subscale_label = "Costly Punishment (Peer Punishment)",
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
    subscale_label = "Costly Countercontrol (Moral Courage)",
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
edu_de <- c(
  "Kein Abschluss"                                                        = "1",
  "Hauptschulabschluss"                                                   = "2",
  "Mittlere Reife (Realschulabschluss)"                                   = "3",
  "Berufsausbildung"                                                      = "4",
  "Abitur / Fachabitur / Fachhochschulreife"                              = "5",
  "Universitätsabschluss (Bachelor, Master, Diplom o.ä.)"                = "6",
  "Promotion"                                                             = "7"
)
edu_en <- c(
  "No degree"                                                             = "1",
  "Lower secondary school certificate"                                    = "2",
  "Intermediate school certificate"                                       = "3",
  "Vocational training"                                                   = "4",
  "Upper secondary / higher education entrance qualification"             = "5",
  "University degree (Bachelor, Master or equivalent)"                    = "6",
  "Doctorate"                                                             = "7"
)

gender_de <- c("Weiblich" = "f", "M\u00e4nnlich" = "m", "Non-bin\u00e4r" = "nb")
gender_en <- c("Female" = "f", "Male" = "m", "Non-binary" = "nb")

# ── Facet descriptions ──────────────────────────────────────────────────────
facet_desc_de <- c(
  hg = "Beschreibt die Bereitschaft, anderen zu helfen, auch wenn es eigene Kosten verursacht \u2013 etwa Zeit, Energie oder pers\u00f6nliches Wohlbefinden.",
  pp = "Beschreibt die Tendenz, unfaires oder egoistisches Verhalten in der Gruppe zu sanktionieren, auch wenn dies mit eigenem Aufwand verbunden ist.",
  mc = "Beschreibt den Mut, moralische \u00dcberzeugungen zu vertreten und durchzusetzen, auch gegen sozialen Widerstand oder auf eigene Kosten."
)
facet_desc_en <- c(
  cr = "Reflects the tendency to help others even at personal cost \u2013 such as time, energy, or personal well-being.",
  cp = "Reflects the tendency to sanction unfair or selfish behavior within a group, even when doing so comes at a personal cost.",
  cc = "Reflects the courage to uphold and act on moral convictions, even in the face of social opposition or personal risk."
)

scale_anchors_de <- c("1","2","3","4","5","6")
scale_anchors_en <- c("1","2","3","4","5","6")

scale_legend_de <- c(
  "trifft überhaupt\nnicht zu",
  "trifft\nnicht zu",
  "trifft eher\nnicht zu",
  "trifft\neher zu",
  "trifft\nzu",
  "trifft\nvollkommen zu"
)
scale_legend_en <- c(
  "strongly\ndisagree",
  "disagree",
  "rather\ndisagree",
  "rather\nagree",
  "agree",
  "strongly\nagree"
)

# ── Helper: percentile ─────────────────────────────────────────────────────────
compute_percentile <- function(score, reference_scores) {
  ref <- reference_scores[!is.na(reference_scores)]
  if (length(ref) < 10) return(NULL)
  round(mean(ref <= score) * 100)
}

# ── Percentile classification ──────────────────────────────────────────────────
classify_percentile <- function(pct, lang = "DE") {
  if (is.null(pct)) return("")
  if (lang == "DE") {
    if (pct < 16) "weit unterdurchschnittlich"
    else if (pct < 26) "unterdurchschnittlich"
    else if (pct < 76) "durchschnittlich"
    else if (pct < 85) "\u00fcberdurchschnittlich"
    else "weit \u00fcberdurchschnittlich"
  } else {
    if (pct < 16) "far below average"
    else if (pct < 26) "below average"
    else if (pct < 76) "average"
    else if (pct < 85) "above average"
    else "far above average"
  }
}

get_reference <- function(lang, gender_val, age_val, edu_val) {
  sub <- norm_data %>% filter(language == lang)
  has_gender <- !is.null(gender_val) && gender_val != ""
  has_age    <- !is.null(age_val)    && !is.na(age_val)
  has_edu    <- !is.null(edu_val)    && edu_val != ""
  if (!has_gender && !has_age && !has_edu) return(list(data = sub, level = "full"))
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
make_gauge <- function(pct, color, lang = "DE") {
  angle_rad <- (1 - pct / 100) * pi
  full_arc  <- data.frame(x = cos(seq(pi, 0, length.out = 300)), y = sin(seq(pi, 0, length.out = 300)))
  fill_frac <- pct / 100
  fill_arc  <- data.frame(
    x = cos(seq(pi, pi - fill_frac * pi, length.out = max(2, round(fill_frac * 300)))),
    y = sin(seq(pi, pi - fill_frac * pi, length.out = max(2, round(fill_frac * 300))))
  )
  needle_x  <- cos(angle_rad) * 0.62
  needle_y  <- sin(angle_rad) * 0.62
  pct_label <- if(lang == "DE") paste0("Prozentrang: ", pct) else paste0("Percentile rank: ", pct)
  ggplot() +
    geom_path(data = full_arc, aes(x = x, y = y), color = "#e8e8e8", linewidth = 16, lineend = "round") +
    geom_path(data = fill_arc, aes(x = x, y = y), color = color, linewidth = 16, lineend = "round") +
    annotate("segment", x = 0, y = 0, xend = needle_x, yend = needle_y, color = "#111", linewidth = 2, lineend = "round") +
    annotate("point", x = 0, y = 0, color = "#111", size = 5) +
    annotate("text", x = 0, y = -0.42, label = pct_label, size = 7, fontface = "bold", color = "#111", family = "sans") +
    annotate("text", x = -1.32, y = 0.0, label = "0%",   size = 5.5, color = "#555", hjust = 1) +
    annotate("text", x =  1.32, y = 0.0, label = "100%", size = 5.5, color = "#555", hjust = 0) +
    coord_fixed(xlim = c(-1.65, 1.65), ylim = c(-0.58, 1.2)) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA), plot.margin = margin(4, 8, 4, 8))
}

# ── Normal distribution plot ──────────────────────────────────────────────────
make_norm_plot <- function(user_score, ref_scores, color, lang = "DE") {
  mu  <- mean(ref_scores, na.rm = TRUE)
  sig <- sd(ref_scores,   na.rm = TRUE)
  if (is.na(sig) || sig == 0) sig <- 0.01
  x_lo    <- max(1, mu - 3.5*sig)
  x_hi    <- min(6, mu + 3.5*sig)
  x_range <- seq(x_lo, x_hi, length.out = 300)
  df_norm  <- data.frame(x = x_range, y = dnorm(x_range, mu, sig))
  y_max    <- max(df_norm$y)
  x_shade  <- seq(x_lo, min(user_score, x_hi), length.out = 200)
  df_shade <- data.frame(x = x_shade, y = dnorm(x_shade, mu, sig))
  pct_val  <- round(mean(ref_scores <= user_score) * 100)
  pct_lbl  <- if(lang == "DE") paste0("Prozentrang: ", pct_val) else paste0("Percentile rank: ", pct_val)
  mean_lbl <- paste0("\u00d8 ", round(user_score, 2))
  ggplot() +
    geom_ribbon(data = df_shade, aes(x = x, ymin = 0, ymax = y), fill = color, alpha = 0.3) +
    geom_line(data = df_norm, aes(x = x, y = y), color = color, linewidth = 1.4) +
    annotate("segment", x = user_score, xend = user_score, y = 0, yend = y_max * 1.10,
             color = "#000", linewidth = 1.1) +
    annotate("text", x = x_lo, y = y_max * 1.35, label = pct_lbl,  color = "#000", size = 7, hjust = 0, fontface = "bold") +
    annotate("text", x = x_hi, y = y_max * 1.35, label = mean_lbl, color = "#000", size = 7, hjust = 1, fontface = "bold") +
    scale_x_continuous(limits = c(1, 6), breaks = 1:6) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.28))) +
    labs(x = if(lang == "DE") "Mittelwert" else "Mean score", y = NULL) +
    theme_minimal(base_family = "sans") +
    theme(
      axis.text.x        = element_text(size = 16, color = "#000"),
      axis.text.y        = element_blank(),
      axis.title.x       = element_text(size = 16, color = "#000", margin = margin(t=6)),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(color = "#eee"),
      plot.background    = element_rect(fill = "white", color = NA),
      plot.margin        = margin(16, 12, 8, 12)
    )
}

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('activeLang', function(lang) {
        document.getElementById('btn_lang_de').classList.toggle('active', lang === 'DE');
        document.getElementById('btn_lang_en').classList.toggle('active', lang === 'EN');
      });
      document.addEventListener('DOMContentLoaded', function() {
        var de = document.getElementById('btn_lang_de');
        if (de) de.classList.add('active');
      });
    ")),
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
      * { box-sizing: border-box; }
      body { font-family: 'Inter', sans-serif; background: #f0f4f9; color: #1a1a2e; margin: 0; padding: 0; font-size: 18px; }
      .app-header {
        background: white; border-bottom: 4px solid #004E9E;
        color: #1a1a2e; padding: 20px 40px 16px;
        display: flex; align-items: center; justify-content: space-between; gap: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.07);
      }
      .app-header-text h2 { margin: 0 0 4px; font-size: 1.8rem; font-weight: 700; color: #004E9E; }
      .app-header-text p  { margin: 0; color: #555; font-size: 1.5rem; }
      .btn-home {
        background: #f0f5ff; color: #004E9E; border: 2px solid #004E9E; border-radius: 10px;
        padding: 9px 18px; font-size: 1.5rem; font-weight: 600;
        cursor: pointer; white-space: nowrap; transition: all 0.18s;
        font-family: 'Inter', sans-serif; flex-shrink: 0;
      }
      .btn-home:hover { background: #004E9E; color: white; }
      .lang-switcher { display: flex; gap: 10px; flex-shrink: 0; }
      .btn-lang {
        background: #f0f5ff; color: #004E9E; border: 2px solid #c5d8f0; border-radius: 10px;
        padding: 9px 18px; font-size: 1.5rem; font-weight: 600;
        cursor: pointer; white-space: nowrap; transition: all 0.18s; font-family: 'Inter', sans-serif;
      }
      .btn-lang:hover { background: #004E9E; color: white; border-color: #004E9E; }
      .btn-lang.active { background: #004E9E; color: white; border-color: #004E9E; font-weight: 700; }
      .lang-bar { background: #0a2744; padding: 10px 40px; display: flex; align-items: center; gap: 14px; }
      .lang-bar .control-label { color: rgba(255,255,255,0.85); font-size: 1.5rem; margin: 0; white-space: nowrap; }
      .lang-bar select { font-size: 1.5rem; padding: 6px 10px; border-radius: 8px; }
      .main-wrap { max-width: 900px; margin: 28px auto; padding: 0 18px 40px; }
      .card { background: white; border-radius: 14px; padding: 28px 30px; margin-bottom: 22px; box-shadow: 0 2px 14px rgba(0,0,0,0.06); }
      .card-title { font-size: 1.8rem; font-weight: 700; color: #004E9E; margin: 0 0 18px; padding-bottom: 12px; border-bottom: 2px solid #dce8f7; }
      .intro-box { background: white; border-radius: 14px; padding: 20px 24px; margin-bottom: 16px; box-shadow: 0 2px 14px rgba(0,0,0,0.06); font-size: 1.5rem; color: #1a1a2e; line-height: 1.6; }
      .scale-banner { background: #e8f0fb; border-left: 4px solid #004E9E; border-radius: 0 8px 8px 0; padding: 12px 18px; font-size: 1.5rem; color: #0f3460; font-weight: 500; margin-bottom: 22px; }
      .optional-hint { font-size: 1.5rem; color: #555; margin-bottom: 18px; padding: 10px 14px; background: #f8f9ff; border-radius: 8px; border-left: 3px solid #aac4e8; }
      .demo-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 18px; }
      @media(max-width:580px){ .demo-grid { grid-template-columns: 1fr; } }
      .matrix-card { padding: 24px 20px; }
      .matrix-header, .matrix-row {
        display: grid;
        grid-template-columns: 3.5fr repeat(6, minmax(40px, 0.8fr));
        align-items: center; gap: 0;
      }
      .matrix-header {
        padding-bottom: 12px; margin-bottom: 4px; border-bottom: 2px solid #dce8f7;
        position: sticky; top: 0; z-index: 100; background: white;
        padding-top: 8px; box-shadow: 0 3px 8px rgba(0,0,0,0.07);
      }
      .matrix-item-col { font-size: 1.5rem; line-height: 1.55; padding-right: 20px; color: #1a1a2e; }
      .matrix-anchor { font-size: 1.3rem; color: #333; font-weight: 500; text-align: center; line-height: 1.3; padding: 0 2px; white-space: pre-line; word-break: break-word; }
      .matrix-row { padding: 12px 0; border-bottom: 1px solid #f0f2f8; }
      .matrix-row:last-child { border-bottom: none; }
      .matrix-cell { display: flex; align-items: center; justify-content: center; }
      .matrix-cell input[type='radio'] { width: 22px; height: 22px; cursor: pointer; accent-color: #004E9E; margin: 0; }
      .btn-primary-fab {
        background: linear-gradient(135deg, #004E9E, #1a6dd4); color: white;
        border: none; border-radius: 12px; padding: 14px 36px;
        font-size: 1.5rem; font-weight: 600; cursor: pointer; width: 100%;
        margin-top: 8px; transition: opacity 0.18s; font-family: 'Inter', sans-serif;
      }
      .btn-primary-fab:hover { opacity: 0.88; }
      .btn-secondary-fab {
        background: white; color: #004E9E; border: 2px solid #004E9E;
        border-radius: 12px; padding: 11px 30px; font-size: 1.5rem; font-weight: 600;
        cursor: pointer; transition: all 0.18s; font-family: 'Inter', sans-serif;
      }
      .btn-secondary-fab:hover { background: #e8f0fb; }
      .btn-print {
        background: white; color: #004E9E; border: 2px solid #004E9E;
        border-radius: 12px; padding: 11px 30px; font-size: 1.5rem; font-weight: 600;
        cursor: pointer; transition: all 0.18s; font-family: 'Inter', sans-serif;
      }
      .btn-print:hover { background: #e8f0fb; }
      .results-buttons { display: flex; gap: 12px; margin-bottom: 30px; flex-wrap: wrap; }
      .subscale-grid { display: grid; grid-template-columns: repeat(3,1fr); gap: 20px; }
      @media(max-width:700px){ .subscale-grid { grid-template-columns: 1fr; } }
      .subscale-panel { background: white; border-radius: 14px; padding: 16px 12px 12px; box-shadow: 0 2px 10px rgba(0,0,0,0.06); }
      .subscale-title { font-size: 1.5rem; font-weight: 700; color: #1a1a2e; text-align: center; margin-bottom: 8px; line-height: 1.3; }
      .subscale-divider { border: none; border-top: 1px solid #eef2ff; margin: 10px 0; }
      .gauge-stats { margin-top: 4px; display: flex; justify-content: center; align-items: center; gap: 8px; flex-wrap: wrap; }
      .gauge-classification { font-size: 1.3rem; color: #555; font-style: italic; text-align: center; margin-top: 2px; }
      .gauge-mean { font-size: 1.5rem; color: #111; font-weight: 500; }
      .pct-box { background: #f0f5fb; border-radius: 10px; padding: 14px 18px; font-size: 1.5rem; color: #1a1a2e; line-height: 1.6; margin-bottom: 14px; }
      .badge-comp { display: inline-block; background: #dce8f7; color: #004E9E; border-radius: 20px; padding: 6px 18px; font-size: 1.5rem; font-weight: 600; }
      .app-footer { text-align: center; font-size: 1.2rem; color: #888; padding: 16px 0 24px; line-height: 1.8; }
      .footer-link { color: #004E9E; text-decoration: none; }
      .footer-link:hover { text-decoration: underline; }
      .err-box { background: #fff0f0; border: 1px solid #ffcdd2; border-radius: 8px; padding: 9px 15px; color: #c62828; font-size: 1.5rem; margin-bottom: 14px; }
      /* Percentile table */
      .pct-table { width: 100%; border-collapse: collapse; margin-top: 14px; font-size: 1.5rem; }
      .pct-table th { background: #e8f0fb; color: #004E9E; font-weight: 600; padding: 10px 14px; text-align: left; border-bottom: 2px solid #dce8f7; }
      .pct-table td { padding: 9px 14px; border-bottom: 1px solid #f0f4f9; color: #333; }
      .pct-table tr:last-child td { border-bottom: none; }
      .pct-table tr:nth-child(even) td { background: #f8fafd; }
      /* Subscale result cards */
      .subscale-result-card { margin-bottom: 20px; }
      .facet-desc { font-size: 1.5rem; color: #1a1a2e; line-height: 1.55; margin-bottom: 16px; }
      .plots-row { display: flex; gap: 16px; margin-bottom: 12px; }
      .plot-col { flex: 1; min-width: 0; }
      @media(max-width: 600px) { .plots-row { flex-direction: column; } }
      .summary-sent { font-size: 1.5rem; color: #1a1a2e; line-height: 1.6; background: #f0f5fb; border-radius: 10px; padding: 12px 16px; margin-top: 4px; }
      /* FAB info */
      .fab-info-card { margin-top: 4px; }
      .fab-info-title { font-size: 1.5rem; font-weight: 700; color: #004E9E; margin-bottom: 10px; }
      .fab-info-text { font-size: 1.3rem; color: #444; line-height: 1.6; }
      .fab-link { color: #004E9E; font-weight: 600; text-decoration: none; }
      .fab-link:hover { text-decoration: underline; }
      .fab-info-card { margin-top: 4px; }
      .fab-info-title { font-size: 1.5rem; font-weight: 700; color: #004E9E; margin-bottom: 10px; }
      .fab-info-text { font-size: 1.3rem; color: #444; line-height: 1.6; }
      .fab-link { color: #004E9E; font-weight: 600; text-decoration: none; }
      .fab-link:hover { text-decoration: underline; }
      .form-control, .selectize-input, .selectize-input input,
      .selectize-dropdown, .selectize-dropdown-content,
      .selectize-dropdown .option, .selectize-dropdown .active {
        font-family: 'Inter', sans-serif !important; font-size: 1.5rem !important;
      }
      .form-control, .selectize-input { border-radius: 9px !important; border: 2px solid #dce8f7 !important; }
      .selectize-dropdown { max-height: 340px !important; }
      .selectize-dropdown-content { max-height: 340px !important; }
      .control-label { font-size: 1.5rem !important; font-family: 'Inter', sans-serif !important; }
      .lang-bar select, .lang-bar .form-control { font-size: 1.5rem !important; font-family: 'Inter', sans-serif !important; }
      @media print {
        .app-header, .lang-bar, .results-buttons, .pct-box, .badge-comp { display: none !important; }
        body { background: white !important; }
        .card { box-shadow: none !important; border: 1px solid #ddd; }
        .main-wrap { margin: 0 !important; padding: 0 !important; max-width: 100% !important; }
      }
    "))
  ),

  div(class = "app-header",
    div(class = "app-header-text",
      tags$h2(textOutput("hdr_title")),
      tags$p(textOutput("hdr_subtitle"))
    ),
    conditionalPanel("output.current_panel == 'fab_items'",
      div(class = "lang-switcher",
        tags$button("Deutsch", class = "btn-lang", id = "btn_lang_de",
          onclick = "Shiny.setInputValue('sel_lang', 'DE', {priority: 'event'})"),
        tags$button("English", class = "btn-lang", id = "btn_lang_en",
          onclick = "Shiny.setInputValue('sel_lang', 'EN', {priority: 'event'})")
      )
    ),
    conditionalPanel("output.current_panel != 'fab_items'",
      actionButton("btn_home", label = textOutput("lbl_home"), class = "btn-home")
    )
  ),

  div(style = "display:none;",
    selectInput("sel_lang", label = NULL, choices = c("Deutsch" = "DE", "English" = "EN"), selected = "DE")
  ),

  div(class = "main-wrap",
    conditionalPanel("output.current_panel == 'fab_items'",
      div(class = "intro-box", textOutput("lbl_intro")),
      div(class = "scale-banner", textOutput("lbl_scale")),
      uiOutput("ui_items"),
      uiOutput("ui_item_err"),
      actionButton("btn_next", textOutput("lbl_next"), class = "btn-primary-fab", style = "margin-top: 14px;")
    ),
    conditionalPanel("output.current_panel == 'fab_demo'",
      div(class = "card",
        div(class = "card-title", textOutput("lbl_demo_hdr")),
        div(class = "optional-hint", textOutput("lbl_optional_hint")),
        div(class = "demo-grid",
          numericInput("inp_age",   textOutput("lbl_age"),    value = NA, min = 18, max = 100),
          selectInput("inp_gender", textOutput("lbl_gender"), choices = c("–" = ""))
        ),
        selectInput("inp_edu", textOutput("lbl_edu"), choices = c("–" = "")),
        uiOutput("ui_demo_err"),
        actionButton("btn_submit", textOutput("lbl_submit"), class = "btn-primary-fab")
      )
    ),
    conditionalPanel("output.current_panel == 'fab_results'",
      div(class = "card",
        div(class = "card-title", textOutput("lbl_results_hdr")),
        uiOutput("ui_comp_badge")
      ),
      # Subscale results (one card per subscale)
      uiOutput("ui_subscale_panels"),
      # Percentile explanation + table BELOW results
      div(class = "card",
        div(class = "fab-info-title", textOutput("lbl_pct_section_hdr")),
        div(class = "pct-box", style = "margin-bottom: 0;", textOutput("lbl_pct_info")),
        uiOutput("ui_pct_table")
      ),
      # FAB info at bottom
      div(class = "card fab-info-card",
        uiOutput("ui_fab_info")
      ),
      div(class = "results-buttons",
        actionButton("btn_back", textOutput("lbl_back"), class = "btn-secondary-fab")
      )
    )
  ),
  tags$footer(class = "app-footer", uiOutput("ui_footer"))
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  rv_panel      <- reactiveVal("fab_items")
  rv_scores     <- reactiveVal(NULL)
  rv_ref        <- reactiveVal(NULL)
  rv_item_order <- reactiveVal(NULL)

  is_de <- reactive({ input$sel_lang == "DE" })

  output$current_panel <- reactive({ rv_panel() })
  outputOptions(output, "current_panel", suspendWhenHidden = FALSE)

  output$hdr_title        <- renderText({ if(is_de()) "FAB \u2013 Fragebogen zu altruistischen Verhaltensweisen" else "FAB \u2013 Facets of Altruistic Behaviors Scale" })
  output$hdr_subtitle     <- renderText({ "" })
  output$lbl_lang         <- renderText({ if(is_de()) "Sprache" else "Language" })
  output$lbl_demo_hdr     <- renderText({ if(is_de()) "Angaben zu Ihrer Person" else "About You" })
  output$lbl_age          <- renderText({ if(is_de()) "Alter" else "Age" })
  output$lbl_gender       <- renderText({ if(is_de()) "Geschlecht" else "Gender" })
  output$lbl_edu          <- renderText({ if(is_de()) "H\u00f6chster Bildungsabschluss" else "Highest level of education" })
  output$lbl_next         <- renderText({ if(is_de()) "Weiter \u2192" else "Next \u2192" })
  output$lbl_intro        <- renderText({
    if(is_de()) "Wie stark ausgeprägt sind Ihre altruistischen Verhaltenstendenzen? Der folgende Fragebogen erfasst Ihre Werte in drei Facetten und vergleicht sie mit einer Normstichprobe. Bitte beantworten Sie alle 15 Aussagen spontan und ehrlich."
    else "How strong are your altruistic behavioral tendencies? The following questionnaire assesses your scores across three facets and compares them to a normative sample. Please answer all 15 statements spontaneously and honestly."
  })
  output$lbl_scale        <- renderText({ if(is_de()) "Bitte geben Sie an, wie sehr die folgenden Aussagen typischerweise auf Sie zutreffen." else "Please state honestly and spontaneously how you would typically behave or act." })
  output$lbl_submit       <- renderText({ if(is_de()) "Auswerten" else "Submit" })
  output$lbl_results_hdr  <- renderText({ if(is_de()) "Ihre Ergebnisse" else "Your Results" })
  output$lbl_pct_section_hdr <- renderText({ if(is_de()) "Erkl\u00e4rung zu den Prozenträngen" else "About Percentile Ranks" })
  output$lbl_pct_info     <- renderText({
    if(is_de()) "Der Prozentrang gibt an, wie Ihre Werte im Vergleich zur Referenzgruppe einzuordnen sind \u2013 er ist rein beschreibend und nicht wertend. Ein Prozentrang von 75 bedeutet, dass Ihre Werte h\u00f6her sind als die von 75\u00a0% der Vergleichspersonen."
    else "The percentile rank describes how your scores compare to the reference group \u2013 it is purely descriptive and not evaluative. A percentile rank of 75 means your scores are higher than those of 75\u00a0% of the comparison group."
  })
  output$lbl_back         <- renderText({ if(is_de()) "\u2190 Zur\u00fcck" else "\u2190 Back" })
  output$lbl_footer        <- renderText({ "" })  # replaced by ui_footer
  output$lbl_home         <- renderText({ if(is_de()) "Start" else "Start" })
  output$lbl_print        <- renderText({ if(is_de()) "Ergebnisse drucken" else "Print results" })
  output$lbl_optional_hint <- renderText({
    if(is_de()) "Ihre Angaben sind freiwillig. Ohne Angaben werden Ihre Ergebnisse mit der gesamten deutschsprachigen Stichprobe verglichen (N\u00a0=\u00a03.757). Mit Angaben werden Ihre Ergebnisse verglichen mit Personen gleichen Geschlechts, \u00e4hnlichen Alters (\u00b110 Jahre) und gleicher Bildung. Sind in dieser Gruppe weniger als 20 Personen vorhanden, wird die Vergleichsgruppe schrittweise erweitert. Es werden keinerlei Daten gespeichert oder \u00fcbertragen."
    else "Your input is optional. Without input, your results will be compared to the full English-speaking sample (N\u00a0=\u00a02,049). With input, your results will be compared to people of the same gender, similar age (\u00b110 years), and same education level. If fewer than 20 people match these criteria, the comparison group is broadened stepwise. No data is stored or transmitted."
  })

  observeEvent(input$sel_lang, {
    updateSelectInput(session, "inp_gender", choices = c("\u2013" = "", if(is_de()) gender_de else gender_en))
    updateSelectInput(session, "inp_edu",    choices = c("\u2013" = "", if(is_de()) edu_de else edu_en))
    rv_item_order(NULL)
    session$sendCustomMessage("activeLang", input$sel_lang)
  })

  output$ui_demo_err <- renderUI(NULL)

  observeEvent(input$btn_next, { rv_panel("fab_demo") })

  output$ui_items <- renderUI({
    items_def <- if(is_de()) items_de else items_en
    legend    <- if(is_de()) scale_legend_de else scale_legend_en
    all_items <- list()
    for (sk in names(items_def))
      for (ik in names(items_def[[sk]]$items))
        all_items[[ik]] <- items_def[[sk]]$items[[ik]]
    if (is.null(rv_item_order())) rv_item_order(sample(names(all_items)))
    item_order <- rv_item_order()
    header_row <- div(class = "matrix-header",
      div(class = "matrix-item-col"),
      lapply(legend, function(l) div(class = "matrix-anchor", l))
    )
    item_rows <- lapply(item_order, function(ik) {
      radio_cells <- lapply(1:6, function(val) {
        div(class = "matrix-cell",
          tags$input(type = "radio", name = paste0("r_", ik), value = val,
            id = paste0("r_", ik, "_", val),
            onclick = paste0("Shiny.setInputValue('r_", ik, "', ", val, ", {priority: 'event'})"))
        )
      })
      div(class = "matrix-row", div(class = "matrix-item-col", all_items[[ik]]), tagList(radio_cells))
    })
    div(class = "card matrix-card", header_row, tagList(item_rows))
  })

  output$ui_item_err <- renderUI(NULL)

  observeEvent(input$btn_submit, {
    age_val <- input$inp_age
    age_ok  <- is.na(age_val) || (age_val >= 18 && age_val <= 100)
    if (!age_ok) {
      output$ui_demo_err <- renderUI(div(class = "err-box",
        if(is_de()) "Bitte geben Sie ein g\u00fcltiges Alter ein (18\u2013100) oder lassen Sie das Feld leer."
        else "Please enter a valid age (18\u2013100) or leave the field empty."))
      return()
    }
    output$ui_demo_err <- renderUI(NULL)
    items_def  <- if(is_de()) items_de else items_en
    sub_means  <- list()
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
    ref <- get_reference(input$sel_lang, input$inp_gender,
                         if(is.na(input$inp_age)) NA else as.numeric(input$inp_age),
                         input$inp_edu)
    rv_ref(ref)
    subscale_cols <- if(is_de()) c(hg="hg", pp="pp", mc="mc") else c(cr="cr", cp="cp", cc="cc")
    pcts <- lapply(names(sub_means), function(sk) compute_percentile(sub_means[[sk]], ref$data[[subscale_cols[[sk]]]]))
    names(pcts) <- names(sub_means)
    ref_scores_list <- lapply(names(sub_means), function(sk) {
      col <- subscale_cols[[sk]]
      ref$data[[col]][!is.na(ref$data[[col]])]
    })
    names(ref_scores_list) <- names(sub_means)
    rv_scores(list(means = sub_means, percentiles = pcts, ref_scores = ref_scores_list))
    rv_panel("fab_results")
  })

  output$ui_comp_badge <- renderUI({
    ref <- rv_ref(); if(is.null(ref)) return(NULL)
    n <- nrow(ref$data)
    lbl_text <- switch(ref$level,
      "specific" = if(is_de()) paste0("Vergleichsgruppe (passend): n\u00a0=\u00a0", n, " Personen") else paste0("Matched comparison group: n\u00a0=\u00a0", n, " participants"),
      "broad"    = if(is_de()) paste0("Vergleichsgruppe (breit): n\u00a0=\u00a0", n, " Personen")    else paste0("Broad comparison group: n\u00a0=\u00a0", n, " participants"),
      if(is_de()) paste0("Gesamtstichprobe: N\u00a0=\u00a0", n, " Personen") else paste0("Full sample: N\u00a0=\u00a0", n, " participants")
    )
    div(style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:10px; margin-bottom:16px;",
      div(class = "badge-comp", style = "margin-bottom:0;", lbl_text),
      tags$button(textOutput("lbl_print"), class = "btn-print", onclick = "window.print()")
    )
  })

  # ── Percentile classification table ─────────────────────────────────────────
  output$ui_pct_table <- renderUI({
    if(is_de()) {
      rows <- list(
        c("< 16",  "weit unterdurchschnittlich"),
        c("16–25", "unterdurchschnittlich"),
        c("26–75", "durchschnittlich"),
        c("76–84", "überdurchschnittlich"),
        c("> 84",  "weit überdurchschnittlich")
      )
      hdr <- c("Prozentrang", "Einordnung")
    } else {
      rows <- list(
        c("< 16",  "far below average"),
        c("16–25", "below average"),
        c("26–75", "average"),
        c("76–84", "above average"),
        c("> 84",  "far above average")
      )
      hdr <- c("Percentile rank", "Classification")
    }
    tags$table(class = "pct-table",
      tags$thead(
        tags$tr(lapply(hdr, function(h) tags$th(h)))
      ),
      tags$tbody(
        lapply(rows, function(r) tags$tr(lapply(r, function(cell) tags$td(cell))))
      )
    )
  })

  output$ui_subscale_panels <- renderUI({
    sc <- rv_scores(); if(is.null(sc)) return(NULL)
    items_def <- if(is_de()) items_de else items_en
    desc_vec  <- if(is_de()) facet_desc_de else facet_desc_en
    ref       <- rv_ref()
    n_ref     <- if(!is.null(ref)) nrow(ref$data) else "?"

    lapply(names(sc$means), function(sk) {
      sub       <- items_def[[sk]]
      mean_val  <- round(sc$means[[sk]], 2)
      pct       <- sc$percentiles[[sk]]
      pct_class <- if(!is.null(pct)) classify_percentile(pct, if(is_de()) "DE" else "EN") else ""
      facet_txt <- if(sk %in% names(desc_vec)) desc_vec[[sk]] else ""

      # Summary sentence
      summary_sent <- if(!is.null(pct)) {
        if(is_de())
          paste0("Auf der Subskala ", sub$subscale_label, " wurde ein Mittelwert von ", mean_val,
                 " (Skala: 1–6) erreicht. Das entspricht einem Prozentrang von ", pct,
                 " und ist damit ", pct_class, " im Vergleich zur Referenzgruppe (N = ", n_ref, " Personen).")
        else
          paste0("On the ", sub$subscale_label, " subscale, a mean score of ", mean_val,
                 " (scale: 1–6) was achieved. This corresponds to a percentile rank of ", pct,
                 ", which is ", pct_class, " compared to the reference group (N = ", n_ref, " participants).")
      } else {
        if(is_de()) "Zu wenige Vergleichsdaten verfügbar." else "Insufficient comparison data available."
      }

      div(class = "card subscale-result-card",
        # Subscale title
        div(class = "card-title", sub$subscale_label),
        # Short definition
        div(class = "facet-desc", facet_txt),
        # Gauges side by side
        div(class = "plots-row",
          div(class = "plot-col",
            plotOutput(paste0("g_", sk), height = "210px")
          ),
          div(class = "plot-col",
            plotOutput(paste0("n_", sk), height = "210px")
          )
        ),
        # Summary sentence
        div(class = "summary-sent", summary_sent)
      )
    })
  })

  observe({
    sc <- rv_scores(); if(is.null(sc)) return()
    items_def    <- if(is_de()) items_de else items_en
    current_lang <- input$sel_lang
    for (sk in names(sc$means)) {
      local({
        key  <- sk; pct <- sc$percentiles[[key]]; sub <- items_def[[key]]; lang <- current_lang
        output[[paste0("g_", key)]] <- renderPlot({
          if(is.null(pct)) ggplot() + annotate("text",x=0,y=0,label="n/a",size=7,color="#aaa") + theme_void()
          else make_gauge(pct, sub$color, lang = lang)
        }, bg = "white")
      })
    }
  })

  observe({
    sc <- rv_scores(); if(is.null(sc)) return()
    items_def    <- if(is_de()) items_de else items_en
    current_lang <- input$sel_lang
    for (sk in names(sc$means)) {
      local({
        key <- sk; user_score <- sc$means[[key]]; ref_scores <- sc$ref_scores[[key]]
        sub <- items_def[[key]]; lang <- current_lang
        output[[paste0("n_", key)]] <- renderPlot({
          if(is.null(ref_scores) || length(ref_scores) < 10)
            ggplot() + annotate("text",x=0,y=0,label="n/a",size=6,color="#aaa") + theme_void()
          else make_norm_plot(user_score, ref_scores, sub$color, lang = lang)
        }, bg = "white")
      })
    }
  })

  output$ui_fab_info <- renderUI({
    if(is_de()) {
      tagList(
        div(class = "fab-info-title", "Zur FAB-Skala"),
        div(class = "fab-info-text",
          "Die FAB-Skala (Facets of Altruistic Behaviors) wurde von Windmann, Binder & Schultze (2021) entwickelt. Sie misst altruistische Verhaltenstendenzen in drei Facetten: Altruistisches Verstärken (Help Giving), Altruistisches Bestrafen (Peer Punishment) und Altruistischer Widerstand (Moral Courage). Items wurden mithilfe des Ant Colony Optimization-Verfahrens ausgewählt und zeigten exzellente Messmodelleigenschaften. ",
          tags$a("Zum Originalartikel →", href = "https://doi.org/10.1027/1864-9335/a000460", target = "_blank", class = "fab-link")
        )
      )
    } else {
      tagList(
        div(class = "fab-info-title", "About the FAB Scale"),
        div(class = "fab-info-text",
          "The FAB scale (Facets of Altruistic Behaviors) was developed by Windmann, Binder & Schultze (2021). It measures altruistic behavioral tendencies across three facets: Costly Rewarding (Help Giving), Costly Punishment (Peer Punishment), and Costly Countercontrol (Moral Courage). Items were selected using Ant Colony Optimization procedures and demonstrated excellent measurement model properties. Note: The English item selection is based on an independent US sample and differs slightly from the German version (manuscript in preparation). ",
          tags$a("Read the original article →", href = "https://doi.org/10.1027/1864-9335/a000460", target = "_blank", class = "fab-link")
        )
      )
    }
  })

  output$ui_footer <- renderUI({
    url <- "https://www.psychologie.uni-frankfurt.de/50042693/Willkommen_bei_der_Allgemeinen_Psychologie_II__br_Prof__Dr__Sabine_Windmann"
    if(is_de()) {
      tagList(
        tags$span("© 2026 Sabine Windmann & Lucie Binder · "),
        tags$a("ACT Labor, Goethe-Universität Frankfurt", href = url, target = "_blank", class = "footer-link"),
        tags$span(" · Es werden keinerlei Daten gespeichert oder übertragen.")
      )
    } else {
      tagList(
        tags$span("© 2026 Sabine Windmann & Lucie Binder · "),
        tags$a("ACT Lab, Goethe University Frankfurt", href = url, target = "_blank", class = "footer-link"),
        tags$span(" · No data is stored or transmitted.")
      )
    }
  })

  observeEvent(input$btn_back, {
    rv_panel("fab_items"); rv_scores(NULL); rv_ref(NULL); rv_item_order(NULL)
  })
  observeEvent(input$btn_home, {
    rv_panel("fab_items"); rv_scores(NULL); rv_ref(NULL); rv_item_order(NULL)
  })
}

shinyApp(ui, server)

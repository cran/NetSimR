#' Card header helper for the Shiny simulator UI
#'
#' @param icon_name Font Awesome icon name.
#' @param title Card title.
#' @param subtitle Optional short description shown under the title.
#' @noRd
sim_card_header <- function(icon_name, title, subtitle = NULL) {
  bslib::card_header(
    class = "sim-card-header",
    div(class = "sim-card-icon", icon(icon_name)),
    div(
      class = "sim-card-titles",
      div(class = "sim-card-title", title),
      if (!is.null(subtitle)) div(class = "sim-card-subtitle", subtitle)
    )
  )
}

#' Hide an input's label from sight, not from screen readers
#'
#' Inputs whose heading is drawn separately (the pill radio groups under a section label,
#' the run name boxes of the Compare tab) still need a label for their accessible name: a
#' label of NULL leaves the group announced without one. Bootstrap's visually-hidden class
#' keeps the label for screen readers and takes it out of the layout.
#' @param input A shiny input whose label is one of its direct children, e.g. from
#'   radioButtons() or textInput().
#' @return The input with the class added to its label.
#' @noRd
sim_hidden_label <- function(input) {
  input$children <- lapply(input$children, function(child) {
    if (inherits(child, "shiny.tag") && identical(child$name, "label")) {
      tagAppendAttributes(child, class = "visually-hidden")
    } else {
      child
    }
  })
  input
}

#' Theme switcher (light / dark / system) for the Shiny simulator UI
#'
#' @noRd
sim_theme_switch <- function() {
  theme_button <- function(value, icon_name, label) {
    tags$button(
      type = "button",
      class = "theme-btn",
      `data-theme-value` = value,
      title = paste(label, "theme"),
      `aria-label` = paste(label, "theme"),
      `aria-pressed` = "false",
      icon(icon_name),
      tags$span(class = "theme-btn-label", label)
    )
  }
  div(
    class = "theme-switch",
    role = "group",
    `aria-label` = "Colour theme",
    theme_button("light", "sun", "Light"),
    theme_button("dark", "moon", "Dark"),
    theme_button("system", "circle-half-stroke", "System")
  )
}

#' Styles for the Shiny simulator UI
#'
#' @noRd
sim_ui_css <- "
:root, [data-bs-theme='light'] {
  --sim-page-bg: #f1f5f9;
  --sim-card-bg: #ffffff;
  --sim-card-header-bg: #ffffff;
  --sim-border: rgba(148, 163, 184, 0.30);
  --sim-border-strong: #cbd5e1;
  --sim-muted: #64748b;
  --sim-heading: #0f172a;
  --sim-label: #334155;
  --sim-accent-soft: #eff6ff;
  --sim-accent-text: #1d4ed8;
  --sim-input-bg: #ffffff;
  --sim-shadow: 0 1px 2px rgba(15, 23, 42, 0.04), 0 8px 24px rgba(15, 23, 42, 0.06);
  --sim-radius: 16px;
}

[data-bs-theme='dark'] {
  --sim-page-bg: #0b1220;
  --sim-card-bg: #111a2b;
  --sim-card-header-bg: #111a2b;
  --sim-border: rgba(148, 163, 184, 0.16);
  --sim-border-strong: #334155;
  --sim-muted: #94a3b8;
  --sim-heading: #f1f5f9;
  --sim-label: #cbd5e1;
  --sim-accent-soft: rgba(59, 130, 246, 0.16);
  --sim-accent-text: #93c5fd;
  --sim-input-bg: #0d1526;
  --sim-shadow: 0 1px 2px rgba(0, 0, 0, 0.3), 0 8px 24px rgba(0, 0, 0, 0.35);

  --bs-body-bg: #0d1526;
  --bs-body-bg-rgb: 13, 21, 38;
  --bs-body-color: #e2e8f0;
  --bs-body-color-rgb: 226, 232, 240;
  --bs-secondary-bg: #16213a;
  --bs-tertiary-bg: #131d33;
  --bs-border-color: #334155;
  --bs-emphasis-color: #f8fafc;
  --bs-link-color: #93c5fd;
  --bs-link-color-rgb: 147, 197, 253;
  --bs-link-hover-color: #bfdbfe;
  --bs-link-hover-color-rgb: 191, 219, 254;
}

html, body {
  background-color: var(--sim-page-bg) !important;
}

body {
  transition: background-color 0.2s ease, color 0.2s ease;
}

/* ---------- Navbar ---------- */
.navbar {
  background: linear-gradient(90deg, #0b1f3d 0%, #13315c 100%) !important;
  box-shadow: 0 2px 16px rgba(15, 23, 42, 0.18);
  padding-top: 0.5rem;
  padding-bottom: 0.5rem;
}

/* the brand keeps a gap after it, so the nav links never touch its subtitle; the gap is a
   margin on .sim-brand, inside .navbar-brand. Where room is short the brand shrinks, its
   title and subtitle ending in an ellipsis: beside the menu button on a narrow screen, and
   beside the nav links on a wide one, which stay on one row (see Responsive) */
.navbar .navbar-header {
  min-width: 0;
}

.navbar .navbar-toggle {
  flex-shrink: 0;
}

.navbar-brand {
  display: block;
  min-width: 0;
  margin-right: 0 !important;
}

.sim-brand {
  display: flex;
  align-items: center;
  gap: 12px;
  margin-right: 2rem;
  min-width: 0;
}

.sim-brand-mark {
  width: 40px;
  height: 40px;
  border-radius: 11px;
  background: linear-gradient(135deg, #60a5fa, #2563eb);
  display: flex;
  align-items: center;
  justify-content: center;
  color: #ffffff;
  font-size: 18px;
  box-shadow: 0 6px 18px rgba(37, 99, 235, 0.35);
  flex-shrink: 0;
}

.sim-brand-text {
  display: flex;
  flex-direction: column;
  line-height: 1.1;
  white-space: nowrap;
  min-width: 0;
}

.sim-brand-title,
.sim-brand-subtitle {
  overflow: hidden;
  text-overflow: ellipsis;
}

.sim-brand-title {
  font-weight: 800;
  font-size: 1.05rem;
  color: #ffffff;
  letter-spacing: -0.01em;
}

.sim-brand-subtitle {
  font-size: 0.75rem;
  color: rgba(255, 255, 255, 0.7);
  margin-top: 2px;
}

.navbar .navbar-nav {
  gap: 0.25rem;
  align-items: center;
}

.navbar .nav-link {
  color: rgba(255, 255, 255, 0.82) !important;
  font-weight: 600;
  border-radius: 10px;
  padding: 0.55rem 0.95rem !important;
  transition: background-color 0.15s ease, color 0.15s ease;
}

.navbar .nav-link:hover,
.navbar .nav-link:focus-visible {
  color: #ffffff !important;
  background-color: rgba(255, 255, 255, 0.10);
}

.navbar .nav-link.active {
  color: #ffffff !important;
  background-color: rgba(255, 255, 255, 0.16) !important;
  box-shadow: inset 0 0 0 1px rgba(255, 255, 255, 0.18);
}

/* ---------- Theme switch ---------- */
.theme-switch {
  display: inline-flex;
  align-items: center;
  gap: 2px;
  padding: 3px;
  border-radius: 999px;
  background: rgba(255, 255, 255, 0.08);
  box-shadow: inset 0 0 0 1px rgba(255, 255, 255, 0.14);
}

.theme-btn {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  border: 0;
  background: transparent;
  color: rgba(255, 255, 255, 0.75);
  font-size: 0.8rem;
  font-weight: 600;
  padding: 0.35rem 0.7rem;
  border-radius: 999px;
  cursor: pointer;
  transition: background-color 0.15s ease, color 0.15s ease;
}

.theme-btn:hover {
  color: #ffffff;
  background: rgba(255, 255, 255, 0.10);
}

.theme-btn:focus-visible {
  outline: 2px solid #93c5fd;
  outline-offset: 1px;
}

.theme-btn.active {
  background: #ffffff;
  color: #0b1f3d;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.2);
}

/* ---------- Page ---------- */
.tab-content {
  padding-top: 0.5rem;
}

.sim-page-intro {
  display: flex;
  flex-wrap: wrap;
  align-items: flex-end;
  justify-content: space-between;
  gap: 0.5rem 1rem;
  margin: 0.25rem 0 1rem 0;
}

.sim-page-title {
  font-size: 1.5rem;
  font-weight: 800;
  letter-spacing: -0.02em;
  color: var(--sim-heading);
  margin: 0;
}

.sim-page-subtitle {
  color: var(--sim-muted);
  margin: 0.2rem 0 0 0;
}

/* ---------- Cards ---------- */
.card {
  --bs-card-bg: var(--sim-card-bg);
  --bs-card-cap-bg: var(--sim-card-header-bg);
  --bs-card-border-color: var(--sim-border);
  border-radius: var(--sim-radius) !important;
  box-shadow: var(--sim-shadow);
  overflow: visible;
}

.card-header.sim-card-header {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  padding: 0.9rem 1.1rem;
  border-bottom: 1px solid var(--sim-border);
  border-top-left-radius: var(--sim-radius) !important;
  border-top-right-radius: var(--sim-radius) !important;
}

.sim-card-icon {
  width: 36px;
  height: 36px;
  flex: 0 0 36px;
  border-radius: 10px;
  display: flex;
  align-items: center;
  justify-content: center;
  background: var(--sim-accent-soft);
  color: var(--sim-accent-text);
  font-size: 15px;
}

.sim-card-title {
  font-weight: 700;
  color: var(--sim-heading);
  line-height: 1.2;
}

.sim-card-subtitle {
  font-size: 0.82rem;
  color: var(--sim-muted);
  line-height: 1.3;
  margin-top: 2px;
}

.card-body {
  padding: 1.1rem;
}

.sim-section-label {
  font-size: 0.72rem;
  font-weight: 800;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: var(--sim-muted);
  margin: 0 0 0.6rem 0;
}

.sim-divider {
  border: 0;
  border-top: 1px solid var(--sim-border);
  margin: 1rem 0;
  opacity: 1;
}

/* ---------- Inputs ---------- */
.shiny-input-container:not(.shiny-input-container-inline) {
  width: 100%;
  max-width: 100%;
}

.control-label, .form-label {
  font-weight: 600;
  font-size: 0.88rem;
  color: var(--sim-label);
  margin-bottom: 0.35rem;
}

.form-control, .form-select {
  border-radius: 10px;
  border-color: var(--sim-border-strong);
  background-color: var(--sim-input-bg);
  color: var(--bs-body-color);
}

.form-control:focus, .form-select:focus {
  border-color: #3b82f6;
  box-shadow: 0 0 0 0.2rem rgba(59, 130, 246, 0.18);
}

.help-block, .form-text {
  color: var(--sim-muted);
  font-size: 0.82rem;
}

.form-switch .form-check-label {
  font-weight: 600;
  font-size: 0.9rem;
  color: var(--sim-label);
}

.bslib-input-switch {
  margin-bottom: 0.5rem;
}

.param-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(140px, 1fr));
  gap: 0 0.75rem;
}

.param-grid-2 {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 0 0.75rem;
}

.param-grid > .shiny-html-output:empty,
.param-grid-2 > .shiny-html-output:empty {
  display: none;
}

/* Pill-style radio buttons */
.pill-radio .shiny-options-group {
  display: flex;
  flex-wrap: wrap;
  gap: 0.45rem;
}

.pill-radio.pill-radio-grid .shiny-options-group {
  display: grid;
  grid-template-columns: 1fr 1fr;
}

.pill-radio .radio-inline {
  position: relative;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  margin: 0 !important;
  padding: 0.42rem 0.85rem !important;
  border: 1px solid var(--sim-border-strong);
  border-radius: 999px;
  background: var(--sim-input-bg);
  color: var(--bs-body-color);
  font-weight: 600;
  font-size: 0.86rem;
  line-height: 1.25;
  text-align: center;
  cursor: pointer;
  transition: background-color 0.15s ease, border-color 0.15s ease, color 0.15s ease;
}

.pill-radio .radio-inline:hover {
  border-color: #3b82f6;
}

@supports selector(:has(*)) {
  .pill-radio .radio-inline input[type='radio'] {
    position: absolute;
    opacity: 0;
    width: 1px;
    height: 1px;
    margin: 0 !important;
    pointer-events: none;
  }

  .pill-radio .radio-inline:has(input:checked) {
    background: #2563eb;
    border-color: #2563eb;
    color: #ffffff;
    box-shadow: 0 4px 12px rgba(37, 99, 235, 0.25);
  }

  .pill-radio .radio-inline:has(input:focus-visible) {
    outline: 2px solid #93c5fd;
    outline-offset: 2px;
  }
}

/* Implied moments under the distribution parameters */
.sim-implied {
  font-size: 0.85rem;
  color: var(--sim-muted);
  margin: -0.25rem 0 0.5rem 0;
}

.sim-implied strong {
  font-weight: 700;
  color: var(--sim-heading);
}

.sim-implied-empty {
  font-style: italic;
}

.sim-truncate {
  margin-top: 0.75rem;
  padding-top: 0.75rem;
  border-top: 1px solid var(--sim-border);
}

/* Expected gross claims in the simulation panel */
#expected_gross_claims:empty {
  display: none;
}

.sim-expected {
  background: var(--sim-accent-soft);
  border: 1px solid var(--sim-border);
  border-radius: 10px;
  padding: 0.65rem 0.8rem;
  margin: 0.5rem 0 0.25rem 0;
}

.sim-expected-label {
  font-size: 0.72rem;
  font-weight: 800;
  letter-spacing: 0.07em;
  text-transform: uppercase;
  color: var(--sim-accent-text);
}

.sim-expected-value {
  font-size: 1.25rem;
  font-weight: 800;
  letter-spacing: -0.01em;
  color: var(--sim-heading);
  line-height: 1.3;
}

.sim-expected-note {
  font-size: 0.78rem;
  color: var(--sim-muted);
}

/* Select inputs */
[data-bs-theme='dark'] .selectize-input,
[data-bs-theme='dark'] .selectize-input.full,
[data-bs-theme='dark'] .selectize-dropdown {
  background: var(--sim-input-bg) !important;
  color: var(--bs-body-color) !important;
  border-color: var(--sim-border-strong) !important;
}

[data-bs-theme='dark'] .selectize-dropdown .active {
  background: var(--sim-accent-soft) !important;
  color: #ffffff !important;
}

.selectize-input {
  border-radius: 10px !important;
}

/* Pareto slices */
.sim-slices-head {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 0.5rem;
  margin-bottom: 0.25rem;
}

.sim-slices-title {
  font-weight: 600;
  font-size: 0.9rem;
  color: var(--sim-label);
}

.sim-hidden-input {
  display: none;
}

.sim-slice-row {
  display: flex;
  align-items: flex-end;
  gap: 0.6rem;
  padding: 0.55rem 0.7rem 0;
  margin-bottom: 0.5rem;
  border: 1px solid var(--sim-border);
  border-radius: 12px;
  background: var(--sim-input-bg);
}

.sim-slice-badge {
  flex: 0 0 28px;
  height: 28px;
  margin-bottom: 1.35rem;
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 0.78rem;
  font-weight: 800;
  background: var(--sim-accent-soft);
  color: var(--sim-accent-text);
}

.sim-slice-fields {
  flex: 1 1 auto;
  min-width: 0;
}

.sim-slice-remove {
  margin-bottom: 1.2rem;
}

.sim-slices-note {
  font-size: 0.85rem;
  color: var(--sim-muted);
  font-style: italic;
  margin: 0.25rem 0 0.6rem 0;
}

/* ---------- Buttons ---------- */
.btn {
  border-radius: 10px;
  font-weight: 600;
}

.btn-run {
  width: 100%;
  padding: 0.75rem 1rem;
  font-size: 1rem;
  font-weight: 700;
  border: 0;
  color: #ffffff;
  background: linear-gradient(135deg, #2563eb, #1d4ed8);
  box-shadow: 0 8px 20px rgba(37, 99, 235, 0.28);
}

.btn-run:hover, .btn-run:focus {
  color: #ffffff;
  background: linear-gradient(135deg, #1d4ed8, #1e40af);
}

.sim-downloads {
  display: grid;
  grid-template-columns: 1fr;
  gap: 0.5rem;
}

.sim-downloads .shiny-html-output:empty {
  display: none;
}

.sim-downloads .btn {
  width: 100%;
}

.sim-downloads-hint {
  grid-column: 1 / -1;
  display: flex;
  align-items: center;
  gap: 0.5rem;
  font-size: 0.85rem;
  color: var(--sim-muted);
  padding: 0.65rem 0.8rem;
  border: 1px dashed var(--sim-border-strong);
  border-radius: 10px;
}

.sim-downloads:has(.shiny-download-link) .sim-downloads-hint {
  display: none;
}

/* ---------- Welcome ---------- */
.sim-hero {
  display: flex;
  flex-direction: column;
  justify-content: center;
  height: 100%;
  position: relative;
  overflow: hidden;
  border-radius: var(--sim-radius);
  padding: 2.25rem 2rem;
  color: #ffffff;
  background:
    radial-gradient(circle at 85% 20%, rgba(96, 165, 250, 0.35), transparent 45%),
    linear-gradient(135deg, #0b1f3d 0%, #13315c 55%, #1d4ed8 130%);
  box-shadow: var(--sim-shadow);
}

.sim-hero-eyebrow {
  font-size: 0.75rem;
  font-weight: 800;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: #93c5fd;
  margin-bottom: 0.6rem;
}

.sim-hero-title {
  font-size: 2rem;
  font-weight: 800;
  letter-spacing: -0.03em;
  line-height: 1.15;
  margin-bottom: 0.75rem;
}

.sim-hero-text {
  font-size: 1.02rem;
  color: rgba(255, 255, 255, 0.8);
  max-width: 640px;
  margin-bottom: 1.4rem;
}

.sim-hero .btn-light {
  align-self: flex-start;
  font-weight: 700;
  color: #0b1f3d;
  padding: 0.6rem 1.1rem;
}

.sim-steps {
  list-style: none;
  counter-reset: step;
  padding: 0;
  margin: 0;
}

.sim-steps li {
  counter-increment: step;
  display: flex;
  gap: 0.75rem;
  align-items: flex-start;
  padding: 0.55rem 0;
}

.sim-steps li + li {
  border-top: 1px solid var(--sim-border);
}

.sim-steps li::before {
  content: counter(step);
  flex: 0 0 26px;
  height: 26px;
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 0.78rem;
  font-weight: 800;
  background: var(--sim-accent-soft);
  color: var(--sim-accent-text);
}

.sim-step-title {
  font-weight: 700;
  color: var(--sim-heading);
  line-height: 1.3;
}

.sim-step-text {
  font-size: 0.85rem;
  color: var(--sim-muted);
}

.sim-stat {
  background: var(--sim-card-bg);
  border: 1px solid var(--sim-border);
  border-radius: var(--sim-radius);
  box-shadow: var(--sim-shadow);
  padding: 1rem 1.1rem;
  height: 100%;
}

.sim-stat-label {
  display: flex;
  align-items: center;
  gap: 0.45rem;
  font-size: 0.72rem;
  font-weight: 800;
  text-transform: uppercase;
  letter-spacing: 0.07em;
  color: var(--sim-muted);
  margin-bottom: 0.4rem;
}

.sim-stat-value {
  font-weight: 600;
  color: var(--sim-heading);
  line-height: 1.45;
}

.sim-muted {
  color: var(--sim-muted);
}

/* ---------- Notifications ---------- */
.shiny-notification {
  background: var(--sim-card-bg);
  color: var(--bs-body-color);
  border: 1px solid var(--sim-border);
  border-radius: 12px;
  box-shadow: var(--sim-shadow);
}

/* ---------- Responsive ---------- */
@media (min-width: 992px) {
  .sim-layout > :first-child {
    position: sticky;
    top: 1rem;
    align-self: start;
  }
}

/* the expanded header is one row: the nav links and the theme switch never wrap, and the
   brand gives way to them instead */
@media (min-width: 992px) {
  .navbar .navbar-nav {
    flex-wrap: nowrap;
  }

  /* the brand is a block (so it can shrink), whose padding would add to the header's
     height, as it did not when it was an inline span */
  .navbar-brand {
    padding-top: 0;
    padding-bottom: 0;
  }

  .navbar .nav-link {
    white-space: nowrap;
  }
}

/* below 1400 px there is no room for every part at full size (the fitting tools have five
   or six tabs): the theme switch shows its icons only and the nav links sit closer */
@media (min-width: 992px) and (max-width: 1399.98px) {
  .theme-btn-label {
    display: none;
  }

  .navbar .nav-link {
    padding: 0.55rem 0.5rem !important;
  }
}

/* and below 1200 px the brand subtitle and the nav link icons are left out too */
@media (min-width: 992px) and (max-width: 1199.98px) {
  .sim-brand-subtitle,
  .navbar .nav-link > i {
    display: none;
  }
}

@media (max-width: 991.98px) {
  .theme-switch {
    margin: 0.5rem 0;
  }

  /* the gap before the menu button */
  .sim-brand {
    margin-right: 0.75rem;
  }
}

@media (max-width: 575.98px) {
  .theme-btn-label {
    display: none;
  }

  .pill-radio.pill-radio-grid .shiny-options-group,
  .param-grid-2 {
    grid-template-columns: 1fr;
  }

  .sim-hero {
    padding: 1.5rem 1.25rem;
  }

  .sim-hero-title {
    font-size: 1.5rem;
  }
}
"

#' Script that applies and remembers the light / dark / system theme choice
#'
#' Shared by the simulator and the distribution fitting tool; each app remembers
#' its own choice. With no stored choice the app follows the system setting.
#' @param storage_key The localStorage key the choice is saved under.
#' @noRd
netsimr_theme_js <- function(storage_key) {
  sub("__STORAGE_KEY__", storage_key, netsimr_theme_js_template, fixed = TRUE)
}

netsimr_theme_js_template <- "
(function () {
  var KEY = '__STORAGE_KEY__';
  var media = window.matchMedia ? window.matchMedia('(prefers-color-scheme: dark)') : null;

  function storedMode() {
    try { return localStorage.getItem(KEY) || 'system'; } catch (e) { return 'system'; }
  }

  function applyTheme(mode) {
    var dark = mode === 'dark' || (mode === 'system' && media && media.matches);
    document.documentElement.setAttribute('data-bs-theme', dark ? 'dark' : 'light');
    var buttons = document.querySelectorAll('.theme-switch .theme-btn');
    for (var i = 0; i < buttons.length; i++) {
      var on = buttons[i].getAttribute('data-theme-value') === mode;
      buttons[i].classList.toggle('active', on);
      buttons[i].setAttribute('aria-pressed', on ? 'true' : 'false');
    }
  }

  applyTheme(storedMode());

  if (media) {
    var onSystemChange = function () { if (storedMode() === 'system') applyTheme('system'); };
    if (media.addEventListener) media.addEventListener('change', onSystemChange);
    else if (media.addListener) media.addListener(onSystemChange);
  }

  document.addEventListener('DOMContentLoaded', function () { applyTheme(storedMode()); });

  document.addEventListener('click', function (event) {
    var button = event.target.closest && event.target.closest('.theme-switch .theme-btn');
    if (!button) return;
    var mode = button.getAttribute('data-theme-value');
    try { localStorage.setItem(KEY, mode); } catch (e) {}
    applyTheme(mode);
  });
})();
"

#' Script that shows busy states on the Run and Report buttons
#'
#' Server-side label updates only reach the browser after a run has finished,
#' so the busy state is set in the browser on click and cleared by a server message.
#' @noRd
sim_run_state_js <- "
(function () {
  function setBusy(el, label) {
    if (!el.hasAttribute('data-idle-html')) el.setAttribute('data-idle-html', el.innerHTML);
    var spinner = document.createElement('span');
    spinner.className = 'spinner-border spinner-border-sm me-2';
    spinner.setAttribute('aria-hidden', 'true');
    el.innerHTML = '';
    el.appendChild(spinner);
    el.appendChild(document.createTextNode(label));
    el.setAttribute('aria-busy', 'true');
  }

  function setIdle(el) {
    if (!el) return;
    if (el.hasAttribute('data-idle-html')) {
      el.innerHTML = el.getAttribute('data-idle-html');
      el.removeAttribute('data-idle-html');
    }
    el.removeAttribute('aria-busy');
  }

  document.addEventListener('click', function (event) {
    if (!event.target.closest) return;

    /* Shiny's own click handler on the button has already run by the time this
       document-level listener fires, so the click is counted before the button is
       disabled. The busy state must be set right away: the server's reply can
       arrive before any deferred timer runs, especially in a background tab. */
    var run = event.target.closest('#RunSimulations');
    if (run && !run.disabled) {
      run.disabled = true;
      setBusy(run, 'Running...');
    }

    var report = event.target.closest('#downloadReportHandler');
    if (report) {
      if (report.classList.contains('disabled')) { event.preventDefault(); return; }
      report.classList.add('disabled');
      setBusy(report, 'Preparing report...');
    }
  });

  /* Shiny requires message handlers to take exactly one argument */
  Shiny.addCustomMessageHandler('netsimr-run-finished', function (message) {
    var run = document.getElementById('RunSimulations');
    if (run) { run.disabled = false; setIdle(run); }
  });

  Shiny.addCustomMessageHandler('netsimr-report-finished', function (message) {
    var report = document.getElementById('downloadReportHandler');
    if (report) { report.classList.remove('disabled'); setIdle(report); }
  });
})();
"

#' The Pareto slices section of the Tail adjustments card
#'
#' All slice rows are in the page; the hidden number input pareto_slice_times decides
#' which are shown. The add button and the remove button of each row change it on the
#' server, and a saved settings file restores it like any other number.
#'
#' @return A tag list.
#' @noRd
sim_pareto_slices_ui <- function() {
  count <- "(Number(input.pareto_slice_times) || 0)"
  tagList(
    div(
      class = "sim-slices-head",
      span(class = "sim-slices-title", "Pareto slices"),
      conditionalPanel(
        condition = paste(count, "<", max_number_of_pareto_slices),
        actionButton("add_pareto_slice", "Add slice", icon = icon("plus"), class = "btn-sm btn-outline-primary")
      )
    ),
    helpText("Splices a Pareto tail onto the severity distribution above each threshold. Thresholds must increase from one slice to the next."),
    div(
      class = "sim-hidden-input",
      numericInput("pareto_slice_times", "Number of Pareto slices", value = 0, min = 0,
                   max = max_number_of_pareto_slices, step = 1)
    ),
    conditionalPanel(
      condition = paste(count, "== 0"),
      div(class = "sim-slices-note", "No slices: the severity distribution is used as it is.")
    ),
    lapply(seq_len(max_number_of_pareto_slices), function(i) {
      conditionalPanel(
        condition = paste(count, ">=", i),
        div(
          class = "sim-slice-row",
          div(class = "sim-slice-badge", i, title = paste("Slice", i)),
          div(
            class = "param-grid-2 sim-slice-fields",
            numericInput(paste0("slice_pareto_param_", 2 * i - 1), "Alpha", value = NULL, min = 0),
            numericInput(paste0("slice_pareto_param_", 2 * i), "Threshold (x_m)", value = NULL, min = 0)
          ),
          tags$button(
            id = paste0("remove_pareto_slice_", i), type = "button",
            class = "btn btn-sm btn-outline-secondary action-button sim-slice-remove",
            title = paste("Remove slice", i), `aria-label` = paste("Remove slice", i),
            icon("xmark")
          )
        )
      )
    }),
    conditionalPanel(
      condition = paste(count, ">=", max_number_of_pareto_slices),
      div(class = "sim-slices-note", paste("The maximum of", max_number_of_pareto_slices, "slices is reached."))
    )
  )
}

#' User interface of the Shiny NetSimR simulator
#'
#' @description The page the simulator opens in: a welcome panel and the
#'   simulator itself, where the claim, reinsurance and run settings are
#'   entered and the results, charts and report are shown.
#' @return The user interface of the application, a bslib navbar page, which
#'   \code{\link{run_shiny_simulator}} pairs with \code{shiny_simulator_server}.
#' @keywords internal
shiny_simulator_ui <- bslib::page_navbar(
  title = div(
    class = "sim-brand",
    div(class = "sim-brand-mark", icon("dice")),
    div(
      class = "sim-brand-text",
      tags$span(class = "sim-brand-title", "NetSimR Simulator"),
      tags$span(class = "sim-brand-subtitle", "Claims & reinsurance simulation")
    )
  ),
  window_title = "NetSimR Claims Simulator",
  id = "sim_navbar",
  selected = "welcome",
  fillable = FALSE,

  navbar_options = bslib::navbar_options(
    bg = "#0b1f3d",
    theme = "dark",
    underline = FALSE
  ),

  theme = bslib::bs_theme(
    version = 5,
    primary = "#2563eb",
    secondary = "#64748b",
    success = "#16a34a",
    info = "#0891b2",
    warning = "#d97706",
    danger = "#dc2626",
    base_font = bslib::font_collection(
      bslib::font_google("Inter", local = FALSE),
      "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"
    ),
    heading_font = bslib::font_collection(
      bslib::font_google("Inter", local = FALSE),
      "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"
    )
  ),

  header = tagList(
    tags$head(
      tags$script(HTML(netsimr_theme_js("netsimr-simulator-theme"))),
      tags$style(HTML(sim_ui_css))
    ),
    #Shiny's built-in busy indicators follow the app theme, so they work in dark mode too
    useBusyIndicators(spinners = TRUE, pulse = TRUE, fade = TRUE),
    busyIndicatorOptions(
      pulse_background = "linear-gradient(90deg, #60a5fa, #2563eb, #60a5fa)",
      pulse_height = "4px"
    ),
    tags$script(HTML(sim_run_state_js))
  ),

  # ---------------------------------------------------------------- Welcome
  bslib::nav_panel(
    title = "Welcome",
    value = "welcome",
    icon = icon("house"),

    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(8, 4)),

      div(
        class = "sim-hero",
        div(class = "sim-hero-eyebrow", "NetSimR"),
        div(class = "sim-hero-title", "Claims & Reinsurance Simulator"),
        p(
          class = "sim-hero-text",
          "Simulate insurance claims from frequency and severity distributions, ",
          "reshape the tail, apply each-and-every-loss and aggregate reinsurance ",
          "structures, then export the results and a full report."
        ),
        tags$button(
          type = "button",
          class = "btn btn-light",
          onclick = "document.querySelector('.navbar a[data-value=\"simulator\"]').click();",
          icon("play"), " Open the simulator"
        )
      ),

      bslib::card(
        sim_card_header("route", "How it works"),
        bslib::card_body(
          tags$ol(
            class = "sim-steps",
            tags$li(div(div(class = "sim-step-title", "Frequency"),
                        div(class = "sim-step-text", "How many claims occur per period."))),
            tags$li(div(div(class = "sim-step-title", "Severity"),
                        div(class = "sim-step-text", "How large each claim is."))),
            tags$li(div(div(class = "sim-step-title", "Tail adjustments"),
                        div(class = "sim-step-text", "Optional Pareto slices and claim caps."))),
            tags$li(div(div(class = "sim-step-title", "Reinsurance"),
                        div(class = "sim-step-text", "EEL and aggregate layers, with reinstatements."))),
            tags$li(div(div(class = "sim-step-title", "Export"),
                        div(class = "sim-step-text", "Download the simulated data or an HTML report.")))
          )
        )
      )
    ),

    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, md = c(4, 4, 4)),
      div(
        class = "sim-stat",
        div(class = "sim-stat-label", icon("chart-bar"), "Frequency models"),
        div(class = "sim-stat-value",
            paste(unname(vapply(freq_dist_options, function(x) x@distr_label, character(1))), collapse = paste0(" ", intToUtf8(183), " ")))
      ),
      div(
        class = "sim-stat",
        div(class = "sim-stat-label", icon("chart-line"), "Severity models"),
        div(class = "sim-stat-value",
            paste(unname(vapply(sev_dist_options, function(x) x@distr_label, character(1))), collapse = paste0(" ", intToUtf8(183), " ")))
      ),
      div(
        class = "sim-stat",
        div(class = "sim-stat-label", icon("shield-halved"), "Reinsurance"),
        div(class = "sim-stat-value",
            paste(setdiff(reinsurance_structures_options, "No Reinsurance Structure"), collapse = paste0(" ", intToUtf8(183), " ")))
      )
    ),

    bslib::card(
      sim_card_header("user", "About", "Feedback and bug reports are very welcome."),
      bslib::card_body(
        p(
          "Created by Yiannis Parizas. For more information, visit my ",
          a("LinkedIn profile", href = "https://www.linkedin.com/in/yiannisparizas/",
            target = "_blank", rel = "noopener noreferrer"),
          ". You can also reach me by email at ",
          a("yiannis.parizas@gmail.com", href = "mailto:yiannis.parizas@gmail.com"),
          "."
        ),
        p(class = "sim-muted mb-0",
          "Please reach out if you have any feedback or encounter any bugs.")
      )
    )
  ),

  # ------------------------------------------------------- Claims simulator
  bslib::nav_panel(
    title = "Claims Simulator",
    value = "simulator",
    icon = icon("dice"),

    div(
      class = "sim-page-intro",
      div(
        h2(class = "sim-page-title", "Claims simulator"),
        p(class = "sim-page-subtitle",
          "Configure the model on the right, then run it from the panel on the left.")
      )
    ),

    bslib::layout_columns(
      class = "sim-layout",
      col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8), xxl = c(3, 9)),

      # ---- Run panel
      bslib::card(
        sim_card_header("gear", "Simulation", "Run settings and outputs"),
        bslib::card_body(
          #the same limits as the validation in find_missing_simulation_settings()
          numericInput('numberOfSimulations', 'Number of simulations',
                       value = 50000L, min = 1L, max = max_number_of_simulations, step = 1L),
          bslib::input_switch('seedSetBinary', 'Custom seed', value = FALSE),
          uiOutput("seed_value"),
          bslib::input_switch('multiprocessingBinary', 'Multiprocessing', value = FALSE),
          helpText("Runs chunks in parallel. Faster for large runs, but without live progress."),

          uiOutput("expected_gross_claims"),

          tags$hr(class = "sim-divider"),

          actionButton("RunSimulations", "Run Simulations", icon = icon("play"), class = "btn-run"),

          tags$hr(class = "sim-divider"),

          div(class = "sim-section-label", "Results"),
          div(
            class = "sim-downloads",
            uiOutput("downloadDataButton"),
            uiOutput("downloadReportButton"),
            div(class = "sim-downloads-hint", icon("circle-info"),
                "Run a simulation to enable the downloads.")
          ),

          #save and load settings, and built-in examples (R/ShinySimulatorSettingsIO.R)
          tags$hr(class = "sim-divider"),
          sim_settings_io_ui()
        )
      ),

      # ---- Model configuration
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, md = c(6, 6, 12, 6, 6)),

        bslib::card(
          sim_card_header("chart-bar", "Frequency", "How many claims occur per simulated period."),
          bslib::card_body(
            div(class = "sim-section-label", "Distribution"),
            div(
              class = "pill-radio",
              sim_hidden_label(radioButtons(
                inputId = 'freqDistr',
                label = 'Frequency distribution',
                inline = TRUE,
                choiceNames = unname(vapply(freq_dist_options, function(x) x@distr_label, character(1))),
                choiceValues = unname(vapply(freq_dist_options, function(x) x@distrID, character(1)))
              ))
            ),
            div(class = "sim-section-label mt-2", "Parameters"),
            div(
              class = "param-grid",
              lapply(seq_len(max(vapply(freq_dist_options, function(x) length(x@paramIDs), integer(1)))),
                     function(i){ uiOutput(paste0('freq_param_', i)) })
            ),
            uiOutput("freq_implied_moments")
          )
        ),

        bslib::card(
          sim_card_header("chart-line", "Severity", "The size of each individual claim."),
          bslib::card_body(
            div(class = "sim-section-label", "Distribution"),
            div(
              class = "pill-radio",
              sim_hidden_label(radioButtons(
                inputId = 'sevDistr',
                label = 'Severity distribution',
                inline = TRUE,
                choiceNames = unname(vapply(sev_dist_options, function(x) x@distr_label, character(1))),
                choiceValues = unname(vapply(sev_dist_options, function(x) x@distrID, character(1)))
              ))
            ),
            div(class = "sim-section-label mt-2", "Parameters"),
            div(
              class = "param-grid",
              lapply(seq_len(max(vapply(sev_dist_options, function(x) length(x@paramIDs), integer(1)))),
                     function(i){ uiOutput(paste0('sev_param_', i)) })
            ),
            uiOutput("sev_implied_moments"),
            conditionalPanel(
              condition = "input.sevDistr == 'Normal'",
              class = "sim-truncate",
              bslib::input_switch('sevTruncateAtZero', 'Truncate at zero', value = FALSE),
              helpText("Draws claims from the Normal distribution conditional on being positive, so no claim is negative.")
            )
          )
        ),

        bslib::card(
          sim_card_header("scissors", "Tail adjustments", "Optionally reshape or cap the severity tail."),
          bslib::card_body(
            bslib::layout_columns(
              col_widths = bslib::breakpoints(sm = 12, md = c(7, 5)),
              div(
                sim_pareto_slices_ui()
              ),
              div(
                bslib::input_switch('sevCapBinary', 'Severity cap', value = FALSE),
                helpText("Caps any single claim at a maximum amount."),
                uiOutput("sev_cap_amount_ui")
              )
            )
          )
        ),

        bslib::card(
          sim_card_header("shield-halved", "Each & every loss (EEL)",
                          "Applied to each claim before it is summed into the total."),
          bslib::card_body(
            div(class = "sim-section-label", "Structure"),
            div(
              class = "pill-radio pill-radio-grid",
              sim_hidden_label(radioButtons('reinsuranceStructureEEL', 'Each and every loss structure',
                                            choices = reinsurance_structures_options, inline = TRUE))
            ),
            div(
              class = "param-grid mt-3",
              uiOutput("reinsuranceStructureDeductibleEEL"),
              uiOutput("reinsuranceStructureLimitEEL")
            ),
            uiOutput("reinsuranceStructureLimitedReinstatements_ui"),
            uiOutput("reinsuranceStructureReinstatementLimit_ui")
          )
        ),

        bslib::card(
          sim_card_header("umbrella", "Aggregate layer (AL)",
                          "Applied to each simulation's total after EEL: deductible first, then the limit and reinstatement cap."),
          bslib::card_body(
            div(class = "sim-section-label", "Structure"),
            div(
              class = "pill-radio pill-radio-grid",
              sim_hidden_label(radioButtons('reinsuranceStructureAL', 'Aggregate layer structure',
                                            choices = reinsurance_structures_options, inline = TRUE))
            ),
            div(
              class = "param-grid mt-3",
              uiOutput("reinsuranceStructureDeductibleAL"),
              uiOutput("reinsuranceStructureLimitAL")
            )
          )
        )
      )
    )
  ),

  # ------------------------------------------------ Report and Compare tabs
  #defined in R/ShinySimulatorTabs.R
  sim_report_tab_ui("report"),
  sim_compare_tab_ui("compare"),

  bslib::nav_spacer(),
  bslib::nav_item(sim_theme_switch())
)

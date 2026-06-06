# generate_jonny5_html.R
# Reads jonny5_template.html and substitutes current values
# Template uses ##PLACEHOLDER## markers for dynamic values

# First run: inject placeholder markers into template
# Subsequent runs: substitute placeholders with current values

html <- paste(readLines("jonny5_template.html"), collapse="\n")

# Safety check
if(nchar(html) < 50000) stop("Template too small — check jonny5_template.html")

# Substitute JS variable block
html <- sub(
  "var BASE=[0-9.]+, DIS=[0-9.]+, CONS=[0-9.]+, CURR=[0-9.]+, GPPI_PCT=[0-9.]+;",
  sprintf("var BASE=%.3f, DIS=%.3f, CONS=%.3f, CURR=%.3f, GPPI_PCT=%.2f;",
          base_d, dis_d, cons_d, curr_d, gppi_pct),
  html
)

# Substitute PADD prices
html <- sub(
  "var PADD=\\{[^}]+\\};",
  paste0("var PADD=", padd_prices_js, ";"),
  html
)

# Substitute chart data
html <- sub("var histLabels=\\[[^;]+;", paste0("var histLabels=", hist_labels, ";"), html)
html <- sub("var histData=\\[[^;]+;",   paste0("var histData=",   hist_values,  ";"), html)
html <- sub("var fcLabels=\\[[^;]+;",   paste0("var fcLabels=",   fc_labels_js, ";"), html)
html <- sub("var fcBase=\\[[^;]+;",     paste0("var fcBase=",     fc_base,      ";"), html)
html <- sub("var fcCons=\\[[^;]+;",     paste0("var fcCons=",     fc_cons,      ";"), html)
html <- sub("var fcDis=\\[[^;]+;",      paste0("var fcDis=",      fc_dis,       ";"), html)
html <- sub("var gppiLabels=\\[[^;]+;", paste0("var gppiLabels=", gppi_labels,  ";"), html)
html <- sub("var gppiData=\\[[^;]+;",   paste0("var gppiData=",   gppi_values,  ";"), html)

if(nchar(html) < 50000) stop("HTML too small after substitution — aborting")

writeLines(html, "jonny5.html")
cat("  ✓ jonny5.html updated\n")

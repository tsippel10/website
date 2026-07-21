# Quarto post-render hook: print the rendered CV.html to PDF with headless Chrome
# so CV_Sippel.pdf is visually identical to CV.html (same CSS, cards, colors, layout).
#
# Ported from the original flat CV repo. This is a `website` project, so the
# rendered pages live in the output dir (_site/) rather than the repo root;
# read CV.html from there and write the PDF alongside it so it is served at
# /CV_Sippel.pdf.

out_dir <- Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR", "_site")

# Centered footer on every page: "Sippel CV Page <page number>". Chrome fills in
# the page number via the special .pageNumber class. An (essentially) empty
# header template keeps Chrome from drawing its default title/date header.
footer_template <- paste0(
  '<div style="width:100%; font-size:12px; font-weight:bold; color:#333; text-align:center;">',
  'Sippel CV Page <span class="pageNumber"></span></div>'
)

pagedown::chrome_print(
  input = file.path(out_dir, "CV.html"),
  output = file.path(out_dir, "CV_Sippel.pdf"),
  options = list(
    printBackground = TRUE, # keep card/heading background colors and borders
    preferCSSPageSize = TRUE,
    displayHeaderFooter = TRUE,
    headerTemplate = "<span></span>", # suppress Chrome's default header
    footerTemplate = footer_template
  )
)

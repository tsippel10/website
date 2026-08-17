# Print a rendered cover-letter HTML to PDF with headless Chrome.
#
# Printing a letter from the browser's Cmd+P dialog stamps Chrome's default
# header and footer onto every page: the document title and print timestamp
# across the top, and the source file:// path plus "Page N of M" across the
# bottom. Those are supplied by the print dialog, not the page, so no amount of
# CSS in cover_letter.css can suppress them -- the PDF has to be produced
# headlessly with displayHeaderFooter turned off. Same technique as the CV's
# make_pdf.R, minus the footer template.
#
# Usage:
#   Rscript make_cover_letter_pdf.R <input.html> [output.pdf]
#
# The output name defaults to the input's basename with a .pdf extension, so
# pass one explicitly when the deliverable needs a recipient-facing filename.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: Rscript make_cover_letter_pdf.R <input.html> [output.pdf]")
}

input  <- args[[1]]
output <- if (length(args) >= 2) args[[2]] else sub("\\.html?$", ".pdf", input)

if (!file.exists(input)) {
  stop("Input not found: ", input, " (render the .qmd first)")
}

pagedown::chrome_print(
  input  = input,
  output = output,
  options = list(
    printBackground   = TRUE,  # keep the letterhead rule and accent colors
    preferCSSPageSize = TRUE,  # honor the @page size/margins in cover_letter.css
    displayHeaderFooter = FALSE
  )
)

cat("Wrote ", output, "\n", sep = "")

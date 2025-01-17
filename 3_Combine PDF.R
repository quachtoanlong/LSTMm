dir = dirname(rstudioapi::getSourceEditorContext()$path)

setwd(dir)

library(pdftools)

pdf_subset("Content_latex.pdf", pages = 2:pdf_length("Content_latex.pdf"),
           output = "Content_latex_trimmed.pdf")

pdf_combine(c("Front matter and ToC.pdf", "Content_latex_trimmed.pdf","Back matter.pdf"), 
            output = "AMRO Working Paper_Cryptocurrencies and Banking Sector Connectedness_Dec 2023.pdf")




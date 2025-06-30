getwd()
wkdir <- "/n/dominici_nsaph_l3/Lab/projects/pm-no2-o3-hospitalization-death-ADRD-coxph/splines_mortality"

# tools::compactPDF(file.path(getwd(), "cox_smooth_base_plot_pm25.pdf"), gs_quality='printer')

install.packages("pdftools")
install.packages("magick")

library(pdftools)
library(magick)
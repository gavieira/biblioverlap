
# loading the necessary packages
library(hexSticker) # hexSticker generator
library(ggplot2)
library(biblioverlap)

#Script to create a hexsticker to the 'biblioverlap' package


###Generating plot to be featured in sticker

db_list <- test_data[1:3] #Getting only the first 3 elements of test_data

overlap <- biblioverlap(db_list) #Overlapping documents

#Plotting Venn Diagram
overlap_venn <- plot_venn(overlap$db_list,
                          label = 'none',
                          set_size = 0,
                          edge_size = 1) +
  guides(fill='none')


#View plot
overlap_venn


###Generating sticker

sticker <- sticker(
  subplot = overlap_venn,
  package = "biblioverlap",
  s_width = 1.5,
  s_height = 1.5,
  s_x = 1,
  s_y = 1.3,
  p_size = 14,
  p_y = .6,
  h_color = 'deepskyblue4',
  h_fill = 'aliceblue',
  h_size = 1.5,
  p_color = 'deepskyblue4',
  dpi = 100
)

#View sticker
sticker

#Printing sticker to file
save_sticker(filename = 'inst/biblioverApp/www/biblioverlap_sticker.png',
             sticker = sticker)

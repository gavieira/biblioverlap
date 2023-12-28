############## Figures for the biblioverlap paper ###################

library(ggVennDiagram) #Need to load ggVennDiagram first because it masks `plot_venn` and `plot_upset` functions from biblioverlap
library(biblioverlap)
library(dplyr)
library(ggplot2)
library(patchwork)

################ Running biblioverlap for example dataset ################
results <- biblioverlap(ufrj_bio_0122)
db_list <- results$db_list


################ Getting biblioverlap's plots ###########################
plot_matching_summary(results$summary, add_logo = FALSE)
plot_venn(db_list, add_logo = FALSE) #gets masked when loading ggVennDiagram alongside biblioverlap
plot_upset(db_list, add_logo = FALSE) #gets masked when loading ggVennDiagram alongside biblioverlap


############### Generating a venn diagram that show the intersects calculated in each round of pairwise comparisons ###########################
#Source: https://stackoverflow.com/questions/72651478/how-do-i-make-certain-regions-of-of-my-venn-diagram-colored-and-have-the-rest-bl

test_data = list(A = 1:1, B = 1:1, C = 1:1, D = 1:1)

p <- ggVennDiagram(test_data, label_size = 15,
                   set_size = 15, label_alpha = 0) +
  scale_color_manual(values = rep("black", 4))

#Names to be plotted instead of intersection counts
new_names <-c('A', 'B', 'C', 'D',
              'AB', 'AC', 'AD', 'BC', 'BD', 'CD',
              'ABC', 'ABD', 'ACD', 'BCD',
              'ABCD')

p$layers[[1]]$mapping <- aes(fill = name) #Each name (intersection) can have an associated color now
p$layers[[4]]$data$both <- new_names #Changing 'both' data column to have the intersect names, which will then be plotted instead


p + scale_fill_manual(values = c(A = 'red',
                                 A..B = 'red',
                                 A..C = 'red',
                                 A..D = 'red',
                                 A..B..C = 'red',
                                 A..B..D = 'red',
                                 A..C..D = 'red',
                                 A..B..C..D = 'red',
                                 B = 'blue',
                                 B..C = 'blue',
                                 B..D = 'blue',
                                 B..C..D = 'blue',
                                 C = 'darkgreen',
                                 C..D = 'darkgreen',
                                 D = 'darkgreen'
) ) + #Specifying a color for each intersect
  theme(legend.position = '') #removing legend


########### Generating venn diagrams for subsets of the data ##################

#Open access docs
open_access_docs <- lapply(db_list, function(db) {
  filter(db, !(`Open Access Colour` %in% c('', 'unknown')))
})
open_access <- plot_venn(open_access_docs, add_logo = FALSE, label = c("count")) +
  labs(title = 'Open Access') +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        legend.position = 'none')

#Closed access docs
closed_access_docs <- lapply(db_list, function(db) {
  filter(db, `Open Access Colour` == '')
})
closed_access <- plot_venn(closed_access_docs, add_logo = FALSE, label = c("count")) +
  labs(title = 'Closed Access') +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        legend.position = 'none')

#Docs 10 or more citações
i10_docs <- lapply(db_list, function(db) {
  filter(db, `Citing Works Count` >= 10)
})
i10 <- plot_venn(i10_docs, add_logo = FALSE, label = c("count")) +
  labs(title = '10 or more citations') +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        legend.position = 'none')

#Preprints
preprint_docs <- lapply(db_list, function(db) {
  filter(db, `Publication Type` == 'preprint')
})
preprints <- plot_venn(preprint_docs, add_logo = FALSE, label = c("count")) +
  labs(title = 'Preprints') +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        legend.position = 'none')


open_access + closed_access + i10 + preprints + plot_annotation(tag_levels = 'A')

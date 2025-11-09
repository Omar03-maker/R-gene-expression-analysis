# Loading relevant libraries 
library(tidyverse)    # includes ggplot2, for data visualisation. dplyr, for data manipulation.
library(RColorBrewer) # for a colourful plot
library(ggrepel)      # for nice annotations

df <- read.csv("#Changer avec chemin d'accès ver Genes_Expression.csv", 
               header = TRUE,           # Si la première ligne contient les noms de colonnes
               sep = ";",               # Séparateur (virgule par défaut)
               dec = ".",               # Séparateur décimal
               stringsAsFactors = TRUE, # Ne pas convertir les chaînes en facteurs
               row.names = 1)  

head(df)

# Vérifier les types de donnees 
str(df)
class(df$pval)
df$pval <- as.numeric(as.character(df$pval))

# Volcano plot classic 
ggplot(data = df, aes(x = log2fc, y = -log10(pval))) + 
  geom_point() +
  theme_minimal()

# Volcano plot advanced 
df$log2fc <- as.numeric(as.character(df$log2fc))
df$pval <- as.numeric(as.character(df$pval))

# Créer la variable diffexpressed pour colorier les points
df$diffexpressed <- "Not significant"                                  # Par défaut tous les points sont "non significatifs"
df$diffexpressed[df$log2fc > 0.6 & df$pval < 0.05] <- "Upregulated"    # Points surexprimés
df$diffexpressed[df$log2fc < -0.6 & df$pval < 0.05] <- "Downregulated" # Points sous-exprimés

# Créer les labels pour les gènes significatifs
top_genes <- df[df$pval < 0.01 & abs(df$log2fc) > 2, ]
top_genes <- head(top_genes[order(top_genes$pval), ], 15)
df$labels <- ifelse(rownames(df) %in% rownames(top_genes), rownames(df), "")

# Convertir diffexpressed en facteur avec un ordre spécifique
df$diffexpressed <- factor(df$diffexpressed, levels = c("Downregulated", "Not significant", "Upregulated"))

# Créer le graphique
ggplot(data = df, aes(x = log2fc, y = -log10(pval), col = diffexpressed, label = labels)) +
  geom_vline(xintercept = c(-0.6, 0.6), col = "gray", linetype = 'dashed') +
  geom_hline(yintercept = -log10(0.05), col = "gray", linetype = 'dashed') + 
  geom_point(size = 2) + 
  scale_color_manual(values = c("black", "grey", "red"),                              # to set the colours of our variable  
                     labels = c("Downregulated", "Not significant", "Upregulated")) + # to set the labels in case from the dataframe (UP, DOWN, NO)
  coord_cartesian(ylim = c(0, 250), xlim = c(-10, 10)) +                              # limits for gene which have minuslog10padj 
  # Legend_title 
  labs(color = 'Legend',                                          
       x = expression("log2FC"), y = expression("-log10p-value")) + 
  scale_x_continuous(breaks = seq(-10, 10, 2)) +                  # to custom the x axis
  ggtitle('Thf-like cells in severe COVID vs healthy patients') + # Plot title 
  geom_text_repel(max.overlaps = Inf)   


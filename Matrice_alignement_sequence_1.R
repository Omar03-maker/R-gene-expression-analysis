# Initialisation des séquences
seqA <- c("R", "C", "A", "L", "M", "M", "N", "P", "Q", "R", "S", "T")
seqB <- c("A", "R", "I", "A", "L", "Q", "L", "M", "M", "N")

# Dimensions de la matrice
n <- length(seqB) + 1 # Lignes (B)
m <- length(seqA) + 1 # Colonnes (A)

# Matrice de similarité (1 pour correspondance, 0 sinon)
similarity <- matrix(0, nrow = n, ncol = m)
rownames(similarity) <- c("", seqB)
colnames(similarity) <- c("", seqA)

for (i in 2:n) {
  for (j in 2:m) {
    if (seqB[i - 1] == seqA[j - 1]) {
      similarity[i, j] <- 1
    }
  }
}

# Matrice des scores
score <- matrix(0, nrow = n, ncol = m)
rownames(score) <- c("", seqB)
colnames(score) <- c("", seqA)

# Calcul des scores
for (i in 2:n) {
  for (j in 2:m) {
    # Score de correspondance diagonal
    diag_score <- score[i - 1, j - 1] + similarity[i, j]
    
    
    # Meilleur score dans la sous-ligne (i-1, j-k), k > 1
    sub_row_scores <- c(0) # Inclure 0 pour gérer les cas sans k > 1
    if (j > 2) {
      for (k in 2:(j - 1)) {
        sub_row_scores <- c(sub_row_scores, score[i - 1, j - k])
      }
    }
    max_sub_row <- max(sub_row_scores)
    
    
    # Meilleur score dans la sous-colonne (i-r, j-1), r > 1
    sub_col_scores <- c(0) # Inclure 0 pour gérer les cas sans r > 1
    if (i > 2) {
      for (r in 2:(i - 1)) {
        sub_col_scores <- c(sub_col_scores, score[i - r, j - 1])
      }
    }
    max_sub_col <- max(sub_col_scores)
    
    
    # Calcul du score final
    score[i, j] <- max(diag_score, max_sub_row + similarity[i, j], max_sub_col + similarity[i, j])
  }
}

# Affichage des matrices
cat("Matrice de similarité:\n")
print(similarity)

cat("\nMatrice des scores:\n")
print(score)


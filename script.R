#install.packages("MASS")
library(MASS)

data <- read.csv("wrestling_stats.csv")

X <- data[, 5:21]

# μικρο δειγμα και καποιες στηλες εχουν μηδενικη διακυμανση αρα τις βγαζω γιατι να μην εχω διαιρεση με μηδεν
X[is.na(X)] <- 0
X <- X[, apply(X, 2, var) != 0]


G <- as.factor(data$Win.Lose)

#pca
pca_model <- prcomp(X, scale. = TRUE) 

#ιδιοτιμες
eigenvalues <- pca_model$sdev^2
print("Ιδιοτιμές του Πίνακα R:")
print(eigenvalues)

#μεταβλητοτητα
prop_variance <- eigenvalues / sum(eigenvalues)
print("Ποσοστό Μεταβλητότητας ανά Συνιστώσα:")
print(prop_variance)

#fa
fa_model <- factanal(X, factors = 2, rotation = "varimax")
print(fa_model)

#da
da_model <- lda(G ~ ., data = data.frame(G, X))
print(da_model)
#Chargement des data et créations du datafram.
mes_donnees <- data.frame(read.csv("kag_risk_factors_cervical_cancer.csv"))

#Remplacement des valeurs manquantes.
mes_donnees[mes_donnees == "?"] <- NA

#Suppression des colonne inexploitable.

mes_donnees <- subset(mes_donnees, select = -STDs..Time.since.first.diagnosis)
mes_donnees <- subset(mes_donnees, select = -STDs..Time.since.last.diagnosis)
mes_donnees_omit <- na.omit(mes_donnees)
mes_donnees_float <- mes_donnees %>%
       mutate(across(everything(), as.numeric))


# Copie du dataframe original pour préserver les données initiales
mes_donnees_sans_aberrantes <- mes_donnees_float
mcor()
# Créer un vecteur logique pour marquer les lignes à supprimer
lignes_a_supprimer <- rep(FALSE, nrow(mes_donnees_sans_aberrantes))

# Boucle sur toutes les colonnes
for (column in names(mes_donnees_sans_aberrantes)) {
  # Calculer la moyenne et l'écart type pour chaque colonne
  moyenne <- mean(mes_donnees_sans_aberrantes[[column]], na.rm = TRUE)
  ecart_type <- sd(mes_donnees_sans_aberrantes[[column]], na.rm = TRUE)
  
  # Définition des seuils pour les valeurs aberrantes
  seuil_haut <- moyenne + 3 * ecart_type
  seuil_bas <- moyenne - 3 * ecart_type
  
  # Marquer les indices des valeurs aberrantes
  lignes_a_supprimer <- lignes_a_supprimer | mes_donnees_sans_aberrantes[[column]] < seuil_bas | mes_donnees_sans_aberrantes[[column]] > seuil_haut
}

# Suppression des lignes marquées contenant des valeurs aberrantes
mes_donnees_sans_aberrantes <- mes_donnees_sans_aberrantes[!lignes_a_supprimer, ]


# Nuage de Points Age vs Cancer
plot(mes_donnees_omit$Age, mes_donnees_omit$Dx.Cancer,
     +      xlab = "Age", ylab = "Dx.Cancer (0 = No, 1 = Yes)",
     +      main = "Scatter Plot of Age vs Dx.Cancer",
     +      pch = 19, col = ifelse(mes_donnees_omit$Dx.Cancer == 1, "red", "blue"))

# Nuage de points, première relation sexuelle
plot(mes_donnees_omit$First.sexual.intercourse, mes_donnees_omit$Dx.Cancer,
     +      xlab = "FirstTime", ylab = "Dx.Cancer (0 = No, 1 = Yes)",
     +      main = "Scatter Plot of First Time vs Dx.Cancer",
     +      pch = 19, col = ifelse(mes_donnees_omit$Dx.Cancer == 1, "red", "blue"))

# Table de Contingence : STDs / Cancer
table_stds_cancer <- table(STDs = mes_donnees_omit$STDs, Dx.Cancer = mes_donnees_omit$Dx.Cancer)
print(table_stds_cancer)

# Table de Contingence : Smokes / Cancer
table_smokes_cancer <- table(Smokes = mes_donnees_omit$Smokes, Dx.Cancer = mes_donnees_omit$Dx.Cancer)
print(table_smokes_cancer)

# Table de Contingence : Hormonal Contraceptives / Cancer
table_HC_cancer <- table(HormonalContra = mes_donnees_omit$Hormonal.Contraceptives, Dx.Cancer = mes_donnees_omit$Dx.Cancer)
print(table_HC_cancer)

mes_donnees_omit <- subset(mes_donnees_omit, select = -STDs.cervical.condylomatosis)
mes_donnees_omit <- subset(mes_donnees_omit, select = -STDs.pelvic.inflammatory.disease)
mes_donnees_omit <- subset(mes_donnees_omit, select = -STDs.AIDS)


mes_donnees_omit$Smokes <- as.factor((mes_donnees_omit$Smokes))
mes_donnees_omit$Hormonal.Contraceptives <- as.factor((mes_donnees_omit$Hormonal.Contraceptives))
mes_donnees_omit$IUD <- as.factor((mes_donnees_omit$IUD))
mes_donnees_omit$STDs <- as.factor((mes_donnees_omit$STDs))
mes_donnees_omit$STDs.condylomatosis <- as.factor((mes_donnees_omit$STDs.condylomatosis))
mes_donnees_omit$STDs.vaginal.condylomatosis <- as.factor((mes_donnees_omit$STDs.vaginal.condylomatosis))
mes_donnees_omit$STDs.vulvo.perineal.condylomatosis <- as.factor((mes_donnees_omit$STDs.vulvo.perineal.condylomatosis))
mes_donnees_omit$STDs.syphilis <- as.factor((mes_donnees_omit$STDs.syphilis))
mes_donnees_omit$STDs.genital.herpes <- as.factor((mes_donnees_omit$STDs.genital.herpes))
mes_donnees_omit$STDs.molluscum.contagiosum <- as.factor((mes_donnees_omit$STDs.molluscum.contagiosum))
mes_donnees_omit$STDs.HIV <- as.factor((mes_donnees_omit$STDs.HIV))
mes_donnees_omit$STDs.Hepatitis.B <- as.factor((mes_donnees_omit$STDs.Hepatitis.B))
mes_donnees_omit$STDs.HPV <- as.factor((mes_donnees_omit$STDs.HPV))
mes_donnees_omit$Dx.Cancer <- as.factor((mes_donnees_omit$Dx.Cancer))
mes_donnees_omit$Dx.CIN <- as.factor((mes_donnees_omit$Dx.CIN))
mes_donnees_omit$Dx.HPV <- as.factor((mes_donnees_omit$Dx.HPV))
mes_donnees_omit$Dx <- as.factor((mes_donnees_omit$Dx))
mes_donnees_omit$Hinselmann <- as.factor((mes_donnees_omit$Hinselmann))
mes_donnees_omit$Schiller <- as.factor((mes_donnees_omit$Schiller))
mes_donnees_omit$Citology <- as.factor((mes_donnees_omit$Citology))
mes_donnees_omit$Biopsy <- as.factor((mes_donnees_omit$Biopsy))
training.idx <- createDataPartition(mes_donnees_omit$Dx.Cancer, p=0.8, list = FALSE)
train <- mes_donnees_omit[training.idx,]
test <- mes_donnees_omit[-training.idx,]

logit.fit <- train(Dx.Cancer ~ ., data = train)
logit.fit

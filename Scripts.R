# PRE-TRAITEMENT DU JEUX DE DONNEES

# importation des libraries

library(FactoMineR)
library(factoextra)
library(missMDA)
library(corrplot)
library(FactoInvestigate)
library(dplyr)
library(mice)
library(ExPosition)

# Importation du jeu de données
birth_data <- read.csv(file.choose(), header = TRUE, sep = ",")

# Identifier les valeurs manquantes pour chaque variable
sapply(birth_data, function(x) sum(is.na(x)))

# Remplacer les valeurs manquantes par la moyenne
birth_data.analyse<-birth_data[, colnames(birth_data) %in% c("fage", "mage", "weeks", "visits", "gained", "weight")]

# TRAITEMENT DE DONNEES

# Analyse des facteurs qui inflence le poids d'un nouveau née

# Analyse de la relation entre les differentes variables
cor(birth_data.analyse.complete$completeObs, use = "pairwise.complete.obs")

# Calcul des composantes principales
res.pca <- PCA(birth_data.analyse.complete$completeObs, scale.unit=TRUE,quanti.sup = 6, ncp = 5, graph = FALSE)

# Affichage de la contribution
eig.val <- get_eigenvalue(res.pca) 
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 70))


# ANALYSE DES RESULTATS DES DIFFERENTES VARIABLES

# Afficher les valeurs propres
var <- get_pca_var(res.pca) 

coord<-var$coord[,1]
contrib<-var$contrib[,1]
cos2<-var$cos2[,1]
display1<-cbind(coord,contrib,cos2)
display1

coord<-var$coord[,2]
contrib<-var$contrib[,2]
cos2<-var$cos2[,2]
display2<-cbind(coord,contrib,cos2)
display2

# CERCLE DU CORRELATION

# Afficher les graphiques (cercle de corrélation, plan factoriel, etc.)
fviz_pca_var(res.pca, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "red"), repel = TRUE)

# ANALYSE DES FACTEURS LIES A LA NAISSANCE PREMATURE D'UN BEBE

# Sélection des variables d'intérêt
birth_subset <- birth_data[, c("fage", "mage", "gained", "weeks", "mature", "habit", "marital", "whitemom", "premie","sex", "visits")]

# Conversion de la variable "premie" en variable catégorielle
birth_subset$premie <- as.factor(birth_subset$premie)

# Réalisation de l'ACM
birth_mca <- MCA(birth_subset, graph = FALSE)

summary(birth_mca)

# CHOIX DES DIFFERENTS AXES

s<-dim(birth_subset)[2]-1
p<-dim(birth_mca$call$Xtot)[2] 

eig.val <- get_eigenvalue(birth_mca) 
eig.val
#fviz_eig(birth_mca, addlabels = TRUE, ylim = c(0, 30))
barplot(birth_mca$eig[,1], main="Les 11 valeurs propres", names.arg=1:nrow(birth_mca$eig)) 
abline(h=1/s,col=2,lwd=2)

birth_mca.benzecri <- epMCA(birth_subset[,0:5], graph = FALSE, correction = "b")

#La fonction epMCA n'accepte pas de variables supplémentaires mais cela n'a pas d'impact sur le calcul des valeurs propres et leur correction

fviz_eig (birth_mca.benzecri, ylim=c(0,100))

# Analyse de la contribution des variables à chaque axe
dimdesc(birth_pca, axes = 1:2)

# Interpretation de l'analyse de contribution

# Créer un tableau croisé entre les variables premie, lowbirthweight et sex
tab2 <- xtabs(~premie + lowbirthweight + sex, data = birth_data)

# Afficher le tableau croisé
tab2

# Visualisation des résultats
fviz_mca_ind(birth_pca, col.ind = birth_subset$premie, 
             palette = c("#00AFBB", "#FC4E07"), # Choix des couleurs pour les deux classes
             addEllipses = TRUE, ellipse.level = 0.95)
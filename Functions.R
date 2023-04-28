# Installation des packages -----
if(!require("dplyr")){
  install.packages("dplyr")
}
if(!require("tidyr")){
  install.packages("tidyr")
}
if(!require("thematic")){
  install.packages("thematic")
}

if(!require("arrow")){
  install.packages("arrow")
}

if(!require("bs4Dash")){
  install.packages("bs4Dash")
}

if(!require("shinybusy")){
  install.packages("shinybusy")
}

if(!require("shiny")){
  install.packages("shiny")
}

if(!require("shinyalert")){
  install.packages("shinyalert")
}

library(dplyr)
library(tidyr)

# Importation des données ------
# library(arrow)
# Effectif = read_csv_arrow("Effectif.csv")
# Conso = read_csv_arrow("consommation.csv")

# Estimation des paramètres de la BD des effectifs ----
## Effectif des bénéficiares ----
table_benef <- function(df_effectif, colname_annee, colname_benef, colname_typeAssure){
  df_effectif = df_effectif[,c(colname_annee, colname_benef, colname_typeAssure)]
  names(df_effectif) = c("Année", "Type Bénéficiaire", "Type Assuré")
  df_effectif <- df_effectif %>%
    mutate(
      Année = as.numeric(Année),
      Année = factor(Année, unique(Année))
      ) %>%
    arrange(Année)
  resultat <- df_effectif %>%
    summarise(
      effectif = n(),
      .by = c(Année, `Type Bénéficiaire`, `Type Assuré`)
      )%>%
    pivot_wider(
      names_from = Année,
      names_expand = TRUE,
      values_from = effectif)
  resultat
}

# resultat = table_benef(Effectif,colname_annee = "Année",
#                        colname_benef = "Type du bénéficiaire",
#                        colname_typeAssure = "Type Assuré")

## Poids ALD ----
poids_ald <- function(df_effectif, colname_Année, colname_ALD){
  df_effectif = df_effectif[, c(colname_Année, colname_ALD)]
  names(df_effectif) = c("Année", "Type ALD")
  df_effectif <- df_effectif %>%
    mutate(
      Année = as.numeric(Année),
      Année = factor(Année, unique(Année))
    ) %>%
    arrange(Année)
  resultat = df_effectif %>%
    summarise(n = n(), .by = c(Année, `Type ALD`)) %>%
    pivot_wider(
      names_from = Année,
      names_expand = TRUE,
      values_from = n
      )
  resultat_pct <- lapply(resultat[,-1], function(x) gsub("\\.",",",paste0(round(x/sum(x)*100,3),"%")) ) %>%
    bind_rows() %>%
    mutate("Type ALD" = resultat$`Type ALD`) %>%
    relocate(`Type ALD`)
  list(Valeurs = resultat, Pourcentage = resultat_pct)
}
# resultat = poids_ald(Effectif, "Année", "TYPE ALD")




# Matrices de consommations ----
## Calcul de des moyenne par famille d'actes ----
Moyenne_par_famille <- function(consommation, famille_actes,
                                montant_engage, base_remboursement_unitaire,
                                taux_remboursement){
  consommation = consommation[,  c(famille_actes,montant_engage, base_remboursement_unitaire,taux_remboursement)]
  names(consommation) = c("famille_d_actes", "montant_engag", "base_de_remboursement_unitaire", "taux_de_remboursement")
  Moyenne_famille <- consommation %>%
    summarise(
      montant_engag = round(mean(montant_engag, na.rm = T), 2) ,
      base_de_remboursement_unitaire = round(mean(base_de_remboursement_unitaire, na.rm = T), 2),
      taux_de_remboursement = round(mean(taux_de_remboursement, na.rm = TRUE), 2),
      .by = famille_d_actes
    )
  Moyenne_famille
}
# consommation = filter(Conso[,-1],ann_es > 2015)
#
# m = Moyenne_par_famille(consommation, "Code acte quantrix", "FE_Inflaté", "RC", "RC" )



# m = Moyenne_sous_famille(consommation, "famille_d_actes", "cl_s", "montant_engag", "base_de_remboursement_unitaire", "taux_de_remboursement" )

##' Calcul des matrices de nombre d'actes consommés-----
matrice_quatinte <- function(consommation, Famille_Actes, type_benef,Age, Sexe, typeAld,quantite){
  db_temp = consommation[,c( Famille_Actes, type_benef,Age, Sexe, typeAld,quantite)]
  names(db_temp) = c("famille_d_actes","type_b_n_ficiaire","age","sexe","type_ald","quantit")
  type_benef <- sort(unique(db_temp$type_b_n_ficiaire))
  typeALD <- unique(db_temp$type_ald)
  sexe_benef <- unique(db_temp$sexe)
  db_temp = db_temp %>%
    mutate(famille_d_actes = factor(famille_d_actes, sort(unique(famille_d_actes))),
           age = factor(age,0:125))
  matrices = list()
  # Selection des colonnes
  for (benef in type_benef) {
    for (ALD in typeALD) {
      for (sex in sexe_benef) {
        temp_data <- db_temp %>%
          filter(
            type_b_n_ficiaire == benef,
            sexe == sex,
            type_ald == ALD
          ) %>%
          group_by(famille_d_actes, age,.drop = FALSE) %>%
          summarise(
            Quantite = sum(quantit,na.rm = TRUE)) %>%
          pivot_wider(
            names_from = age,
            values_from = Quantite,
            names_expand = TRUE
          )
        path <- paste0( ifelse(is.na(benef),"NA",benef), " ",
                        ifelse(is.na(ALD),"NA",ALD), " ",
                        ifelse(is.na(sex),"NA",sex))
        path <- gsub("#N/A","NA",path)

        matrices[[path]] = temp_data
      }
    }
  }
  matrices
}

# m_quantite <- matrice_quatinte(test_conso,Famille_Actes = "famille_d_actes",
#                                type_benef = "Categorie",Age = "age",Sexe = "sexe",
#                                typeAld = "type_ald",quantite = "quantit" )



##' Calcul des nombre de sinistrés -----
matrice_sinsitre <- function(consommation, famille_d_actes,type_b_n_ficiaire,code_b_n_ficiaire,age,sexe,type_ald, ANNEE){
  db_temp = consommation[,c(famille_d_actes,type_b_n_ficiaire,code_b_n_ficiaire,age,sexe,type_ald,ANNEE)]
  names(db_temp) = c("famille_d_actes","type_b_n_ficiaire","code_b_n_ficiaire","age","sexe","type_ald","ANNEE")
  type_benef <- sort(unique(db_temp$type_b_n_ficiaire))
  typeALD <- unique(db_temp$type_ald)
  sexe_benef <- unique(db_temp$sexe)
  # Selection des colonnes
  db_temp = db_temp %>%
    mutate(famille_d_actes = factor(famille_d_actes, sort(unique(famille_d_actes))),
           age = factor(age,0:125))
  matrices = list()

  for (benef in type_benef) {
    for (ALD in typeALD) {
      for (sex in sexe_benef) {
        temp_data <- db_temp %>%
          filter(
            type_b_n_ficiaire == benef,
            sexe == sex,
            type_ald == ALD
          ) %>%
          distinct(
            code_b_n_ficiaire,
            ANNEE,
            famille_d_actes,
            .keep_all = TRUE
          ) %>%
          group_by(famille_d_actes, age, ANNEE, .drop = FALSE) %>%
          summarise(Sinistres = n()) %>%
          select(-ANNEE) %>%
          pivot_wider(
            names_from = age,
            values_from = Sinistres,
            names_expand = TRUE,
            values_fn = sum
          )
        path <- paste0( ifelse(is.na(benef),"NA",benef), " ",
                        ifelse(is.na(ALD),"NA",ALD), " ",
                        ifelse(is.na(sex),"NA",sex))
        path <- gsub("#N/A","NA",path)
        matrices[[path]] = temp_data
      }
    }
  }
  matrices
}

# m_sinistre = matrice_sinsitre(consommation,famille_d_actes = "Code acte quantrix",type_b_n_ficiaire = "Statut nv",
#                               code_b_n_ficiaire = "ID_PERSONNE",age = "Age nv",sexe = "Sexe nv",type_ald = "ALD nv",ANNEE = "ANNEE")
#
# m_sinistre = matrice_sinsitre(test_conso,famille_d_actes = "famille_d_actes",type_b_n_ficiaire = "Categorie",
#                               code_b_n_ficiaire = "code_b_n_ficiaire",age = "age",sexe = "sexe",type_ald = "type_ald",ANNEE = "ann_es")
##' Calcul de la Fréquence d’actes par personne sinistré -------
matrice_freq_acte <- function(m_sinistre,m_quantite){
  Freq_list = list()
  for (noms in names(m_sinistre)) {
    TNALDH_sinistre = m_sinistre[[noms]]
    TNALDH_Quantite = m_quantite[[noms]]
    famille = TNALDH_Quantite$famille_d_actes
    TNALDH_sinistre = TNALDH_sinistre[,-1]
    TNALDH_Quantite = TNALDH_Quantite[,-1]
    Freq = TNALDH_Quantite/TNALDH_sinistre
    Freq = Freq %>%
      mutate(Famille = famille) %>%
      relocate(Famille)
    Freq_list[[noms]] = Freq
  }
  Freq_list

}

# m_freq = matrice_freq_acte(m_sinistre,m_quantite)

# m = matrice_freq_acte(consommation, "famille_d_actes","type_b_n_ficiaire","code_b_n_ficiaire","age","sexe","type_ald","num_ro_du_dossier","quantit")

##' Calcul des effectifs pour tout type (benef, ald, age) ------
matrice_effectif <- function(df_effectif, type_b_n_ficiaire, type_ald, sexe, Age){
  df_effectif = df_effectif[,c(type_b_n_ficiaire, type_ald, sexe, Age)]
  names(df_effectif) = c("Type du bénéficiaire", "TYPE ALD", "sexe", "Age")
  type_benef <- sort(unique(df_effectif$`Type du bénéficiaire`))
  typeALD <- unique(df_effectif$`TYPE ALD`)
  sexe_benef <- unique(df_effectif$sexe)
  df_effectif <- df_effectif %>%
    mutate(Age =factor(Age, 0:125))
  matrices = list()
  for (benef in type_benef) {
    for (ALD in typeALD) {
      for (sex in sexe_benef) {
        effectif_benef = df_effectif %>%
          filter(
            `Type du bénéficiaire` == benef,
            sexe == sex,
            `TYPE ALD` == ALD
          ) %>%
          count(Age,.drop = FALSE)
        path <- paste0( ifelse(is.na(benef),"NA",benef), " ",
                        ifelse(is.na(ALD),"NA",ALD), " ",
                        ifelse(is.na(sex),"NA",sex))
        path <- gsub("#N/A","NA",path)
        matrices[[path]] = effectif_benef
      }
    }
  }
  matrices
}

# m_effectifs = matrice_effectif(Effectif_MODEP_2022,
#                                type_b_n_ficiaire = "CATEGORIE",
#                                type_ald = "Type ALD",sexe = "SEXE",Age = "Age")
#

##' Calcul du taux de sinistralité -------

matrice_taux_sinistralite <- function(m_effectifs,m_sinistre){
  matrices = list()

  for (nom in names(m_effectifs)) {
    effectif_benef = m_effectifs[[nom]]
    effectif_benef = as.vector(effectif_benef$n)[1:126]

    temp_sinistres = m_sinistre[[nom]]

    famille = temp_sinistres$famille_d_actes
    temp_sinistres = temp_sinistres[,-1]

    sinistralite = apply(temp_sinistres, 1, function(x) x/ effectif_benef) %>%
      t() %>%
      as.data.frame()

    names(sinistralite) = as.character(0:125)
    sinistralite = sinistralite %>%
      mutate(famille_d_actes = famille) %>%
      relocate(famille_d_actes)
    matrices[[nom]] = sinistralite

  }
  matrices
}

# matrices_taux = matrice_taux_sinistralite(m_effectifs,m_sinistre)

# m = matrice_taux_sinistralite(consommation, df_effectif, "famille_d_actes","type_b_n_ficiaire",
#                               "code_b_n_ficiaire","age","sexe","type_ald","num_ro_du_dossier",
#                               "quantit", "Type du bénéficiaire", "TYPE ALD", "sexe", "Age")




## Calcul de montant engagé moyen -----

matrice_cout_moyen <- function(consommation, type_b_n_ficiaire, type_ald,
                               sexe, famille_d_actes, age, montant_engag_inflat,quantite){
  tbl_sinistres <- consommation[, c(type_b_n_ficiaire, type_ald, sexe, famille_d_actes, age,  montant_engag_inflat,quantite)]
  names(tbl_sinistres) <- c("type_b_n_ficiaire", "type_ald", "sexe", "famille_d_actes", "age", "montant_engag_inflat","quantit")

  type_benef <- sort(unique(tbl_sinistres$type_b_n_ficiaire))
  typeALD <- unique(tbl_sinistres$type_ald)
  sexe_benef <- unique(tbl_sinistres$sexe)
  tbl_sinistres <- tbl_sinistres %>%
    mutate(famille_d_actes = factor(famille_d_actes, sort(unique(famille_d_actes))),
           age = factor(age, 0:125))
  matrices = list()

  for (benef in type_benef) {
    for (ALD in typeALD){
      for (sex in sexe_benef) {

        cout = tbl_sinistres %>%
          filter(
            type_b_n_ficiaire == benef,
            sexe == sex,
            type_ald == ALD
          ) %>%
          group_by(famille_d_actes, age, .drop = FALSE) %>%
          summarise(Cout = sum(montant_engag_inflat,na.rm = TRUE)/sum(quantit,na.rm = TRUE)) %>%
          pivot_wider(
            names_from = age,
            values_from = Cout,
            names_expand = TRUE
          )
        resultat = cout[,1:127]
        path <- paste0( ifelse(is.na(benef),"NA",benef), " ",
                        ifelse(is.na(ALD),"NA",ALD), " ",
                        ifelse(is.na(sex),"NA",sex))
        path <- gsub("#N/A","NA",path)
        matrices[[path]] =resultat
      }
    }
  }
  matrices
}


# m_conso <- matrice_cout_moyen(consommation,"Statut nv", "ALD nv", "Sexe nv", "Code acte quantrix", "Age nv",  "FE_Inflaté")
# m_conso <- matrice_cout_moyen(test_conso,"Categorie", "type_ald", "sexe", "famille_d_actes",
#                               "age",  "montant_engag_inflat","quantit")




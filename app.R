#' Cette application permet de calculer toutes les matrices dont on a besoin
#'lors de la projection des prestations des bénéficiaire

library(shiny)
library(bs4Dash)
library(shinybusy)
source("Functions.R")
library(shinyalert)
library(thematic)


# Define UI for application that draws a histogram----
ui <- dashboardPage(
  dashboardHeader(
    title = "ARM",
    skin = "light"
  ),
  controlbar = dashboardControlbar(
    collapsed = TRUE,
    div(class = "p-3", skinSelector()),
    pinned = FALSE
  ),
  footer = dashboardFooter(left = "@Abdoul Oudouss Diakité"),
  # Sidebar content-------------
  dashboardSidebar(
    status = "navy", skin = "light", collapsed = F,
    br(),
    sidebarMenu(
      menuItem(text = "Importation", tabName = "import", icon = icon("upload")),
      menuItem(text = "Paramètres", tabName = "parametres", icon = icon("sliders")),
      menuItem(text = "Statistiques", tabName = "StatsEff", icon = icon("chart-pie")),
      menuItem(text = "Matrices", tabName = "ConsoMatrice", icon = icon("calculator"))
    ),
  ),
  # Body content----------
  dashboardBody(
    # Boxes need to be put in a row (or column)
    # Sidebar Items
    tabItems(
      # Importation-------
      tabItem(
        tabName = "import",
        add_busy_spinner(
          spin = "half-circle",
          margins = c(300, 600), width = "150px", height = "150px"
        ),
        fluidRow(
          # Import:  Importaion
          box(
            status = "navy", solidHeader = T, width = 6,
            title = "Effectifs",
            # Input: Select a file
            fileInput("FileEff", "Choisir la base de données des effectifs",
              buttonLabel = "Parcourir...",
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            ),

            # Horizontal line
            tags$hr(),
            fluidRow(
              column(
                4,
                # Input: Checkbox if file has header
                checkboxInput("header", "Header", TRUE)
              ),
            ),
            # Horizontal line
            tags$hr(),
            fluidRow(
              column(
                4,
                # Input: Select separator
                radioButtons("sep", "Separator",
                  choices = c(
                    Comma = ",",
                    Semicolon = ";",
                    Tab = "\t"
                  ),
                  selected = ","
                )
              ),
              column(
                5,
                offset = 1,
                # Input: Select quotes
                radioButtons("quote", "Quote",
                  choices = c(
                    None = "",
                    "Double Quote" = '"',
                    "Single Quote" = "'"
                  ),
                  selected = '"'
                )
              )
            )
          ),
          box(
            status = "navy", solidHeader = T, width = 6,
            title = "Consommation",
            # Input: Select a file
            fileInput("FileConso", "Choisir la base de données de consommations",
              buttonLabel = "Parcourir...",
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            ),

            # Horizontal line
            tags$hr(),
            fluidRow(
              column(
                4,
                # Input: Checkbox if file has header
                checkboxInput("headerConso", "Header", TRUE)
              ),
            ),
            # Horizontal line
            tags$hr(),
            fluidRow(
              column(
                4,
                # Input: Select separator
                radioButtons("sepConso", "Separator",
                  choices = c(
                    Comma = ",",
                    Semicolon = ";",
                    Tab = "\t"
                  ),
                  selected = ","
                )
              ),
              column(
                5,
                offset = 1,
                # Input: Select quotes
                radioButtons("quoteConso", "Quote",
                  choices = c(
                    None = "",
                    "Double Quote" = '"',
                    "Single Quote" = "'"
                  ),
                  selected = '"'
                )
              )
            )
          ),
          tabBox(
            solidHeader = T, width = 12,
            type = "tabs", side = "right",
            title = "Table",
            tabPanel(
              "Effectif",
              DT::dataTableOutput("BDE", width = "100%")
            ),
            tabPanel(
              "Consommation",
              DT::dataTableOutput("BDC", width = "100%")
            )
          )
        )
      ),
      # Parametres-------
      tabItem(
        tabName = "parametres",
        add_busy_spinner(
          spin = "half-circle",
          margins = c(300, 600), width = "150px", height = "150px"
        ),
        fluidRow(
          box(
            solidHeader = T, width = 6, maximizable = TRUE,
            title = "Effectif",
            uiOutput("EffUI")
          ),
          box(
            solidHeader = T, width = 6, maximizable = TRUE,
            title = "Consommation",
            uiOutput("ConsoUI")
          )
        )
      ),
      # Statistiques-------
      tabItem(
        tabName = "StatsEff",
        tags$h2("Statistiques")
      ),
      # Matrices de consommation-------
      tabItem(
        tabName = "ConsoMatrice",
        add_busy_spinner(
          spin = "half-circle",
          margins = c(300, 600), width = "150px", height = "150px"
        ),
        fluidRow(
          box(status = "navy",
            solidHeader = T, width = 6, maximizable = TRUE, collapsed = FALSE,
            title = "Moyenne par famille d'actes",
            DT::dataTableOutput("MPFA")
          ),
          box(status = "navy",
            solidHeader = T, width = 6, maximizable = TRUE, collapsed = FALSE,
            title = "Effectifs par âge",
            tags$caption("Effectif réparti par âge de chaque population sélectionné"),
            uiOutput("EffUI2"),
            tags$hr(),
            DT::dataTableOutput("EffTbl")
          )
        ),
        fluidRow(
          box(status = "navy",
            solidHeader = T, width = 6, maximizable = TRUE, collapsed = TRUE,
            title = "Nombre de sinistrés",
            tags$caption("Le nombre de sinistrés par âge et par familles et selon la population séléctionée"),
            uiOutput("NbSinUI"),
            tags$hr(),
            DT::dataTableOutput("NbSinTbl")
          ),
          box(status = "navy",
            solidHeader = T, width = 6, maximizable = TRUE, collapsed = TRUE,
            title = "Fréquence d’actes par personne sinistré",
            uiOutput("FreqUI"),
            tags$hr(),
            DT::dataTableOutput("FreqTbl")
          )
        ),
        fluidRow(
          box(status = "navy",
            solidHeader = T, width = 6, maximizable = TRUE, collapsed = TRUE,
            title = "Quantité d'actes consommés",
            tags$caption("Cette table contient la quantité d'actes consommée selon le type du bénéficiaire, le type d'ALD et le sexe et ce pour tout âge"),
            uiOutput("quantUI"),
            tags$hr(),
            DT::dataTableOutput("quantTbl")
          ),
          box(status = "navy",
            solidHeader = T, width = 6, maximizable = TRUE, collapsed = TRUE,
            title = "Taux de sinistralité",
            uiOutput("TauxUI"),
            tags$hr(),
            DT::dataTableOutput("TauxTbl")
          )
        ),
        fluidRow(
          box(status = "navy",
              solidHeader = T, width = 6, maximizable = TRUE, collapsed = TRUE,
              title = "Coûts moyens",
              tags$caption("Moyenne des montants engagés inflaté par famille d'acte et par âge."),
              uiOutput("CoutUI"),
              tags$hr(),
              DT::dataTableOutput("CoutTbl")
          ),
          box(status = "navy",
              solidHeader = T, width = 6, maximizable = FALSE, collapsed = FALSE,
              title = "Télécharger toutes les matrices",
              actionButton("download", "Commencer le téléchargement",
                           status = "success",
                           icon = icon("download"),
                           class = "btn-block",width = "100%"
              ),
              tags$hr(),
              textOutput("MessageFinal")
          )

        )
      )
    )
  )
)

# Server --------
server <- function(input, output) {
  options(shiny.maxRequestSize = 1000 * 1024^2)
  useAutoColor()
  df_effectif <- reactive(tryCatch(
    {
      req(input$FileEff)

      arrow::read_delim_arrow(input$FileEff$datapath,
        col_names = input$header,
        delim = input$sep,
        quote = input$quote
      )
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  ))

  consommation <- reactive(tryCatch(
    {
      req(input$FileConso)

      arrow::read_delim_arrow(input$FileConso$datapath,
        col_names = input$headerConso,
        delim = input$sepConso,
        quote = input$quoteConso
      )
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  ))


  output$BDE <- DT::renderDataTable(
    df_effectif(),
    options = list(
      paging = TRUE, ## paginate the output
      pageLength = 25,
      scrollX = TRUE,
      lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "All")),
      scrollY = 200
    )
  )

  output$BDC <- DT::renderDataTable(
    consommation(),
    options = list(
      paging = TRUE, ## paginate the output
      pageLength = 25,
      scrollX = TRUE,
      lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "All")),
      scrollY = 200
    )
  )

  output$EffUI <- renderUI({
    fluidRow(
      column(
        6,
        selectInput("IDBE", "Identifiant Bénéficiare", c("", names(df_effectif())), multiple = FALSE, selected = "")
      ),
      column(
        6,
        selectInput("ABE", "Age", c("", names(df_effectif())), multiple = FALSE, selected = "")
      ),
      column(
        12,
        tags$hr()
      ),
      column(
        6,
        selectInput("TBE", "Type Bénéficiare", c("", names(df_effectif())), multiple = FALSE, selected = "")
      ),
      column(
        6,
        selectInput("TAE", "Type Assuré", c("", names(df_effectif())), multiple = FALSE, selected = "")
      ),
      column(
        12,
        tags$hr()
      ),
      column(
        6,
        selectInput("TALDE", "Type ALD", c("", names(df_effectif())), multiple = FALSE, selected = "")
      ),
      column(
        6,
        selectInput("SBE", "Sexe", c("", names(df_effectif())), multiple = FALSE, selected = "")
      ),
      column(
        12,
        tags$hr()
      ),
      column(
        12,
        actionButton("columnsEff", "Confirmer",
          status = "success",
          icon = icon("check"), class = "btn-block"
        )
      )
    )
  })

  output$ConsoUI <- renderUI({
    fluidRow(
      column(
        6,
        selectInput("IDBC", "Identifiant Bénéficiare", c("", names(consommation())), multiple = FALSE, selected = "")
      ),
      column(
        6,
        selectInput("ABC", "Age", c("", names(consommation())), multiple = FALSE, selected = "")
      ),
      column(
        12,
        tags$hr()
      ),
      column(
        6,
        selectInput("TBC", "Type Bénéficiare", c("", names(consommation())), multiple = FALSE, selected = "")
      ),
      column(
        6,
        selectInput("TNR", "Taux de remboursement", c("", names(consommation())), multiple = FALSE, selected = "")
      ),
      column(
        12,
        tags$hr()
      ),
      column(
        6,
        selectInput("TALDC", "Type ALD", c("", names(consommation())), multiple = FALSE, selected = "")
      ),
      column(
        6,
        selectInput("SBC", "Sexe", c("", names(consommation())), multiple = FALSE, selected = "")
      ),
      column(
        12,
        tags$hr()
      ),
      column(
        6,
        selectInput("FAC", "Familles d'actes", c("", names(consommation())), multiple = FALSE, selected = "")
      ),
      column(
        6,
        selectInput("YEAR", "Année", c("", names(consommation())), multiple = FALSE, selected = "")
      ),
      column(
        12,
        tags$hr()
      ),
      column(
        6,
        selectInput("BRU", "Base de remboursement", c("", names(consommation())), multiple = FALSE, selected = "")
      ),
      column(
        6,
        selectInput("MER", "Montant engagé", c("", names(consommation())), multiple = FALSE, selected = "")
      ),
      column(
        12,
        tags$hr()
      ),
      column(
        6,
        selectInput("MEI", "Montant engagé inflaté", c("", names(consommation())), multiple = FALSE, selected = "")
      ),
      column(
        6,
        selectInput("Qantite", "Quantité d'actes", c("", names(consommation())), multiple = FALSE, selected = "")
      ),
      column(
        12,
        tags$hr()
      ),
      column(
        12,
        actionButton("columnsConso", "Confirmer",
          status = "success",
          icon = icon("check"), class = "btn-block"
        )
      )
    )
  })

  eff_cols <- reactive({
    list(
      id_benef = input$IDBE,
      age_benef = input$ABE,
      type_benef = input$TBE,
      type_ass = input$TAE,
      type_ALD = input$TALDE,
      sexe_benef = input$SBE
    )
  }) %>%
    bindEvent(input$columnsEff)
  observeEvent(input$columnsEff,
               {
                 shinyalert("OK!","Selection effectuée !",showConfirmButton = T,timer = 1000,type = "success")
               })
  conso_cols <- reactive({
    list(
      id_benef = input$IDBC,
      age_benef = input$ABC,
      type_benef = input$TBC,
      taux_remb = input$TNR,
      type_ALD = input$TALDC,
      sexe_benef = input$SBC,
      famille_acte = input$FAC,
      ANNEE = input$YEAR,
      base_remb = input$BRU,
      mont_eng = input$MER,
      mont_eng_inf = input$MEI,
      quantit = input$Qantite
    )
  }) %>%
    bindEvent(input$columnsConso)
  observeEvent(input$columnsConso,
               {
                 shinyalert("OK!","Selection effectuée !",showConfirmButton = T,timer = 1000,type = "success")
               })

  ## Calcul de montant engagé moyen -----
  matrices_cout_moyen <- reactive({
    matrice_cout_moyen(
      consommation(), conso_cols()$type_benef, conso_cols()$type_ALD,
      conso_cols()$sexe_benef, conso_cols()$famille_acte,
      conso_cols()$age_benef,
      conso_cols()$mont_eng_inf,conso_cols()$quantit
    )
  })

  ## Calcul du taux de sinistralité -------
  matrices_taux_sinistralite <- reactive({
    matrice_taux_sinistralite(matrices_effectifs(), matrices_sinsitré())
  })

  ## Calcul des effectifs pour tout type (benef, ald, age) ------
  matrices_effectifs <- reactive({
    matrice_effectif(
      df_effectif(), eff_cols()$type_benef, eff_cols()$type_ALD,
      eff_cols()$sexe_benef, eff_cols()$age_benef
    )
  })


  ## Calcul de la Fréquence d’actes par personne sinistré ----
  matrices_freq_actes <- reactive({
    matrice_freq_acte(
      matrices_sinsitré(),matrices_quant()
    )
  })

  ## Calcul des nombre de sinistrés -----
  matrices_sinsitré <- reactive({
    matrice_sinsitre(
      consommation(), conso_cols()$famille_acte, conso_cols()$type_benef,
      conso_cols()$id_benef, conso_cols()$age_benef, conso_cols()$sexe_benef,
      conso_cols()$type_ALD,
      conso_cols()$ANNEE
    )
  })

  ## Calcul de des moyenne par famille d'actes ----
  Moyenne_Par_Famille_Acte <- reactive({
    Moyenne_par_famille(
      consommation(), conso_cols()$famille_acte,
      conso_cols()$mont_eng, conso_cols()$base_remb,
      conso_cols()$taux_remb
    )
  })

  ## Calcul des matrices des quantites d'actes consommés ----
  matrices_quant <- reactive({
    matrice_quatinte(
      consommation(), conso_cols()$famille_acte, conso_cols()$type_benef,
      conso_cols()$age_benef, conso_cols()$sexe_benef,
      conso_cols()$type_ALD,conso_cols()$quantit
    )
  })






  # -------

  # Quantite acte ----
  output$quantUI <- renderUI({
    fluidRow(
      column(8,
             selectInput("quantSelect", "Séléctionner une matrice",
                         c("",names(matrices_quant())),multiple = FALSE,selected = "")
      ),
      column(4,
             actionButton("quantBtn", "Afficher",
                          style = "margin-top:30px; color: #fff; background-color: #001f3f; border-color: #001f3f",width = "100%")
      )
    )
  })
  quant_selected <- reactive({
    input$quantSelect
  }) %>%
    bindEvent(input$quantBtn)

  # Nb sinistré -----
  output$NbSinUI <- renderUI({
    fluidRow(
      column(8,
             selectInput("NbSinSelect", "Séléctionner une matrice",
                         c("",names(matrices_sinsitré())),multiple = FALSE,selected = "")
      ),
      column(4,
             actionButton("NbSinBtn", "Afficher",
                          style = "margin-top:30px; color: #fff; background-color: #001f3f; border-color: #001f3f",width = "100%")
      )
    )
  })
  NbSin_selected <- reactive({
    input$NbSinSelect
  }) %>%
    bindEvent(input$NbSinBtn)

  # Freqence actes -----
  output$FreqUI <- renderUI({
    fluidRow(
      column(8,
             selectInput("FreqSelect", "Séléctionner une matrice",
                         c("",names(matrices_freq_actes())),multiple = FALSE,selected = "")
      ),
      column(4,
             actionButton("FreqBtn", "Afficher",
                          style = "margin-top:30px; color: #fff; background-color: #001f3f; border-color: #001f3f",width = "100%")
      )
    )
  })
  Freq_Selected <- reactive({
    input$FreqSelect
  }) %>%
    bindEvent(input$FreqBtn)

  # Effectif ------
  output$EffUI2 <- renderUI({
    fluidRow(
      column(8,
             selectInput("EffSelect", "Séléctionner une matrice",
                         c("",names(matrices_effectifs())),multiple = FALSE,selected = "")
      ),
      column(4,
             actionButton("EffBtn", "Afficher",
                          style = "margin-top:30px; color: #fff; background-color: #001f3f; border-color: #001f3f",width = "100%")
      )
    )
  })
  Eff_Selected <- reactive({
    input$EffSelect
  }) %>%
    bindEvent(input$EffBtn)

  # Taux sinistralité ----
  output$TauxUI <- renderUI({
    fluidRow(
      column(8,
             selectInput("TauxSelect", "Séléctionner une matrice",
                         c("",names(matrices_taux_sinistralite())),multiple = FALSE,selected = "")
      ),
      column(4,
             actionButton("TauxBtn", "Afficher",
                          style = "margin-top:30px; color: #fff; background-color: #001f3f; border-color: #001f3f",width = "100%")
      )
    )
  })
  Taux_Selected <- reactive({
    input$TauxSelect
  }) %>%
    bindEvent(input$TauxBtn)

  # Taux sinistralité ----
  output$CoutUI <- renderUI({
    fluidRow(
      column(8,
             selectInput("CoutSelect", "Séléctionner une matrice",
                         c("",names(matrices_cout_moyen())),multiple = FALSE,selected = "")
      ),
      column(4,
             actionButton("CoutBtn", "Afficher",
                          style = "margin-top:30px; color: #fff; background-color: #001f3f; border-color: #001f3f",width = "100%")
      )
    )
  })
  Cout_Selected <- reactive({
    input$CoutSelect
  }) %>%
    bindEvent(input$CoutBtn)

  output$CoutTbl <- DT::renderDataTable(
    matrices_cout_moyen()[[Cout_Selected()]],
    extensions = "Buttons",
    options = list(
      searching = FALSE,
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel", "pdf", "print"),
      paging = FALSE, ## paginate the output
      pageLength = 10,
      scrollX = TRUE,
      scrollY = 200
    )
  )


  output$MPFA <- DT::renderDataTable(
    Moyenne_Par_Famille_Acte(),
    extensions = "Buttons",
    options = list(
      searching = FALSE,
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel", "pdf", "print"),
      paging = FALSE, ## paginate the output
      pageLength = 10,
      scrollX = TRUE,
      scrollY = 200
    )
  )

  output$EffTbl <- DT::renderDataTable(
    matrices_effectifs()[[Eff_Selected()]],
    extensions = "Buttons",
    options = list(
      searching = FALSE,
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel", "pdf", "print"),
      paging = FALSE, ## paginate the output
      pageLength = 10,
      scrollX = TRUE,
      scrollY = 200
    )
  )

  output$NbSinTbl <- DT::renderDataTable(
    matrices_sinsitré()[[NbSin_selected()]],
    extensions = "Buttons",
    options = list(
      searching = FALSE,
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel", "pdf", "print"),
      paging = FALSE, ## paginate the output
      pageLength = 10,
      scrollX = TRUE,
      scrollY = 200
    )
  )

  output$FreqTbl <- DT::renderDataTable(
    matrices_freq_actes()[[Freq_Selected()]],
    extensions = "Buttons",
    options = list(
      searching = FALSE,
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel", "pdf", "print"),
      paging = FALSE, ## paginate the output
      pageLength = 10,
      scrollX = TRUE,
      scrollY = 200
    )
  )


  output$quantTbl <- DT::renderDataTable(
    matrices_quant()[[quant_selected()]],
    extensions = "Buttons",
    options = list(
      searching = FALSE,
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel", "pdf", "print"),
      paging = FALSE, ## paginate the output
      pageLength = 10,
      scrollX = TRUE,
      scrollY = 200
    )
  )

  output$TauxTbl <- DT::renderDataTable(
    matrices_taux_sinistralite()[[Taux_Selected()]],
    extensions = "Buttons",
    options = list(
      searching = FALSE,
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel", "pdf", "print"),
      paging = FALSE, ## paginate the output
      pageLength = 10,
      scrollX = TRUE,
      scrollY = 200
    )
  )

  observeEvent(input$download,
               {
                 Message_final = tryCatch(
                   {

                     writexl::write_xlsx(Moyenne_Par_Famille_Acte(),"Telechargements/Moyenne_Par_Famille_Acte.xlsx")

                     for (i in names(matrices_quant())) {
                       path = paste0("Telechargements/Quantité actes consommés/",i,".xlsx")
                       writexl::write_xlsx(matrices_quant()[[i]],path)
                     }

                     for (i in names(matrices_sinsitré())) {
                       path = paste0("Telechargements/Nombre de sinistrés/",i,".xlsx")
                       writexl::write_xlsx(matrices_sinsitré()[[i]],path)
                     }

                     for (i in names(matrices_freq_actes())) {
                       path = paste0("Telechargements/Fréquence d’actes par personne sinistré/",i,".xlsx")
                       writexl::write_xlsx(matrices_freq_actes()[[i]],path)
                     }

                     for (i in names(matrices_effectifs())) {
                       path = paste0("Telechargements/Effectifs par âge/",i,".xlsx")
                       writexl::write_xlsx(matrices_effectifs()[[i]],path)
                     }

                     for (i in names(matrices_taux_sinistralite())) {
                       path = paste0("Telechargements/Taux de sinistralité/",i,".xlsx")
                       writexl::write_xlsx(matrices_taux_sinistralite()[[i]],path)
                     }

                     for (i in names(matrices_cout_moyen())) {
                       path = paste0("Telechargements/Couts moyens/",i,".xlsx")
                       writexl::write_xlsx(matrices_cout_moyen()[[i]],path)
                     }

                     c("OK!","Télchargement effectué avec succés !","success")
                   },
                   error = function(e){
                     c("Oops!","Téléchargement incomplet, une erreur s'est produite !","error")
                   })
                 shinyalert(Message_final[1], Message_final[2] , type = Message_final[3])
                 output$MessageFinal <- renderText(Message_final[2])
               })
}

# Run the application
shinyApp(ui = ui, server = server)

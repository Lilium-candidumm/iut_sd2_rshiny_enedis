library(shiny) # Site
library(shinydashboard) # Mise en forme 
library(bslib) # Mise en forme 
library(fontawesome) # Icones
library(leaflet) # Carte
library(DT) # Render datatable
library(dplyr) # Tri données
library(ggplot2) # Graphiques

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


existants = read.csv('./Données/base_de_donnees.csv',sep = ",",dec = ".")

user_base <- tibble::tibble(
  user = c("mtabo", "lmich"),
  password = sapply(c("0108", "1305"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("Martin", "Lucas")
)


  
ui <- dashboardPage(
  dashboardHeader(
    title = "Les DPE du Rhône",
    tags$li(class = "dropdown", style = "margin-right: 10px;", # Ajouté pour placer le bouton dans l'en-tête
            shinyauthr::logoutUI(id = "logout")
    )
  ),
  
  dashboardSidebar(
    # Sidebar only visible after login
    uiOutput("sidebarpanel")
  ),
  
  dashboardBody(
    # Login module, reste dans le body
    shinyauthr::loginUI(id = "login"),
    
    # Main content of the dashboard, visible only when logged in
    uiOutput("body")
  )
)
  
  
  
  
  

server <- function(input, output) {
  
  # Credentials authentication logic
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout functionality
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  # Sidebar rendering logic based on authentication
  output$sidebarpanel <- renderUI({
    req(credentials()$user_auth)  # Show only if authenticated
    
    sidebarMenu(
      menuItem("Accueil", tabName = "Accueil", icon = icon("home")),
      menuItem("Vue d'ensemble", tabName = "VueEnsemble", icon = icon("eye")),
      menuItem("Carte interactive", tabName = "carte", icon = icon("map")),
      menuItem("Corrélation", tabName = "correlation", icon = icon("blackboard")),
      menuItem("Explorer les données", tabName = "visu_donnees", icon = icon("list-alt")),
      selectInput(inputId = "departement_sidebar", 
                  label = "Filtrer par département :",
                  choices = c(sort(unique(existants$Code_postal_.BAN.))), 
                  multiple = TRUE),
      checkboxGroupInput(inputId = "type_batiment", 
                   label = "Sélectionner le Type de Bâtiment:",
                   choices = sort(unique(existants$Type_bâtiment)), 
                   inline = TRUE) 
    )
  })
  

  ##################################Accueil#########################
  
  output$body <- renderUI({
    req(credentials()$user_auth) 

  
    tabItems(
      
      
      
      tabItem(tabName = "Accueil",
              fluidPage(
                tags$style(HTML("
    .justify {
      text-align: justify;
      font-size: 18px; 
    }
    .centered {
      text-align: center; 
      font-weight: bold; 
    }
    
  ")),
                
                h2(class = "centered", "Qu'est-ce qu'un DPE ?"),  
                
                br(),
                
                div(class = "justify",  
                    "Le diagnostic de performance énergétique (DPE) renseigne sur la performance énergétique et climatique d’un logement ou d’un bâtiment (étiquettes A à G), en évaluant sa consommation d’énergie et son impact en terme d’émissions de gaz à effet de serre.
      Il a pour objectif d’informer l’acquéreur ou le locataire sur la « valeur verte », de recommander des travaux à réaliser pour l’améliorer et d’estimer ses charges énergétiques."
                )
              ),
              br(),
              br(),
              div(style = "text-align: center;",
                  img(src = "DPE.png", height ="50%", width = "50%",style = "border: 2px solid black;")),
              br(),
              br(),
              div(class = "justify",
                  "Ce site a été réalisé dans le cadre d'un projet de 2ème année en Science des Données."
              ),
              br(),
              div(class = "justify",
                  "Nous devions utiliser les données des DPE en France afin d'en faire une présentation globale au travers de ce site, ceci ayant pour but de nous initier à Rshiny ainsi que d'avoir un aperçu de ses possibilités. Les données sont
                  celles provenant de l'API sur les DPE en france"
              )
      ),
      
      
 #################################Vue d'ensemble#############################"     
      
      tabItem(tabName = "VueEnsemble",
              fluidPage(
                fluidRow(
                  infoBox("Nombre de DPE",length(existants$N.DPE), icon = icon("file"),color = "teal"),
                  infoBox("Moyennne du coût de chauffage/an",paste(round(mean(existants$Coût_chauffage, na.rm = TRUE),2),'€',sep = " "),icon = icon("fire"),color = "maroon"),
                  infoBox("Moyenne des emissions de CO2/an",paste(round(mean(existants$Emission_GES_5_usages,na.rm = TRUE),2),"kg de CO2/an",sep = " "),icon = icon("leaf"),color = "olive")
                ),
                
                fluidRow(
                  box(plotOutput(outputId = "histDPE"),
                      width = 6,
                      background = "light-blue",
                      title = "Répartition des étiquettes DPE")
                  ,
                  box(plotOutput(outputId = "pie"),
                      width = 6,
                      background = "light-blue",
                      title = "Répartition des types de bâtiments")
                ),
                br(),
                fluidRow(
                  box(plotOutput(outputId = "test"),
                      width = 6,
                      background = "light-blue",
                      title = "Répartition des principaux types d'énergies d'un logement"),
                  box(plotOutput(outputId = "moustache"),
                      width = 6,
                      background = "light-blue",
                      title = "Répartition de la surface des logements")
                  
                )
                
              )
      ),
      
 
 
 ###################################Carte###############################
 
      tabItem(tabName = 'carte',
              card(
                tags$style(type = "text/css", "
    #carte {
      height: 90vh !important;  
    }
  "),
                
                leafletOutput(outputId = "carte", width = '100%')
              )
              
      ),
      
      
      
     
 
 
 ###############################Corrélation############################## 
      
      tabItem(tabName = "correlation",
              fluidPage(sidebarLayout(
                sidebarPanel(
                  
                  
                  selectInput(inputId = "x",
                              label = "Axe des abscisses (x):",
                              choices = sort(c("Coût_éclairage","Emission_GES_ECS","Emission_GES_chauffage",
                                               "Coût_total_5_usages","Conso_ECS_é_finale",
                                               "Emission_GES_5_usages","Besoin_chauffage","Conso_chauffage_é_finale",
                                               "Coût_chauffage","Année_construction","Emission_GES_5_usages_par_m.",
                                               "Conso_5_usages_é_finale","Emission_GES_éclairage","Conso_5_usages.m._é_finale",
                                               "Surface_habitable_logement")),
                              multiple = FALSE),
                  
                  selectInput(inputId = "y",
                              label = "Axe des ordonnées (y):",
                              choices = sort(c("Coût_éclairage","Emission_GES_ECS","Emission_GES_chauffage",
                                               "Coût_total_5_usages","Conso_ECS_é_finale",
                                               "Emission_GES_5_usages","Besoin_chauffage","Conso_chauffage_é_finale",
                                               "Coût_chauffage","Année_construction","Emission_GES_5_usages_par_m.",
                                               "Conso_5_usages_é_finale","Emission_GES_éclairage","Conso_5_usages.m._é_finale",
                                               "Surface_habitable_logement")),
                              multiple = FALSE),
                  
                  
                  
                  actionButton(inputId = "refresh",
                               label = "Tirage de 1000 DPE aléatoires")
                  
                  
                  
                  
                ),
                card(
                  plotOutput(outputId = "plot"),
                  downloadButton(outputId = "telecharger",label = "Télécharger le graphique")
                )),
                card(br(),br(), div(class = "justify",
                                    "Ce graphique permet de tester la corrélation entre deux variables. Utilisez les deux menus déroulants pour sélectionner
                                    une variable pour chaque axe, ensuite cliquez sur le bouton de tirage. Vous pouvez également si vous le souhaitez télécharger le graphique en PNG"),
                     br(),
                     div(class = "justify",
                         "Nous avons choisi pour représenter graphiquement la corrélation entre deux variables de tirer aléatoirement 1000 logements dans notre base de données. Ainsi, cela nous
                         permet de disposer d'un gain de temps de calcul, d'affichage et d'une meilleure lecture du graphique tout en ayant assez de logements pour que les données soient statistiquement
                         pertinentes")
                     )
              )
      ),
      
 
 
 #################################Visu données###########################
 
 
      tabItem("visu_donnees",
              fluidPage(
                titlePanel("Visualisation des données"),
                
                fluidRow(
                  column(2,
                         selectInput("departement",
                                     "Départment:",
                                     choices = c(sort(unique(existants$Code_postal_.BAN.))),
                                     multiple = TRUE)
                  ),
                  column(2,
                         selectInput("Etiquette_DPE",
                                     "Etiquette DPE:",
                                     choices = c(sort(unique(existants$Etiquette_DPE))),
                                     multiple = TRUE)
                  ),
                  column(2,
                         downloadButton("telecharger_donnees", "Télécharger le CSV") 
                  )
    
                ),
                dataTableOutput(outputId = "donnees") 
                
              )
      )
      
      
      
      
      
      
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ####################################Vue d'ensemble filtre###################################

  
  
  df_filtre <- reactive({
    # Commencer avec l'ensemble de données existants
    data <- existants
    
    # Filtrer par département si des valeurs sont sélectionnées
    if (!is.null(input$departement_sidebar) && length(input$departement_sidebar) > 0) {
      data <- data %>% filter(Code_postal_.BAN. %in% input$departement_sidebar)
    }
    
    # Filtrer par type de bâtiment si des valeurs sont sélectionnées
    if (!is.null(input$type_batiment) && length(input$type_batiment) > 0) {
      data <- data %>% filter(Type_bâtiment %in% input$type_batiment)
    }
    
    return(data)  # Retourne les données filtrées
  })
  
  ####################################Vue d'ensemble###################################
  
  output$histDPE = renderPlot({
    feur = table(factor(df_filtre()$Etiquette_DPE, levels = c("G", "F", "E", "D", "C", "B", "A")))
    barplot(feur,
            col = c("#fc0000", "#fca800", "#fce06d", "#ecf359", "#bbf359", "#3cc12f", "#237a1a"),
            xlab="Etiquette DPE",
            ylab = "Nombre de logements",
            border = "black",
            space = 0.3,
            cex.names = 1.5,  # Augmenter la taille des étiquettes des barres
            cex.axis = 1,  # Augmenter la taille des graduations des axes
            cex.lab = 1.2, 
            lwd = 1)
  })
  
  output$pie = renderPlot({
    pipi = table(df_filtre()$Type_bâtiment)
    pie(pipi, col = c("#AAFFAA", "#FFEE44", "#FFAAAA"))
  })
  
  output$moustache = renderPlot({
    surface <- df_filtre()$Surface_habitable_logement
    
    Q1 <- quantile(surface, 0.25, na.rm = TRUE)
    Q3 <- quantile(surface, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    
    borne_inf <- Q1 - 1.5 * IQR
    borne_sup <- Q3 + 1.5 * IQR
    
    surface_filtre <- surface[surface >= borne_inf & surface <= borne_sup]
    
    boxplot(
      surface_filtre, 
      ylab = "Surface habitable (m²)",      
      main = "La Surface Habitable (sans valeurs aberrantes)", 
      col = "#ffac33",                       
      notch = TRUE,                           
      boxwex = 0.5,                         
      outpch = 16,                           
      outcol = "#FC4E07",                    
      cex.main = 1.5,                        
      cex.lab = 1.2                         
    )
  })
  
  output$test = renderPlot({
    type <- df_filtre() %>%
      filter(!is.na(Type_énergie_n.1)) %>%          
      group_by(Type_énergie_n.1) %>%                
      summarise(count = n()) %>%                 
      arrange(desc(count), Type_énergie_n.1) %>%                  
      slice(1:3)                                 
    
    df_with_top_5 <- df_filtre() %>%
      filter(!is.na(Type_énergie_n.1)) %>%          
      mutate(Type_énergie_n.1 = ifelse(Type_énergie_n.1 %in% type$Type_énergie_n.1,
                                       Type_énergie_n.1, "Autres"))  
    
    Type_énergie_n.1_counts <- df_with_top_5 %>%
      group_by(Type_énergie_n.1) %>%
      summarise(count = n())
    
    ggplot(Type_énergie_n.1_counts, aes(x = "", y = count, fill = Type_énergie_n.1)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      labs(fill = "Type d'énergie")
  })
  
  
  
  ####################################Carte##########################################
  
  output$carte = renderLeaflet({
    leaflet(existants) %>% 
      addTiles() %>% 
      addMarkers(~lon, 
                 ~lat,
                 clusterOptions = markerClusterOptions(),
                 popup = paste(existants$numero," ",existants$nom_voie,", ",
                               existants$nom_commune," ",existants$code_postal,
                               "<br>","Etiquette DPE: ",existants$Etiquette_DPE,
                               "<br>","Etiquette GES: ",existants$Etiquette_GES,
                               "<br>","Type d'énergie chauffage: ",existants$Type_énergie_principale_chauffage,
                               "<br>","Coût du chauffage:",existants$Coût_chauffage," €",
                               sep = "")
      )
  })
  
  
  
  df_sample <- reactiveVal(data.frame())
  
  observeEvent(input$refresh, {
    df_filtered <- existants %>%
      filter(!is.na(!!sym(input$x)) & 
               !is.na(!!sym(input$y)) & 
               (!!sym(input$x)) != 0 & 
               (!!sym(input$y)) != 0)
    
    # Tirer un échantillon aléatoire de 1000 lignes
    df_sample(df_filtered %>% sample_n(min(1000, nrow(df_filtered))))
    
  })
  
  # Affichage du graphique après avoir cliqué sur "refresh"
  output$plot <- renderPlot({
    # Récupérer l'échantillon actuel
    sample_data <- df_sample()
    
    if (nrow(sample_data) > 0) {
      # Calcul des quantiles pour les valeurs x et y
      Q1_x <- quantile(sample_data[[input$x]], 0.25)
      Q3_x <- quantile(sample_data[[input$x]], 0.75)
      IQR_x <- Q3_x - Q1_x
      
      Q1_y <- quantile(sample_data[[input$y]], 0.25)
      Q3_y <- quantile(sample_data[[input$y]], 0.75)
      IQR_y <- Q3_y - Q1_y
      
      # Filtrer les valeurs aberrantes en dehors de la plage [Q1 - 1.5*IQR, Q3 + 1.5*IQR]
      sample_data <- sample_data %>%
        filter(
          (!!sym(input$x)) >= (Q1_x - 1.5 * IQR_x) & (!!sym(input$x)) <= (Q3_x + 1.5 * IQR_x),
          (!!sym(input$y)) >= (Q1_y - 1.5 * IQR_y) & (!!sym(input$y)) <= (Q3_y + 1.5 * IQR_y)
        )
      
      cor_coef <- cor(sample_data[[input$x]], sample_data[[input$y]], use = "complete.obs")
      
      # Générer le graphique avec les valeurs filtrées
      ggplot(data = sample_data, aes_string(x = input$x, y = input$y)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        labs(title = "Graphique sans valeurs aberrantes") +
        annotate("text", x = Inf, y = Inf, 
                 label = paste("Coefficient de corrélation =", round(cor_coef, 4)), 
                 hjust = 1.1, vjust = 1.1, size = 5, color = "red", fontface = "bold")
      
      
    }
  })
  
  
   #################################corrélation#######################
  
  
  output$telecharger = downloadHandler(
    filename = function(){
      paste("DPE-plot",".png",sep = "")
      
    },
    content = function(file){
      png(file)
      sample_data <- df_sample()
      
      if (nrow(sample_data) > 0) {
        # Calcul des quantiles pour les valeurs x et y
        Q1_x <- quantile(sample_data[[input$x]], 0.25)
        Q3_x <- quantile(sample_data[[input$x]], 0.75)
        IQR_x <- Q3_x - Q1_x
        
        Q1_y <- quantile(sample_data[[input$y]], 0.25)
        Q3_y <- quantile(sample_data[[input$y]], 0.75)
        IQR_y <- Q3_y - Q1_y
        
        # Filtrer les valeurs aberrantes
        sample_data <- sample_data %>%
          filter(
            (!!sym(input$x)) >= (Q1_x - 1.5 * IQR_x) & (!!sym(input$x)) <= (Q3_x + 1.5 * IQR_x),
            (!!sym(input$y)) >= (Q1_y - 1.5 * IQR_y) & (!!sym(input$y)) <= (Q3_y + 1.5 * IQR_y)
          )
        
        cor_coef <- cor(sample_data[[input$x]], sample_data[[input$y]], use = "complete.obs")
        
        # Générer le graphique avec les valeurs filtrées
        graph = ggplot(data = sample_data, aes_string(x = input$x, y = input$y)) +
          geom_point() +
          geom_smooth(method = "lm", se = FALSE, color = "blue") +
          labs(title = "Graphique sans valeurs aberrantes") +
          annotate("text", x = Inf, y = Inf, 
                   label = paste("Coefficient de corrélation =", round(cor_coef, 4)), 
                   hjust = 1.1, vjust = 1.1, size = 5, color = "red", fontface = "bold") +
          theme_minimal()  # Appliquer un thème si nécessaire
        
        print(graph)
      }
      
      dev.off()
    }
  )
  
  
  ##################################Visu Données##########################
  
  output$donnees = renderDataTable({
    
    donnees_filtrees = existants
    
    
    if(!is.null(input$departement)){
      donnees_filtrees = donnees_filtrees %>% 
        filter(Code_postal_.BAN. %in% input$departement) 
      
    }
    
    if(!is.null(input$Etiquette_DPE)){
      donnees_filtrees = donnees_filtrees %>%
        filter(Etiquette_DPE %in% input$Etiquette_DPE)
    }
    
    datatable(data = donnees_filtrees,
              options = list(
                pageLength = 10,
                dom = 'Bfrtip',
                scrollX = TRUE,
                buttons = list('colvis'),  
                columnDefs = list(
                  list(visible = FALSE, targets = c(11:63)) 
                )
              ),
              extensions = 'Buttons',  
              rownames = FALSE
    )
  })
  
  
  output$telecharger_donnees <- downloadHandler(
    filename = function() {
      paste("donnees_filtrees",".csv", sep = "")
    },
    content = function(file) {
    
      donnees_filtrees = existants
      
      if (!is.null(input$departement)) {
        donnees_filtrees = donnees_filtrees %>% 
          filter(Code_postal_.BAN. %in% input$departement) 
      }
      
      if (!is.null(input$Etiquette_DPE)) {
        donnees_filtrees = donnees_filtrees %>%
          filter(Etiquette_DPE %in% input$Etiquette_DPE)
      }
      
     
      write.csv(donnees_filtrees, file, row.names = FALSE)
    }
  )
  
  
}

shinyApp(ui, server)

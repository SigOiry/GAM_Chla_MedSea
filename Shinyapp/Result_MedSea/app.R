# library(brms)
library(tidyverse)
library(patchwork)
# library(Utilities.Package)


df_MLD_Manual <- read.csv("df_MLD_Raw.csv")
df_MLD_Relatif <- read.csv("df_MLD_Relatif.csv")
df_SST_Manual <- read.csv("df_SST_Raw.csv")
df_SST_Relatif <- read.csv("df_SST_Relatif.csv")
df_sum_Nutri_Manual <- read.csv("df_Sum_Nutri_Raw.csv")
df_sum_Nutri_Relatif <- read.csv("df_Sum_Nutri_Relatif.csv")
df_NP_Manual <- read.csv("df_NP_Raw.csv")
df_NP_Relatif <- read.csv("df_NP_Relatif.csv")



colpal <- c("Adriatic" = "goldenrod","Lyon" = "darkgreen","Cyprus" = "darkred")


##### MLD #####



p1 <- df_MLD_Manual %>% 
    ggplot(aes(x = MLD, y = Estimate, ymin = Q2.5, ymax = Q97.5, color = Box, fill = Box)) +
    geom_line() +
    scale_color_manual(values = colpal)+
    scale_fill_manual(values = colpal)+
    facet_wrap(~Box)+
    geom_ribbon(alpha = 0.2, color = NA) +
    labs(title = "Marginal effect of MLD on Chla in raw scales", y = "Chla (mg/m³)", x = "MLD (m)") +
    theme_minimal()

p2 <- df_MLD_Relatif %>% 
    ggplot(aes(x = MLD, y = Estimate, ymin = Q2.5, ymax = Q97.5, color = Box, fill = Box)) +
    geom_line() +
    scale_color_manual(values = colpal)+
    scale_fill_manual(values = colpal)+
    facet_wrap(~Box)+
    geom_ribbon(alpha = 0.2, color = NA) +
    labs(title = "Marginal effect of MLD on Chla in relative scales", y = "Relative Chla", x = "Relative MLD") +
    theme_minimal()


p3 <- df_SST_Manual %>% 
    ggplot(aes(x = SST, y = Estimate, ymin = Q2.5, ymax = Q97.5, color = Box, fill = Box)) +
    geom_line() +
    scale_color_manual(values = colpal)+
    scale_fill_manual(values = colpal)+
    facet_wrap(~Box)+
    geom_ribbon(alpha = 0.2, color = NA) +
    labs(title = "Marginal effect of SST on Chla in raw scales", y = "Chla (mg/m³)", x = "SST (°C)") +
    theme_minimal()

p4 <- df_SST_Relatif %>% 
    ggplot(aes(x = SST, y = Estimate, ymin = Q2.5, ymax = Q97.5, color = Box, fill = Box)) +
    geom_line() +
    scale_color_manual(values = colpal)+
    scale_fill_manual(values = colpal)+
    facet_wrap(~Box, scale = "free")+
    geom_ribbon(alpha = 0.2, color = NA) +
    labs(title = "Marginal effect of SST on Chla in relative scales", y = "Relative Chla", x = "Relative SST") +
    theme_minimal()

p5 <- df_sum_Nutri_Manual %>% 
    ggplot(aes(x = sum_Nutri, y = Estimate, ymin = Q2.5, ymax = Q97.5, color = Box, fill = Box)) +
    geom_line() +
    scale_color_manual(values = colpal)+
    scale_fill_manual(values = colpal)+
    facet_wrap(~Box)+
    geom_ribbon(alpha = 0.2, color = NA) +
    labs(title = "Marginal effect of Sum_Nutri on Chla in raw scales", y = "Chla (mg/m³)", x = "Sum_Nutri (mmol.m-3)") +
    theme_minimal()


p6 <- df_sum_Nutri_Relatif %>% 
    ggplot(aes(x = sum_Nutri, y = Estimate, ymin = Q2.5, ymax = Q97.5, color = Box, fill = Box)) +
    geom_line() +
    scale_color_manual(values = colpal)+
    scale_fill_manual(values = colpal)+
    facet_wrap(~Box)+
    geom_ribbon(alpha = 0.2, color = NA) +
    labs(title = "Marginal effect of Sum_Nutri on Chla in relative scales", y = "Relative Chla", x = "Relative Sum_Nutri") +
    theme_minimal()

p7 <- df_NP_Manual %>% 
  ggplot(aes(x = N_P, y = Estimate, ymin = Q2.5, ymax = Q97.5, color = Box, fill = Box)) +
  geom_line() +
  scale_color_manual(values = colpal)+
  scale_fill_manual(values = colpal)+
  facet_wrap(~Box)+
  geom_ribbon(alpha = 0.2, color = NA) +
  labs(title = "Marginal effect of NPratios on Chla in raw scales", y = "Chla (mg/m³)", x = "Ratio N/P") +
  theme_minimal()


p8 <- df_NP_Relatif %>% 
  ggplot(aes(x = N_P, y = Estimate, ymin = Q2.5, ymax = Q97.5, color = Box, fill = Box)) +
  geom_line() +
  scale_color_manual(values = colpal)+
  scale_fill_manual(values = colpal)+
  facet_wrap(~Box)+
  geom_ribbon(alpha = 0.2, color = NA) +
  labs(title = "Marginal effect of NPratio on Chla in relative scales", y = "Relative Chla", x = "Relative Ratio N/P") +
  theme_minimal()

plot_MLD_RAW <- p1

plot_MLD_Relative <- p2

plot_SST_RAW <- p3

plot_SST_Relative <- p4

plot_Nutri_Raw <- p5

plot_Nutri_Relatives <- p6

plot_NP_Raw <- p7

plot_NP_Relatives <- p8





# app.R ---------------------------------------------------------------
# Load packages -------------------------------------------------------
library(shiny)
library(ggplot2)
library(bslib)    # needed only if you create/modify the ggplots here

# --------------------------------------------------------------------
# 1. Put your eight ggplot objects in the workspace.
#    Name (or assign) them however you like; below we assume:
#      var1_raw, var1_rel, var2_raw, var2_rel, var3_raw, var3_rel, var4_raw, var4_rel
#    ── OR ── replace the 'plots' list below with whatever names you use.

# Example placeholders (delete these lines once your real plots exist)
# var1_raw <- ggplot() + geom_blank() + labs(title = "Var 1 · Raw")
# var1_rel <- ggplot() + geom_blank() + labs(title = "Var 1 · Relative")
# ... repeat for the other six ...

# Nest them in a list ⇒ plots[["Var 1"]][["Raw"]], etc.
# plots <- list(
#   WindSpeed = list(Raw = plot_WindSpeed_RAW, Relative = plot_WindSpeed_Relative),
#   SST = list(Raw = plot_SST_RAW, Relative = plot_SST_Relative),
#   IceCover = list(Raw = plot_IceCover_Raw, Relative = plot_IceCover_Relatives)
# )

plots <- list(
    Raw = list(
        MLD = plot_MLD_RAW,
        SST       = plot_SST_RAW,
        Nutriment  = plot_Nutri_Raw,
        `Ratio N/P` = plot_NP_Raw
    ),
    Relative = list(
        MLD = plot_MLD_Relative,
        SST       = plot_SST_Relative,
        Nutriment  = plot_Nutri_Relatives,
        `Ratio N/P` = plot_NP_Relatives
    )
)

# --------------------------------------------------------------------
# 2. UI ---------------------------------------------------------------
# ── 2.  USER INTERFACE  ───────────────────────────────────────────────
ui <- fillPage(                           # full viewport
    tags$head(
        tags$style(HTML("
      body, html { margin: 0; }           /* remove default margins     */
      #plot-box, #plot-box > .shiny-plot-output {
        height: 80vh; width: 100vw;      /* plot occupies full screen  */
      }
      #controls {
        position: absolute;
        top: 1rem; right: 1rem;           /* corner placement           */
        width: 230px;
        background: rgba(255,255,255,.92);
        border-radius: .5rem;
        box-shadow: 0 0.5rem 1rem rgba(0,0,0,.15);
        padding: .8rem 1rem;
        z-index: 1000;
      }
      #controls .radio { margin-bottom: .4rem; }
    "))
    ),
    
    div(id = "plot-box",
        plotOutput("displayPlot", height = "100%", width = "100%"),
        
        # floating panel
        div(id = "controls",
            radioButtons("scale", "Scale",
                         choices  = c("Raw", "Relative"),
                         selected = "Raw"),
            radioButtons("variable", "Variable",
                         choices  = names(plots$Raw),
                         selected = names(plots$Raw)[1])
        )
    )
)

# ── 3.  SERVER  ───────────────────────────────────────────────────────
server <- function(input, output, session) {
    
    chosenPlot <- reactive( plots[[input$scale]][[input$variable]] )
    
    output$displayPlot <- renderPlot(res = 120,{
        chosenPlot()
    })
}



# ---------------------------------------------------------------------
shinyApp(ui, server)
# rsconnect::deployApp(appDir = "ShinyApp/Result_MedSea")

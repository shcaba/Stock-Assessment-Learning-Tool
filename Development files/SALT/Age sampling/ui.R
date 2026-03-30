library(shiny)
library(bslib)
library(ggplot2)
library(shinyWidgets)
library(FSAsim)
library(r4ss)
library(plotly)
library(twosamples)

ui <- page_sidebar(
  title = "Sampling Age Distributions: How many samples are needed?",
  sidebar = sidebar(
    width = 400,
    h5(
      "Set up the life history to explore. Growth, mortality, and selectivity will create and age distribution."
    ),
    card(
      card_header("Growth Parameters"),
      fluidRow(
        column(
          width = 4,
          numericInput("Linf.pval", "L∞", value = 60, min = 1)
        ),
        column(
          width = 4,
          numericInput(
            "K.pval",
            "k",
            value = 0.133,
            min = 0.01,
            step = 0.01
          )
        ),
        column(
          width = 4,
          numericInput("t0.pval", "t0", value = 0, step = 0.1)
        )
      ),
    ),
    card(
      card_header("Mortality Parameters"),
      fluidRow(
        column(
          width = 6,
          numericInput(
            "M.pval",
            "Natural mortality",
            value = 0.2,
            min = 0.001,
            step = 0.001
          )
        ),
        column(
          width = 6,
          numericInput(
            "F.pval",
            "Fishing mortality",
            value = 0,
            min = 0,
            step = 0.001
          )
        )
      )
    ),
    card(
      card_header("Selectivity Parameters"),
      fluidRow(
        column(
          width = 6,
          numericInput("L50_asc.pval", "L50 ascending", value = 30, min = 1)
        ),
        column(
          width = 6,
          numericInput("L95_asc.pval", "L95 ascending", value = 40, min = 1)
        )
      ),
      fluidRow(
        column(
          width = 6,
          numericInput("peak_length.pval", "Peak length", value = 45, min = 1)
        ),
        column(
          width = 6,
          numericInput("desc_sd.pval", "Descending SD", value = 100, min = 1)
        )
      )
      # actionButton(
      #   "calculate",
      #   "Calculate Population",
      #   class = "btn-primary w-100"
      # )
    ),
    # card(
    #   card_header("Population Parameters"),
    #   numericInput("R0", "R0 (Initial recruitment)", value = 1000, min = 1),
    #   numericInput("max_age", "Maximum age", value = 20, min = 1, max = 50)
    # ),
    h5(
      "Explore how sample size improves estimation of the age distribution and total mortality."
    ),
    h6(
      "Reps are how many times each sample size is taken. Max sample is the highest sample value of the profile. P-value limit is user chosen and suggest sufficient similarity in distributions is achieved. Age range to run the catch curve estimation of Z. Default values are based on age at full selectivity and maximum age."
    ),
    card(
      card_header("Distribution test options"),
      fluidRow(
        column(
          width = 4,
          #   radioButtons(
          #     inputId = "dist_test",
          #     label = "Distribution test",
          #     choices = c("DTS", "WASS", "CVM", "AD", "KS"),
          #     selected = "DTS"
          #   )
          numericInput(
            "reps.pval",
            "How many reps?",
            value = 100,
            min = 1,
            max = 1000000000
          )
        ),
        column(
          width = 4,
          numericInput(
            "maxsamp",
            "Maximum sample",
            value = 2000,
            min = 1000,
            step = 1
          )
        ),
        column(
          width = 4,
          numericInput(
            "Plim.pval",
            "P-value limit",
            value = 0.85,
            min = 0,
            max = 1,
            step = 0.01
          )
        ),
      ),
      fluidRow(
        column(
          width = 6,
          numericInput(
            "CC.sel_agemin",
            "Min age",
            value = 10,
            min = 1
          )
        ),
        column(
          width = 6,
          # numericInput(
          #   "reps.pval",
          #   "How many reps?",
          #   value = 100,
          #   min = 1,
          #   max = 1000000000
          # )
          numericInput(
            "CC.sel_agemax",
            "Max age",
            value = 5.4 / 0.2,
            min = 1
          )
        )
      ),
      actionButton(
        "calculate.pval",
        "Calculate Distribution Tests",
        class = "btn-primary w-100"
      )
    ),
    card(
      card_header(
        "Compare age distribution from chosen sample size and age range"
      ),
      numericInput(
        "sampsize",
        "Sample size",
        value = 700,
        min = 1,
        max = 1000000,
        step = 1
      )
    ),
    actionButton(
      "calculate.sampsize",
      "Show Comparison",
      class = "btn-primary w-100"
    )
  ),
  layout_columns(
    col_widths = c(4, 4, 4),
    card(
      card_header("Selectivity by Age"),
      plotlyOutput("selectivity_plot")
    ),
    card(
      card_header("Length at Age"),
      plotlyOutput("length_plot")
    ),
    card(
      card_header("Age Distribution"),
      plotlyOutput("age_plot")
    )
  ),
  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Distribution test p-value by sample size"),
      plotlyOutput("pvals_plot")
    ),
    card(
      card_header("Estimate total mortality"),
      plotlyOutput("Z_comp_plot_II")
    ),
  ),
  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header(
        "Comparison of true and sampled age distributions based on sample size"
      ),
      plotOutput("Ageprop_plot")
    ),
    card(
      card_header(
        "Comparison of true and sampled cumulative distributions based on sample size"
      ),
      plotOutput("CDF_plot")
    )
  )
)

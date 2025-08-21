## timeline Script 


library(scales)
library(lubridate)
library(ggplot2)
library(dplyr)
library(grid)     
library(ggforce)# for unit()






# setwd


print("Figure 2")

# Load data 



status_levels <- c("Science Legislation", "COVID Events",
                   "Electoral Politics" ,"Climate Policy","AI and LLMs")
status_colors <- c("#0070C0", "#00B050","#C00000","#FFC000","#CC5500")


# Define new events with dates
bottom_events <- data.frame(
  date = as.Date(c(
    '2022-11-08',  # 2022 Midterms
    '2023-06-09',  # Trump Indicted
    '2022-08-09',  # CHIPS Passed into Law
    '2022-08-16',  # IRA Passed
    '2023-01-01',  # ChatGPT hits 100 million users
    '2023-10-01',  # Start of 2024 Campaign
    '2021-12-01',  # Omicron Curve Crests
    '2021-03-01',  # COVID-19 Vaccine Rollout (Start)
    '2021-02-19',  # Rejoined Paris Climate Agreement
   # '2020-12-11',  # FDA Approves Pfizer Vaccine
    '2021-12-10',  # US Debt Ceiling Crisis
    '2021-08-09'   # Federal Climate Change Report
  )),
  event = c(
    "2022 Midterms", 
    "Trump Indicted", 
    "CHIPS Passed into Law", 
    "IRA Passed", 
    "ChatGPT Hits\n 100 Million Users", 
    "Start of 2024 Campaign", 
    "Omicron\n Curve Crests",
    "COVID-19\n Vaccine Rollout", 
    "Rejoined Paris\n Climate Agreement",
#    "FDA Approves Pfizer Vaccine", 
    "US Debt Ceiling Crisis", 
    "Federal Climate\n Change Report"
  ),
  y=-0.2
)%>%
  mutate(
    Type = case_when(
      event %in% c("2022 Midterms","Trump Indicted","Start of 2024 Campaign","US Debt Ceiling Crisis") ~ "Electoral Politics",
      event %in% c("CHIPS Passed into Law","IRA Passed") ~ "Science Legislation",
      event %in% c("Omicron\n Curve Crests","COVID-19\n Vaccine Rollout","FDA Approves Pfizer Vaccine ") ~ "COVID Events",
      event %in% c("Rejoined Paris\n Climate Agreement","Federal Climate\n Change Report") ~ "Climate Policy",
      event == "ChatGPT Hits\n 100 Million Users" ~ "AI and LLMs",
      TRUE ~ NA_character_
    ),
    Type = factor(Type, levels = status_levels),
    # y = negative integer row per Type * 0.3
    y = -as.integer(Type) * 0.3
  )

bottom_events$y[4] <- -0.9


# Combine the original data frame with the new events


# Sort by the Date column to ensure chronological order










# if you want fancier circles, you could also use library(ggforce)

# ─── 1) Define your key dates and “studies” ─────────────────────────────

study_dates <- data.frame(
  study = c("Study 1", "Study 2", "Study 3"),
  date  = as.Date(c("2022-06-01", "2023-06-01", "2023-12-01"))
)
# ─── after you’ve defined study_dates ────────────────────────────────────
# turn Study dates into centers for ellipses
study_ellipses <- study_dates %>%
  mutate(
    # x0 must be numeric because dates are mapped under the hood to days
    x0 = as.numeric(date),
    y0 = 1.8,        # same y you use for geom_text()
    a  = 80,         # half‐width of the ellipse in days
    b  = 0.15,       # half‐height of the ellipse in y‐units
    angle = 0
  )



clusters <- data.frame(
  xmin  = as.Date(c("2022-03-01", "2022-03-01")),
  xmax  = as.Date(c("2024-03-15", "2024-03-15")),
  ymin  = c(0.9,      0.15),
  ymax  = c(1.5,      0.7),
  label = c("Types of\nStudies Tested", 
            "Robustness\nChecks")
)


cluster1_df <- data.frame(
  x     =  as.Date(c("2022-06-01","2022-12-01", "2023-06-01",
                     "2023-09-01", "2023-12-01"
                     )),
  y     = c(rep( 1.3, 5)),
  label = c(
    "Carbon soils", " →", 
    "Carbon soils\nCarbon sequestration","→",
    "Carbon soils\nQuantum Computers\nFootball Helmets"
  
  )
)

cluster2_df <- data.frame(
  x     = study_dates$date[2],
  y     = clusters$ymax[2] - 0.2,
  label = c(
    "Attention checks\nSwapping Error\nDon't Know Option"
  )
)




# ───  Build the plot ───────────────────────────────────────────────────

g<-ggplot() +

# ─── then, just before your geom_text() that draws the “Study 1/2/3” ──────
  annotate("rect",
           xmin = as.Date("2021-01-01"),
           xmax = study_dates$date[1],
           ymin = -Inf, ymax = Inf,
           fill  = "#ADD8E6", alpha = 0.2) +
  annotate("rect",
           xmin = study_dates$date[3],
           xmax = as.Date("2024-06-01"),
           ymin = -Inf, ymax = Inf,
           fill  = "#ADD8E6", alpha = 0.2) +

  # ── horizontal timeline with arrow
  geom_segment(
    aes(x = as.Date("2021-01-01"), 
        xend = as.Date("2024-06-01"), 
        y = 0, yend = 0),
    arrow = arrow(length = unit(0.3, "cm"), ends = "last"),
    size  = 0.5,
    color = "black"
  ) +
  
  # ── vertical dashed lines at each study date
  geom_vline(
    data = study_dates,
    aes(xintercept = date),
    linetype = "dashed",
    color    = "black",
    size     = 0.4
  ) +
  
  # ── study labels just above the line
  geom_label(
    data    = study_dates,
    aes(x = date, y = 1.8, label = study),
    fontface = "bold",
    vjust    = 0,
    size     = 5
  ) +
  
  # ── red boxes around your two clusters
  annotate(
    "rect",
    xmin = clusters$xmin[1], xmax = clusters$xmax[1],
    ymin = clusters$ymin[1], ymax = clusters$ymax[1],
    fill  = "white",
    color = "red",
    size  = 0.5
  ) +
  annotate(
    "rect",
    xmin = clusters$xmin[2], xmax = clusters$xmax[2],
    ymin = clusters$ymin[2], ymax = clusters$ymax[2],
    fill  = "white",
    color = "red",
    size  = 0.5
  ) +
  geom_text(data = cluster1_df,
            aes(x = x, y = y, label = label),
            size = 5, lineheight = 0.9, hjust = 0.5) +
  geom_text(data = cluster2_df,
            aes(x = x, y = y, label = label),
            size = 5, lineheight = 0.9, hjust = 0.5) +
  # ── labels for those clusters
  geom_text(
    data = clusters,
    aes(
      x = xmin + (xmax - xmin) / 2,
      y = ymax + 0.1,
      label = label
    ),
    color   = "red",
    fontface = "bold",
    size     = 4.5,
    lineheight = 0.9
  ) +
  
  # ── bottom events (points + angled labels)
  # lollipop sticks colored by Type
  geom_segment(data = bottom_events,
               aes(x = date, xend = date, 
                   y = 0, yend = y, color = Type),
               size = 0.3,show.legend = F) +
  # points
  geom_point(data = bottom_events,
             aes(x = date, y = 0, color = Type),
             size = 3) +
  # nicer labels
  geom_label(data = bottom_events,
             aes(x = date, y = y, label = event,color=Type),
             angle         = 45,
             hjust         = 1,
             label.padding = unit(0.15, "lines"),
             label.r       = unit(0.2, "lines"),
             size = 5,
             fill = "white",
             show.legend = F) +
  
  # manual scales
  scale_color_manual(name = "Event Type",
                     values = status_colors,
                     breaks = status_levels) +
  scale_fill_manual(name = "Event Type",
                    values = status_colors,
                    breaks = status_levels) +
  scale_y_continuous(
    limits = c(-2.1, 1.9),
    breaks = seq(-2, 1, 0.5)
  ) +
  # ── clean up the axes and theme
  scale_x_date(
    limits = c(as.Date("2020-11-15"), 
               as.Date("2024-06-01")),
    date_breaks = "6 months",
    date_labels = "%b\n%Y"
  ) +
  theme_classic() +
  theme(
    axis.line.y      = element_blank(),
    axis.ticks.y     = element_blank(),
    axis.text.y      = element_blank(),
    axis.title       = element_blank(),
    axis.text = element_text(size = 25),
    panel.grid       = element_blank(),
    legend.text = element_text(size = 25),
    legend.title = element_blank(),
    legend.position = "bottom",
  )

pdf("Plots/timeline.plot.pdf",
    width = 16, height = 9)
plot(g)
dev.off()


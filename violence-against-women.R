# ------------------------------------------------------------------------------------- #
# violence-against-women                                                                #
# DATA STUDY ABOUT VIOLENCE AGAINST WOMEN IN DIFFERENT COUNTRIES                        #
# Author: Dennis Gluesenkamp                                                            #
# ------------------------------------------------------------------------------------- #


# ---- Libraries/packages used in this script ----
library(dplyr)                            # data manipulation focused on data frames
library(ggplot2)                          # general plotting engine
library(ggrepel)                          # labels with allocation line
library(showtext)                         # usage of Google fonts


# ---- Function to get country groups ----
getCountriesOf <- function(organization) {
  if(base::tolower(organization) == 'eu'){                        # European Union
    org_df <- c('AT', 'BE', 'BG', 'CY', 'CZ',
                'DE', 'DK', 'EE', 'ES', 'FI',
                'FR', 'GB', 'GR', 'HR', 'HU',
                'IE', 'IT', 'LT', 'LU', 'LV',
                'MT', 'NL', 'PL', 'PT', 'RO',
                'SE', 'SI', 'SK')
  } else if (base::tolower(organization) == 'eu_neighborhood') {  # EU neighbors
    org_df <- c('AD', 'AL', 'AT', 'BA', 'BE',
                'BG', 'CH', 'CY', 'CZ', 'DE',
                'DK', 'EE', 'ES', 'FI', 'FR',
                'GB', 'GR', 'HR', 'HU', 'IE',
                'IT', 'LI', 'LT', 'LU', 'LV',
                'ME', 'MK', 'MT', 'NL', 'NO',
                'PL', 'PT', 'RO', 'RS', 'SE',
                'SI', 'SK', 'XK')
  } else if (base::tolower(organization) == 'oecd') {             # OECD members
    org_df <- c('AT', 'AU', 'BE', 'CA', 'CH',
                'CL', 'CZ', 'DE', 'DK', 'EE',
                'ES', 'FI', 'FR', 'GB', 'GR',
                'HU', 'IE', 'IL', 'IS', 'IT',
                'JP', 'KR', 'LT', 'LU', 'LV',
                'MX', 'NL', 'NO', 'NZ', 'PL',
                'PT', 'SE', 'SI', 'SK', 'TR',
                'US')
  }
  return(org_df)
}


# ---- Style configuration ----
font_add_google(name = 'Fjalla One', family = 'Fjalla One')
font_add_google(name = 'Lora', family = 'Lora')
showtext_auto()
fontTitle = 'Fjalla One'
fontText  = 'Lora'
fontSize  = 12

cBackground = '#191718'
cShadow     = '#59504b'
cTitle      = '#c38d72'
cText       = '#d9c2b6'
cScatter    = c('#db5461', '#2e3b94') # first normal scatter, second highlight scatter
cHue        = c('#eaddd7', '#e9beaf', '#e69e8e', '#e27b74', '#db5461', '#665E62')
cGradual    = c('#f8f3f1', '#920000')

# Note: The following specifications are aligned to the later used ggsave statement which
# is optimized for a squared output.
themeViolence <- theme(
  plot.background  = element_rect(fill       = cBackground),
  panel.background = element_rect(fill       = cBackground),
  plot.title       = element_text(size       = 1.75 * fontSize,
                                  face       = 'plain',
                                  family     = fontTitle,
                                  color      = cTitle,
                                  hjust      = 0.0,
                                  margin     = unit(c(1.0, 0.0, 2.0, 0.0), 'pt')),
  plot.subtitle    = element_text(size       = 1.25 * fontSize,
                                  face       = 'italic',
                                  family     = fontText,
                                  color      = cTitle,
                                  hjust      = 0.0,
                                  margin     = unit(c(0.0, 0.0, -1.5, 0.0), 'pt')),
  plot.caption     = element_text(size       = 0.70 * fontSize,
                                  face       = 'plain',
                                  family     = fontText,
                                  color      = cText,
                                  hjust      = 0.0,
                                  lineheight = 0.3,
                                  margin     = unit(c(-0.75, 0.0, -0.75, 0.0), 'pt')),
  axis.title       = element_text(size       = 1.15 * fontSize,
                                  face       = 'bold',
                                  family     = fontText,
                                  color      = cText,
                                  hjust      = 0.5),
  axis.text        = element_text(size       = 1.05 * fontSize,
                                  face       = 'plain',
                                  family     = fontText,
                                  color      = cText,
                                  hjust      = 0.5),
  
  panel.border       = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  axis.ticks.x       = element_blank(),
  axis.ticks.y       = element_blank(),
  axis.line.x.top    = element_blank(),
  axis.line.x.bottom = element_blank(),
  axis.line.y.left   = element_blank(),
  axis.line.y.right  = element_blank(),
  
  legend.position   = 'right',
  legend.background = element_blank(),
  legend.title      = element_blank(),
  legend.text       = element_text(size   = 1.00 * fontSize,
                                   face   = 'plain',
                                   family = fontText,
                                   color  = cTitle,
                                   hjust  = 0.0),
  legend.key        = element_blank(),
  legend.key.size   = unit(2, "mm"),
  legend.margin     = margin(0.0, 0.0, 0.0, -15.0),
  
  complete = FALSE
)


# ---- Country ISO-code ----
iso <- ISOcodes::ISO_3166_1 %>%
  dplyr::add_row(Alpha_2 = 'XK',
                 Alpha_3 = 'XKX',
                 Name = 'Kosovo') %>%
  dplyr::mutate(Name = case_when(
    Name == 'Czechia'                ~ 'Czech Republic',
    Name == 'Macedonia, Republic of' ~ 'Macedonia',
    TRUE                             ~ Name
  )) %>%
  dplyr::left_join(dplyr::mutate(ggplot2::map_data('world'),
                                 region = case_when(
                                   region == 'UK'   ~ 'United Kingdom',
                                   TRUE             ~ region
                                 )),
                   by = c('Name' = 'region')) %>%
  dplyr::select(Alpha_2, Alpha_3, Name, long, lat, group) %>%
  dplyr::rename(code2 = Alpha_2,
                code3 = Alpha_3,
                country = Name,
                mapgroup = group)

oecd <- iso %>%
  dplyr::filter(code3 %in% c('AUS', 'AUT', 'BEL', 'CAN', 'CHE', 'DEU', 'DNK', 'ESP', 'FIN', 'FRA',
                             'GBR', 'GRC', 'IRE', 'ISL', 'ITA', 'JPN', 'KOR', 'LUX', 'MEX', 'NLD',
                             'NOR', 'NZL', 'PRT', 'SWE', 'TUR', 'USA'))


# ---- Attitude towards violence against women ----
# The percentage of women who agree that a husband/partner is justified in beating his
# wife/partner under certain circumstances
# Source: OECD (2019), Violence against women (indicator). doi: 10.1787/f1eb4876-en
# (Accessed on 01 October 2019)
attitudeviolence <- utils::read.csv('dat/violence.csv', sep = ',', stringsAsFactors = FALSE) %>%
  dplyr::filter(REG == 'ALL' & INC == 'AIC' & VAR == 'RP_1_1') %>%
  dplyr::select(LOCATION, Time, Value) %>%
  dplyr::rename(code3 = LOCATION,
                year = Time,
                attitudeviolence = Value) %>%
  dplyr::arrange(year, code3) %>%
  dplyr::select(code3, year, attitudeviolence) %>%
  stats::reshape(timevar = 'year', idvar = 'code3', direction = 'wide') %>%
  dplyr::rename(attitudeviolence = attitudeviolence.2014)
# Note: There are several measurement times in the original data set. However, only the
# most recent survey from 2014 is used for this evaluation.


# ---- Prevalence of violence in the lifetime
# The percentage of women who have experienced physical and/or sexual violence from an
# intimate partner at some time in their life
# Source: OECD (2019), Violence against women (indicator). doi: 10.1787/f1eb4876-en
# (Accessed on 01 October 2019)
prevalenceviolence <- utils::read.csv('dat/violence.csv', sep = ',', stringsAsFactors = FALSE) %>%
  dplyr::filter(REG == 'ALL' & INC == 'AIC' & VAR == 'RP_1_2') %>%
  dplyr::select(LOCATION, Time, Value) %>%
  dplyr::rename(code3 = LOCATION,
                year = Time,
                prevalenceviolence = Value) %>%
  dplyr::arrange(year, code3) %>%
  dplyr::select(code3, year, prevalenceviolence) %>%
  stats::reshape(timevar = 'year', idvar = 'code3', direction = 'wide') %>%
  dplyr::rename(prevalenceviolence = prevalenceviolence.2014)
# Note: There are several measurement times in the original data set. However, only the
# most recent survey from 2014 is used for this evaluation.


# ---- Creation of pooling data frame ----
df <- iso %>%
  dplyr::select(code3) %>%
  base::unique() %>%
  dplyr::left_join(attitudeviolence, by = 'code3') %>%
  dplyr::left_join(prevalenceviolence, by = 'code3')


# ---- Creation of data frame and viz for violence in the EU ----
df_euviolence <- iso %>%
  dplyr::filter(code2 %in% getCountriesOf('eu')) %>%
  dplyr::left_join(df, by = 'code3') %>%
  dplyr::select(code2, code3, country, long, lat, mapgroup,
                prevalenceviolence) %>%
  dplyr::mutate(category = case_when(              # Introduce categories for clearer viz
    prevalenceviolence <= 0.15 ~ 'below 16%',
    prevalenceviolence <= 0.20 ~ '16% to 20%',
    prevalenceviolence <= 0.25 ~ '21% to 25%',
    prevalenceviolence <= 0.30 ~ '26% to 30%',
    prevalenceviolence > 0.30  ~ 'above 30%',
    TRUE                       ~ 'no value'
  ))

# Factorize violence categories
df_euviolence$category <- factor(df_euviolence$category,
                                 levels = c('below 16%',
                                            '16% to 20%',
                                            '21% to 25%',
                                            '26% to 30%',
                                            'above 30%',
                                            'no value'))

# Create supporting data frame comprising EU neighbors
df_euviolence_support <- iso %>%
  dplyr::filter(code2 %in% getCountriesOf('eu_neighborhood')) %>%
  dplyr::filter(lat < 72)

# Drawing chart
plotViolenceEU <- ggplot() +
  geom_polygon(data  = df_euviolence_support,
               color = cBackground,
               fill  = cShadow,
               alpha = 0.2,
               size  = 0.1,
               aes(x     = long,
                   y     = lat,
                   group = mapgroup)) +
  geom_polygon(data  = df_euviolence,
               color = cBackground,
               size  = 0.25,
               aes(x     = long,
                   y     = lat,
                   group = mapgroup,
                   fill  = category)) +
  scale_fill_manual(values = c('#eaddd7', '#e9beaf', '#e69e8e', '#e27b74', '#db5461', '#665E62')) +
  labs(title    = 'Women experience domestic violence across the EU',
       subtitle = 'Share of women suffered violence during lifetime by partners',
       x        = NULL,
       y        = NULL,
       caption  = 'Data source: OECD (2019), Violence against women (indicator). doi: 10.1787/f1eb4876-en\n(Accessed on 01 October 2019)',
       tag      = NULL) +
  themeViolence +
  theme( # Alternate specific elements in the standard theme
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

# Export chart
ggsave(
  filename = 'out/violence_eu.png',
  plot = plotViolenceEU,
  type = 'cairo',
  width = 55,
  height = 55,
  units = 'mm'
)


# ---- Creation of data frame and viz for violence in OECD countries ----
df_oecdviolence <- iso %>%
  dplyr::filter(code2 %in% getCountriesOf('oecd')) %>%
  dplyr::left_join(df, by = 'code3') %>%
  dplyr::select(code2, code3, country,
                prevalenceviolence) %>%
  base::unique() %>%
  dplyr::mutate(category = case_when(              # Introduce categories for clearer viz
    prevalenceviolence <= 0.05 ~ 'below 5%',
    prevalenceviolence <= 0.10 ~ '6% to 10%',
    prevalenceviolence <= 0.15 ~ '11% to 15%',
    prevalenceviolence <= 0.20 ~ '16% to 20%',
    prevalenceviolence <= 0.25 ~ '21% to 25%',
    prevalenceviolence <= 0.30 ~ '26% to 30%',
    prevalenceviolence <= 0.35 ~ '31% to 35%',
    prevalenceviolence <= 0.40 ~ '36% to 40%',
    prevalenceviolence > 0.40  ~ 'above 40%',
    TRUE                       ~ 'no value'
  )) %>%
  dplyr::filter(!is.na(prevalenceviolence)) %>%
  dplyr::mutate(flag = paste0('img/flags/', tolower(code2), '.svg'))

# Reorder countries prevalence value 
df_oecdviolence$code3 <- factor(df_oecdviolence$code3,
                                levels = df_oecdviolence$code3[base::order(-df_oecdviolence$prevalenceviolence)])

# Drawing chart
plotViolenceOECD <- ggplot() +
  geom_segment(data  = df_oecdviolence,
               size  = 0.15,
               alpha = 0.65,
               aes(x     = code3,
                   xend  = code3,
                   y     = 0.0,
                   yend  = prevalenceviolence,
                   color = prevalenceviolence)) +
  geom_point(data = df_oecdviolence,
             aes(x     = code3,
                 y     = prevalenceviolence,
                 fill  = prevalenceviolence,
                 color = prevalenceviolence)) +
  geom_text(data  = df_oecdviolence,
            color = cBackground,
            size  = 4.0,
            aes(x      = code3,
                y      = prevalenceviolence,
                label  = sprintf("%1.0f", 100*prevalenceviolence),
                hjust  = 0.5,
                family = fontTitle)) +
  geom_text(data  = df_oecdviolence,
            color = cText,
            aes(x      = code3,
                y      = 0,
                label  = country,
                angle  = 90,
                hjust  = 0.0,
                family = fontTitle)) +
  scale_color_gradient(low = cGradual[1], high = cGradual[2]) +
  scale_fill_gradient(low = cGradual[1], high = cGradual[2]) +
  scale_y_continuous(breaks   = c(0.0, 0.1, 0.2, 0.3, 0.4),
                     labels   = c('0%', '10%', '20%', '30%', '40%'),
                     position = 'right') +
  labs(title    = 'Domestic violence is common in OECD countries',
       subtitle = 'Share of women suffered violence during lifetime by partners',
       x        = NULL,
       y        = NULL,
       caption  = 'Data source: OECD (2019), Violence against women (indicator). doi: 10.1787/f1eb4876-en\n(Accessed on 01 October 2019); No value for Republic of Korea and Israel',
       tag      = NULL) +
  themeViolence +
  theme( # Alternate specific elements in the standard theme
    axis.title         = element_text(color    = cTitle),
    axis.text          = element_text(color    = cTitle),
    axis.text.x        = element_blank(),
    panel.grid.major.y = element_line(color    = cTitle,
                                      size     = 0.1,
                                      linetype = 'dotted'),
    legend.position    = 'none'
  )

# Export chart
ggsave(
  filename = 'out/violence_oecd.png',
  plot = plotViolenceOECD,
  type = 'cairo',
  width = 55,
  height = 55,
  units = 'mm'
)


# ---- Relationship between attitude towards and prevalence of violence ----
df_relation <- iso %>%
  dplyr::left_join(df, by = 'code3') %>%
  dplyr::select(code2, code3, country,
                attitudeviolence,
                prevalenceviolence) %>%
  base::unique() %>%
  dplyr::mutate(highlight = case_when( # Highlight specific countries
    code2 %in% c('TL', 'ET', 'CD', 'JO', 'JM', 'CA', 'DE',
                 'CN', 'IN', 'US', 'DO', 'BO', 'JP', 'RU') ~ TRUE,
    TRUE                                                   ~ FALSE
  )) %>%
  dplyr::mutate(country = case_when(   # Abbreviate long country names
    code2 == 'TL' ~ 'East Timor',
    code2 == 'CD' ~ 'DR Congo',
    code2 == 'BO' ~ 'Bolivia',
    code2 == 'DO' ~ 'Dominican Rep.',
    code2 == 'RU' ~ 'Russia',
    TRUE          ~ country
  ))
# Note: The selection for highlighted countries is purely subjective; Here, some
# major industrial nations and outliers are choosen

# Drawing chart
plotRelationPrevAtt <- ggplot() +
  geom_line(data   = df_relation,
            stat   = 'smooth',
            method = 'loess',
            span   = 1.5,
            alpha  = 0.075,
            color  = cTitle,
            aes(x = attitudeviolence,
                y = prevalenceviolence)) +
  geom_ribbon(data   = df_relation,
              stat   = 'smooth',
              method = 'loess',
              span   = 1.5,
              alpha  = 0.035,
              fill   = cText,
              aes(x = attitudeviolence,
                  y = prevalenceviolence)) +
  geom_point(data  = df_relation,
             alpha = 0.45,
             size  = 0.75,
             aes(x     = attitudeviolence,
                 y     = prevalenceviolence,
                 color = highlight)) +
  geom_text_repel(data          = dplyr::filter(df_relation, code2 %in% c('TL', 'JM', 'US',
                                                                          'DO', 'RU')),
                  family        = fontTitle,
                  color         = cTitle,
                  hjust         = 0.5,
                  nudge_x       = 0.07,
                  nudge_y       = 0.07,
                  segment.size  = 0.1,
                  segment.color = cTitle,
                  aes(x = attitudeviolence,
                      y = prevalenceviolence,
                      label = country)) +
  geom_text_repel(data          = dplyr::filter(df_relation, code2 %in% c('BO', 'ET', 'CD',
                                                                          'JO', 'DE', 'CN',
                                                                          'IN', 'CA', 'JP')),
                  family        = fontTitle,
                  color         = cTitle,
                  hjust         = 0.5,
                  nudge_x       = 0.07,
                  nudge_y       = -0.07,
                  segment.size  = 0.1,
                  segment.color = cTitle,
                  aes(x = attitudeviolence,
                      y = prevalenceviolence,
                      label = country)) +
  scale_color_manual(values = c(cScatter[1], cScatter[2])) +
  scale_size(range = c(0, 5)) +
  scale_x_continuous(limits = c(0.00, 0.95),
                     breaks = c(0.0, 0.2, 0.4, 0.6, 0.8),
                     labels = c('0%', '20%', '40%', '60%', '80%')) +
  scale_y_continuous(limits = c(0.00, 0.75),
                     breaks = c(0.0, 0.2, 0.4, 0.6, 0.8),
                     labels = c('0%', '20%', '40%', '60%', '80%')) +
  labs(title    = 'Violence prevalence vs. attitude across countries',
       subtitle = 'Consent to violence and experiences of violence partly coincide',
       x        = 'Women agreeing violence in partnership can be justified',
       y        = 'Share of women suffered violence by partners',
       caption  = 'Data source: OECD (2019), Violence against women (indicator). doi: 10.1787/f1eb4876-en\n(Accessed on 01 October 2019); All countries fully available with regard to data points',
       tag      = NULL) +
  themeViolence +
  theme( # Alternate specific elements in the standard theme
    plot.title         = element_text(hjust = 1.0),
    plot.subtitle      = element_text(hjust  = 1.0,
                                      margin = unit(c(0.0, 0.0, 1.0, 0.0), "pt")),
    plot.caption       = element_text(margin = unit(c(+2.0, 0.0, -0.75, 0.0), "pt")),
    axis.title         = element_text(size  = 1.00 * fontSize,
                                      color = cTitle),
    axis.text          = element_text(size  = 0.85 * fontSize,
                                      color = cTitle),
    panel.grid.major.x = element_line(color    = cTitle,
                                      size     = 0.05,
                                      linetype = 'solid'),
    panel.grid.major.y = element_line(color    = cTitle,
                                      size     = 0.05,
                                      linetype = 'solid'),
    legend.position    = 'none'
  )

# Export chart
ggsave(
  filename = 'out/relation_prevatt.png',
  plot = plotRelationPrevAtt,
  type = 'cairo',
  width = 55,
  height = 55,
  units = 'mm'
)



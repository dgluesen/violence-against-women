# ------------------------------------------------------------------------------------- #
# violence-against-women                                                                #
# DATA STUDY ABOUT VIOLENCE AGAINST WOMEN IN DIFFERENT COUNTRIES                        #
# Author: Dennis Gluesenkamp                                                            #
# ------------------------------------------------------------------------------------- #


# ---- Libraries/packages used in this script ----
library(dplyr)                            # data manipulation focused on data frames
library(ggplot2)                          # general plotting engine
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
  axis.text.x        = element_blank(),
  axis.text.y        = element_blank(),
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
  themeViolence

# Export chart
ggsave(
  filename = 'out/violence_eu.png',
  plot = plotViolenceEU,
  type = 'cairo',
  width = 55,
  height = 55,
  units = 'mm'
)

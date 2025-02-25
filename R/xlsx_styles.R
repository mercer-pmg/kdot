#' List of XLSX workbook styles
#'
#' @return A list.
#' @export
#'
#'
xlsx_styles <- function(){

  workbook_styles <- list(

    fill_dark_rasp   = openxlsx::createStyle(fgFill = "#820361"),
    fill_dark_spring = openxlsx::createStyle(fgFill = "#00826B"),
    fill_light_iris  = openxlsx::createStyle(fgFill = "#CFCCFF"),
    fill_pale_iris   = openxlsx::createStyle(fgFill = "#E9E9FF"),

    text_dark_rasp   = openxlsx::createStyle(fontColour = "#820361"),
    text_rasp        = openxlsx::createStyle(fontColour = "#C00686"),
    text_dark_spring = openxlsx::createStyle(fontColour = "#00826B"),

    font_ibm_plex_sans10 = openxlsx::createStyle(
      fontName     = "IBM Plex Sans",
      fontSize = 11),

    font_ibm_plex_sans10 = openxlsx::createStyle(
      fontName     = "Merriweather",
      fontSize = 10),

    nav_banner = openxlsx::createStyle(
      fontName       = "Merriweather",
      fontSize       = 20,
      fontColour     = "white",
      textDecoration = "bold",
      valign         = "center"),

    nav_section = openxlsx::createStyle(
      fontName = "Merriweather",
      fontSize = 11,
      fontColour = "#FFFFFF",
      textDecoration = "bold"),

    nav_subsection = openxlsx::createStyle(
      fontName = "Merriweather",
      fontSize = 10,
      textDecoration = c("bold", "italic"),
      fgFill = "#D1D1D6"),

    nav_entry = openxlsx::createStyle(
      fontName = "IBM Plex Sans",
      fontSize = 10,
      fgFill   = "#D1D6D6",
      indent   = 1),

    quilt_title = openxlsx::createStyle(
      fontName       = "Merriweather",
      fontSize       = 16,
      textDecoration = "bold"),

    quilt_header = openxlsx::createStyle(
      fontName = "Merriweather",
      fontSize = 10,
      wrapText = TRUE,
      valign = "bottom",
      halign = "center",
      numFmt = "TEXT"),

    quilt_body = openxlsx::createStyle(
      fontName       = "IBM Plex Sans",
      fontSize       = 10,
      fontColour     = "white",
      textDecoration = "bold",
      border         = "TopBottomLeftRight",
      borderColour   = "white",
      valign         = "center",
      halign         = "center",
      wrapText       = TRUE),

    quilt_fills = list(
      fill_1 = openxlsx::createStyle(fgFill = "#E3761D"),
      fill_2 = openxlsx::createStyle(fgFill = "#49287F"),
      fill_3 = openxlsx::createStyle(fgFill = "#8C8E90"),
      fill_4 = openxlsx::createStyle(fgFill = "#633E07"),
      fill_5 = openxlsx::createStyle(fgFill = "#3D90F7"),
      fill_6 = openxlsx::createStyle(fgFill = "#7EA134"),
      fill_7 = openxlsx::createStyle(fgFill = "#71CAEA"),
      fill_8 = openxlsx::createStyle(fgFill = "#BC99FD"),
      fill_9 = openxlsx::createStyle(fgFill = "#1C4489")),

    quilt_summary_header = openxlsx::createStyle(
      fontName       = "Merriweather",
      fontSize       = 12,
      halign         = "center",
      textDecoration = "bold",
      border         = "bottom",
      borderStyle    = "medium"
    )


  )

  return(workbook_styles)

}

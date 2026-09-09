# generate allometric reference table with clickable link.
allometric_ref <- data.frame(
  Method = c(
    "Komiyama2005_Mangrove", "Chave2005_Mangrove",
    "Manuri2014_Peat_D", "Manuri2014_Peat_DWD", "Manuri2014_Peat_DH", "Manuri2014_Peat_DWDH",
    "Widyasari2010_Peat", "Istomo2002_Peat_Power", "Istomo2006_Peat_Polynomial",
    "Manuri2017_DG1", "Manuri2017_DGH1", "Manuri2017_DG2_W", "Manuri2017_DG2_M", "Manuri2017_DG2_E",
    "Basuki2009_DWD", "Ketterings2001", "Chave2014_DWDH", "Chave2005_Moist_DWD", "Brown1997_Moist"
  ),
  Type = c(
    "Mangrove", "Mangrove",
    "Peat", "Peat", "Peat", "Peat", "Peat", "Peat", "Peat",
    "Mineral", "Mineral", "Mineral", "Mineral", "Mineral",
    "Mineral", "Mineral", "Mineral", "Mineral", "Mineral"
  ),
  Ecosystem = c(
    "Mangrove, Southeast Asia",
    "General mangrove; calibration primarily Americas",
    "Indonesian tropical peat-swamp forest; D only",
    "Indonesian tropical peat-swamp forest; D + WD",
    "Indonesian tropical peat-swamp forest; D + H",
    "Indonesian tropical peat-swamp forest; D + WD + H",
    "Burned peat-swamp forest, Merang, South Sumatra",
    "Indonesian peat-swamp forest",
    "Peat-swamp forest, Riau, Sumatra",
    "Natural lowland forests, Indo-Malay region; D + WD",
    "Natural lowland forests, Indo-Malay region; D + WD + H",
    "Western Indo-Malay; for Indonesia mainly Sumatra-Kalimantan",
    "Middle region; Indonesia FRL applies to Java, Bali, Nusa Tenggara, Sulawesi and Maluku",
    "Eastern region; Indonesia FRL applies to Papua",
    "Lowland mixed dipterocarp forest, East Kalimantan",
    "Mixed secondary forest, Sumatra/Jambi",
    "Pantropical tropical forest; D + WD + H",
    "Tropical moist forest; D + WD",
    "Tropical moist broadleaf forest"
  ),
  Equation = c(
    "AGB = 0.251 · ρ · D^2.46",
    "AGB = ρ · exp(-1.349 + 1.980·lnD + 0.207·lnD² - 0.0281·lnD³)",
    "AGB = 0.136 · D^2.513",
    "AGB = 0.242 · D^2.473 · WD^0.736",
    "AGB = 0.081 · D^2.049 · H^0.672",
    "AGB = 0.150 · D^2.095 · WD^0.664 · H^0.552",
    "AGB = 0.153108 · D^2.40",
    "AGB = 0.1886 · D^2.3702",
    "AGB = 0.0145·D³ - 0.4659·D² + 30.64·D - 263.32",
    "AGB = 0.171 · D^2.564 · G^0.909",
    "AGB = 0.088 · (D²·G·H)^0.954",
    "AGB = 0.167 · D^2.560 · G^0.889",
    "AGB = 0.151 · D^2.560 · G^0.889",
    "AGB = 0.206 · D^2.560 · G^0.889",
    "ln(AGB) = -0.744 + 2.188·lnD + 0.832·lnWD",
    "AGB = 0.11 · ρ · D^2.62",
    "AGB = 0.0673 · (ρ·D²·H)^0.976",
    "AGB = ρ · exp(-1.499 + 2.148·lnD + 0.207·lnD² - 0.0281·lnD³)",
    "AGB = exp(-2.134 + 2.530·lnD), equivalent to 0.118·D^2.53"
  ),
  `Suggested status` = c(
    "Preferred mangrove", "Alternative mangrove",
    "Preferred peat candidate when only D is available",
    "Preferred peat candidate when WD available",
    "Alternative peat model when H available but WD unavailable",
    "Preferred peat candidate with reliable WD + H",
    "Local alternative", "Local peat model", "Local peat model",
    "Preferred lowland model when H unavailable",
    "Preferred lowland model when H is reliable",
    "Indonesia FRL relevant", "Indonesia FRL relevant", "Indonesia FRL relevant",
    "Local alternative", "Local alternative",
    "Preferred pantropical when H is reliable",
    "Alternative pantropical", "Alternative"
  ),
  Reference = local({
    # Small helper so each repeated citation (Manuri 2014 x4, Manuri 2017 x5,
    # Chave 2005 x2) only has its URL typed once, rather than retyped and
    # risking a copy-paste mismatch across rows.
    ref_link <- function(label, url) sprintf('<a href="%s" target="_blank">%s</a>', url, label)

    komiyama2005 <- ref_link("Komiyama et al. 2005", "https://www.cambridge.org/core/journals/journal-of-tropical-ecology/article/abs/common-allometric-equations-for-estimating-the-tree-weight-of-mangroves/6067C26CECE5B0EF18A319B8DB89B771")
    chave2005    <- ref_link("Chave et al. 2005", "https://pubmed.ncbi.nlm.nih.gov/15971085")
    manuri2014   <- ref_link("Manuri et al. 2014", "https://www.sciencedirect.com/science/article/pii/S0378112714005209")
    widyasari2010 <- ref_link("Widyasari et al. 2010", "https://repository.ipb.ac.id/handle/123456789/71517")
    istomo2002   <- ref_link("Istomo 2002, as compiled by Anitha et al. 2015 Springer", "https://link.springer.com/article/10.1007/s13595-015-0507-4")
    manuri2017   <- ref_link("Manuri et al. 2017", "https://doi.org/10.1007%2Fs13595-017-0618-1")
    basuki2009   <- ref_link("Basuki et al. 2009", "https://www.sciencedirect.com/science/article/pii/S0378112709000516")
    ketterings2001 <- ref_link("Ketterings et al. 2001", "https://www.sciencedirect.com/science/article/pii/S0378112700004606")
    chave2014    <- ref_link("Chave et al. 2014", "https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.12629")
    brown1997    <- ref_link("Brown 1997", "https://www.fao.org/4/w4095e/w4095e00.htm")

    c(
      komiyama2005,
      chave2005,
      manuri2014, manuri2014, manuri2014, manuri2014,
      widyasari2010,
      istomo2002,
      "Istomo 2006",  # no source URL provided yet - plain text until one is
      manuri2017, manuri2017, manuri2017, manuri2017, manuri2017,
      basuki2009,
      ketterings2001,
      chave2014,
      chave2005,
      brown1997
    )
  }),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# allometric_choices: named vector ("label shown to user" = "internal method code") 
allometric_choices <- stats::setNames(allometric_ref$Method, allometric_ref$Method)

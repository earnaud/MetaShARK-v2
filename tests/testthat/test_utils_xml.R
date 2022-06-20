# path <- "~/Documents/Test_data/EAL/2021-04-23_Kakila_emldp/2021-04-23_Kakila/eml/Kakila database of marine mammal observation data in the AGOA sanctuary - French Antilles.xml"

# read ----
{
  EML <- xmlParse(path)
  # save original ----
  {
    EML.save <- xmlClone(EML)
    # get Node ----
    {
      eml.root <- xmlRoot(EML)
      # get as list ----
      {
        # Get file as a list
        eml <- xmlToList(eml.root)
        # Get root node
        eml.back <- xmlClone(eml.root) |>
          removeChildren("dataset")
        # === edit `eml` here === #

        # rebuild using `eml` list
        eml.back <- listToXML(eml.back, eml)
      }
      EML.new <- xmlDoc(eml.back)
    }
    identicalXML(EML.save, EML.new)
  }
  saveXML(EML.new, file = "~/Desktop/eml_test.xml", encoding = "UTF-8")
}

identicalXML(EML, xmlDoc(eml.root))
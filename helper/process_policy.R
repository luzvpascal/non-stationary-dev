library(XML)

# Main function
process_policy <- function(filename, horizon, fileout) {
  # Read XML
  doc <- xmlTreeParse(filename, useInternalNodes = TRUE, encoding = "ISO-8859-1")
  root <- xmlRoot(doc)

  # Extract all <Vector> nodes
  vectors <- getNodeSet(root, "//Vector")

  # Get AlphaVector node and attributes
  alpha <- root[["AlphaVector"]]
  vectorLength <- xmlGetAttr(alpha, "vectorLength")
  numObsValue <- as.integer(xmlGetAttr(alpha, "numObsValue"))
  numVectors <- as.integer(xmlGetAttr(alpha, "numVectors"))

  # Adjusted numObsValue
  newNumObsValue <- numObsValue * horizon
  newNumVectors <- numVectors * horizon

  # Open output file
  con <- file(fileout, "w", encoding = "UTF-8")

  # Write XML header & <Policy> opening
  cat('<?xml version="1.0" encoding="ISO-8859-1"?>\n', file = con)
  cat(sprintf('<Policy version="%s" type="%s" model="%s">\n',
              xmlGetAttr(root, "version"),
              xmlGetAttr(root, "type"),
              xmlGetAttr(root, "model")), file = con)

  # Write AlphaVector opening with updated numObsValue
  cat(sprintf('<AlphaVector vectorLength="%s" numObsValue="%s" numVectors="%s">\n',
              vectorLength, newNumObsValue, newNumVectors), file = con)

  # For each timestep t
  for (t in 1:horizon) {
    for (vec in vectors) {
      action <- xmlGetAttr(vec, "action")
      obsValue <- as.integer(xmlGetAttr(vec, "obsValue"))
      values <- xmlValue(vec)

      newObs <- tuple_to_index(t, obsValue+1) - 1

      cat(sprintf('<Vector action="%s" obsValue="%d">%s </Vector>\n',
                  action, newObs, values), file = con)
    }
  }

  # Close XML tags
  cat('</AlphaVector>\n', file = con)
  cat('</Policy>\n', file = con)

  close(con)
}

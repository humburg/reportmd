cwlVersion: v1.0
class: CommandLineTool
label: Compile an Rmarkdown document
baseCommand: [Rscript, -e, 'args <- as.list(commandArgs(TRUE))', -e, 'do.call(rmarkdown::render, args[-1])', --args]
requirements:
  - class: InlineJavascriptRequirement
  - class: InitialWorkDirRequirement
    listing:
      - $(inputs.rmd)
inputs:
  rmd:
    type: File
    inputBinding:
      position: 1
      valueFrom: $(self.basename)
  dep:
  - "null"
  - type: array
    items: File
outputs:
  html:
    type: File
    outputBinding:
      glob: $(inputs.rmd.basename.replace("/\.[^/.]+/$", ".html"))
    secondaryFiles:
    - ^_cache


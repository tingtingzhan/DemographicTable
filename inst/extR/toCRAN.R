
devtools::load_all('~/Dropbox/Packages/packageAdvanced')


file.copy(from = file.path('~/Dropbox/Packages/tzh/R', paste0(c(
  'class1List'
), '.R')), to = './R', overwrite = TRUE)


removeLocalPackage('DemographicTable')
updateDESCRIPTION('.')
checkDocument('.')
checkRelease('.')


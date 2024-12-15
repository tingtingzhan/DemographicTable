
#library(adv.tzh) # devtools::install_github('tingtingzhan/adv.tzh')
devtools::load_all('../adv.tzh')

removeLocalPackage('DemographicTable')
updateDESCRIPTION('.')
document_('.')
release_('.')


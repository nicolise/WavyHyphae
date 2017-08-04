Data is from BR_JV_ManualHyphalDat_032817.csv

These data are from categorical observations of hyphal waviness. Each observation is a 1-10 scale of how wavy (1 = straight line, 
10 = extremely wavy) the hyphae growing from a single B. cinerea spore are. Spores were collected from PDA plates,
diluted in water, and replated onto 0.75% PDA. Plates were incubated for 2-3 days on the bench (depended on growth rates) and
phenotyped under a light microscope.

Variables are:
Ordering - a counting variable. Not informative.
Isolate - which Botrytis cinerea genotype was measured.
PDAConc - what concentration of Potato Dextrose Agar was used on this plate.
Phenotype - a 1-10 scale of how wavy (1 = straight line, 10 = extremely wavy) the hyphae are for this data point.
Rep - A unique A1-E5 identifier combining PlateBlock with DotRep. No Rep is repeated within an Isolate.
PlateBlock - an A-E count of which plate this was to measure a particular isolate.
DotRep - a 1-5 count of which observation this data point was on a single plate.
Date - which day the data point was collected. R does not recognize this format as a date, so it needs to be recoded.

PDAConc should be constant throughout the experiment.
DotRep (and Rep) is nested within PlateBlock.
Check for interactions between Isolate:Date and Isolate:PlateBlock.


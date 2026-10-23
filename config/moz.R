# Paths are relative to the project root. Keep source files outside version control.
moz_config <- list(
  tool = 'inputs/kobo-used.xlsx',
  data = 'inputs/REACH_MOZ_Data_MSNA_01102026.xlsx',
  feedback = 'inputs/feedback/moz_corrections.csv',
  output = 'outputs/moz'
)
moz_sheets <- c(
  Submissions = 'MOZ 2026 MSNA Kobo Tool FINAL',
  roster = 'roster', nut_ind = 'nut_ind', edu_ind = 'edu_ind',
  health_ind = 'health_ind', health_wra = 'health_wra',
  prot_separated_children_breakdown = 'prot_separated_children_brea...',
  prot_hosted_separated_children_breakdown = 'prot_hosted_separated_childr...'
)

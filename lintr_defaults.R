library(lintr)

# Excluded linters
# closed_curly_linter
# camel_case_linter
# implicit_integer_linter
# paren_brace_linter
# pipe_continuation_linter
# trailing_blank_lines_linter
# commented_code_linter
# object_usage_linter,
# extraction_operator_linter

filename <- "app.R"
lint(filename, linters=list(
  absolute_path_linter,
  assignment_linter,
  commas_linter,
  cyclocomp_linter,
  equals_na_linter,
  function_left_parentheses_linter,
  infix_spaces_linter,
  line_length_linter,
  nonportable_path_linter,
  object_length_linter,
  object_name_linter("camelCase"),
  open_curly_linter,
  semicolon_linter,
  seq_linter,
  single_quotes_linter,
  spaces_inside_linter,
  spaces_left_parentheses_linter,
  todo_comment_linter,
  trailing_whitespace_linter,
  undesirable_function_linter,
  undesirable_operator_linter,
  unneeded_concatenation_linter,
  T_and_F_symbol_linter
))


# Sometimes its a README fix, or something like that - which isn't relevant for
# including in a CHANGELOG for example

has_app_changes = !files_modified.grep(/lib/).empty?
has_test_changes = !files_modified.grep(/spec/).empty?

if ["KrauseFx", "orta"].include?(pr_author) == false
  warn "Author @#{pr_author} is not a contributor"
end

if (pr_body + pr_title).include?("WIP")
  warn "Pull Request is Work in Progress"
end

if has_app_changes && !has_test_changes
  warn "Tests were not updated"
end

if pr_body.length < 5
  fail "Please provide a summary in the Pull Request description"
end

declared_trivial = pr_title.include?("#trivial") || !has_app_changes
if !files_modified.include?("CHANGELOG.md") && !declared_trivial
  fail "Please include a CHANGELOG entry. \nYou can find it at [CHANGELOG.md](https://github.com/danger/danger/blob/master/CHANGELOG.md)."
end

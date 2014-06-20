Pry.config.commands.import Pry::ExtendedCommands::Experimental

Pry.config.pager = false

Pry.config.color = false

Pry.config.commands.alias_command "lM", "ls -M"

Pry.config.commands.command "add", "Add a list of numbers together" do |*args|
  output.puts "Result is: #{args.map(&:to_i).inject(&:+)}"
end

Pry.config.history.should_save = false

Pry.config.prompt = [proc { "input> " },
                     proc { "     | " }]

# Disable pry-buggy-plug:
Pry.plugins["buggy-plug"].disable!

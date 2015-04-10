note
	description: "Git checkout command."
	author: "Olivier Ligot"

class
	GIT_CHECKOUT_COMMAND

inherit
	GIT_COMMAND

create
	make,
	make_master

feature {NONE} -- Initialization

	make (a_branch: STRING)
			-- Checkout the branch `a_branch'.
		do
			initialize
			arguments.force_last (a_branch)
			branch := a_branch
		ensure
			branch_set: branch = a_branch
		end

	make_master
			-- Checkout the master branch.
		do
			make ("master")
		end

feature -- Access

	branch: STRING
			-- Branch to checkout

	name: STRING = "checkout"
			-- Git subcommand name

end

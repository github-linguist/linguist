helpers
installGitFileTree
	"GitFileTree is the tool we will use to commit on GitHub."

	<script>
	Metacello new
		baseline: 'FileTree';
		repository:
			'github://dalehenrich/filetree:pharo' , SystemVersion current dottedMajorMinor
				, '_dev/repository';
		load: 'Git'
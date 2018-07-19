(Directory name: 'a_directory')
  allFilesMatching: '*.st' do: [ :f | (f name) displayNl ]

task echoDirListViaAntBuilder() {
    description = 'Uses the built-in AntBuilder instance to echo and list files'
    //Docs: http://ant.apache.org/manual/Types/fileset.html
    
    //Echo the Gradle project name via the ant echo plugin
    ant.echo(message: project.name)
    ant.echo(path)
    ant.echo("${projectDir}/samples")
    
    //Gather list of files in a subdirectory
    ant.fileScanner{
        fileset(dir:"samples")
    }.each{
        //Print each file to screen with the CWD (projectDir) path removed.
        println it.toString() - "${projectDir}"
    }
}

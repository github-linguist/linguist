jettyUrl = 'http://localhost:8081/'

def servers

stage 'Dev'
node {
    checkout scm
    servers = load 'servers.groovy'
    mvn '-o clean package'
    dir('target') {stash name: 'war', includes: 'x.war'}
}

stage 'QA'
parallel(longerTests: {
    runTests(servers, 30)
}, quickerTests: {
    runTests(servers, 20)
})

stage name: 'Staging', concurrency: 1
node {
    servers.deploy 'staging'
}

input message: "Does ${jettyUrl}staging/ look good?"

stage name: 'Production', concurrency: 1
node {
    sh "wget -O - -S ${jettyUrl}staging/"
    echo 'Production server looks to be alive'
    servers.deploy 'production'
    echo "Deployed to ${jettyUrl}production/"
}

def mvn(args) {
    sh "${tool 'Maven 3.x'}/bin/mvn ${args}"
}

def runTests(servers, duration) {
    node {
        checkout scm
        servers.runWithServer {id ->
            mvn "-o -f sometests test -Durl=${jettyUrl}${id}/ -Dduration=${duration}"
        }
    }
}

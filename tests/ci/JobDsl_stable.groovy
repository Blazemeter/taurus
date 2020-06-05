pipelineJob('TAURUS-COMMUNITY-STABLE'){
    properties {
        githubProjectUrl('http://github.com/Blazemeter/taurus')
        disableConcurrentBuilds()
    }
    triggers {
        scm('H/30 * * * *')
    }
    logRotator {
        daysToKeep(30)
        numToKeep(30)
    }
    definition {
        cpsScm {
            scm{
                git{
                    remote {
                        url('http://github.com/Blazemeter/taurus.git')
                        branch('refs/tags/*')
                        credentials('github-token')
                    }
                }
            }
            scriptPath('Jenkinsfile')
        }
    }
}
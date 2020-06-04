pipelineJob('TAURUS-COMMUNITY-MASTER'){
    properties {
        githubProjectUrl('http://github.com/Blazemeter/taurus')
        disableConcurrentBuilds()
    }
    triggers {
        scm('*/5 * * * *')
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
                        branch('*/master')
                        credentials('github-token')
                    }
                }
            }
            scriptPath('Jenkinsfile')
        }
    }
}
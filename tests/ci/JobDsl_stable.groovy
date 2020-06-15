pipelineJob('TAURUS-COMMUNITY-STABLE'){
    properties {
        githubProjectUrl('http://github.com/Blazemeter/taurus')
        disableConcurrentBuilds()
    }
    triggers {
        genericTrigger {
            genericVariables {
                genericVariable {
                    key("ref")
                    value("\$.ref")
                }
                genericVariable {
                    key("commit")
                    value("\$.after")
                }
            }
            causeString('job triggered with $ref, commit hash - $commit')
            token('Q6QyWaD4rdY42Kqt')
            regexpFilterText('$ref')
            regexpFilterExpression('^(refs/tags/.+)$')
        }
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
                        branch('$ref')
                        credentials('github-token')
                    }
                }
            }
            scriptPath('Jenkinsfile')
        }
    }
}
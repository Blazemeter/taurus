multibranchPipelineJob('TAURUS-IMAGE-BUILDER') {
    factory {
        workflowBranchProjectFactory {
            scriptPath('tests/ci/Jenkinsfile-image-builder')
        }
    }
    branchSources {
        branchSource {
            source {
                git {
                    remote('https://github.com/Blazemeter/taurus.git')
                    credentialsId('github-token')
                    traits {
                        gitBranchDiscovery()
                        gitTagDiscovery()
                        headWildcardFilter {
                            includes('master release-* 1.*')
                            excludes('')
                        }
                    }
                }
            }
        }
    }
    configure {
        def buildStrategies = it / sources / data / 'jenkins.branch.BranchSource' / buildStrategies
        buildStrategies << 'jenkins.branch.buildstrategies.basic.TagBuildStrategyImpl' {
            atLeastMillis '-1'
            atMostMillis '604800000'
        }
        buildStrategies << 'jenkins.branch.buildstrategies.basic.NamedBranchBuildStrategyImpl' {
            filters {
                'jenkins.branch.buildstrategies.basic.NamedBranchBuildStrategyImpl_-WildcardsNameFilter' {
                    includes('*')
                    excludes('')
                    caseSensitive false
                }
            }
        }
    }
    orphanedItemStrategy {
        discardOldItems {
            daysToKeep(1)
            numToKeep(100)
        }
    }
    triggers {
        periodic(1)
    }
}

multibranchPipelineJob('TAURUS-IMAGE-BUILDER'){
    factory {
        workflowBranchProjectFactory {
            scriptPath('tests/ci/Jenkinsfile-image-builder')
        }
    }
    branchSources {
        git {
            id('1') // IMPORTANT: use a constant and unique identifier per branch source
            remote('https://github.com/Blazemeter/taurus.git')
            credentialsId('github-token')
            includes('*')
            excludes('master fix/* feat/*')
        }
    }
    configure {
        def traits = it / sources / data / 'jenkins.branch.BranchSource' / source / traits
        traits << 'jenkins.plugins.git.traits.CloneOptionTrait' {
            extension( class: 'hudson.plugins.git.extensions.impl.CloneOption' ) {
                noTags(false)
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
        periodic(30)
    }
}

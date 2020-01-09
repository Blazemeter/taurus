multibranchPipelineJob('TAURUS-IMAGE-BUILDER'){
    branchSources {
        git {
            id('1') // IMPORTANT: use a constant and unique identifier per branch source
            remote('https://github.com/Blazemeter/taurus.git')
            credentialsId('github-token')
            includes('*')
        }
    }
    configure {
        it / factory(class: 'org.jenkinsci.plugins.workflow.multibranch.WorkflowBranchProjectFactory') {
            owner(class: 'org.jenkinsci.plugins.workflow.multibranch.WorkflowMultiBranchProject', reference: '../..')
            scriptPath("tests/ci/Jenkinsfile-image-builder") //Set a specific scriptPath in MultiBranchPipelineJob DSL
        }
    }
    orphanedItemStrategy {
        discardOldItems {
            daysToKeep(1)
            numToKeep(100)
        }
    }
}

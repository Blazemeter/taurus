@Library("jenkins_library") _

node() {
    try {
        stage('Checkout') {
            cleanWs()
            scmVars = checkout scm
            commitHash = scmVars.GIT_COMMIT
            isTag =  scmVars.GIT_BRANCH.startsWith("refs/tags/")
            IMAGE_TAG = env.JOB_NAME + ":" + env.CHANGE_ID + "." + env.BUILD_NUMBER
            IMAGE_TAG = IMAGE_TAG.toLowerCase()
        }

        stage("Docker Image Build") {
            sh """ 
            docker build -t ${JOB_NAME} .
            """
        }

        stage("Create Artifacts") {
            sh """
                sed -ri "s/OS: /Rev: ${commitHash}; OS: /" bzt/cli.py           
            """

            if (!isTag) {
                sh """ 
                sed -ri "s/VERSION = .([^\\"]+)./VERSION = '\\1.${BUILD_NUMBER}'/" bzt/__init__.py
                """
            }

            withCredentials([file(credentialsId: 'blazemeter-taurus-website-prod', variable: 'CRED_JSON')]) {
                def WORKSPACE_JSON = 'Google_credentials.json'
                def input = readJSON file: CRED_JSON
                writeJSON file: WORKSPACE_JSON, json: input
                sh """
                    docker run --entrypoint /bzt-configs/build-artifacts.bash -v `pwd`:/bzt-configs -e KEY_FILE=${WORKSPACE_JSON} -e IMAGE_TAG=${IMAGE_TAG} -t ${JOB_NAME} ${isTag} ${BUILD_NUMBER}
                    """
            }
        }



    } catch (e) {
        currentBuild.result = "FAILED"
        throw e
    } finally {
        smartSlackNotification(channel: "taurus-dev", buildStatus:currentBuild.result ?: 'SUCCESS')
    }
}

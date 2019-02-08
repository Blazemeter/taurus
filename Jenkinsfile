@Library("jenkins_library") _

pipeline
{
    agent
    {
        dockerfile
        {
            filename 'Dockerfile.deploy'
            args "-u root -v /var/run/docker.sock:/var/run/docker.sock"
        }
    }
    stages
    {
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

                sh """
                    docker run --entrypoint /bzt-configs/build-artifacts.bash -v `pwd`:/bzt-configs -t ${JOB_NAME} ${BUILD_NUMBER}
                    """

            }
            stage("Deploy site") {

                PROJECT_ID="blazemeter-taurus-website-prod"
                withCredentials([file(credentialsId: "${PROJECT_ID}", variable: 'CRED_JSON')]) {
                    def WORKSPACE_JSON = 'Google_credentials.json'
                    def input = readJSON file: CRED_JSON
                    writeJSON file: WORKSPACE_JSON, json: input
                    gcloud auth activate-service-account --key-file ${WORKSPACE_JSON}
                }

                gcloud config set project ${PROJECT_ID}
                gcloud config set compute/zone us-central1-a

                // if (isTag) {
                    sh "./build-base-site.sh"
                //  }
                sh """
                    ./build-snapshot-site.sh
                    ./deploy-site.sh
                    """
            }
        } catch (e) {
            currentBuild.result = "FAILED"
            throw e
        } finally {
            smartSlackNotification(channel: "taurus-dev", buildStatus:currentBuild.result ?: 'SUCCESS')
        }
    }
}

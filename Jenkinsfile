@Library("jenkins_library") _

pipeline {
    agent any
    options {
        timestamps()
        skipDefaultCheckout()
    }
    stages {
        stage('Checkout') {
            steps {
                cleanWs()
                script {
                    scmVars = checkout scm
                    commitHash = scmVars.GIT_COMMIT
                    isTag =  scmVars.GIT_BRANCH.startsWith("refs/tags/")
                    IMAGE_TAG = env.JOB_NAME + "." + env.BUILD_NUMBER
                    IMAGE_TAG = IMAGE_TAG.toLowerCase()
                }
            }
        }
        stage("Docker Image Build") {
            steps {
                sh """
                   docker build -t ${JOB_NAME} .
                   """
            }
        }
        stage("Integration Tests") {
            steps {
                sh """
                   docker run -v `pwd`:/bzt-configs -v `pwd`/integr-artifacts:/tmp/artifacts ${JOB_NAME} -sequential examples/all-executors.yml
                   """
            }
        }
        stage("Create Artifacts") {
            steps {
                script {
                    sh """
                       sed -ri "s/OS: /Rev: ${commitHash}; OS: /" bzt/cli.py
                       """

                    if (!isTag) {
                        sh """
                           sed -ri "s/VERSION = .([^\\"]+)./VERSION = '\\1.${BUILD_NUMBER}'/" bzt/__init__.py
                           """
                    }

                    sh """
                       docker run --entrypoint /bzt-configs/build-artifacts.sh -v `pwd`:/bzt-configs ${JOB_NAME} ${BUILD_NUMBER}
                       """
                }
                archiveArtifacts artifacts: 'dist/*.whl', fingerprint: true
            }
        }
        stage("Deploy site") {
            steps {
                sh """
                   docker build -t deploy-image -f site/Dockerfile.deploy .
                   """
                script {
                    PROJECT_ID="blazemeter-taurus-website-prod"
                    withCredentials([file(credentialsId: "${PROJECT_ID}", variable: 'CRED_JSON')]) {
                        def WORKSPACE_JSON = 'Google_credentials.json'
                        def input = readJSON file: CRED_JSON
                        writeJSON file: WORKSPACE_JSON, json: input
                        sh """
                           docker run --entrypoint /bzt/site/deploy-site.sh \
                           -e KEY_FILE=${WORKSPACE_JSON} \
                           -e PROJECT_ID=${PROJECT_ID} \
                           -e BUILD_NUMBER=${BUILD_NUMBER} \
                           -u root \
                           -v /var/run/docker.sock:/var/run/docker.sock \
                           -v `pwd`:/bzt -t deploy-image \
                           ${isTag}
                          """
                    }
                }
            }
        }
    }
    post {
        always {
            smartSlackNotification(channel: "taurus-dev", buildStatus:currentBuild.result ?: 'SUCCESS')
        }
    }
}

@Library("jenkins_library") _

node() {
    try {
        stage('Checkout') {
            cleanWs()
            scmVars = checkout scm
            commitHash = scmVars.GIT_COMMIT
            isTag =  scmVars.GIT_BRANCH.startsWith("refs/tags/")
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

            withCredentials(credentialsId: 'blazemeter-taurus-website-prod', variable: 'CRED_JSON') {
                def WORKSPACE_JSON = 'Google_credentials.json'
                def input = readJSON file: CRED_JSON
                writeJSON file: WORKSPACE_JSON, json: input
                sh """
                    docker run --entrypoint /bzt-configs/build-artifacts.bash -v `pwd`:/bzt-configs -e KEY_FILE=${WORKSPACE_JSON} -t ${JOB_NAME} ${isTag} ${BUILD_NUMBER}
                    """
            }
        }

        stage("Post-build") {
            archiveArtifacts artifacts: 'dist/*.tar.gz'
            sh """ 
            mkdir -p s3
            cp dist/bzt-*.tar.gz s3/bzt-${BUILD_NUMBER}.tar.gz 
            """

            setAWSCredentialKeys('test_jenkins2_key')
            s3Upload bucket: "deployment.blazemeter.com",
                    file: "s3/bzt-${BUILD_NUMBER}.tar.gz",
                    path: "jobs/${JOB_NAME}/${BUILD_NUMBER}/bzt-${BUILD_NUMBER}.tar.gz",
                    acl: "PublicRead"
        }
    } catch (e) {
        currentBuild.result = "FAILED"
        throw e
    } finally {
        smartSlackNotification(channel: "taurus-dev", buildStatus:currentBuild.result ?: 'SUCCESS')
    }
}

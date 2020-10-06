@Library("jenkins_library") _

pipeline {
    agent {
        dockerfile {
            filename 'tests/ci/Dockerfile.build'
            args '-u root -v /var/run/docker.sock:/var/run/docker.sock'
        }
    }
    options {
        timestamps()
    }
    stages {
        stage('Checkout') {
            steps {
                script {
                    tagName = sh(returnStdout: true, script: "git tag --points-at HEAD").trim()
                    isRelease = !tagName.isEmpty()
                    IMAGE_TAG = env.JOB_NAME + "." + env.BUILD_NUMBER
                    IMAGE_TAG = IMAGE_TAG.toLowerCase()
                    imageName = "blazemeter/taurus"
                    extraImageTag = isRelease ? "${imageName}:${tagName} -t ${imageName}:latest" : "${imageName}:unstable"
                    VERSION = sh(returnStdout: true, script: "git describe --tags \$(git rev-list --tags --max-count=1)").trim()
                    GIT_INFO = sh(returnStdout: true, script: "\$(git rev-parse --abbrev-ref HEAD) \$(git show --oneline -s)").trim()
                    if (!isRelease) {
                        VERSION = "${VERSION}.${BUILD_NUMBER}"
                    }
                }
                sh """
                   echo 'BUILD_NUM=\"${BUILD_NUMBER}\"' > bzt/resources/version/build.py
                   echo 'VERSION=\"${VERSION}\"' > bzt/resources/version/version.py
                   echo 'GIT_INFO=\"${GIT_INFO}\"' > bzt/resources/version/gitinfo.py
                   """
            }
        }
        stage("Create artifacts") {
            steps {
                script {
                    sh "./build-artifacts.sh"
                }
                archiveArtifacts artifacts: 'dist/*.whl', fingerprint: true
                archiveArtifacts artifacts: 'build/nsis/*._x64.exe', fingerprint: true
            }
        }
        stage("Docker Image Build") {
            steps {
                script {
                    sh "docker build --no-cache -t ${JOB_NAME} -t ${extraImageTag} ."
                }
            }
        }
        stage("Integration Tests") {
            steps {
                sh """
                   docker run -v `pwd`:/bzt-configs -v `pwd`/integr-artifacts:/tmp/artifacts ${JOB_NAME} -sequential examples/all-executors.yml
                   """
            }
        }
        // stage("Docker Image Push") {
        //     steps {
        //         withDockerRegistry([ credentialsId: "dockerhub-access", url: "" ]) {
        //             sh "docker push ${imageName}"
        //         }
        //     }
        // }
        // stage("Deploy site") {
        //     steps {
        //         sh """
        //            docker build -t deploy-image -f site/Dockerfile.deploy .
        //            """
        //         script {
        //             PROJECT_ID="blazemeter-taurus-website-prod"
        //             withCredentials([file(credentialsId: "${PROJECT_ID}", variable: 'CRED_JSON')]) {
        //                 def WORKSPACE_JSON = 'Google_credentials.json'
        //                 def input = readJSON file: CRED_JSON
        //                 writeJSON file: WORKSPACE_JSON, json: input
        //                 sh """
        //                    docker run --entrypoint /bzt/site/deploy-site.sh \
        //                    -e KEY_FILE=${WORKSPACE_JSON} \
        //                    -e PROJECT_ID=${PROJECT_ID} \
        //                    -e BUILD_NUMBER=${BUILD_NUMBER} \
        //                    -u root \
        //                    -v /var/run/docker.sock:/var/run/docker.sock \
        //                    -v `pwd`:/bzt -t deploy-image \
        //                    ${isRelease}
        //                   """
        //             }
        //         }
        //     }
        // }
    }
    post {
        always {
            // smartSlackNotification(channel: "taurus-dev", buildStatus:currentBuild.result ?: 'SUCCESS')
            cleanWs()
        }
    }
}

@Library("jenkins_library") _

pipeline {
    agent {
        dockerfile {
            label 'google'
            filename 'tests/ci/Dockerfile'
            args '-u root -v /var/run/docker.sock:/var/run/docker.sock'
        }
    }
    options {
        timestamps()
    }
    parameters{
        booleanParam(name: 'PERFORM_PRISMA_SCAN', defaultValue: true, description: 'Perform a Prisma scan for the local image')
    }
    stages {
        stage('Checkout') {
            steps {
                script {
                    initJenkinsGlobal()
                    tagName = sh(returnStdout: true, script: "git tag --points-at HEAD").trim()
                    isRelease = !tagName.isEmpty()
                    IMAGE_TAG = env.JOB_NAME + "." + env.BUILD_NUMBER
                    IMAGE_TAG = IMAGE_TAG.toLowerCase()
                    imageName = "blazemeter/taurus"
                    date = sh(returnStdout: true, script: "echo \$(date '+%Y-%m-%d')").trim()
                    commitSha = GIT_COMMIT.take(8)
                    imageTag = "${imageName}:master-${commitSha}-${date}"
                    extraImageTag = isRelease ? "${imageName}:${tagName} -t ${imageTag} -t ${imageName}:latest" : "${imageName}:unstable -t ${imageTag}"
                    sh "./build-info.sh ${isRelease}"
                }
            }
        }
        stage("Create artifacts") {
            steps {
                script {
                    sh "./build-artifacts.sh"
                }
                archiveArtifacts artifacts: 'dist/*.whl', fingerprint: true
            }
        }
        stage("Docker Image Build") {
            steps {
                script {
                    sh "docker build --no-cache -t ${JOB_NAME.toLowerCase()} -t ${extraImageTag} ."
                }
            }
        }
        stage("Prisma scan") {
            when { expression { return PERFORM_PRISMA_SCAN } }
            steps {
                script{
                    prismaCloudScanImage(dockerAddress: 'unix:///var/run/docker.sock',
                            image: "${JOB_NAME.toLowerCase()}",
                            logLevel: 'info',
                            resultsFile: 'prisma-cloud-scan-results.json',
                            ignoreImageBuildTime: true)
                    prismaCloudPublish(resultsFilePattern: 'prisma-cloud-scan-results.json')
                }
            }
        }
        stage("Integration Tests") {
            steps {
                sh """
                   docker run -v `pwd`:/bzt-configs -v `pwd`/integr-artifacts:/tmp/artifacts ${JOB_NAME.toLowerCase()} -sequential examples/all-executors.yml
                   """
            }
        }
        stage("Deploy an artifact to PyPi") {
            when { expression { isRelease } }
            steps {
                withCredentials([usernamePassword(credentialsId: 'bzt-pypi', usernameVariable: 'USERNAME', passwordVariable: 'PASSWORD')]) {
                   sh "python3 -m twine upload -u ${USERNAME} -p ${PASSWORD} dist/*"
               }
            }
        }
        stage("Docker Image Push") {
            steps {
                withDockerRegistry([ credentialsId: "dockerhub-access", url: "" ]) {
                    sh "docker image push --all-tags ${imageName}"
                }
            }
        }
        stage("Deploy site") {
            when { expression { isRelease } }
            steps {
                script {
                    PROJECT_ID = "blazemeter-taurus-website-prod"
                    withCredentials([file(credentialsId: PROJECT_ID, variable: 'CRED_JSON')]) {
                        sh """
                           gcloud auth activate-service-account --key-file ${CRED_JSON}
                           gcloud config set project ${PROJECT_ID}
                           gcloud config set compute/zone us-central1-a
                           """
                    }
                    sh """
                       export PROJECT_ID=${PROJECT_ID}
                       ./site/deploy-site.sh ${isRelease}
                       """
                }
            }
        }
    }
    post {
        always {
            smartSlackNotification(channel: "bm-taurus-dev", buildStatus:currentBuild.result ?: 'SUCCESS')
            cleanWs()
        }
    }
}

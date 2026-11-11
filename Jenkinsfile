@Library("jenkins_library") _
clearWorkspaceAsRoot()

import com.blazemeter.buildkit.BuildkitManager

BuildkitManager buildkitManager = new BuildkitManager(this)

pipeline {
    agent {
        kubernetes {
            yaml agentYaml(image: 'us.gcr.io/verdant-bulwark-278/jenkins-docker-agent:taurus-agent-2')
            defaultContainer 'jenkins-docker-agent'
            retries 2
        }
    }
    options {
        timestamps()
    }
    parameters {
        booleanParam(name: 'PERFORM_PRISMA_SCAN', defaultValue: true, description: 'Perform a Prisma scan for the local image')
        booleanParam(name: 'UPLOAD_WHL_TO_GCS', defaultValue: false, description: 'Upload the community WHL package to gs://blazemeter-images/crane-components/')
    }
    stages {
        stage('Checkout') {
            steps {
                script {
                    sh 'git config --global --add safe.directory "$(pwd)"'
                    initJenkinsGlobal()
                    tagName = sh(returnStdout: true, script: "git tag --points-at HEAD").trim()
                    isRelease = !tagName.isEmpty()
                    imageName = "blazemeter/taurus"
                    date = sh(returnStdout: true, script: "echo \$(date '+%Y-%m-%d')").trim()
                    commitSha = GIT_COMMIT.take(8)
                    dateTag = "master-${commitSha}-${date}"
                    env.GCR_IMAGE = "us.gcr.io/verdant-bulwark-278/taurus:master-${BUILD_NUMBER}"
                    sh "./build-info.sh ${isRelease}"
                }
                stash name: 'taurus-source'
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
                    def gcrRepo = "us.gcr.io/verdant-bulwark-278/taurus"
                    def tags = isRelease
                        ? [env.GCR_IMAGE, "${gcrRepo}:${tagName}", "${gcrRepo}:${dateTag}", "${gcrRepo}:latest"]
                        : [env.GCR_IMAGE, "${gcrRepo}:unstable", "${gcrRepo}:${dateTag}"]
                    buildkitManager.build(imageName: 'taurus', tags: tags)
                }
            }
        }
        stage("Prisma scan") {
            when { expression { return params.PERFORM_PRISMA_SCAN } }
            steps {
                script {
                    runPrismaCloudScanOnK8s(imageTag: env.GCR_IMAGE, buildkitManager: buildkitManager)
                }
            }
        }
        stage("Integration Tests") {
            agent {
                kubernetes {
                    defaultContainer 'taurus-integration'
                    yaml """
apiVersion: v1
kind: Pod
spec:
  nodeSelector:
    jenkins-agent-n4: "true"
  tolerations:
  - key: "dedicated"
    operator: "Equal"
    value: "jenkins-pod-fleet"
    effect: "NoSchedule"
  containers:
  - name: taurus-integration
    image: ${env.GCR_IMAGE}
    command:
    - cat
    tty: true
    resources:
      requests:
        memory: "4Gi"
        cpu: "2"
      limits:
        memory: "8Gi"
        cpu: "4"
"""
                }
            }
            steps {
                unstash 'taurus-source'
                sh "mkdir -p integr-artifacts && bzt -sequential examples/all-executors.yml -o settings.artifacts-dir=${WORKSPACE}/integr-artifacts"
            }
            post {
                always {
                    archiveArtifacts artifacts: "integr-artifacts/**", allowEmptyArchive: true
                }
            }
        }
        stage("Deploy an artifact to PyPi") {
            when { expression { isRelease } }
            steps {
                withCredentials([string(credentialsId: 'pypi-api-token', variable: 'TOKEN')]) {
                    sh "python3 -m twine upload -u __token__ -p ${TOKEN} dist/*"
                }
            }
        }
        stage("Docker Image Push") {
            steps {
                script {
                    def dockerhubTags = isRelease ? [tagName, dateTag, "latest"] : ["unstable", dateTag]
                    buildkitManager.pull(env.GCR_IMAGE)
                    dockerhubTags.each { tag ->
                        sh "docker tag ${env.GCR_IMAGE} ${imageName}:${tag}"
                    }
                    withDockerRegistry([credentialsId: "dockerhub-access", url: ""]) {
                        sh "docker image push --all-tags ${imageName}"
                    }
                }
            }
        }
        stage("Upload WHL to GCS") {
            when { expression { return params.UPLOAD_WHL_TO_GCS } }
            steps {
                sh "gsutil cp -a public-read dist/*.whl gs://blazemeter-images/crane-components/"
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
            script {
                container('jenkins-docker-agent') {
                    buildkitManager.cleanImages()
                }
            }
            smartSlackNotification(channel: "bm-taurus-dev", buildStatus:currentBuild.result ?: 'SUCCESS')
        }
    }
}

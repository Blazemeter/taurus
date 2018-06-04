@Library("jenkins_library") _

node() {
    try {
        stage('Checkout') {
            cleanWs()
            scmVars = checkout scm
            commitHash = scmVars.GIT_COMMIT
            isTag = !"".equals("${env.GIT_TAG_NAME}") && !"null".equals("${env.GIT_TAG_NAME}")
            sh "echo " + scmVars.inspect()

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
                docker run --entrypoint /bzt-configs/build-artifacts.bash -v `pwd`:/bzt-configs -t ${JOB_NAME}
                """
        }

        stage("Create Website Update") {
            if (isTag) {
                sh """
                python site/Taurus/kwindexer.py site/dat/docs site/dat/docs/KeywordIndex.md
                cp site/dat/docs/img/*.png site/img/
                
                TAURUS_VERSION=\$(python -c 'import bzt; print(bzt.VERSION)')
                sed -ri "s/_TAURUS_VERSION_/_\${TAURUS_VERSION}_/" site/dat/docs/Installation.md
                mkdir -p site/msi
                cp build/nsis/*.exe site/msi/                
                """
            } else {
                sh """ 
                cp -r site/dat/kb ./           
                rm -r site/*
                 
                mkdir -p site/snapshots
                cp dist/*.tar.gz site/snapshots
                wget -P site/snapshots https://s3.amazonaws.com/deployment.blazemeter.com/jobs/taurus-pbench/10/blazemeter-pbench-extras_0.1.10.1_amd64.deb
                cp build/nsis/*${BUILD_NUMBER}*.exe site/snapshots
                
                mkdir -p site/dat
                mv ./kb site/dat/                            
                """
            }

            zip archive: true, dir: 'site', glob: '**/*.*', zipFile: 'site/site.zip'
        }

        stage('Update Website') {
            withCredentials([usernamePassword(credentialsId: 'tauruswebsite', usernameVariable: 'WEB_USER', passwordVariable: 'WEB_PASS')]) {
                sh """ 
            SRV="https://cphost13.qhoster.net:2083"
            SESS=`curl -m 60 -v -c ./cj.txt \$SRV/login/ -d "user=$WEB_USER&pass=$WEB_PASS" | cut -d / -f2`
            
            curl -v -b ./cj.txt -m 180 "\$SRV/\$SESS/json-api/cpanel" \
              -F "cpanel_jsonapi_user=user" -F "cpanel_jsonapi_apiversion=2" -F "cpanel_jsonapi_module=Fileman" -F "cpanel_jsonapi_func=uploadfiles" \
              -F "dir=public_html" -F"overwrite=1" -F "file-1=@site/site.zip"
            
            
            PARAM1="cpanel_jsonapi_module=Fileman&cpanel_jsonapi_func=fileop&cpanel_jsonapi_apiversion=2&filelist=1&multiform=1&doubledecode=0"
            PARAM2="op=extract&metadata=undefined&sourcefiles=%2fhome%2fgettauru%2fpublic_html%2fsite.zip&destfiles=%2fpublic_html"
            curl -v -b ./cj.txt "\$SRV/\$SESS/json-api/cpanel" -d "\$PARAM1&\$PARAM2" -m 60 || echo Failed to unpack site update! 
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
    } finally {
        smartSlackNotification(channel: "taurus-dev")
    }
}

<?php

namespace Taurus;

require __DIR__ . '/vendor/autoload.php';

use Google\Cloud\Storage\StorageClient;

$file_list = glob('./bzt-usage-stats/*');
$allowed = array('json', 'csv');
$file_names = [];

foreach ($file_list as $file_path) {
    $file_ext = pathinfo($file_path, PATHINFO_EXTENSION);
    if (in_array(strtolower($file_ext), $allowed)) {
        $file_name = explode("/", $file_path)[2];
        sendFileToGCS($file_path, $file_name);
    }
}

function sendFileToGCS($file_path, $file_name)
{
    $project_id = 'blazemeter-taurus-website-prod';
    $cred_path = getEnv('GOOGLE_CLOUD_CRED');
    $bucket_name = 'taurus-statistic';

    // Create connection with GCS
    $storage = new StorageClient([
        'projectId' => $project_id,
        'keyFilePath' => $cred_path  # Our key
    ]);
    $bucket = $storage->bucket($bucket_name);

    // Upload file
    $uploader = $bucket->getResumableUploader(
        fopen($file_path, 'r'), [
            'name' => $file_name,
            'predefinedAcl' => 'publicRead',
        ]
    );

    // Sending a file to the cloud GCS

    try {
        $uploader->upload();
        echo "File Upload";
    }  catch (GoogleException $ex) {
        $resumeUri = $uploader->getResumeUri();
        $object = $uploader->resume($resumeUri);
        echo 'No File Uploaded';
    }
}
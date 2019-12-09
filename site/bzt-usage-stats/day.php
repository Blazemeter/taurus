<?php

namespace Taurus;

include('../vendor/autoload.php');

getDayDataFromDB();

function getDayDataFromDB()
{
    $db_cred_path = getEnv('DB_CRED');
    $data = file_get_contents($db_cred_path);
    $json_cred = json_decode($data, true);

    $db_conn = pg_connect('host='. $json_cred['host'] .
                        ' port='. $json_cred['port'] .
                        ' dbname='. $json_cred['dbname'] .
                        ' user='. $json_cred['user'] .
                        ' password='. $json_cred['password']);


    $query = "SELECT * FROM aggregate_data";

    $contests = pg_query($query) or die('Query failed: ' . pg_last_error());
    $column_number = pg_num_fields($contests);

    $day_arr = array();
    while ($row = pg_fetch_row($contests)) {
        $day_json = new \stdClass();
        for ($col = 0; $col < $column_number; $col++) {
            $fieldname = pg_field_name($contests, $col);
            $day_json->$fieldname = $row[$col];
        }
        $day_arr[] = $day_json;
    }

    echo json_encode($day_arr);
}

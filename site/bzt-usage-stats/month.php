<?php

namespace Taurus;

include('../vendor/autoload.php');

getMonthDataFromDB();

function getMonthDataFromDB()
{
    $db_cred_path = getEnv('DB_CRED');
    $data = file_get_contents($db_cred_path);
    $json_cred = json_decode($data, true);

    $db_conn = pg_connect('host='. $json_cred['host'] .
                        ' port='. $json_cred['port'] .
                        ' dbname='. $json_cred['dbname'] .
                        ' user='. $json_cred['user'] .
                        ' password='. $json_cred['password']);


    $query = 'select month, sum(desktop_launch) AS "desktop_launch", sum(cloud_launch) AS "cloud_launch", sum(new_users) as "new_users"  from aggregate_data group by month order by month;';

    $contests = pg_query($query) or die('Query failed: ' . pg_last_error());
    $column_number = pg_num_fields($contests);

    $month_arr = array();
    while ($row = pg_fetch_row($contests)) {
        $month_json = new \stdClass();
        for ($col = 0; $col < $column_number; $col++) {
            $fieldname = pg_field_name($contests, $col);
            $month_json->$fieldname = $row[$col];
        }
        $month_arr[] = $month_json;
    }

    echo json_encode($month_arr);
}


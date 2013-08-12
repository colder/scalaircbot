<?php
ini_set("date.timezone", "Europe/Zurich");

if (!file_exists("config-prod.xml")) {
    exit("File not found: config-prod.xml");
}


$data = simplexml_load_file("config-prod.xml");

$host = (string)$data->db['host'];
$user = (string)$data->db['user'];
$pass = (string)$data->db['pass'];
$db   = (string)$data->db['database'];

$date = date("d-m-Y");

$snapshot = "database/snapshot-$date.sql.gz";

`mysqldump -u$user -h$host -p$pass $db | gzip > $snapshot`;
`git add $snapshot`;
`git ci -m "Add new database snapshot"`;
`git push origin master`;



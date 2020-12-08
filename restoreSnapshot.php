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

$snapshot = $_SERVER['argv'][1];

`gunzip -c $snapshot | mysql -u$user -h$host -p$pass $db`;

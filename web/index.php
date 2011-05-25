<?php

require_once dirname(__FILE__).'/.config.php';

try {
    $pdo = new PDO($dsn, $user, $password);
} catch (PDOException $e) {
    exit('Connection to the bot database failed.');
}


?>
<?php echo '<?xml version="1.0" encoding="UTF-8"?>'; ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
    <head>
        <title>Bot Factoids</title>
        <style type="text/css">
            input[name="q"] {
                width: 400px;
            }
        </style>
    </head>
    <body>
        <h1>Bot Factoids</h1>
        <form method="POST" action="">
            <fieldset>
                <legend>Search Database</legend>
                <input type="text" name="q" value="" />
                <input type="submit" name="search" value="Search" />
            </fieldset>
        </form>

<?php

    if (!isset($_POST['q']) || $_POST['q'] === '') {
        $query = "SELECT token, description, hits FROM irc_factoids ORDER BY hits DESC LIMIT 50";
        $res = $pdo->query($query);
    } else {
        $query = "SELECT token, description, hits FROM irc_factoids WHERE MATCH (token, description) AGAINST (?) LIMIT 50";
        $res = $pdo->prepare($query);

        $res->execute(array($_POST['q']));
    }


    echo '
        <h2>'.$res->rowCount().' Results:</h2>
        <div class="results">';

    foreach ($res as $fact) {
        echo '
            <div>
                <h3>'.htmlentities($fact['token'], ENT_QUOTES).'</h3>
                <p>'.htmlentities($fact['description'], ENT_QUOTES).'</p>
            </div>';
    }
?>
        </div>
    </body>
</html>

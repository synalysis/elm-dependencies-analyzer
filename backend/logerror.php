<?php
require_once dirname(__FILE__) . '/config.php';

# ================================================================================
# CONSTANTS

const DIR_LOG_ERROR = config\DIR_LOG . '/error';

# ================================================================================
# MAIN


try {
    $method = $_SERVER['REQUEST_METHOD'];

    if ($method === "OPTIONS") {
        header('Access-Control-Allow-Origin: *');
        header('Access-Control-Allow-Headers: Content-Type');
        header('Access-Control-Allow-Methods: POST, OPTIONS');

    } elseif ($method === "POST") {
        $data = file_get_contents('php://input');

        # LOGFILE
        if ($data !== false) {
            $postJson = json_decode($data, true);
            if ($postJson !== null) {
                if (array_key_exists('tag', $postJson)) {
                    if (preg_match("/^([0-9]{4})$/", $postJson['tag'], $m) === 1) {
                        $logFile = $m[1];
                    } else {
                        $logFile = 'invalidtag';
                    }
                } else {
                    $logFile = 'notag';
                }
            } else {
                $logFile = 'nojson';
            }
        } else {
            $logFile = 'nodata';
            $data    = 'nodata';
        }

        # LOGDATA
        list ($usecAsSec, $sec) = explode(' ', microtime());
        $usec    = intval($usecAsSec * 1000 * 1000);
        $logData = sprintf("%s\nTIME %s.%06s\nORIGIN %s\n%s\n",
            "======================================================================",
            gmdate('Y-m-d H:i:s', $sec),
            $usec,
            $_SERVER['HTTP_ORIGIN'],
            $data
        );

        # MKDIR
        if (! file_exists(DIR_LOG_ERROR)) {
            if (! mkdir(DIR_LOG_ERROR, 0777, true)) throw new Exception('mkdir');
        }

        # WRITE
        $fh = fopen(DIR_LOG_ERROR . '/' . $logFile, "a");
        if (flock($fh, LOCK_EX)) {
            fwrite($fh, $logData);
            fflush($fh);
            flock($fh, LOCK_UN);
        }
        fclose($fh);
    }
} finally {
    header('Access-Control-Allow-Origin: *');
    header('Content-Type: text/plain');
    echo 'OK';
}

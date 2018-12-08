<?php

# ================================================================================
# CONSTANTS

const DIR_CACHE         = "/var/www/com.markuslaire/cache/elm-dependencies-analyzer";
const DIR_LOG           = "/var/www/com.markuslaire/log/elm-dependencies-analyzer";

const PATH_ALL_PACKAGES = DIR_CACHE . '/all.json';
const URL_ALL_PACKAGES  = "https://package.elm-lang.org/all-packages";

/*
 * Version-tagged elm.json from GitHub should never change, so using longer expiry there.
 */
const REMOTE_LONG_EXPIRY_SECONDS        = 7 * 24 * 60 * 60;
const REMOTE_NORMAL_EXPIRY_SECONDS      = 1 * 24 * 60 * 60;
const REMOTE_NOTFOUND_EXPIRY_SECONDS    = 1 * 24 * 60 * 60;
const CLIENT_EXPIRY_SECONDS             = 1 * 24 * 60 * 60;

const JSON_SOFT_404                     = '{"error":"404"}';

const HTTP_STATUS_OK                    = 200;
const HTTP_STATUS_NOT_MODIFIED          = 304;
const HTTP_STATUS_NOT_FOUND             = 404;
const HTTP_STATUS_METHOD_NOT_ALLOWED    = 405;
const HTTP_STATUS_INTERNAL_SERVER_ERROR = 500;
const HTTP_STATUS_BAD_GATEWAY           = 502;

const EXAMPLES = [
    'example/via-core'         => '{"1.0.0":0,"1.0.1":0,"1.0.2":0}',
    'example/via-core/1.0.0'   => '{"dependencies":{"example/wants-core": "1.0.0 <= v < 1.0.1"}}',
    'example/via-core/1.0.1'   => '{"dependencies":{"example/wants-core": "1.0.1 <= v < 1.0.2"}}',
    'example/via-core/1.0.2'   => '{"dependencies":{"example/wants-core": "1.0.2 <= v < 1.0.3"}}',

    'example/via-http'         => '{"1.0.0":0,"2.0.0":0,"3.0.0":0}',
    'example/via-http/1.0.0'   => '{"dependencies":{"example/wants-http": "1.0.0 <= v < 2.0.0"}}',
    'example/via-http/2.0.0'   => '{"dependencies":{"example/wants-http": "2.0.0 <= v < 3.0.0"}}',
    'example/via-http/3.0.0'   => '{"dependencies":{"example/wants-http": "3.0.0 <= v < 4.0.0"}}',

    'example/wants-core'       => '{"1.0.0":0,"1.0.1":0,"1.0.2":0}',
    'example/wants-core/1.0.0' => '{"dependencies":{"elm/core": "1.0.0 <= v < 1.0.1"}}',
    'example/wants-core/1.0.1' => '{"dependencies":{"elm/core": "1.0.1 <= v < 1.0.2"}}',
    'example/wants-core/1.0.2' => '{"dependencies":{"elm/core": "1.0.2 <= v < 1.0.3"}}',

    'example/wants-http'       => '{"1.0.0":0,"2.0.0":0,"3.0.0":0}',
    'example/wants-http/1.0.0' => '{"dependencies":{"elm/http": "1.0.0 <= v < 2.0.0"}}',
    'example/wants-http/2.0.0' => '{"dependencies":{"elm/http": "2.0.0 <= v < 3.0.0"}}',
    'example/wants-http/3.0.0' => '{"dependencies":{"elm/http": "3.0.0 <= v < 4.0.0"}}',
];

# ================================================================================
# FUNCTIONS

/**
 * Fetch file from remote url and save to cache.
 *
 * Return simplified status code with cached data.
 *
 * Possible return values:
 * - [ HTTP_STATUS_OK         , $data ]
 * - [ HTTP_STATUS_NOT_FOUND  , null  ]
 * - [ HTTP_STATUS_BAD_GATEWAY, null  ]
 *
 */
# -------===========---------------------------
function FetchRemote($filePath, $url, $logId) {
# -------===========---------------------------
    $dirname      = dirname($filePath);
    $notFoundPath = $filePath . '.404';
    $etagPath     = $filePath . '.etag';
    $statusPath   = $filePath . '.status';
    $lockPath     = $filePath . '.fetching';

    $beginFloatTime = microtime(true);

    # MKDIR
    if (! file_exists($dirname)) {
        if (! mkdir($dirname, 0777, true)) throw new Exception("mkdir");
    }

    $timeBeforeLock = time();

    # ACQUIRE LOCK
#    RemoteLog(null, ">", null, $logId);
    $lock = fopen($lockPath, "c");
    if ($lock === false) throw new Exception("fopen - lockPath");
    flock($lock, LOCK_EX);

    try {
        # IF OTHER PROCESS JUST DID REMOTE FETCH, RETURN SAME THING
        if (file_exists($statusPath) && filemtime($statusPath) >= $timeBeforeLock) {
            $prevStatusStr = FileGetContentsOrThrow($statusPath, "statusPath");
            switch ($prevStatusStr) {
                case HTTP_STATUS_OK:
                    $data = FileGetContentsOrThrow($filePath, "filePath, re-OK");                    
                    $return = [ HTTP_STATUS_OK, $data ];
                    break;

                case HTTP_STATUS_NOT_FOUND:
                    $return = [ HTTP_STATUS_NOT_FOUND, null ];
                    break;

                case HTTP_STATUS_BAD_GATEWAY:
                    $return = [ HTTP_STATUS_BAD_GATEWAY, null ];
                    break;

                default:
                    throw new Exception('invalid previous status');
            }

            RemoteLog($beginFloatTime, "*", $return[0], $logId);
            return $return;
        }

        # OPTIONS
        $headers = [];
        $isConditionalRequest = false;
        if (file_exists($filePath) && file_exists($etagPath)) {
            $oldEtag = FileGetContentsOrThrow($etagPath, "etagPath");            

            if ($oldEtag !== "") {
                array_push($headers, "If-None-Match: \"$oldEtag\"");
                $isConditionalRequest = true;
            }
        }
        $options = [
            'http' => [
              'ignore_errors' => true,
              'header' => $headers,
            ]
        ];

        # FETCH
        $context = stream_context_create($options);
        $data    = file_get_contents($url, false, $context);
        list ($status, $etag) = ParseResponseHeaders($http_response_header);

        # HANDLE
        if ($status === HTTP_STATUS_OK) {
            if ($data === false) throw new Exception("file_get_contents ok, no data");

            # REMOVE 404
            if (file_exists($notFoundPath)) {
                if (! unlink($notFoundPath)) throw new Exception("unlink 404");
            }

            # SAVE TO CACHE
            $tmpPath = $filePath . '.tmp-' . microtime(true);
            try {
                if (file_put_contents($tmpPath, $data) !== strlen($data))
                    throw new Exception("file_put_contents - data");
                if (file_exists($filePath)) {
                    if (! unlink($filePath)) throw new Exception("unlink");
                }
                if (! rename($tmpPath, $filePath)) throw new Exception("rename");            
            } finally {
                @unlink($tmpPath);
            }

            # SAVE ETAG
            $etagOrEmpty = $etag ?? "";
            if (file_put_contents($etagPath, $etagOrEmpty) !== strlen($etagOrEmpty))
                throw new Exception("file_put_contents - etag");

            $return = [ HTTP_STATUS_OK, $data ];

        } elseif ($status === HTTP_STATUS_NOT_MODIFIED) {
            if (! touch($etagPath)) throw new Exception("touch etagPath");

            if (! file_exists($filePath)) throw new Exception("not modified -> ! file_exists");
            $data = FileGetContentsOrThrow($filePath, "filePath, NotModified");

            $return = [ HTTP_STATUS_OK, $data ];

        } elseif ($status === HTTP_STATUS_NOT_FOUND) {
            if (! touch($notFoundPath)) throw new Exception("touch notFoundPath");
            if (! touch($etagPath)) throw new Exception("touch etagPath");

            $return = [ HTTP_STATUS_NOT_FOUND, null ];

        } else {
            if (! touch($etagPath)) throw new Exception("touch etagPath");

            $return = [ HTTP_STATUS_BAD_GATEWAY, null ];
        }

        # SAVE STATUS
        if (file_put_contents($statusPath, $return[0]) !== strlen($return[0]))
            throw new Exception('file_put_contents - statusPath');

        RemoteLog($beginFloatTime, $isConditionalRequest ? "C" : "-", $status, $logId);

        # RETURN
        return $return;

    } finally {
#        RemoteLog(null, "<", null, $logId);
        flock($lock, LOCK_UN);
        fclose($lock);
        @unlink($lockPath);
    }
}

# -------======================-----------------------
function FileGetContentsOrThrow($path, $description) {
# -------======================-----------------------
    if (! file_exists($path)) throw new Exception("file_get_contents > file_exists: $description");
    $data = file_get_contents($path);
    if ($data === false) throw new Exception("file_get_contents: $description");
    return $data;
}

/**
 * Convert unixtime to format used e.g. by headers
 *   "Expires", "If-Modified-Since" and "Last-Modified"
 */
# -------========---------
function HttpDate($time) {
# -------========---------
    return gmdate("D, d M Y H:i:s", $time) . " GMT";
}

# -------====================------------
function ParseResponseHeaders($headers) {
# -------====================------------
    $status = null;
    $etag   = null;

    foreach ($headers as $header) {
        # In case of redirected request, there can be several HTTP status lines.
        if (preg_match('#^HTTP/\d\.\d\s(\d{3}) #', $header, $m) === 1) {
            $status = intval($m[1]);
        }
        if (preg_match('#^ETag:\s*(?:W\\\\)?"?(.+?)"?$#i', $header, $m) === 1) {
            $etag = $m[1];
        }
    }

    if ($status === null) throw new Exception('no status found in http_response_header');

    return [ $status, $etag ];
}


# -------=========-------------------------------------------
function RemoteLog($beginFloatTime, $type, $status, $logId) {
# -------=========-------------------------------------------
    list ($usecAsSec, $sec) = explode(" ", microtime());
    $usec    = intval($usecAsSec * 1000 * 1000);
    $logFile = DIR_LOG . '/' . gmdate("Y-m-d", $sec);
    $logLine = sprintf("%s.%06s  %5s  %5s %s  %s %s\n",
        gmdate("Y-m-d H:i:s", $sec),
        $usec,
        ($beginFloatTime == null ? "" : intval((microtime(true) - $beginFloatTime) * 1000)),
        getmypid(),
        $type,
        $status ?? "---",
        $logId
    );
    $fh = fopen($logFile, "a");
    if (flock($fh, LOCK_EX)) {
        fwrite($fh, $logLine);
        fflush($fh);
        flock($fh, LOCK_UN);
    }
    fclose($fh);
}

# -------=====--------------------------------------
function Serve($author, $project, $version = null) {
# -------=====--------------------------------------

    # ========================================
    # GET ALL-PACKAGES

    if (! file_exists(PATH_ALL_PACKAGES)
            || time() - filemtime(PATH_ALL_PACKAGES) > REMOTE_NORMAL_EXPIRY_SECONDS) {
        list ($allPackagesStatus, $allPackagesJson) =
            FetchRemote(PATH_ALL_PACKAGES, URL_ALL_PACKAGES, "ALL PACKAGES");
        if ($allPackagesJson === null) throw new Exception('FetchRemote - all packages');
    } else {
        $allPackagesJson = FileGetContentsOrThrow(PATH_ALL_PACKAGES, "all packages");
    }

    $allPackages = json_decode($allPackagesJson, true);
    if ($allPackages === null) throw new Exception("json_decode - all packages");

    # ========================================
    # VALIDATE REQUEST

    # VALIDATE AUTHOR/PROJECT
    if (! array_key_exists("$author/$project", $allPackages)) {
        ServePageNotFound(true);
        return;
    }

    # VALIDATE VERSION
    if ($version !== null && ! in_array($version, $allPackages["$author/$project"])) {
        ServePageNotFound(true);
        return;       
    }

    # ========================================
    # SETUP

    if ($version === null) {
        $logId        = "$author/$project";
        $filePath     = DIR_CACHE . "/$author/$project/releases.json";
        $remoteUrl    = "https://package.elm-lang.org/packages/$author/$project/releases.json";
        $remoteExpiry = REMOTE_NORMAL_EXPIRY_SECONDS;

    } else {
        $logId        = "$author/$project/$version";
        $filePath     = DIR_CACHE . "/$author/$project/$version.json";
        $remoteUrl    = "https://raw.githubusercontent.com/$author/$project/$version/elm.json";
        $remoteExpiry = REMOTE_LONG_EXPIRY_SECONDS;
    }

    $notFoundPath = $filePath . '.404';
    $etagPath     = $filePath . '.etag';

    # ========================================
    # SERVE

    if (file_exists($notFoundPath) && time() - filemtime($notFoundPath) < REMOTE_NOTFOUND_EXPIRY_SECONDS) {
        # SERVE CACHED NOT FOUND

        ServePageNotFound(true);
        return;

    } elseif (file_exists($filePath) && file_exists($etagPath)
            && time() - filemtime($etagPath) < $remoteExpiry) {
        # SERVE CACHED

        $data = FileGetContentsOrThrow($filePath, "filePath, cached");

        ServeJson($data, filemtime($filePath), true);
        return;

    } else {
        # FETCH FROM REMOTE SERVER

        list ($status, $data) = FetchRemote($filePath, $remoteUrl, $logId);

        if ($status === HTTP_STATUS_OK) {
            ServeJson($data, filemtime($filePath));
            return;

        } elseif ($status === HTTP_STATUS_NOT_FOUND) {
            ServePageNotFound(true);
            return;

        } else {
            ServeBadGateway();
            return;
        }
    } 
}

# -------===============----
function ServeBadGateway() {
# -------===============----
    header('Access-Control-Allow-Origin: *');
    http_response_code(HTTP_STATUS_BAD_GATEWAY);
}

# -------=========--------------------------------------------
function ServeJson($data, $mtime, $maybeNotModified = false) {
# -------=========--------------------------------------------

    $dateLastmod = HttpDate($mtime);
    $dateExpires = HttpDate(time() + CLIENT_EXPIRY_SECONDS);
    $etag        = md5($data);

    if ($maybeNotModified) {
        $ifModifiedSince = $_SERVER['HTTP_IF_MODIFIED_SINCE'] ?? false;
        $ifNoneMatch     = $_SERVER['HTTP_IF_NONE_MATCH'    ] ?? false;

        // If-Modified-Since is ignored if used with If-None-Match
        // > https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/If-Modified-Since

        if ($ifNoneMatch !== false) {
            // remove possible "W/" prefix
            if (substr($ifNoneMatch, 0, 2) === "W/") $ifNoneMatch = substr($ifNoneMatch, 0);

            // remove possible quotes (only " is actually allowed by specification)
            $ifNoneMatch = rtrim(ltrim($ifNoneMatch, "'\""), "'\"");

            if ($ifNoneMatch === $etag) {
                ServeNotModified($dateExpires, $etag);
                return;                
            }
        
        } elseif ($ifModifiedSince !== false && @strtotime($ifModifiedSince) >= $mtime) {
            ServeNotModified($dateExpires, $etag);
            return;
        }
    }

    header("Cache-Control: public, max-age=" . CLIENT_EXPIRY_SECONDS);
    header("Last-Modified: $dateLastmod");
    header("Expires: $dateExpires");
    header("ETag: \"$etag\"");

    header('Content-Type: application/json');
    header('Content-Length: ' . strlen($data));

    header('Access-Control-Allow-Origin: *');

    echo $data;
}

# -------=====================----
function ServeMethodNotAllowed() {
# -------=====================----
    http_response_code(HTTP_STATUS_METHOD_NOT_ALLOWED);
}

# -------================-----------------------
function ServeNotModified($dateExpires, $etag) {
# -------================-----------------------
    header("Cache-Control: public, max-age=" . CLIENT_EXPIRY_SECONDS);
    header("Expires: $dateExpires");
    header("ETag: \"$etag\"");

    header('Access-Control-Allow-Origin: *');
    http_response_code(HTTP_STATUS_NOT_MODIFIED);
}

# -------=================-----------------
function ServePageNotFound($soft = false) {
# -------=================-----------------
    if ($soft) {
        # soft errors are cached better than actual errors
        ServeJson(JSON_SOFT_404, time(), true);

    } else {
        header('Access-Control-Allow-Origin: *');
        http_response_code(HTTP_STATUS_NOT_FOUND);
    }
}

# ================================================================================
# MAIN

try {
    $method = $_SERVER['REQUEST_METHOD'];
    $query  = $_SERVER['QUERY_STRING'];

    if ($method !== "GET") {
        ServeMethodNotAllowed();
        exit;
    }

    # EXAMPLES
    if (array_key_exists($query, EXAMPLES)) {
        ServeJson(EXAMPLES[$query], time());
        exit;
    }

    # TODO: what characters are allowed for AUTHOR & PROJECT

    # "AUTHOR/PROJECT"
    if (preg_match("/^([0-9a-zA-Z-]+)\/([0-9a-zA-Z-]+)$/", $query, $m) === 1) {
        $author    = $m[1];
        $project   = $m[2];

        Serve($author, $project);
        exit;
    }

    # "AUTHOR/PROJECT/VERSION"
    if (preg_match("/^([0-9a-zA-Z-]+)\/([0-9a-zA-Z-]+)\/(\d+\.\d+\.\d+)$/", $query, $m) === 1) {
        $author    = $m[1];
        $project   = $m[2];
        $version   = $m[3];

        Serve($author, $project, $version);
        exit;
    }

    # SERVE NOT FOUND
    ServePageNotFound();
    exit;

} catch (Throwable $e) {
    header('Content-Type: text/plain');
    echo "ERROR";
    #echo $e->getMessage();
    http_response_code(HTTP_STATUS_INTERNAL_SERVER_ERROR);
}

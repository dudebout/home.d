exif-date () {
    exiftool -s3 -d '%Y-%m-%dT%H:%M:%S%z' -DateTimeOriginal "$1"
}

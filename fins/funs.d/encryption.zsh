encrypt-file () {
    file=$1
    openssl aes-256-cbc -in "$file" -out "$file.encrypted"
}

decrypt-file () {
    file=$1
    openssl aes-256-cbc -d -in "$file" -out "${file%.encrypted}"
}

# It is important to specify the message digest (-md) for portability:
# the default digest was changed from MD5 to SHA256 in Openssl 1.1.

encrypt-file () {
    file=$1
    openssl aes-256-cbc -md sha256 -in "$file" -out "$file.encrypted"
}

decrypt-file () {
    file=$1
    openssl aes-256-cbc -md sha256 -d -in "$file" -out "${file%.encrypted}"
}

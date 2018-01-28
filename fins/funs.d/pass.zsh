__pass-jq () {
    local selector="$1"
    local pass_name="$2"

    local old=$(xclip -o)
    (
        # TODO make sure this is safe if you exit the terminal
        pass show "$pass_name" | jq --raw-output "$selector" | xclip -i
        sleep 45
        echo $old | xclip -i
    ) &
}

pass-url () {
    __pass-jq .url "$1"
}

pass-username () {
    __pass-jq .username "$1"
}

pass-password () {
    __pass-jq .password "$1"
}

pass-notes () {
    __pass-jq .notes "$1"
}

pass-security-questions () {
    __pass-jq '.security_questions | .[] | "\(.question): \"\(.answer)\""' "$1"

}

pass-security-question () {
    # TODO grep on $2 instead of using it as a number
    __pass-jq '.security_questions | .['"$2"'] | "\(.answer)"' "$1"

}

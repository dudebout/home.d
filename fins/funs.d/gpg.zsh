empty-gpg-agent () {
    echo RELOADAGENT | gpg-connect-agent
}

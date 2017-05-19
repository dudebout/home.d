replug-usb () {
    local rel_dev=$1 # example zfrog0 or sdc

    local abs_dev=$(readlink --canonicalize /dev/$rel_dev)
    local block=$(echo $abs_dev | sed -n 's#/dev/\(sd[a-z]\)#\1#p')

    if [[ $block = '' ]]; then
        echo 'No block device found'
        return
    fi

    local sys_entry=$(readlink --canonicalize /sys/block/$block)

    for usb_storage in $(cd /sys/bus/usb/drivers/usb-storage; ls -d *:*); do
        if echo $sys_entry | grep -q $usb_storage; then
            echo "Replugging $usb_storage ($rel_dev -> $abs_dev)"
            sudo bash -c "echo $usb_storage > /sys/bus/usb/drivers/usb-storage/unbind"
            sudo bash -c "echo $usb_storage > /sys/bus/usb/drivers/usb-storage/bind"
            return
        fi
    done

    echo 'USB device not found'
}

// github:ivoarch/.dot-org-files/conkeror.org

set_protocol_handler("org-protocol", find_file_in_path("emacsclient"));
set_user_agent("Mozilla/5.0 (X11; Linux x86_64; rv:54.0) Gecko/20100101 Firefox/54.0");
define_webjump("read", "javascript:location.href='org-protocol://capture?template=r&url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)+'&body='+encodeURIComponent(window.getSelection())");
url_remoting_fn = load_url_in_new_buffer;
hints_display_url_panel = false;
hints_minibuffer_annotation_mode(true);

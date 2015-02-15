package require Ffidl

if {[catch {
    ffidl::callout OpenImage {pointer-utf8} int [ffidl::symbol fakeimglib.so openimage]
}]} then {
    # Create the OpenImage command by other means here...
}
set handle [OpenImage "/the/file/name"]

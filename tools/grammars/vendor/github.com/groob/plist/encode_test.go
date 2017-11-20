package plist

import (
	"bytes"
	"testing"
	"time"
)

var fooRef = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><string>foo</string></plist>
`

var utf8Ref = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><string>UTF-8 ☼</string></plist>
`

var zeroRef = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><integer>0</integer></plist>
`

var oneRef = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><integer>1</integer></plist>
`

var minOneRef = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><integer>-1</integer></plist>
`

var realRef = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><real>1.2</real></plist>
`

var falseRef = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><false></false></plist>
`

var trueRef = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><true></true></plist>
`

var arrRef = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><array><string>a</string><string>b</string><string>c</string><integer>4</integer><true></true></array></plist>
`

var byteArrRef = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><array><data>/////////////////////w==</data></array></plist>
`

var time1900Ref = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><date>1900-01-01T12:00:00Z</date></plist>
`

var dataRef = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><data>PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPCFET0NUWVBFIHBsaXN0IFBVQkxJQyAiLS8vQXBwbGUvL0RURCBQTElTVCAxLjAvL0VOIiAiaHR0cDovL3d3dy5hcHBsZS5jb20vRFREcy9Qcm9wZXJ0eUxpc3QtMS4wLmR0ZCI+CjxwbGlzdCB2ZXJzaW9uPSIxLjAiPjxzdHJpbmc+Zm9vPC9zdHJpbmc+PC9wbGlzdD4K</data></plist>
`

var emptyDataRef = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><data></data></plist>
`

var dictRef = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict><key>bool</key><true></true><key>foo</key><string>bar</string></dict></plist>
`

var indentRef = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
   <dict>
      <key>CFBundleInfoDictionaryVersion</key>
      <string>6.0</string>
      <key>band-size</key>
      <integer>8388608</integer>
      <key>bundle-backingstore-version</key>
      <integer>1</integer>
      <key>diskimage-bundle-type</key>
      <string>com.apple.diskimage.sparsebundle</string>
      <key>size</key>
      <integer>4398046511104</integer>
   </dict>
</plist>
`

var indentRefOmit = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
   <dict>
      <key>CFBundleInfoDictionaryVersion</key>
      <string>6.0</string>
      <key>bundle-backingstore-version</key>
      <integer>1</integer>
      <key>diskimage-bundle-type</key>
      <string>com.apple.diskimage.sparsebundle</string>
      <key>size</key>
      <integer>4398046511104</integer>
   </dict>
</plist>
`

var encodeTests = []struct {
	in  interface{}
	out string
}{
	{"foo", fooRef},
	{"UTF-8 ☼", utf8Ref},
	{0, zeroRef},
	{1, oneRef},
	{uint64(1), oneRef},
	{-1, minOneRef},
	{1.2, realRef},
	{false, falseRef},
	{true, trueRef},
	{[]interface{}{"a", "b", "c", 4, true}, arrRef},
	{time.Date(1900, 01, 01, 12, 00, 00, 0, time.UTC), time1900Ref},
	{[]byte(fooRef), dataRef},
	{map[string]interface{}{
		"foo":  "bar",
		"bool": true},
		dictRef},
	{struct {
		Foo  string `plist:"foo"`
		Bool bool   `plist:"bool"`
	}{"bar", true},
		dictRef},
	{[][16]byte{
		[16]byte{0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF},
	}, byteArrRef},
}

func TestEncodeValues(t *testing.T) {
	for _, tt := range encodeTests {
		b, err := Marshal(tt.in)
		if err != nil {
			t.Error(err)
			continue
		}
		out := string(b)
		if out != tt.out {
			t.Errorf("Marshal(%v) = \n%v, \nwant\n %v", tt.in, out, tt.out)
		}
	}
}

func TestNewLineString(t *testing.T) {
	multiline := struct {
		Content string
	}{
		Content: "foo\nbar",
	}

	b, err := MarshalIndent(multiline, "   ")
	if err != nil {
		t.Fatal(err)
	}
	var ok = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
   <dict>
      <key>Content</key>
      <string>foo
bar</string>
   </dict>
</plist>
`
	out := string(b)
	if out != ok {
		t.Errorf("Marshal(%v) = \n%v, \nwant\n %v", multiline, out, ok)
	}

}

func TestIndent(t *testing.T) {
	sparseBundleHeader := struct {
		InfoDictionaryVersion string `plist:"CFBundleInfoDictionaryVersion"`
		BandSize              uint64 `plist:"band-size"`
		BackingStoreVersion   int    `plist:"bundle-backingstore-version"`
		DiskImageBundleType   string `plist:"diskimage-bundle-type"`
		Size                  uint64 `plist:"size"`
	}{
		InfoDictionaryVersion: "6.0",
		BandSize:              8388608,
		Size:                  4 * 1048576 * 1024 * 1024,
		DiskImageBundleType:   "com.apple.diskimage.sparsebundle",
		BackingStoreVersion:   1,
	}
	b, err := MarshalIndent(sparseBundleHeader, "   ")
	if err != nil {
		t.Fatal(err)
	}
	out := string(b)
	if out != indentRef {
		t.Errorf("MarshalIndent(%v) = \n%v, \nwant\n %v", sparseBundleHeader, out, indentRef)
	}
}

func TestOmitNotEmpty(t *testing.T) {
	sparseBundleHeader := struct {
		InfoDictionaryVersion string `plist:"CFBundleInfoDictionaryVersion"`
		BandSize              uint64 `plist:"band-size,omitempty"`
		BackingStoreVersion   int    `plist:"bundle-backingstore-version"`
		DiskImageBundleType   string `plist:"diskimage-bundle-type"`
		Size                  uint64 `plist:"size"`
	}{
		InfoDictionaryVersion: "6.0",
		BandSize:              8388608,
		Size:                  4 * 1048576 * 1024 * 1024,
		DiskImageBundleType:   "com.apple.diskimage.sparsebundle",
		BackingStoreVersion:   1,
	}
	b, err := MarshalIndent(sparseBundleHeader, "   ")
	if err != nil {
		t.Fatal(err)
	}
	out := string(b)
	if out != indentRef {
		t.Errorf("MarshalIndent(%v) = \n%v, \nwant\n %v", sparseBundleHeader, out, indentRef)
	}
}

func TestOmitIsEmpty(t *testing.T) {
	sparseBundleHeader := struct {
		InfoDictionaryVersion string `plist:"CFBundleInfoDictionaryVersion"`
		BandSize              uint64 `plist:"band-size,omitempty"`
		BackingStoreVersion   int    `plist:"bundle-backingstore-version"`
		DiskImageBundleType   string `plist:"diskimage-bundle-type"`
		Size                  uint64 `plist:"size"`
	}{
		InfoDictionaryVersion: "6.0",
		Size:                4 * 1048576 * 1024 * 1024,
		DiskImageBundleType: "com.apple.diskimage.sparsebundle",
		BackingStoreVersion: 1,
	}
	b, err := MarshalIndent(sparseBundleHeader, "   ")
	if err != nil {
		t.Fatal(err)
	}
	out := string(b)
	if out != indentRefOmit {
		t.Errorf("MarshalIndent(%v) = \n%v, \nwant\n %v", sparseBundleHeader, out, indentRefOmit)
	}
}

type marshalerTest struct {
	marshalFuncInvoked bool
	MustMarshal        string
}

func (m *marshalerTest) MarshalPlist() (interface{}, error) {
	m.marshalFuncInvoked = true
	return &m.MustMarshal, nil
}

func TestMarshaler(t *testing.T) {
	want := []byte(`<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><string>pants</string></plist>
`)
	m := marshalerTest{MustMarshal: "pants"}
	have, err := Marshal(&m)
	if err != nil {
		t.Fatal(err)
	}

	if !bytes.Equal(have, want) {
		t.Errorf("expected \n%s got \n%s\n", have, want)
	}
}

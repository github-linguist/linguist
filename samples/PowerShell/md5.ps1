$string = "The quick brown fox jumped over the lazy dog's back"
$data = [Text.Encoding]::UTF8.GetBytes($string)
$hash = [Security.Cryptography.MD5]::Create().ComputeHash($data)
([BitConverter]::ToString($hash) -replace '-').ToLower()
